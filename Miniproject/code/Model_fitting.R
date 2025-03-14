#!/usr/bin/Rscript

#### 1. Data Management ####
rm(list = ls())  # Clear the environment

# Load required packages
library(minpack.lm)  # For non-linear least squares
library(parallel)    # For parallel processing
library(tidyverse)   # For data manipulation
library(AICcmodavg)  # For calculating AICc and Akaike Weights

# Read in data (modify path as appropriate)
data <- read.csv("../data/LogisticGrowthData.csv")

# Create the group_id column for each experiment
data <- data %>%
  mutate(
    unique_id = paste(Temp, Species, Medium, Rep, Citation, sep = "_"),
    group_id = cumsum(c(TRUE, unique_id[-1] != unique_id[-length(unique_id)]))
  )

# Identify all unique group IDs
group_list <- unique(data$group_id)

# Remove subsets with negative PopBio values
data <- data[data$PopBio >= 0, ]

# Create a column of log-transformed population data using the natural logarithm
data$LogPopBio <- log(data$PopBio)
data <- na.omit(data)  # Remove any rows with NA

# Split the data into subsets by group_id
data.subsets <- group_split(data, group_id)

#### 2. Model Definitions ####

# Logistic model (redefined for log-space)
logistic.logspace <- function(t, r_max, log_K, log_N0) {
  K <- exp(log_K)
  N0 <- exp(log_N0)
  log_Nt <- log(N0) + log(K) + r_max * t - log(K + N0 * (exp(r_max * t) - 1))
  return(log_Nt)
}

# Four-parameter Gompertz model (fitted in log space) **FIXED**
gompertz.function <- function(t, r_max, K, N_0, t_lag) {
  N_0 + (K - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t) / ((K - N_0) * log(10)) + 1))
}

#### 3. Model Fitting Functions ####

# Global threshold: skip fitting any models if there are fewer than 6 datapoints
global_threshold <- 6

# Function to fit all four models for a given subset
fit.all.models <- function(x) {
  if(nrow(x) < global_threshold) {
    return(list(logistic = NULL, gompertz = NULL, quad = NULL, cubic = NULL))
  }
  
  # Fit quadratic and cubic models on log-transformed data
  fit.quad <- lm(LogPopBio ~ poly(Time, 2), data = x)
  fit.cubic <- lm(LogPopBio ~ poly(Time, 3), data = x)
  
  # Initialize best fits for logistic and Gompertz models
  max.Rsq.logit <- -Inf
  best_fit_logit <- NULL
  max.Rsq.gompertz <- -Inf
  best_fit_gompertz <- NULL
  
  set.seed(123)
  for (i in 1:100) {
    ## Logistic Model (fitted in log-space)
    log_K_start <- rnorm(1, mean = max(x$LogPopBio), sd = 0.1)
    log_N0_start <- rnorm(1, mean = min(x$LogPopBio), sd = 0.1)
    
    fit.logit.try <- tryCatch(
      nlsLM(
        LogPopBio ~ logistic.logspace(Time, r_max, log_K, log_N0),
        data = x,
        start = list(r_max = 0.1, log_K = log_K_start, log_N0 = log_N0_start),
        lower = c(r_max = -0.5, log_K = -Inf, log_N0 = -Inf),
        control = nls.lm.control(maxiter = 100),
        trace = FALSE
      ),
      error = function(e) NULL
    )
    
    if (!is.null(fit.logit.try)) {
      res.logit <- x$LogPopBio - predict(fit.logit.try, x)
      RSS.logit <- sum(res.logit^2)
      TSS.logit <- sum((x$LogPopBio - mean(x$LogPopBio))^2)
      Rsq.logit <- 1 - RSS.logit / TSS.logit
      if (Rsq.logit > max.Rsq.logit) {
        max.Rsq.logit <- Rsq.logit
        best_fit_logit <- fit.logit.try
      }
    }
    
    ## Four-parameter Gompertz Model (fitted in log space)
    t_lag_start <- x$Time[which.max(diff(diff(x$LogPopBio)))]
    gK_start <- rnorm(1, mean = max(x$LogPopBio), sd = 0.1)
    gN_0_start <- rnorm(1, mean = min(x$LogPopBio), sd = 0.1)
    
    fit.gomp.try <- tryCatch(
      nlsLM(
        LogPopBio ~ gompertz.function(Time, r_max, K, N_0, t_lag),
        data = x,
        start = list(t_lag = t_lag_start, r_max = 0.1, N_0 = gN_0_start, K = gK_start),
        lower = c(r_max = 0, N_0 = -150, K = -150, t_lag = 0),
        control = nls.lm.control(maxiter = 100),
        trace = FALSE
      ),
      error = function(e) NULL
    )
    
    if (!is.null(fit.gomp.try)) {
      res.gomp <- x$LogPopBio - predict(fit.gomp.try, x)
      RSS.gomp <- sum(res.gomp^2)
      TSS.gomp <- sum((x$LogPopBio - mean(x$LogPopBio))^2)
      Rsq.gomp <- 1 - RSS.gomp / TSS.gomp
      if (Rsq.gomp > max.Rsq.gompertz) {
        max.Rsq.gompertz <- Rsq.gomp
        best_fit_gompertz <- fit.gomp.try
      }
    }
  }
  
  return(list(
    logistic = best_fit_logit,
    gompertz = best_fit_gompertz,
    quad = fit.quad,
    cubic = fit.cubic
  ))
}

# Function to compute R² and AICc for each model
compute.metrics <- function(models, x) {
  df <- data.frame(
    group_id = unique(x$group_id),
    logistic.R2 = NA, logistic.AICc = NA,
    gompertz.R2 = NA, gompertz.AICc = NA,
    quad.R2 = NA, quad.AICc = NA,
    cubic.R2 = NA, cubic.AICc = NA
  )
  
  # Logistic model metrics (log-space)
  fit.logit <- models$logistic
  if (!is.null(fit.logit)) {
    res.logit <- x$LogPopBio - predict(fit.logit, x)  # Direct residuals
    RSS.logit <- sum(res.logit^2)
    TSS.logit <- sum((x$LogPopBio - mean(x$LogPopBio))^2)
    R2.logit <- 1 - RSS.logit / TSS.logit
    df$logistic.R2 <- R2.logit
    df$logistic.AICc <- AICc(fit.logit)
  }
  
  # Gompertz model metrics (log-space)
  fit.gomp <- models$gompertz
  if (!is.null(fit.gomp)) {
    res.gomp <- x$LogPopBio - predict(fit.gomp, x)
    RSS.gomp <- sum(res.gomp^2)
    TSS.gomp <- sum((x$LogPopBio - mean(x$LogPopBio))^2)
    R2.gomp <- 1 - RSS.gomp / TSS.gomp
    df$gompertz.R2 <- R2.gomp
    df$gompertz.AICc <- AICc(fit.gomp)
  }
  
  # Quadratic model metrics (log-space)
  fit.quad <- models$quad
  if (!is.null(fit.quad)) {
    res.quad <- x$LogPopBio - predict(fit.quad, x)
    RSS.quad <- sum(res.quad^2)
    TSS.quad <- sum((x$LogPopBio - mean(x$LogPopBio))^2)
    R2.quad <- 1 - RSS.quad / TSS.quad
    df$quad.R2 <- R2.quad
    df$quad.AICc <- AICc(fit.quad)
  }
  
  # Cubic model metrics (log-space)
  fit.cubic <- models$cubic
  if (!is.null(fit.cubic)) {
    res.cubic <- x$LogPopBio - predict(fit.cubic, x)
    RSS.cubic <- sum(res.cubic^2)
    TSS.cubic <- sum((x$LogPopBio - mean(x$LogPopBio))^2)
    R2.cubic <- 1 - RSS.cubic / TSS.cubic
    df$cubic.R2 <- R2.cubic
    df$cubic.AICc <- AICc(fit.cubic)
  }
  
  #### Calculate Akaike Weights ####
  # Replace NA with Inf for models that failed to fit
  df$logistic.AICc[is.na(df$logistic.AICc)] <- Inf
  df$gompertz.AICc[is.na(df$gompertz.AICc)] <- Inf
  df$quad.AICc[is.na(df$quad.AICc)] <- Inf
  df$cubic.AICc[is.na(df$cubic.AICc)] <- Inf
  
  AICc_values <- c(df$logistic.AICc, df$gompertz.AICc, df$quad.AICc, df$cubic.AICc)
  min_AICc <- min(AICc_values, na.rm = TRUE)
  delta_AICc <- AICc_values - min_AICc
  exp_AICc <- exp(-0.5 * delta_AICc)
  sum_exp_AICc <- sum(exp_AICc, na.rm = TRUE)
  Akaike_weights <- exp_AICc / sum_exp_AICc
  
  df$Akaike_weight_logistic <- Akaike_weights[1]
  df$Akaike_weight_gompertz <- Akaike_weights[2]
  df$Akaike_weight_quad <- Akaike_weights[3]
  df$Akaike_weight_cubic <- Akaike_weights[4]
  
  return(df)
}

#### 4. Run Model Fitting for Each Subset ####
fit.function <- function(x) {
  if(nrow(x) < global_threshold) {
    return(data.frame(
      group_id = unique(x$group_id),
      logistic.R2 = NA, logistic.AICc = NA,
      gompertz.R2 = NA, gompertz.AICc = NA,
      quad.R2 = NA, quad.AICc = NA,
      cubic.R2 = NA, cubic.AICc = NA,
      Akaike_weight_logistic = NA,
      Akaike_weight_gompertz = NA,
      Akaike_weight_quad = NA,
      Akaike_weight_cubic = NA
    ))
  }
  
  models <- fit.all.models(x)
  metrics <- compute.metrics(models, x)
  return(metrics)
}

#### 5. Execute Analysis ####
print("Beginning model fitting...")
poss.fit.function <- possibly(.f = fit.function, quiet = TRUE)
Results <- bind_rows(mclapply(data.subsets, poss.fit.function, mc.cores = detectCores()))

# Extract unique (group_id, PopBio_units) pairs
data_units <- data %>%
  distinct(group_id, PopBio_units)

# Join these onto Results table
Results <- Results %>%
  left_join(data_units, by = "group_id")

# Extract unique (group_id, Temp) paris
data_temp <- data %>%
  distinct(group_id, Temp)

# Join these onto Results table
Results <- Results %>%
  left_join(data_temp, by = "group_id")

print("Model fitting complete!")

# Save the results to a CSV file
write.csv(Results, file = "../results/model_fitting_results.csv", row.names = FALSE)
MFR <- read.csv("../results/model_fitting_results.csv", header = TRUE)
View(MFR)
