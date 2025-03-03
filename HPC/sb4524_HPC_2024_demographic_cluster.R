# CMEE 2024 HPC exercises R code pro forma
# For stochastic demographic model cluster run

rm(list=ls()) # good practice 
graphics.off() # turns off graphics
source("Demographic.R") # sources all functions needed
source("sb4524_HPC_2024_main.R")
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX")) # read in job number
#iter <- 3 # hash this out later

# set seed for reproducibility 
set.seed(iter)

# define growth/reproduction/projection matrix
growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0, 0.5, 0.4, 0.0, 0.0, 0.0, 0.4, 0.7, 0.0, 0.0, 0.0, 0.25, 0.4), nrow = 4, ncol = 4, byrow = TRUE)
reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), nrow = 4, ncol = 4, byrow = TRUE)
projection_matrix <- growth_matrix + reproduction_matrix

# define clutch distribution
clutch_distribution <- c(0.06, 0.08, 0.13, 0.15, 0.16, 0.18, 0.15, 0.06, 0.03)

# define simulation length
simulation_length <- 120

# define number of simulations
num_simulations <- 150

# starting condition list
num_stages <- 4
initial_conditions <- list(
  "large_adult" = state_initialise_adult(num_stages, initial_size = 100),
  "small_adult" = state_initialise_adult(num_stages, initial_size = 10),
  "large_spread" = state_initialise_spread(num_stages, initial_size = 100),
  "small_spread" = state_initialise_spread(num_stages, initial_size = 10)
)

# assign initial conditions based on job number
initial_condition_names <- names(initial_conditions)
selected_condition <- initial_condition_names[(iter - 1) %% 4 + 1]
initial_state <- initial_conditions[[selected_condition]]

# create filename for storing results
output <- paste0("simulation_results_", iter, ".RData")

# create list to store results
simulation_results <- vector("list", num_simulations)

# run through all simulations
for (i in 1:num_simulations) {
  simulation_results[[i]] <- stochastic_simulation(
  initial_state, 
  growth_matrix, 
  reproduction_matrix, 
  clutch_distribution, 
  simulation_length)
}

# save RDa files
save(simulation_results, file=paste("output_",iter,".rda",sep=""))

# explanation of line 37-40
# initial_condition_names gets the names of all initial conditions in the list
# selected_condition goes through all 'iter' values (from 0-100) and assigns them to 1, 2, 3 or 4, which correspond to each of the conditions
# it does this by doing (iter - 1) %% 4 + 1, %% finds the remainder of (iter - 1) / 4
# this means that if iter = 1, then (1 - 1) %% 4 = 0 and since R indexing starts at 1 we add + 1
# we do iter - 1 so that the indexing starts at 0 + 1 and not 2, you could theoretically do iter %% 4 + 1 and it would work fine, but it would start as 2, 3, 4, 1