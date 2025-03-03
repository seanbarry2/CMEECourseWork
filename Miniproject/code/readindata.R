# Simple script to read in and visualize data
library(tidyverse)
df <- read.csv("../data/LogisticGrowthData.csv")
df <- df[, !grepl("^X$", names(df))]
df <- df[, !names(df) %in% "Time_units"]

par(mfrow = c(1, 1))  # Arrange plots in a 1-row, 2-column layout
par(mar = c(4, 4, 2, 1))  # Adjust margins (bottom, left, top, right)

# Scatter plot for PopBio vs Time
plot(df$Time, df$PopBio, 
     xlab = "Time", ylab = "PopBio", 
     main = "PopBio vs Time", 
     pch = 16, col = "blue")

# Scatter plot for Temp vs Time
plot(df$Time, df$Temp, 
     xlab = "Time", ylab = "Temperature", 
     main = "Temperature vs Time", 
     pch = 16, col = "red")

# 1) PopBio vs Time, colored by Species
ggplot(df, aes(x = Time, y = PopBio, color = Species)) +
  geom_point() +
  labs(
    title = "Population Biology Over Time (by Species)",
    x = "Time",
    y = "PopBio"
  ) +
  theme_minimal()

# 2) PopBio vs Time, faceted by Medium
#    Here, color is still by Species, but each Medium gets its own panel.
ggplot(df, aes(x = Time, y = PopBio, color = Species)) +
  geom_point() +
  facet_wrap(~ Medium) +
  labs(
    title = "PopBio Over Time Faceted by Medium",
    x = "Time",
    y = "PopBio"
  ) +
  theme_minimal()

# 3) Temperature vs Time, colored by Species
ggplot(df, aes(x = Time, y = Temp, color = Species)) +
  geom_point() +
  labs(
    title = "Temperature Over Time (by Species)",
    x = "Time",
    y = "Temperature"
  ) +
  theme_minimal()
