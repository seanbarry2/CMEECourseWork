# CMEE 2024 HPC exercises R code pro forma
# For neutral model cluster run

rm(list=ls()) # good practice 
graphics.off() # turns off graphics
source("sb4524_HPC_2024_main.R")
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX")) # read in job number
#iter <- 1 # hash this out later

set.seed(iter)

# Decide community size. 
# We want 25 runs per size for a total of 100 runs:
#      1..25   => size=500
#      26..50  => size=1000
#      51..75  => size=2500
#      76..100 => size=5000
if (iter >= 1 && iter <= 25) {
  size <- 500
} else if (iter >= 26 && iter <= 50) {
  size <- 1000
} else if (iter >= 51 && iter <= 75) {
  size <- 2500
} else if (iter >= 76 && iter <= 100) {
  size <- 5000
} else {
  stop("iter out of range (1..100).")
}

# Speciation rate
speciation_rate <- 0.006185

output_file_name <- paste0("neutral_run", iter, ".rda")

# Other parameters
#    - We use interval_rich = 1
#    - interval_oct = size/10
#    - burn_in_generations = 8 * size
#    - wall_time = 690 minutes (11.5 hours) as recommended
wall_time_in_minutes  <- 690
interval_rich         <- 1
interval_oct          <- size / 10
burn_in_generations   <- 8 * size

# Call the function to run the simulation & save results
neutral_cluster_run(
  speciation_rate    = speciation_rate,
  size               = size,
  wall_time          = wall_time_in_minutes, 
  interval_rich      = interval_rich,
  interval_oct       = interval_oct,
  burn_in_generations= burn_in_generations,
  output_file_name   = output_file_name
)