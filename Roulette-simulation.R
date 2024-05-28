# Load necessary libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Define the number of simulations
num_spins <- 1000
num_runs <- 25

# Roulette wheel numbers (European roulette)
roulette_wheel <- c(rep("Red", 18), rep("Black", 18), "Green")

# Initialize a list to store results of each run
all_runs <- list()

# Run the Monte Carlo simulation 100 times
for (run in 1:num_runs) {
  # Initialize vectors to store results for this run
  results <- character(num_spins)
  black_counts <- numeric(num_spins)
  
  # Run the simulation for num_spins
  for (i in 1:num_spins) {
    spin_result <- sample(roulette_wheel, 1)
    results[i] <- spin_result
    black_counts[i] <- sum(results == "Black")
  }
  
  # Calculate the cumulative percentage of black results
  cumulative_black_percentage <- (black_counts / (1:num_spins)) * 100
  
  # Store the results in a data frame
  run_data <- data.frame(
    Spin = 1:num_spins,
    CumulativeBlackPercentage = cumulative_black_percentage,
    Run = factor(run)
  )
  
  # Add the data frame to the list
  all_runs[[run]] <- run_data
}

# Combine all runs into a single data frame
plot_data <- do.call(rbind, all_runs)

# Plot the cumulative percentage of black results for each run
ggplot(plot_data, aes(x = Spin, y = CumulativeBlackPercentage, group = Run, color = Run)) +
  geom_line(alpha = 0.6) +
  labs(title = "Cumulative Percentage of Black Results in Roulette (25 Runs)",
       x = "Number of Spins",
       y = "Cumulative Percentage of Black Results") +
  theme_minimal() +
  theme(legend.position = "none") # Remove legend for clarity
