# Simulation testing of the teamEM function

# This function is a testing function that will take distribution inputs from
# the user, create the simulated Gaussian mixture, run the teamEM function on 
# this data and evaluate the accuracy of the estimates
# Inputs : data - a data frame with 3 columns (FishID, Length, Age)
#          means - a vector of length 3 with the true means
#          sds - a vector of length 3 with the true standard deviations
# Outputs : A statement detailing no of iterations
#           A statement detailing the true values, estimated values and 
#             confidence intervals

simulation <- function(data, means, sds) {
  # Sourcing the code from the Working File
  source("teamEM.r")
  
  # Checks for the input; the data input should have checks from the teamEM func
  if (!is.vector(means) || !is.vector(sds) || 
      length(means) != 3 || length(sds) != 3 || 
      any(means < 0) || any(sds < 0)) {
    stop("invalid input argument")
  }
  
  # Simulating data from 3 different Gaussian distributions
  sim_1 <- rnorm(1000, mean = means[1], sd = sds[1])
  sim_2 <- rnorm(1000, mean = means[2], sd = sds[2])
  sim_3 <- rnorm(1000, mean = means[3], sd = sds[3])
  
  # Combining the above vectors into a dataframe and allocating the known ages
  Age = c(rep(1, times = 1000), rep(2, times = 1000), rep(3, times = 1000))
  sim_tot <- data.frame(Length = c(sim_1, sim_2, sim_3),
                        Age = Age )
  
  # Random sampling 1000 values from the above vector for the Gaussian mixture
  sim_rand <- sim_tot[sample(nrow(sim_tot), 1000),]
  
  # Viewing the sample as a histogram
  hist(sim_rand$Length, breaks = 20, 
       main = "Histogram of simulated data", xlab = "Fish Lengths")
  
  # Creating the initial data frame for the teamEM function
  FishID = 1:1000
  test_dat <- data.frame(FishID = FishID, 
                         Length = sim_rand$Length, 
                         Age = sim_rand$Age)
  
  # Allocating unknown values to 900 fish lengths
  chosen <- sample(1:1000, 900)
  test_dat$Age[chosen] <- NA
  
  # Re-checking how many values allocated
  known <- test_dat[!(is.na(test_dat$Age)), ]
  nrow(known)
  hist(known$Length, breaks = 20)
  
  # Testing the function for the above data and finding confidence intervals
  # for the means of each age group
  test_results <- teamEM(test_dat)
  
  # Extracting the estimates, calc. standard error and margin error
  test_est <- test_results$estimates
  std_error <- test_est$sigma / sqrt(length(test_dat$Length))
  alpha = 0.05
  norm = qnorm(alpha/2, mean = test_est$mu, sd = test_est$sigma)
  m_error <- std_error * norm
  
  
  # Calculating the bounds for each age group
  ci_lower <- test_est$mu - m_error
  ci_upper <- test_est$mu + m_error
  
  # Printing out final statement
  print(paste("The values for the gaussian mixture are mu =",
              means[1], ",", means[2], ",", means[3], "with sd = 2, 5, 10.",
  "The EM algorithm produced means of mu =",round(test_est$mu[1], 2),
  ",", round(test_est$mu[2], 2), ",", round(test_est$mu[3], 2), ".",
  "Hence, the confidence intervals for the means are (", round(ci_lower[1], 2), ",", 
  round(ci_upper[1], 2), ")", ", (", round(ci_lower[2], 2), ",",
  round(ci_upper[2], 2), ")", ", (", round(ci_lower[3], 2), ",",
  round(ci_upper[3], 2), ")"))
}

# Testing the code with known values
simulation(data, means = c(20, 40, 70), sds = c(2, 5, 10))
