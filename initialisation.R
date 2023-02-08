library(tidyverse)
# Loading of fish data
load("FishLengths.RData")
# Rename the data frame to fit any, line to be deleted
data <- x

# Check if the input data is appropriate for the teamEM function: 
# data must be data frame, have more than two rows, have values, have 3 
# numeric Age groups, Length column should be numeric with some missing 
# values, FishID should be a whole number.
if (all(is.na(data)) || !is.data.frame(data) || nrow(data) < 2 || 
    !is.vector(data$Age) || !is.numeric(data$Age) || all(is.na(data$Age)) || 
    any(data$Age[!is.na(data$Age)] > 4) ||
    any(data$Age[!is.na(data$Age)] < 1) || 
    any(!data$Age[!is.na(data$Age)] %% 1 == 0) ||
    !is.vector(data$Length) || !is.numeric(data$Length) ||
    all(is.na(data$Length)) || any(is.na(data$Length)) || 
    any(data$Length <= 0) || !is.vector(data$FishID) || asdf
    !is.numeric(data$Length) || any(is.na(data$FishID)) || 
    any(data$FishID < 0) || any(!data$FishID %% 1 == 0)) {
  stop("invalid data argument")
}

# 1. Initialisation
# Initialisation function 
# Input:
#  data - fish length with age groups data frame with some latent variables.
# Output:
#  inits - data frame with initial mu, sigma and lambda values for each of the 
# age groups in the fish data.
initialise <- function(data) {
  # Pre-initialise.
  # Subset the groups with known age values.
  age1_known <- filter(data, Age == 1)
  age2_known <- filter(data, Age == 2)
  age3_known <- filter(data, Age == 3)
  
  # Compute the mean and standard deviation values for each of known age groups
  summ_age1 <- summarise(age1_known, mean(Length), sd(Length))
  summ_age2 <- summarise(age2_known, mean(Length), sd(Length))
  summ_age3 <- summarise(age3_known, mean(Length), sd(Length))
  
  # Separate the latent data
  unknown <- filter(data, is.na(data$Age))
  
  # Find the probability density functions for each of the unknown age groups
  p1 <- dnorm(unknown$Length, mean = summ_age1[1, 1], sd = summ_age1[1, 2])
  p2 <- dnorm(unknown$Length, mean = summ_age2[1, 1], sd = summ_age2[1, 2])
  p3 <- dnorm(unknown$Length, mean = summ_age3[1, 1], sd = summ_age3[1, 2])
  
  # First compare the probability of the Length value belonging to each age 
  # group, then assign the age value 1, 2 or 3
  unknown$Age[p1 >= p2 & p1 > p3] <- 1
  unknown$Age[p2 > p1 & p2 > p3] <- 2
  unknown$Age[p3 > p2 & p3 > p1] <- 3
  
  # Combine the data frames of the known and now assigned latent values
  data <- rbind(filter(data, !is.na(Age)), unknown)
  
  # Initialise.
  # Initial values from the Fish age distribution.
  # Data frames for each age group including both the known and the assigned
  # values.
  # Data frames for each age group
  age1 <- filter(data, Age == 1)
  age2 <- filter(data, Age == 2)
  age3 <- filter(data, Age == 3)
  
  # Find means of each age group
  mu_x <- aggregate(data$Length, by = list(data$Age), FUN = mean)
  mu1 <- mu_x[1, 2]
  mu2 <- mu_x[2, 2]
  mu3 <- mu_x[3, 2]
  
  # Find variances of each age group
  var_x <- aggregate(data$Length, by = list(data$Age), FUN = var)
  
  # Find standard deviation of each age group
  sigma_x <- aggregate(data$Length, by = list(data$Age), FUN = sd)
  
  # Total number of fish data
  n <- nrow(data)
  
  # Calculate initial probabilities
  lambda_x <- c((nrow(age1) / n), (nrow(age2) / n), (nrow(age3) / n))
  lambda1 <- lambda_x[1]
  lambda2 <- lambda_x[2]
  lambda3 <- lambda_x[3]
  
  # Create data frame with initial values for the output
  inits <- data.frame(mu = c(mu_x[1, 2], mu_x[2, 2], mu_x[3, 2]),
                      sigma = c(sigma_x[1, 2], sigma_x[2, 2], sigma_x[3, 2]),
                      lambda = c(lambda_x[1], lambda_x[2], lambda_x[3]))
  rownames(inits)[1] <- "Age1"
  rownames(inits)[2] <- "Age2"
  rownames(inits)[3] <- "Age3"
  
  # Return the initial values
  return(inits)
}
