# Libraries
library(tidyverse)

# Loading of fish data
load("FishLengths.RData")
data <- x


# Expectation-Maximization Algorithm Manually

# Inputs:
#   data - data frame with N observations and 3 columns: FishID, Length,
#     Age (k numbered groups for known, and NA for unknown). 
#   epsilon - desired tolerance value for convergence.
#   maxit - maximum number of iterations.


# Outputs:
#   inits - dataframe with mu, sigma, lambda for k age groups.
#   posterior - posterior probabilities of N observations in k age groups.
#   estimates - new mu, sigma, lambda for k age groups.
#   converged - TRUE if convergence is met before the maximum iteration,
#     FALSE otherwise.
#   likelihood - vector of length equals to number of iterations, maximum 
#   length equals to maxit.

fishEM <- function(data, epsilon = 1e-08, maxit = 1000) {

  # Check if the input data is appropriate for the fishEM function: 
  # data must be data frame, have more than two rows, have values, have 3 
  # numeric Age groups, Length column should be numeric with some missing 
  # values, FishID should be a whole number
  
  if (all(is.na(data)) || !is.data.frame(data) || nrow(data) < 2 || 
      !is.vector(data$Age) || !is.numeric(data$Age) || all(is.na(data$Age)) || 
      any(data$Age[!is.na(data$Age)] > 4) || 
      any(data$Age[!is.na(data$Age)] < 1) || 
      any(!data$Age[!is.na(data$Age)] %% 1 == 0) ||
      !is.vector(data$Length) || !is.numeric(data$Length) || 
      all(is.na(data$Length)) || 
      any(data$Length <= 0) || 
      !is.vector(data$FishID) || 
      !is.numeric(data$Length) || any(is.na(data$FishID)) || 
      any(data$FishID < 0) || any(!data$FishID %% 1 == 0)) {
    stop("invalid data argument")
  }
  
  # Check if the epsilon is in the correct format
  if (!is.numeric(epsilon) || !is.vector(epsilon) || 
      length(epsilon) > 1 || epsilon < 0) {
    stop("invalid epsilon argument")
  }
  
  # Check if the value maxit is a single numeric whole number, greater than 0.
  if (!is.numeric(maxit) || !is.vector(maxit) || 
      length(maxit) > 1 || !maxit %% 1 == 0 || maxit < 1) {
    stop("invalid maxit argument")
  }
  
  # Divide the data into known and unknown fish ages 
  known <- x[!(is.na(x$Age)), ]
  unknown <- x[(is.na(x$Age)), ]
  
  # Vector created to store likelihood values, will have max length 1000
  likelihood <- rep(NA, times = 1000)
  
  # Reset of counter to record number of iterations run
  counter <- 0
  
  
  # 1. Initialisation
  
  
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
  # group, then assign the value 1, 2 or 3
  unknown$Age[p1 >= p2 & p1 > p3] <- 1
  unknown$Age[p2 > p1 & p2 > p3] <- 2
  unknown$Age[p3 >= p2 & p3 > p1] <- 3
  
  # Combine the data frames of the known and now assigned latent values
  data <- rbind(filter(data, !is.na(Age)), unknown)
  
  y <- data$Length
  
  # Initialise
  # Initial values from the Fish age distribution
  # Data frames for each age group including both the known and the assigned
  # values
  # Data frames for each age group
  age1 <- filter(data, Age == 1)
  age2 <- filter(data, Age == 2)
  age3 <- filter(data, Age == 3)
  
  # Find means of each age group
  mu_x <- aggregate(data$Length, by = list(data$Age), FUN = mean)
  
  # Find variances of each age group
  var_x <- aggregate(data$Length, by = list(data$Age), FUN = var)
  
  # Find standard deviation of each age group
  sigma_x <- aggregate(data$Length, by = list(data$Age), FUN = sd)
  
  # Total number of fish data
  n <- nrow(data)
  
  # Calculate initial probabilities
  lambda_x <- c((nrow(age1) / n), (nrow(age2) / n), (nrow(age3) / n))
  
  
  # Create data frame with initial values for the output
  inits <- data.frame(mu = c(mu_x[1, 2], mu_x[2, 2], mu_x[3, 2]),
                      sigma = c(sigma_x[1, 2], sigma_x[2, 2], sigma_x[3, 2]),
                      lambda = c(lambda_x[1], lambda_x[2], lambda_x[3]),
                      row.names = c('Age1', 'Age2', 'Age3'))
  
  
  # 2. Expectation
  
  # Create for-loop with maximum iterations of maxit times
  for (i in 1 : maxit) {  
    
    # Check if estimations from previous iteration exists
    # If not then use the initial data
    if (counter == 0){
      estimates <- inits
    } else {
      estimates <- estimates 
    }
    
    # Numerators and denominators for conditional probabilities
    num1 <- dnorm(y, estimates[1, 1], estimates[1, 2]) * 
      estimates[1, 3]
    num2 <- dnorm(y, estimates[2, 1], estimates[2, 2]) * 
      estimates[2, 3]
    num3 <- dnorm(y, estimates[3, 1], estimates[3, 2]) * 
      estimates[3, 3]
    den <- mapply(sum, num1, num2, num3)
    
    # Posterior probabilities for observations belonging to age group 1
    post1 <- num1 / den
    # Posterior probabilities for observations belonging to age group 2
    post2 <- num2 / den
    # Posterior probabilities for observations belonging to age group 3
    post3 <- num3 / den
    
    # Create dataframe with posterior probabilities
    posterior <- data.frame(Age1 = post1, Age2 = post2, Age3 = post3)
    
    
    
    # 3. Maximization 
    
    
    # This step takes the values from the expectation step and 
    # computes the new mean and variance 
    
    # This is the same as the transpose of the output from post_prob
    post_matrix <- t(as.matrix(posterior))
    
    # For new means
    num_mu_mat <- post_matrix %*% data$Length
    din_mat <- rowSums(post_matrix)
    #new estimated means for this iteration 
    maximized_mu_mat <- num_mu_mat/din_mat
    
    # For new variances
    sq_mat <- cbind(y - maximized_mu_mat[1,1], 
                    y - maximized_mu_mat[2,1], 
                    y - maximized_mu_mat[3,1])
    
    num_sigma_mat <- post_matrix %*% (sq_mat^2)
    # Here the sigma values will be on the diagonal, extracting below -
    num_sigma <- rbind(num_sigma_mat[1,1], 
                       num_sigma_mat[2,2], 
                       num_sigma_mat[3,3])
    
    # New estimated sigma for this iteration 
    maximized_sigma_mat_sq3 <- sqrt(num_sigma/din_mat)
    
    # New estimated lambda for this iteration 
    maximized_lambda_mat <- (1/n) * din_mat
    
    # Data frame of maximized values
    estimates <- data.frame(mu = c(maximized_mu_mat), sigma = c(maximized_sigma_mat_sq3), 
                            lambda = maximized_lambda_mat)
    
    rownames(estimates) <- c('Age1', 'Age2', 'Age3')
    
    # 4. Testing Convergence
    
    # Vector created to store likelihood values, will have max length 1000
    
    lam_dens_1 <- maximized_lambda_mat[1] *
      dnorm(y, 
            maximized_mu_mat[1,1], 
            maximized_sigma_mat_sq3[1,1])
    
    lam_dens_2 <- maximized_lambda_mat[2] *
      dnorm(y, 
            maximized_mu_mat[2,1], 
            maximized_sigma_mat_sq3[2,1])
    
    lam_dens_3 <- maximized_lambda_mat[3] *
      dnorm(y, 
            maximized_mu_mat[3,1], 
            maximized_sigma_mat_sq3[3,1])
    
    sum_lam_dens <- lam_dens_1 + lam_dens_2 + lam_dens_3
    log_lam_dens <- log(sum_lam_dens)
    log_lik <- sum(log_lam_dens)
    options(digits = 12)

    
    # Storing the log-lik in the likelihood vector
    likelihood[counter + 1] <- log_lik
    
    # Evaluating if log-lik has converged to less than epsilon
    if (counter > 1) {
      if (abs(log_lik - likelihood[counter]) < epsilon) {
        converged <- TRUE
        likelihood <- likelihood[1 : counter]
        print(paste("No of iterations is ", counter))
        # The required list that the fishEM function should return 
        final_return <- list(estimates = estimates, inits = inits,
                             converged = converged, posterior = posterior, 
                             likelihood = likelihood)
        return(final_return)
        break
      }
    }
    
    
    
    # This step is to reassign age groups if llk hasn't converged
    # Find the probability density functions for each of the unknown age groups
    p1 <- dnorm(unknown$Length, 
                mean = maximized_mu_mat[1, 1], 
                sd = maximized_sigma_mat_sq3[1, 1])
    p2 <- dnorm(unknown$Length, 
                mean = maximized_mu_mat[2, 1], 
                sd = maximized_sigma_mat_sq3[2, 1])
    p3 <- dnorm(unknown$Length, 
                mean = maximized_mu_mat[3, 1], 
                sd = maximized_sigma_mat_sq3[3, 1])
    
    # First compare the probability of the Length value belonging to each age 
    # group, then assign the value 1, 2 or 3
    # Select data with prior known values
    unknown$Age[p1 >= p2 & p1 > p3] <- 1
    unknown$Age[p2 > p1 & p2 > p3] <- 2
    unknown$Age[p3 >= p2 & p3 > p1] <- 3
    
    # Combine the data frames of the known and now assigned latent values
    data <- rbind(known, unknown)
    
    
    # Counter inserted to keep track of number of iterations
    counter <- counter + 1
    
    # If the number of iterations reaches the maximum allowed 
    #  as input by the user 
    if (counter == maxit){
      converge <- FALSE 
      print('Falied to converge within the max iterations given ')
      break
    }
  }
}




