#Test file for teamEM 
source('teamEM.r')

test_teamEM <- function(data, epsilon = 0.0001){
  test <- teamEM(data)
  
  #test the number of values in the posterior
  pass.test1 <- if ((nrow(test$posterior)) == 1000 & (ncol(test$posterior)) == 3) TRUE else FALSE
  print(paste("Posterior has 1000 rows and 3 columns:", pass.test1)) 
  
  #test sum of posterior probabilities
  test_post <- (rowSums(test$posterior, rep(3, 1000)) - 1) < 1e-10
  pass.test2 <- if (var(test_post) == 0 & test_post[1] == TRUE) TRUE else FALSE
  print(paste("Posterior probability for each observation adds up to 1:",
              pass.test2)) 

  #test log likelihood is epsilon different from the previous 
  pass.test3 <- (abs(test$likelihood[length(test$likelihood) - 1] - test$likelihood[length(test$likelihood)]) < epsilon)
  print(paste("Difference in (likelihood(i+1) and likelihood(i) is ",abs(test$likelihood[length(test$likelihood)] - test$likelihood[length(test$likelihood) - 1]), 
              " Is difference less than epsilon:", pass.test3))
  
  #does converge match with the what is said by epsilon 
  pass.test4 <- ((test$converge == TRUE && (abs(test$likelihood[length(test$likelihood) - 1] - test$likelihood[length(test$likelihood)]) < epsilon)) || 
                   (test$converge == FALSE && (abs(test$likelihood[length(test$likelihood) - 1] - test$likelihood[length(test$likelihood)]) >  epsilon)) )
  print(paste("Does converge output match with what is confered from differnece in likelihoods: ", pass.test4))
  
  #does the output list have 5 elements 
  pass.test5 <- (length(test) == 5)
  print(paste("Correct number of outputs:", pass.test5))
  
  
}

# Plotting of output distribution on histogram to test fit
test <- teamEM(data)

ggplot(x, aes(x = Length)) + geom_histogram(binwidth = 1) +
  mapply(
    function(mean, sd, lambda, n, binwidth) {
      stat_function(
        fun = function(x) {
          # This is to account for dnorm being expressed as densities
          # while histogram is in frequency, and adjusting for the
          # amplitude
          (dnorm(x, mean = mean, sd = sd)) * n * binwidth * lambda
        }
      )
    },
    mean = test$estimates[,1], # mean
    sd = test$estimates[,2], # standard deviation
    lambda = test$estimates[,3], # lambda
    n = length(x$Length), # sample size
    binwidth = 1 # binwidth used for histogram
  ) +
  labs(x = "Length", y = "Frequency",
       title = "Plot of the estimated Gaussian mixture on its histogram")

