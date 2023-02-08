# Testing of the teamEM function

# Sourcing the code from the Working File
source("~/MT4113/assignment2-team-1/Working_file.R")

# setting seed to freeze sample distribution
set.seed(1234)

# Simulating data from 3 different Gaussian distributions
sim_1 <- rnorm(1000, mean = 20, sd = 2)
sim1 <- data.frame(Length = sim_1, Age = 1)
sim_2 <- rnorm(1000, mean = 40, sd = 5)
sim2 <- data.frame(Length = sim_2, Age = 2)
sim_3 <- rnorm(1000, mean = 60, sd = 10)
sim3 <- data.frame(Length = sim_3, Age = 3)

sim_tot <- rbind(sim1, sim2, sim3)

# Random sampling 1000 values from the above vector for the Gaussian mixture
sim_rand <- sim_tot[sample(nrow(sim_tot), 1000),]

# Viewing the sample as a histogram
hist(sim_rand$Length, breaks = 40, xlim = c(0, 120))

# Creating the initial data frame for the teamEM function
test_dat <- data.frame(FishID = 1:1000, Length = sim_rand$Length, Age = sim_rand$Age)

# Allocating known values to 100 fish lengths by studying histogram
chosen <- sample(1:1000, 900)
test_dat$Age[chosen] <- NA
#lengths_chosen <- test_dat$Length[chosen]

# Allocating fishes with Age 1
#Age1 <- chosen[test_dat$Length[chosen] < 25]
#test_dat$Age[Age1] <- 1
# Allocating fishes with Age 3
#Age3 <- chosen[test_dat$Length[chosen] > 50]
#test_dat$Age[Age3] <- 3
# Allocating the rest with Age 2
#Age2 <- chosen[test_dat$Length[chosen] >= 25 &
#               test_dat$Length[chosen] <= 50]
#test_dat$Age[Age2] <- 2


# Re-checking how many values allocated
known <- test_dat[!(is.na(test_dat$Age)), ]
nrow(known)
hist(known$Length, breaks = 40)

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

# Printing out final statements
print("The means chosen for the gaussian mixture were mu = 20, 40, 70 with sd = 2, 5, 10")
print(paste("The EM algorithm produced means of mu =",round(test_est$mu[1], 2),
            ",", round(test_est$mu[2], 2), ",", round(test_est$mu[3], 2)))

