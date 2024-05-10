# Preset
# Preset
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notion for entire R session

# Used libraries
library(ggplot2)
library(pacman)
library(tidyverse)
library(tidyr)
library(janitor)
library(dplyr)

####ALY 6050- Module-1: Analysis of a Betting Strategy in Sports####

###Part-1###

#1: Calculate the probability that the Red Sox will win the series

#R or RS: red_sox_boston
#Y or YN: yankees_new_york

# Probability of Red Sox winning a game in Boston
p_red_sox_boston <- 0.6

# Probability of Red Sox winning a game in New York
p_red_sox_win_ny <- 1 - 0.57

# Probability of Yankees winning a game in New York
p_yankees_new_york <- 0.57

# Probability of Yankees winning a game in Boston
p_yankees_boston <- 1-0.6 

# No of games

no_games <- 3

############

#If the first game is played in Boston, the second game is played in New York, and the
#third game (if it becomes necessary) is in Boston

#######

# Probability of Red Sox winning the first two games

p_red_sox_win_1st_2nd <- p_red_sox_boston * p_red_sox_win_ny
p_red_sox_win_1st_2nd

# Probability of Red Sox winning the first game, Yankees winning the second, and Red Sox winning the third

p_red_sox_win_1st_3rd <- p_red_sox_boston * p_yankees_new_york * p_red_sox_boston
p_red_sox_win_1st_3rd

# Probability of Red Sox winning the second and third games

p_red_sox_win_2nd_3rd <- p_yankees_boston*p_red_sox_win_ny * p_red_sox_boston
p_red_sox_win_2nd_3rd 

# Total probability of Red Sox winning the series

p_red_sox_win_series <- p_red_sox_win_1st_2nd + p_red_sox_win_1st_3rd + p_red_sox_win_2nd_3rd

cat("Probability of Red Sox winning the series is:", p_red_sox_win_series)


#2: Construct a probability distribution for your net win (X) in the series. Calculate your
#expected net win (the mean of X) and the standard deviation of X.

#Win of each out come

net_win_red_sox_win_series <- 500
net_win_red_sox_lose_series <- -520

net_win_distribution <- c(net_win_red_sox_win_series, net_win_red_sox_lose_series)
probabilities <- c(p_red_sox_win_series, 1 - p_red_sox_win_series)

expected_net_win <- (net_win_red_sox_win_series*p_red_sox_win_series) + 
  (net_win_red_sox_lose_series*(1-p_red_sox_win_series))
  
cat("Expected net win of winnng the series is:", expected_net_win)

variance <- sum(probabilities*(net_win_distribution-expected_net_win )^2)
cat("Variance of winnng the series is:", variance)

standard_dev <- sqrt(variance)
cat("Standard Deviation of winnng the series is:", standard_dev )

#3: R to create 10,000 random values for X. Let these random values be
#denoted by Y. Use these Y values to estimate your expected net win by using a 95%
#confidence interval. Does this confidence interval contain E(X)?

# Generate 10,000 random values for X
Y <- rnorm(10000, mean = expected_net_win, sd = standard_dev )

# sample mean (Y_bar)
Y_bar <- mean(Y)
Y_bar
# standard error (SE)
SE <- sd(Y) / sqrt(length(Y))
SE
# z-score for 95% confidence interval
z <- qnorm(0.975)
z
# Calculate lower and upper bounds of the confidence interval
lower_bound <- Y_bar - z * SE
lower_bound
upper_bound <- Y_bar + z * SE
upper_bound

# Check if expected_net_win falls within the confidence interval
if (expected_net_win >= lower_bound && expected_net_win <= upper_bound) {
  cat("The confidence interval contains the expected net win.")
} else {
  cat("The confidence interval does not contain the expected net win.")
}

#4:Construct a frequency distribution for Y. Next, use the Chi-squared goodness of fit
#test to verify how closely the distribution of Y has estimated the distribution of X.

#H0= there is no significant difference between the observed frequencies and the expected frequencies
#H1=  there is significant difference between the observed frequencies and the expected frequencies

# number of bins for the frequency distribution
num_bins <- 10

# Creating bins for the frequency distribution
breaks <- seq(min(Y), max(Y), length.out = num_bins + 1)

# observed frequencies for each bin
observed_freq <- table(cut(Y, breaks = breaks, include.lowest = TRUE))
observed_freq 
# expected frequencies based on the distribution of X
expected_freq <- diff(pnorm(breaks, mean = expected_net_win, sd = standard_dev)) * length(Y)
expected_freq 
# Perform the chi-squared goodness-of-fit test
chisq_test <- chisq.test(x=observed_freq, p = expected_freq/sum(expected_freq))
chisq_test

if(chisq_test$p.value<=0.05){
  cat("p-value is less than 0.05. Therefore, we reject the null hypothesis ")
} else {
  cat("p-value is greater than 0.05. Therefore, we fail to reject the null hypothesis ")
}

####Part-2####

###########
#Repeat part 1 above but assume that the first game is played in New York, the second
#game is played in Boston, and the third game (if it becomes necessary) is in New York.
###########
#1: Calculate the probability that the Red Sox will win the series

# Probability of Red Sox winning the first two games

p2_red_sox_win_1st_2nd <- p_red_sox_win_ny*p_red_sox_boston 
p2_red_sox_win_1st_2nd

# Probability of Red Sox winning the first game, Yankees winning the second, and Red Sox winning the third

p2_red_sox_win_1st_3rd <- p_red_sox_win_ny * p_yankees_boston  * p_red_sox_win_ny
p2_red_sox_win_1st_3rd

# Probability of Red Sox winning the second and third games

p2_red_sox_win_2nd_3rd <-  p_yankees_new_york*p_red_sox_boston*p_red_sox_win_ny
p2_red_sox_win_2nd_3rd 

# Total probability of Red Sox winning the series

p2_red_sox_win_series <- p2_red_sox_win_1st_2nd + p2_red_sox_win_1st_3rd + p2_red_sox_win_2nd_3rd

cat("Probability of Red Sox winning the series is:", p2_red_sox_win_series)

#2: Construct a probability distribution for your net win (X) in the series. Calculate your
#expected net win (the mean of X) and the standard deviation of X.

#Win of each out come

net_win_red_sox_win_series <- 500
net_win_red_sox_lose_series <- -520

net_win_dist <- c(net_win_red_sox_win_series, net_win_red_sox_lose_series)
prob <- c(p2_red_sox_win_series, 1 - p2_red_sox_win_series)

exp_net_win <- (net_win_red_sox_win_series*p2_red_sox_win_series) + 
  (net_win_red_sox_lose_series*(1-p2_red_sox_win_series))

cat("Expected net win of winnng the series is:", exp_net_win)

var <- sum(prob*(net_win_dist-exp_net_win )^2)
cat("Variance of winnng the series is:", var)

st_dev <- sqrt(var)
cat("Standard Deviation of winnng the series is:", st_dev )

##3.  R to create 10,000 random values for X. Let these random values be
#denoted by Y. Use these Y values to estimate your expected net win by using a 95%
#confidence interval. Does this confidence interval contain E(X)?

# Generate 10,000 random values for X
Y2 <- rnorm(10000, mean = exp_net_win, sd = st_dev )

# sample mean (Y_bar)
Y2_bar <- mean(Y2)
Y2_bar
# standard error (SE)
SE2 <- sd(Y2) / sqrt(length(Y2))
SE2
# z-score for 95% confidence interval
z2 <- qnorm(0.975)
z2
# Calculate lower and upper bounds of the confidence interval
lower_bound2 <- Y2_bar - z2 * SE2
lower_bound2
upper_bound2 <- Y2_bar + z2 * SE2
upper_bound2

# Check if expected_net_win falls within the confidence interval
if (exp_net_win >= lower_bound2 && exp_net_win <= upper_bound2) {
  cat("The confidence interval contains the expected net win.")
} else {
  cat("The confidence interval does not contain the expected net win.")
}


#4:Construct a frequency distribution for Y. Next, use the Chi-squared goodness of fit
#test to verify how closely the distribution of Y has estimated the distribution of X.

#H0= there is no significant difference between the observed frequencies and the expected frequencies
#H1=  there is significant difference between the observed frequencies and the expected frequencies

# number of bins for the frequency distribution
num_bins2 <- 10

# Creating bins for the frequency distribution
breaks2 <- seq(min(Y2), max(Y2), length.out = num_bins2 + 1)

# observed frequencies for each bin
observed_freq2 <- table(cut(Y2, breaks = breaks2, include.lowest = TRUE))
observed_freq2
# expected frequencies based on the distribution of X
expected_freq2 <- diff(pnorm(breaks2, mean = exp_net_win, sd = st_dev)) * length(Y2)
expected_freq2 
# Perform the chi-squared goodness-of-fit test
chisq_test2 <- chisq.test(x=observed_freq2, p = expected_freq2/sum(expected_freq2))
chisq_test2

if(chisq_test2$p.value<=0.05){
  cat("p-value is less than 0.05. Therefore, we reject the null hypothesis ")
} else {
  cat("p-value is greater than 0.05. Therefore, we fail to reject the null hypothesis ")
}

####Part-3####

###
#Assume that the series is a best-of-five series where the
#first team that wins three games wins the series with games alternating between Boston
#and New York, with the first game being played in Boston.
###

# Probability of Red Sox winning a game in Boston
p_red_sox_boston <- 0.6

# Probability of Red Sox winning a game in New York
p_red_sox_win_ny <- 1 - 0.57

# Probability of Yankees winning a game in New York
p_yankees_new_york <- 0.57

# Probability of Yankees winning a game in Boston
p_yankees_boston <- 1-0.6 

#1: Calculate the probability that the Red Sox will win the series

# Probability of Red Sox winning two games in Boston

p_red_sox_win_Boston <- (p_red_sox_boston)^2*p_red_sox_win_ny
p_red_sox_win_Boston

# Probability of Red Sox winning two games in NY

p_red_sox_win_NY <- (p_red_sox_win_ny)^2 * p_red_sox_boston
p_red_sox_win_NY

# Probability of Red Sox winning all three in Boston

p_red_sox_win_3_in_Boston <- (p_red_sox_boston)^3
p_red_sox_win_3_in_Boston

# Probability that the Red Sox will win the series

total_prob <- p_red_sox_win_Boston + p_red_sox_win_NY + p_red_sox_win_3_in_Boston

cat("Probability that the Red Sox will win the series is:", total_prob )

#2: Construct a probability distribution for your net win (X) in the series. Calculate your
#expected net win (the mean of X) and the standard deviation of X.

#Win of each out come

net_win_red_sox_win_series <- 500
net_win_red_sox_lose_series <- -520

net_win_dist2 <- c(net_win_red_sox_win_series, net_win_red_sox_lose_series)
prob2 <- c(total_prob, 1 - total_prob)

exp_net_win2 <- (net_win_red_sox_win_series*total_prob) + 
  (net_win_red_sox_lose_series*(1-total_prob))

cat("Expected net win of winnng the series is:", exp_net_win2)

var2 <- sum(prob*(net_win_dist-exp_net_win )^2)
cat("Variance of winnng the series is:", var2)

st_dev2 <- sqrt(var2)
cat("Standard Deviation of winnng the series is:", st_dev2 )

##3.  R to create 10,000 random values for X. Let these random values be
#denoted by Y. Use these Y values to estimate your expected net win by using a 95%
#confidence interval. Does this confidence interval contain E(X)?

# Generate 10,000 random values for X
Y3 <- rnorm(10000, mean = exp_net_win2, sd = st_dev2 )

# sample mean (Y_bar)
Y3_bar <- mean(Y3)
Y3_bar
# standard error (SE)
SE3 <- sd(Y3) / sqrt(length(Y3))
SE3
# z-score for 95% confidence interval
z3 <- qnorm(0.975)
z3
# Calculate lower and upper bounds of the confidence interval
lower_bound3 <- Y3_bar - z3 * SE3
lower_bound3
upper_bound3 <- Y3_bar + z3 * SE3
upper_bound3

# Check if expected_net_win falls within the confidence interval
if (exp_net_win2 >= lower_bound3 && exp_net_win2 <= upper_bound3) {
  cat("The confidence interval contains the expected net win.")
} else {
  cat("The confidence interval does not contain the expected net win.")
}

#4:Construct a frequency distribution for Y. Next, use the Chi-squared goodness of fit
#test to verify how closely the distribution of Y has estimated the distribution of X.

#H0= there is no significant difference between the observed frequencies and the expected frequencies
#H1=  there is significant difference between the observed frequencies and the expected frequencies

# number of bins for the frequency distribution
num_bins3 <- 10

# Creating bins for the frequency distribution
breaks3 <- seq(min(Y3), max(Y3), length.out = num_bins3 + 1)

# observed frequencies for each bin
observed_freq3 <- table(cut(Y3, breaks = breaks3, include.lowest = TRUE))
observed_freq3
# expected frequencies based on the distribution of X
expected_freq3 <- diff(pnorm(breaks3, mean = exp_net_win2, sd = st_dev2)) * length(Y2)
expected_freq3 
# Perform the chi-squared goodness-of-fit test
chisq_test3 <- chisq.test(x=observed_freq3, p = expected_freq3/sum(expected_freq3))
chisq_test3

if(chisq_test3$p.value<=0.05){
  cat("p-value is less than 0.05. Therefore, we reject the null hypothesis ")
} else {
  cat("p-value is greater than 0.05. Therefore, we fail to reject the null hypothesis ")
}


