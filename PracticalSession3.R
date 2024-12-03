# The NHANES Data

library(NHANES)
data(NHANES)
dim(NHANES)

# Variable names are listed below
names(NHANES)

# ==============================================
# Question #1: Calculate the mean, median, and variance of the weight
# ==============================================

attach(NHANES)
pop_mean = mean(na.omit(Weight))
pop_median = median(na.omit(Weight))
pop_var = var(na.omit(Weight))

# ==============================================
# Question #2: Draw 5 samples of size 500 from the population
# ==============================================

# For each sample, calculate the mean and the varaince and plot
# the histogram for the sample.
# Use the R function qqnorm to check if the distribution of the variable
# weight in the sample follows a normal distribution.
dev.off()
set.seed(123)
n = 5
size = 500

for(i in 1:n){
  s = rnorm(size, pop_mean, pop_var)
  s_mean = mean(s)
  s_var = mean(s)
  print(paste0("Sample ",i," Sample, Mean = ",s_mean," Variance = ", s_var))
  hist(s,main = paste("Histogram of Sample #",i), xlab = "Sample Data")
  qqnorm(s, main = paste("QQ Plot os Sample #",i) )
  qqline(s, col = "red")
}

# ==============================================
# Question #3: Calculate the population correlation between the variables
# ==============================================

# Weight and Height. Draw 5 samples of Weight and Height (size 100) from the
# population and calculate the sample correlation. Produce a scatterplot, per
# sample, for Weight VS Height.
# Question #3.1: Calculate the individual BMI in the 5 samples and plot the
# subjects' age

# Note since the dataset is not unbalanced. Here I opted to trim the larger datast
# by sampling without replacement.
dev.off()
w = na.omit(Weight) # This has a length of 9,922
h = na.omit(Height) # This has a length of 9,647
a = sample(na.omit(Age),9647,replace = FALSE) # This has a length of 10,000
w_trim = sample(w,9647,replace = FALSE)

# Get the population correlation
cor(w_trim,h)

size = 100
for(i in 1:n){
  h_s = runif(size, max = max(h), min = min(h))
  w_trim_s = runif(size, max = max(w_trim), min = min(w_trim))
  a_s = runif(size, max = max(a), min = min(a))
  bmi = w_trim_s/(h_s*h_s)
  
  print(paste0("Sample #",i," Correlation", cor(h_s,w_trim_s)))
  par(mfrow = c(1,2))
  plot(w_trim_s, h_s,main = paste0("Scatterplot of Sample #",i,"- WeightxHeight"), ylab = "Height", xlab="Weight")
  plot(a_s,bmi,main = paste0("Scatterplot of Sample #",i,"- AgexBMI"), ylab = "BMI", xlab="Age")
}

# ==============================================
# Question #4: Compute for the Probabilities using R
# ==============================================

### Question #4.1: Let X be a random variable, X~B(10,0.3).
# Calculate the probability P(X<3), P(5 < X < 9) .

# Given
# - Binomial Distribution with trials = 10 and P(success). = 0.3
trials = 10
psuccess = 0.3

# Note:
# dbinom() - finds P(X = k) for a data that follows binomial distribution. | Density/Distribution
# pbinom() - finds P(X <= k) for a data that follows binomial distribution. | Probability/Cumulative
# qbinom() - find the nth quantile, that is if P(x <= k) is given, it finds k. | Quantile
# rbinom() - generated n random variables of a binomial distribution. | Random

# Get P(X < 3) or P(X <= 2)
# Approach #1
p_x_lessthan_equalto_2 <- pbinom(2, size = trials, prob = psuccess)
print(p_x_lessthan_equalto_2)

# Approach #2
p_lessthan_3 = pbinom(0, size = trials, prob = psuccess) + 
  dbinom(1, size = trials, prob = psuccess) + dbinom(2, size = trials, prob = psuccess)
print(p_lessthan_3)

# Get P(5 < X <9) 
p_between_6and8 = dbinom(6, size = trials, prob = psuccess) + 
  dbinom(7, size = trials, prob = psuccess) + dbinom(8, size = trials, prob = psuccess)
print(p_between_6and8)


### Question #4.2: Plot the densities (in one figure) for N(0,1) and N(2,1)
dev.off()
library(ggplot2)

# Generate a sequence of x values for the plot
x <- seq(-5, 7, length.out = 1000)

# Create a data frame with the densities for N(0,1) and N(2,1)
data <- data.frame(
  x = x,
  N01 = dnorm(x, mean = 0, sd = 1),   # Density for N(0,1)
  N21 = dnorm(x, mean = 2, sd = 1)    # Density for N(2,1)
)

# Plot the densities using ggplot
ggplot(data, aes(x = x)) +
  geom_line(aes(y = N01, color = "N(0,1)")) +  # Plot N(0,1)
  geom_line(aes(y = N21, color = "N(2,1)")) +  # Plot N(2,1)
  labs(title = "Density Plots for N(0,1) and N(2,1)",
       x = "x", y = "Density") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), name = "Distribution")

# Question#4.3: Plot the densities (in one figure) for N(0,1) and N(0,2)

dev.off()
# Create a data frame with the densities for N(0,1) and N(0,2)
data <- data.frame(
  x = x,
  N01 = dnorm(x, mean = 0, sd = 1),   # Density for N(0,1)
  N02 = dnorm(x, mean = 0, sd = 2)    # Density for N(0,2)
)

# Plot the densities using ggplot
ggplot(data, aes(x = x)) +
  geom_line(aes(y = N01, color = "N(0,1)")) +  # Plot N(0,1)
  geom_line(aes(y = N02, color = "N(0,2)")) +  # Plot N(0,2)
  labs(title = "Density Plots for N(0,1) and N(0,2)",
       x = "x", y = "Density") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), name = "Distribution")

### Question #4.4: Calculate the probability function for a random variable X ~ B(2,0.5)
dev.off()

# Parameters for the binomial distribution
trials <- 2       # Number of trials
prob_success <- 0.5  # Probability of success on each trial

# Possible number of successes (0, 1, 2)
successes <- 0:trials

# Calculate the PMF for each number of successes
pmf_values <- dbinom(successes, size = trials, prob = prob_success)

# Create a bar plot of the PMF
barplot(pmf_values, names.arg = successes, col = "skyblue",
        main = "Probability Mass Function of B(2, 0.5)",
        xlab = "Number of Successes", ylab = "Probability",
        ylim = c(0, max(pmf_values) + 0.1))

# Add the probability values on top of the bars
text(x = successes, y = pmf_values, labels = round(pmf_values, 3), pos = 3, cex = 1.2)

### Question #4.5: Let X be a random variable X~P(3). Calculate the probability P(X < 4)
ppois(q = 3, lambda = 3)

### Question #4.6: Draw a random sample of 55 observations from P(3), calculate
# the mean and variance of the sample
s = rpois( n = 55, lambda = 3)
mean(s)
var(s)

### Question #4.7: Let X be a random variable, X ~ N(1,2), calculate the probability P(X <= 0)
p_lessthan_1 = pnorm(q = 0, mean = 1,sd = 2)
print(p_lessthan_1)

### Question #4.8: Let X be a random variable, X ~ N(0,1), calculate the probability P(X <= 0)
p_lessthan_1 = pnorm(q = 0, mean = 0,sd = 1)
print(p_lessthan_1)


### Question #4.9: Let X_1,...X_10 be a random sample from N(10,1) and let X_bar be the sample
# mean. Calculate the probability that P(X_bar <= 9) and the probability that P(X <= 9)

mean_X_bar <- 10
sd_X_bar <- 1 / sqrt(10)  # Standard deviation of the sample mean

# Probability that the sample mean is less than or equal to 9
P_X_bar_less_9 <- pnorm(9, mean = mean_X_bar, sd = sd_X_bar)
print(P_X_bar_less_9)

# Parameters for individual X
mean_X <- 10
sd_X <- 1

# Probability that an individual X is less than or equal to 9
P_X_less_9 <- pnorm(9, mean = mean_X, sd = sd_X)
P_X_less_9

### Question #4.10: Draw a random sample of 1,000 observations from N(10,1). Calculate the sample
# mean. What is the proportion of observations smaller than 9? Plot a histogram of the sample.
dev.off()
s = rnorm(n = 1000, mean = 10, sd = 1)
s_prop = length(s[s<9])/length(s) *100
print(s_prop)
mean(s)
hist(s)

# ==============================================
# Question #5: Produce the following plots:
# ==============================================

# Histogram for the BMI
# Scatterplot of the BMI versus age.
# Scatterplot of the BMI versus age in which the individuals with Diabetes and without Diabetes are plotted in different colors.

# Boxplot of age across the level of the factor Diabetes.
# Normal probability plot for height.
# Normal probability plot for weight ( ) by Diabetes status (in one page).
# Define the z-scores for the weight by Calculate the mean and variance of and plot the normal probability plot for .

# ==============================================
# Question #6: two samples t-test
# ==============================================

# In this question we use the NHANES dataset (the R object NHANES).
# Use a two-sample t-test to test the null hypothesis that the BMI of Diabetes patients is equal to
# the BMI of non Diabetes patients against two sided alternative. Assume that the variance in equal in the
# two groups.
# Use a graphical display to visualize the distribution of the BMI for Diabetes and non Diabetes patients.
# Calculate the mean BMI by Diabetes status.
