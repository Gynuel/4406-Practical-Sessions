# The NHANES Data

library(NHANES)
data(NHANES)
dim(NHANES)

# Variable names are listed below
names(NHANES)

# Question #1: Calculate the mean, median, and variance of the weight
attach(NHANES)
pop_mean = mean(na.omit(Weight))
pop_median = median(na.omit(Weight))
pop_var = var(na.omit(Weight))

# Question #2: Draw 5 samples of size 500 from the population
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
  print(paste("Sample ",i," Sample, Mean = ",s_mean," Variance = ", s_var))
  hist(s,main = paste("Histogram of Sample #",i), xlab = "Sample Data")
  qqnorm(s, main = paste("QQ Plot os Sample #",i) )
  qqline(s, col = "red")
}

# Question #3: Calculate the population correlation between the variables
# Weight and Height. Draw 5 samples of Weight and Height (size 100) from the
# population and calculate the sample correlation. Produce a scatterplot, per
# sample, for Weight VS Height

# Note since the dataset is not unbalanced. Here I opted to trim the larger datast
# by sampling without replacement.
dev.off()
w = na.omit(Weight) # This has a length of 9,922
h = na.omit(Height) # This has a length of 9,647
w_trim = sample(w,9647,replace = FALSE)

# Get the population correlation
cor(w_trim,h)

size = 100
for(i in 1:n){
  h_s = rnorm(size, mean(h), var(h))
  w_trim_s = rnorm(size, mean(w_trim), var(w_trim))
  
  print(paste("Sample ",i," Sample, Mean = ",s_mean," Variance = ", s_var))
  hist(s,main = paste("Histogram of Sample #",i), xlab = "Sample Data")
  qqnorm(s, main = paste("QQ Plot os Sample #",i) )
  qqline(s, col = "red")
}
