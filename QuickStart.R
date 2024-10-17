#########################################################################
#########################################################################
# External Datasets                                                     #
#########################################################################
#########################################################################

spwh3<-read.table('spwh3.txt', header=FALSE,na.strings="NA", dec=".")
cashdat<-read.table('cashdat.txt', header=FALSE,na.strings="NA", dec=".")
sero<-read.table('sero1.txt', header=FALSE,na.strings="NA", dec=".")
spwh2<-read.table('spwh2.txt', header=FALSE, 
                  na.strings="NA", dec=".")

##########################################################################
##########################################################################
## QUICKT START                                                         ##
##########################################################################
##########################################################################

###############################################
# Normal distribution                         #
###############################################

rnorm(100,0,1)   # This function takes in number of values, mean and sd. Outputs an array.
x<-rnorm(100,0,1)
x
mean(x)
var(x)
hist(x)


x<-rnorm(10000,0,1)
mean(x)
var(x)
hist(x,nclass=50) # This nclass here refers to the bins/bars in the plot


x1<-rnorm(10000,0,1)
x2<-rnorm(10000,1,1)
par(mfrow=c(2,1))
hist(x1,nclass=50,xlim=c(-4,4))
hist(x2,nclass=50,xlim=c(-4,4))


x1<-rnorm(10000,0,1)
x2<-rnorm(10000,0,2)
par(mfrow=c(2,1))
hist(x1,nclass=50,xlim=c(-6,6))
hist(x2,nclass=100,xlim=c(-6,6))

###############################################
# The cars data: plot and summary stat        #
###############################################

cars

plot(cars$speed,cars$dist,pch="+",xlab="speed",ylab="stopping distance",col=3)
title("Speed Vs. Stopping diatance",col=2)

dim(cars)
cars[,1]
head(cars)
mean(cars$speed)
max(cars$speed)
min(cars$speed)

help(attach)

attach(cars)
mean(speed)
max(speed)
min(speed)
detach(cars)


cor(cars)
var(cars$speed)
print(cars)
print(cars$speed)

#########################################################################
#########################################################################
# Practical Session No. 1                                               #
#########################################################################
#########################################################################

# Question No. 1

n = 10
sample_size = rnorm(n,0,1)
sample_mean = mean(sample_size)
sample_variance = var(sample_size)
sample_median = median(sample_size)
cat("The mean of the sample size is ", sample_mean,
      "and the variance is", sample_variance)

column_names = c("mean","var","median")
column_values = c(sample_mean, sample_variance, sample_median)

cat("## Stat Value")
for (i in 1:3){
  cat("## ",i,column_names[i], column_values[i],"\n")
}

# Question No. 2
dev.off()
n = 100
set.seed(123)  # Set seed for reproducibility
sample_size = rt(n, df = 3)
hist(sample_size, col=3)


# Question No. 3
dev.off()
x = c(1:10)
y = x*x
plot(x,y)

# Question No. 4
print(cars$speed)
print(cars$dist)
dev.off()
plot(cars$speed, cars$dist)
cor(cars$speed, cars$dist, method="spearman")
min(cars$speed)
max(cars$speed)
median(cars$speed)
y = (median(cars$dist)+3)*10
mean(y)


# Question 5
x <- c(1,3,5,7)
y <-c("one","three","five","seven")
data.frame(x,y)

# Question 6
nrow(airquality)
ncol(airquality)
x <- airquality$Temp
Month <- airquality$Month
dev.off()
boxplot(x~Month)

# Question 7
y = airquality$Ozone[is.na(airquality$Ozone) == FALSE]
mean(y)
median(y)
min(y)
max(y)
sd(y)

w <- c("Mean","Median","Minimum","Maximum","S.D")
x <- c(mean(y),median(y),min(y),max(y),sd(y))
data.frame(w,x)

dev.off()
par(mfrow = c(1, 2))
hist(y, main="Histogram for Ozone level")
boxplot(y)

# Question 8
dev.off()
Ratio = airquality$Wind/airquality$Temp
plot(airquality$Day, Ratio, main = "ratio versus day")

# Question 9
dev.off()
plot(airquality$Day, Ratio, main = "ratio versus day")
# Add a horizontal line at y = 0
abline(h = mean(Ratio), col = "red", lwd = 2) #lty = 2, lwd = 2)

# Question 10
dev.off()
sample <-c(1:1000)
set.seed(123)
for (i in 1:1000){
  sample[i] = mean(rbinom(31,1,0.5))  
}
hist(sample, nclass = 10)