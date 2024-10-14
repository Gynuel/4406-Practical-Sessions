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
