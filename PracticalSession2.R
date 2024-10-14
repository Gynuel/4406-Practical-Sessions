###############################################
# The ToothGrowth data: practical session 2   #
###############################################

ToothGrowth # Dataset
names(ToothGrowth)  # Gets the column name of the data
t.test(len~supp,var.equal=TRUE, data=ToothGrowth)
# t.test(supp~len,var.equal=TRUE,data=ToothGrowth) -- This is wrong because grouping factor must have exactly 2 levels
# Grouping factor is after ~

par(mfrow=c(1,2))
boxplot(split(ToothGrowth$len,ToothGrowth$sup)) # This splits per group
boxplot(ToothGrowth$len,ToothGrowth$sup) # This splits per data/column
dev.off()

###############################################
# The faithful data: basic plot               #
###############################################

help(faithful)
faithful
faithful$eruption
mean(faithful$eruption)
x=faithful$eruption
mean(x)
median(x)
range(x)
min(x)
max(x)

hist(faithful$eruptions,main="eruptions time",col=2, nclass =50) 
hist(faithful$waiting) 
hist(faithful$eruptions,main="eruptions time") 
hist(faithful$eruptions,main="eruptions time",col=3,xlab="time of eruptions") 


par(mfrow=c(1,2))
hist(faithful$eruptions,main="eruptions time",col=2) 
hist(faithful$waiting,main="waiting time",col=3,xlab="waiting time") 
