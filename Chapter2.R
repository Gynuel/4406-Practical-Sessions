##########################################################################
##########################################################################
## CHAPTER 2: working with R functions                                  ##
##########################################################################
##########################################################################

##########################################################################
##########################################################################
## CHAPTER 2: extra example (working with external file)                ##
##########################################################################
##########################################################################


spwh3<-read.table('spwh3.txt', header=FALSE,na.strings="NA", dec=".")

dim(spwh3)
spwh3<-data.frame(spwh3)
names(spwh3)<-c("id","y","x1","gender")
y1<-spwh3$y[spwh3$gender==0] #Get all 'y' values of rows with Female Gender
y2<-spwh3$y[spwh3$gender==1] #Get all 'y' values of rows with Male Gender
t.test(y1,y2)


##########################################################################
##########################################################################
## CHAPTER 2: extra example (basic plot)                                ##
##########################################################################
##########################################################################


z<-rnorm(100,3,1)
mean(z)
median(z)
max(z)
min(z)

hist(z)
par(mfrow=c(1,2))
hist(z,col=4)
hist(z,col=5,nclass=25)
