##########################################################################
##########################################################################
## Programming in R: Introduction                                       ##
##########################################################################
##########################################################################


##########################################################################
##########################################################################
## CHAPTER 1 (basic programming)                                        ##
##########################################################################
##########################################################################

x <- 5
x
x^2
x + 6

x<-c("A","A","A","A","B","B","B","B")
x


y<-c(10,11,9,15,3,5,7,2)
y

ya<-y[x=="A"]
ya

tapply(y,x,mean) # Applies the mean function to every group/factor 'x' in the values 'y'

z<-data.frame(x,y)
z

z$x
z$y

w<-c(1,2,40,2,3,9,200,4,6000)
matw<-matrix(w,3,3) # Creates the variable 'w' into a 3x3 matrix
matw
diag(matw) # Gets the diagonal values of a matrix
solve(matw) # This computes the inverse of the matrix (assuming invertible)
# In linear algebra the inverse of a matrix A is the Matrix B such that
# A X B = I (Identity matrix)


x<-c(25,36,21)
gender<-c("M","M","F")
xdat<-data.frame(x,gender)
xdat
