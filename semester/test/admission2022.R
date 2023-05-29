admission <- read.table("t11-6.txt",header=TRUE)  #diference with .dat is I added header in :-)#


pdf("scatter.pdf")
	plot(admission$GPA,admission$GMAT,pch=admission$ADMIT,ylim=c(200,800),xlim=c(2,4),xlab="GPA",ylab="GMAT")  #believe or not, ADMIT is dependent/response, GPA and GMAT are predictors#
dev.off()

### in the following, we do the LDA and QDA as textbook formulas, output can be seen in PDF files##
###afterwards we use MASS: lda() and qda() as well  ###

#
# LDA - classification regions, Box's M-test is supposed to be an exercise for you now
#

s1 <- admission$ADMIT==1
s2 <- admission$ADMIT==2
s3 <- admission$ADMIT==3

x1 <- admission[s1,c(1,2)]
x2 <- admission[s2,c(1,2)]
x3 <- admission[s3,c(1,2)]

n1 <- nrow(x1)
n2 <- nrow(x2)
n3 <- nrow(x3)

m1 <- colMeans(x1)
m2 <- colMeans(x2)
m3 <- colMeans(x3)
s1 <- cov(x1)
s2 <- cov(x2)
s3 <- cov(x3)
sp <- ((n1-1)*s1+(n2-1)*s2+(n3-1)*s3)/(n1+n2+n3-3) #Spooled is HERE#
spi <- solve(sp)
p <- c(1/3,1/3,1/3)  #can be different, but here I assume they are equal likely#


#for the plots of LDA#
gpa <- seq(2,4,.01)
gmat <- seq(200,800,2)
l1 <- length(gpa)
l2 <- length(gmat)
d1 <- matrix(0,l1,l2)
d2 <- matrix(0,l1,l2)

for (i in 1:l1){
for (j in 1:l2){
xc <- c(gpa[i],gmat[j])
f1 <- log(p[1])-t(xc-m1)%*%spi%*%(xc-m1)/2
f2 <- log(p[2])-t(xc-m2)%*%spi%*%(xc-m2)/2
f3 <- log(p[3])-t(xc-m3)%*%spi%*%(xc-m3)/2
d1[i,j] <- f1-max(f2,f3)
d2[i,j] <- f2-max(f1,f3) 
}
}

pdf("LDA.pdf")
	plot(admission$GPA,admission$GMAT,pch=admission$ADMIT,ylim=c(200,800),xlim=c(2,4),xlab="GPA",ylab="GMAT")
	contour(gpa,gmat,d1,levels=0,add=T,col="blue",lwd=2)
	contour(gpa,gmat,d2,levels=0,add=T,col="blue",lwd=2)
	text(3.5,750,"R1",col="blue")
	text(2.25,250,"R2",col="blue")
	text(3.5,250,"R3",col="blue")
dev.off()

#
# QDA - classification regions
#

s1i <- solve(s1)
s2i <- solve(s2)
s3i <- solve(s3)

gpa <- seq(2,4,.01)
gmat <- seq(200,800,2)
l1 <- length(gpa)
l2 <- length(gmat)
d1 <- matrix(0,l1,l2)
d2 <- matrix(0,l1,l2)

c1 <- log(p[1])-log(det(s1))/2
c2 <- log(p[2])-log(det(s2))/2
c3 <- log(p[3])-log(det(s3))/2


for (i in 1:l1){
	for (j in 1:l2){
		xc <- c(gpa[i],gmat[j])
		f1 <- c1-t(xc-m1)%*%s1i%*%(xc-m1)/2
		f2 <- c2-t(xc-m2)%*%s2i%*%(xc-m2)/2
		f3 <- c3-t(xc-m3)%*%s3i%*%(xc-m3)/2
		d1[i,j] <- f1-max(f2,f3)
		d2[i,j] <- f2-max(f1,f3) 
	}
}

pdf("QDA.pdf")
	plot(admission$GPA,admission$GMAT,pch=admission$ADMIT,ylim=c(250,700),xlim=c(2,4),xlab="GPA",ylab="GMAT")
	contour(gpa,gmat,d1,levels=0,add=T,col="blue",lwd=2)
	contour(gpa,gmat,d2,levels=0,add=T,col="blue",lwd=2)
	text(3.75,400,"R1",col="blue")
	text(2.5,275,"R2",col="blue")
	text(3.5,300,"R3",col="blue")
dev.off()
#
# linear discriminant analysis with MASS
#

library(MASS)  #need to install if you never use it before: I hope this is not the case :-)#

p <- c(1/3,1/3,1/3)


#
# LDA - predict new case (book page 615)
#

result <- lda(ADMIT~GPA+GMAT,admission,prior=p)

xnew <- data.frame(GPA=3.21,GMAT=497)
pr <- predict(result,xnew)
pr

#
# LDA - APER: Apparent error rate
#

pr <- predict(result,admission)

y <- admission$ADMIT
yhat <- pr$class

T1 <- table(y,yhat)

#
# LDA - Cross-Validation, just add CV=TRUE, smart R...##
#

result <- lda(ADMIT~GPA+GMAT,admission,prior=c(1/3,1/3,1/3),CV=TRUE)
yhatcv <- result$class
T2 <- table(y,yhatcv)

#
# QDA - APER
#

result <- qda(ADMIT~GPA+GMAT,admission,prior=p)
pr <- predict(result,admission)
yhat <- pr$class
T3 <- table(y,yhat)

#
# DQA - CV
#

result <- qda(ADMIT~GPA+GMAT,admission,prior=p,CV=TRUE)
yhatcv <- result$class
T4 <- table(y,yhatcv)

###'nnet' package documentation#
# https://cran.r-project.org/web/packages/nnet/nnet.pdf #

library(nnet)  

## multinom(formula, data, weights, subset, na.action,
# #            contrasts = NULL, Hess = FALSE, summ = 0, censored = FALSE,
#             model = FALSE, ...)
#


multinomModel <- multinom(ADMIT~GPA+GMAT,admission)  
prlogi <- predict(multinomModel, admission) #nearly the same syntax#
prlogi #directly to class part...#
T5 <- table(y, prlogi)

###It seems to me, multinom does not have CV by default, so I will leave this as a self-code exercise, one understands the procedure, then one should also be able to implement it ###
