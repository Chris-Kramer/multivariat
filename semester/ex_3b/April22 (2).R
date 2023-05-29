####Lecture Apr 22, some code copy-paste from other lecturers##
#a good guidence by https://www.r-bloggers.com/2021/06/qq-plots-in-r-quantile-quantile-plots-quick-start-guide/
#

#
# Microwave oven radiation data
#

#
# Assessing univariate normality: histogram + fitted normal density function
#

x <- read.table("t4-1.dat",header=FALSE)

y <- read.table("t4-5.dat",header=FALSE)

x <- x$V1

y <- y$V1

m <- mean(x)
s <- sd(x)
z <- seq(0,.4,.01)
f <- dnorm(z,m,s)
pdf("./histclosedoor.pdf")   #histogram vs density function#
	hist(x,breaks=seq(0,.4,.05),probability=TRUE,main="",xlab="Radiation (door closed)")
	lines(z,f,type="l")  #the density function of normal distribution with sample mean and sample sd #
	title("Door Closed")
dev.off()


##### open-door's turn ###### 


m <- mean(y)
s <- sd(y)
z <- seq(0,.6,.01)
f <- dnorm(z,m,s)
pdf("./histopendoor.pdf")
	hist(y,probability=TRUE,main="",xlab="Radiation (door open)")
	lines(z,f,type="l")
	title("Door Open")
dev.off()


# Assessing univariate normality: normal quantile plots as textbook, DIY  #
#

x <- sort(x)	# empirical quantiles for (1-0.5)/n, (2-0.5)/n, ..., (n-0.5)/n #
q <- qnorm((1:42-0.5)/42)  # (0.9279049) theoretical quantiles for (1-0.5)/n, (2-0.5)/n, ..., (n-0.5)/n #

pdf('qqplotclosed1.pdf')
plot(q,x,xlab="Standard normal quantile",ylab="Ordered data")   
title("(a)")
dev.off()

cororigin <- cor(q,x)

# #
# # alternatively using existing package, my code
# #

dfc <- read.table("t4-1.dat",header=FALSE)  #dfc for closed door#
pdf("qqplotclosed2.pdf")
	qqc <- qqnorm(dfc$V1)
	qqline(dfc$V1)
dev.off()
corqq <- cor(qqc$x, qqc$y)   #corc (0.9279049) is almost the same as cororigin (0.9276303) until third decimal, so difference is rather neglectable#

pdf("qqplotsimu.pdf")   ##a good one with normal simulated data#
	simu <- rnorm(100)
	qqs <- qqnorm(simu)
	qqline(simu)
dev.off()
cors <- cor(qqs$x, qqs$y)  ##0.9926489  which is far better than the t4-1 data##



### NOW EXERCISE: could you do the above for the open-door data 't4-5.dat'?  #####

### Box-Cox Transformation part ###
##read R-doc https://www.rdocumentation.org/packages/MASS/versions/7.3-56/topics/boxcox
library(MASS)

pdf("reproduceF412.pdf")
boxcoxTransc <- boxcox(dfc$V1~1,lambda=seq(-.5,1.5,.01))
title("(Fig 4.12)")
dev.off()

#find the maximum#
flagidx <- which(boxcoxTransc$y==max(boxcoxTransc$y))

optlam <- boxcoxTransc$x[flagidx]  #0.28#

vec1 <- dfc$V1

transvec <- (vec1^.28-1)/.28  #according to (4-34)#

pdf("qqplottransclosed.pdf")   ##a good one with normal simulated data#
	qqts <- qqnorm(transvec)
	qqline(transvec)
dev.off()
cortrans <- cor(qqts$x, qqts$y) 




########################April 22 stop here  ###########################################


# #
# # Evaluate bivariate normality: scatterplot with 0.25, 0.50 and 0.75 probability ellipse
# #

# x <- read.table("t4-1.dat",header=FALSE)
# x <- x$V1

# y <- read.table("t4-5.dat",header=FALSE)
# y <- y$V1

# radiation <- data.frame(x,y)

# m <- mean(radiation)

# plot(radiation$x,radiation$y,xlab="Radiation (door closed)",ylab="Radiation (door open)",xlim=c(0,0.7),ylim=c(0,.7))
# lines(c(m[1],m[1]),c(-10,m[2]),lty=2)
# lines(c(-10,m[1]),c(m[2],m[2]),lty=2)
# points(m[1],m[2],pch=4)


# c <- cov(radiation)
# cinv <- solve(c) 

# x1 <- seq(-.1,.6,.01)
# x2 <- seq(-.1,.6,.01)

# n <- length(x1)

# f <- matrix(0,n,n)

# for (i in 1:n){
# for (j in 1:n){

# xv <- c(x1[i],x2[j])

# f[i,j] <- t(xv-m)%*%cinv%*%(xv-m)

# }
# }

# chiq <- qchisq(seq(.25,.75,.25),2)
# contour(x1,x2,f,levels=chiq,xlab="Radiation (door closed)",ylab="Radiation (door open)")
# points(radiation$x,radiation$y)
# lines(c(m[1],m[1]),c(-10,m[2]),lty=2)
# lines(c(-10,m[1]),c(m[2],m[2]),lty=2)
# points(m[1],m[2],pch=4)

# #
# # Evaluate bivariate normality: count the number of points
# # in the 0.25, 0.50 and 0.75 probability ellipse
# #

# d <- rep(0,42)
# for (i in 1:42){
# xv <- t(radiation[i,])
# d[i] <- t(xv-m)%*%cinv%*%(xv-m)
# }

# sum(d < qchisq(.25,2))
# sum(d < qchisq(.5,2))
# sum(d < qchisq(.75,2))


# #
# # Evaluate bivariate normality: chi-square quantile plot of squared Mahalanobis distances
# #


# d <- sort(d)
# q <- qchisq(1:42/43,2)
# plot(q,d,xlab="Chi-square quantiles",ylab="Ordered squared Mahalanobis distances")
# abline(0,1)