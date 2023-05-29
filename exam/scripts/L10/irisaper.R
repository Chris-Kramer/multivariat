iris <- read.table("t11-5.txt",header=TRUE)

library(MASS)

result <- lda(y~x1+x2+x3+x4,iris,prior=c(1/3,1/3,1/3))
pr <- predict(result,iris)

plot(pr$x[,1],pr$x[,2],pch=iris$y)

y <- iris$y
yhatcv <- pr$class
t <- table(y,yhatcv)
aper <- (sum(t)-sum(diag(t)))/sum(t)
aper

postscript("iris.eps",horizontal=FALSE,onefile=FALSE,height=5,width=6,pointsize=10)
plot(iris[,1:4],pch=iris$y)
dev.off()


result1 <- lda(y~x1+x2+x3,iris,prior=c(1/3,1/3,1/3))
pr1 <- predict(result1,iris)

plot(pr1$x[,1],pr1$x[,2],pch=iris$y)

y1 <- iris$y
yhatcv1 <- pr1$class
t1 <- table(y,yhatcv1)
aper1 <- (sum(t1)-sum(diag(t1)))/sum(t1)
aper1