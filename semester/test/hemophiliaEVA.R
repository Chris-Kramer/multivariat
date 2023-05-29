

#revisit the hemophilia data set for CV of glm and SOC [AUC]#


example <- read.table("t11-8.txt",header=TRUE)  
n <- nrow(example)

pdf("scatterplotHemo.pdf") #Have a look at the old friend#
	plot(example$x1,example$x2,pch=example$y,xlab="log10(AHF activity)",ylab="log10(AHF-like antigen)")
dev.off()

#
# recode the Y variable to a (0,1) variable
#

example$y <- example$y-1

#
# fit logistic regression model
#

result <- glm(y~x1+x2,example,family=binomial(link="logit"))
summary(result)

p <- result$fitted.values
y <- example$y

#
# compute the apparent error rate
#

pr <- as.numeric( p >= .5)
table(y,pr)

#
# Plot the ROC curve, documentation of R is always one of your best friends
## google is the other one, if you can find the 'good' ones:  https://www.r-bloggers.com/2014/12/a-small-introduction-to-the-rocr-package/
#  https://cran.r-project.org/web/packages/ROCR/index.html

library(ROCR)
pred <- prediction(p,y)
perf <- performance(pred,"tpr","fpr")
pdf("HemophiliaLogiRoc.pdf")
	plot(perf,colorize=F,lwd=3)
dev.off()

#
# compute the area under the curve
#


AUChemo <- performance(pred, measure="auc")
AUChemo1 <- AUChemo@y.values  #0.96#


#
# cross-validation
## nothing fancy, just a for-loop
#

prcv <- rep(0,n)
for (i in 1:n){
	example1 <- example[-i,]
	res1 <- glm(y~x1+x2,example1,family=binomial(link="logit"))
	xc <- example[i,2:3]
	lp <- predict(res1,xc)
	prcv[i] <- exp(lp)/(1+exp(lp))
}

#
# compute the CV error rate
#

pr <- as.numeric( prcv >= .5)
table(y,pr)

#
# Plot the CV ROC curve
#

pred1 <- prediction(prcv,y)
perf1 <- performance(pred1,"tpr","fpr")
pdf("HemophiliaLogiROCcv.pdf")  #looks like the coloful one is better 0.96>0,94, with a bigger area under the curve#
	plot(perf,colorize=T,lwd=3)
	plot(perf1,colorize=F,lwd=3, add=T)
dev.off()



#
# compute the area under the curve
#

AUChemoCV <- performance(pred1, measure="auc")
AUChemoCV1 <- AUChemoCV@y.values   #0,94#

