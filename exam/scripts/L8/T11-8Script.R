#
# cmds for Example 11. 11, with T11-8.dat file
#
#strange data set, n2=45 instead of 22, results are not the same as textbook
# so to be 100% sure lda() is the same thing as the (11-18) business, I implemented them both to convince you guys (well, I know that, but still need to show you guys)
#
## LDA see https://www.geeksforgeeks.org/linear-discriminant-analysis-in-r-programming/
##


example <- read.table("T11-8.dat",header=F)

colnames(example) <- c('y', 'x1', 'x2')


################################################################
# linear discriminant analysis using (11-18) - FISHER'S method #
################################################################
sncarrier <- example$y==1 #normal#
scarrier <- example$y==2  #carrier#

ncarrier <- example[sncarrier,c(2,3)]
carrier <- example[scarrier,c(2,3)]

#two centers#
m1 <- colMeans(ncarrier)
m2 <- colMeans(carrier)

#Spooled covariance matrix s as (11-17)#
s1 <- cov(ncarrier)
s2 <- cov(carrier)
s <- (29*s1+44*s2)/73

#inverse#
si <- solve(s)

a0 <- t(m1-m2)%*%si

m <- t(m1-m2)%*%si%*%(m1+m2)/2  #2nd term in (11-18)#

groupflag <- rep(2, nrow(example))

for(i in 1:nrow(example)){
	value <- a0 %*% t(as.matrix(example[i,2:3]))  #sweat with all the vector/matrix business#
	if(value >= m){
		groupflag[i] <- 1
	}
}

groupflag


##########################################
# linear discriminant analysis using LDA #
##########################################
library(MASS)

result1 <- lda(y ~ x1+x2,example,prior=c(.5,.5))  #x1 and x2 are predictors#
result1  #model of LDA#

examplex <- example[,c(2,3)]

pr <- predict(result1,examplex)  #make predictions based on the model provided by result1 and apply on examplex, output a lot of things#
pr

y <- example$y
yhat <- pr$class  #prediction of the group index#

groupflag==yhat  #see whether the two command lines reach to the same result, on my laptop is YES#

####################
# Confusion matrix #
####################
# confusion matrix for learning sample prediction, first part of evaluation for classification methods.
ConM <- table(y,yhat)
ConM

#######
# QDA #
#######
# qda analogue  #I am not going to check qda() is the same business as using (11-29), but you could give it a try

resultq <- qda(y~x1+x2,example,prior=c(.5,.5))
resultq

examplex <- example[,c(2,3)]

prq <- predict(resultq,examplex)
prq

y <- example$y
yhatq <- prq$class

#
# confusion matrix for learning sample prediction
ConMq <- table(y,yhatq)
ConMq

#
# IMPORTANT: recode the Y variable to a (0,1) variable #must be 0 or 1 for glm(), so now 0 for "1", and 1 for "2"#
#
example$y <- example$y-1

#######################
# Logistic regression #
#######################
# fit logistic regression model
resultlogi <- glm(y~x1+x2,example,family=binomial(link="logit"))
summary(resultlogi)

plogi <- resultlogi$fitted.values
y <- example$y


########
# APER #
########
# compute the apparent error rate

pr <- as.numeric( plogi >= .5)
table(y,pr)

##############
# Box m test #  
##############

# Test Homogeneous covariance matrices #

g <- 2 #two groups#
p <- 2 #two attributes#

examplegroup1 <- example[example$y==0, c(2,3)] #remember that we alter the indices in the logistic part, so 0 is the new 1#

examplegroup2 <- example[example$y==1, c(2,3)]

s1 <- cov(examplegroup1)
s2 <- cov(examplegroup2)

n1 <- nrow(examplegroup1)
n2 <- nrow(examplegroup2)
n <- n1+n2

w <- (n1-1)*s1+(n2-1)*s2  #Within matrix#
spooled <- w/(n-g)

# Compute M (6-50)
M <- (n-g)*log(det(spooled))-(n1-1)*log(det(s1))-(n2-1)*log(det(s2))

# Compute correction factor (6-51)
u <- (1/(n1-1)+1/(n2-1)-1/(n-g))*(2*p^2+3*p-1)/(6*(p+1)*(g-1))

# Test statistic
C <- (1-u)*M

# critical value
critvalue <- qchisq(.95,p*(p+1)*(g-1)/2)   #v=p*(p+1)*(g-1)/2#


### final decision ####
decisionflag <- (C > critvalue)   #False, therefore we should accept the H0, i.e. homogeneous covariance matrix, so in principle LDA#




















