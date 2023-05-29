#matrix calculation#

vec <- c(1, -1, 0, 0, 1, -1)

matA <- matrix(vec, ncol=3, byrow=T)

mu <- c(1,2,1)

Sigma <- matrix(c(3,-1,1, -1,1,0, 1,0,2), ncol=3, byrow=T)

mu1 <- matA %*% mu

Sigma1 <- matA %*% Sigma %*% t(matA)

####total variance ##

totalVar <- sum(diag(Sigma1)) 

####generalized variance  ###

genVar <- det(Sigma1)