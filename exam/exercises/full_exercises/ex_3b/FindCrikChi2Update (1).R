

##### find critical value for chi-square distribution ####

##set up the proper n and p, different datasets different n and p of course#

n0 <- 42

p0 <- 2

alpha1 <- 0.05  #upper quantile/significant level#


N0 <- 1000 #iteration 1000 times, you could increase it, in fact should do it for different N until increase N does not change the critical value significantly#



FindcrikChi <- function(n=n0, p=p0, alpha=alpha1, N=N0){
	
	cricvec <- rep(0, N)  #vector for the rQ result collection#
	
	for(i in 1:N){
		#iteration to estimate rQ#
		numvec <- rchisq(n, p)  #generate a data set of size n, degree of freedom=p#
		d <- sort(numvec)
		q <- qchisq((1:n-0.5)/n, p)
		cricvec[i] <- cor(d,q)		
	}
	
	scricvec <- sort(cricvec)
	cN <- ceiling(N* alpha) #to be on the safe side I use ceiling instead of floor(), take the 'worst' alpha*N cor as rQ, everything lower than that is deemed as rejection#
	cricvalue <- scricvec[cN]
	result <- list(cN, cricvalue, scricvec)
	return(result)
}



result1 <- FindcrikChi(, , , )

val1 <- result1[[1]]

val2 <- result1[[2]]

print(val1)
print(val2)
