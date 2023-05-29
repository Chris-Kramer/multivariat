df <- read.table("./T11-6.dat", header=F, sep='')
head(df)
colnames(df) <- c('GPA', 'GMAT', 'POP')
head(df)
summary(df)
colMeans(df)

pdf('./centrality.pdf')
plot(df$GPA, df$GMAT, col='blue', lwd=2)
points(2.974588, 488.447059, col='red', lwd=2)
dev.off()

n <- nrow(df)

##### check the denominator ######
var1 <- 0
mean1 <- mean(df[,1])

for(i in 1:n){
	var1 <- var1 + (df[i,1]-mean1)^2/n
}

var2 <- 0
for(i in 1:n){
	var2 <- var2 + (df[i,1]-mean1)^2/(n-1)
}


