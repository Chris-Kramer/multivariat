##load data, remember to check header T or F, also the location of the data needs to be right##

	dfBull <- read.table("./T1-10.dat", sep="", header=F, stringsAsFactors=F)

## assign the names of attributes as in the textbook, remember ''##

	colnames(dfBull) <- c('Breed', 'SalePr', 'YrHgt', 'FtFrBody', 'PrctFFB', 'Frame', 'BkFat', 'SaleHt', 'SaleWt')

## have a first look at data###

	head(dfBull)
	n <- nrow(dfBull)  #n#
	p <- ncol(dfBull)  #p#
	summary(dfBull)


### categoritical or numerical? a for-loop can take care of them all###
	unique(dfBull$Breed)
	unique(dfBull$Frame)
	unique(dfBull$BkFat)
	unique(dfBull$SalePr)
	
## scatter plot matrix more fancy ones see: http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs   ##
## Fig 1.5 in textbook P16##

	pdf('dfBullpair.pdf')
	pairs(dfBull[,1:4], pch = 19)
	dev.off()
	
	pdf('dfBullpairpart2.pdf')
	pairs(dfBull[,5:9], pch = 19)
	dev.off()

	mvec <- colMeans(dfBull) #sample mean vector#
	covM <- cov(dfBull)		#sample covariance matrix#
	corM <- cor(dfBull)	#sample correlation matrix#

### focus on the numerical attributes
	
	dfBullnum <- dfBull[, c(2,3,4,5,8,9)]
	head(dfBullnum)
	pdf('dfBullpairnum.pdf')
	pairs(dfBullnum, pch = 19)
	dev.off()
	
	##sample covariance matrix#
	covMnum <- cov(dfBullnum)
	##generalized sample variance##
	det(cov(dfBullnum))
	## total sample variance ###
	sum(diag(cov(dfBullnum)))
	
##### plot of centor of data ####
	pdf("./centralitysalebull.pdf")
	plot(dfBull$SaleHt, dfBull$SaleWt, col='blue', lwd=2)
	points(54.12632, 1555.28947, col='red', lwd=8)
	dev.off()
	
##### cov(saleHt, saleWt) #######

	covSaleHW <- cov(dfBull$SaleHt, dfBull$SaleWt)
	
#######uncorrelated vs independent (not my own code)#####
	Vec1 <- runif(200, min=-1, max=1)
	Vec2 <- ifelse(Vec1>0, Vec1, -Vec1)
	pdf('uncorrelated.pdf')
	plot(Vec1, Vec2, pch=16)
	dev.off()
	cov(Vec1, Vec2) 
	
########## depend on the scale of the data ###################
	covHW1 <- cov(dfBull$SaleHt, dfBull$SaleWt)
	covHW2 <- cov(dfBull$SaleHt, dfBull$SaleWt/1000)
	
	corHW1 <- cor(dfBull$SaleHt, dfBull$SaleWt)
	corHW2 <- cor(dfBull$SaleHt, dfBull$SaleWt/1000)
	
	
	
	