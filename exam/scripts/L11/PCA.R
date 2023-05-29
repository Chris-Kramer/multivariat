#script for PCA w.r.t Example 8.4, log transformation#

df1 <- read.table("E8-4.dat")
df <- log(df1)


head(df)
summary(df)


S <- cov(df)
S

pca1 <- prcomp(df)
pca2 <- prcomp(df, scale=T)  #result will be different, pca2 is free-of-scale (of the attributes)#

pca1  #the coefficients of the original coefficients#

#Rotation (n x k) = (3 x 3):
#         PC1        PC2        PC3
#V1 0.6831023 -0.1594791  0.7126974
#V2 0.5102195 -0.5940118 -0.6219534
#V3 0.5225392  0.7884900 -0.3244015
## There are originally 3 attributes, V1, V2 and V#. coefficients forthe first component(PC1) = 0.683*V1 + 0.510*V2 + 0.523*V3

summary(pca1)  #the proportion of variance#


#Importance of components:
#                          PC1     PC2     PC3
#Standard deviation     0.1527 0.02446 0.01897 #standard dev of each PC#
#Proportion of Variance 0.9605 0.02466 0.01483    #individual proportion of $kth$ component#
#Cumulative Proportion  0.9605 0.98517 1.00000	#accumulate the first several compoenents, at the end should be 1#


pdf("screeplotE84.pdf")
screeplot(pca1, npcs=3, type="l")  #make a scree plot to find the 'elbow' point#
dev.off()




