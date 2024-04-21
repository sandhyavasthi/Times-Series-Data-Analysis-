#The package fAssets was written to explore and investigate datasetsof financial asssets
library(fAssets)

help(fAssets)

# Load Swiss Pension Fund Data: 
LPP <- LPP2005REC[, 1:3] 
head(LPP)

#arrange assets
assetsArrange(x=LPP, "pca") 
assetsArrange(x=LPP, "hclust") 
assetsArrange(x=LPP, "abc")

