install.packages(c("FactoMineR"))

library(FactoMineR)

Pizza[2]<-NULL 
#Lab1
PCA.Pizza<-PCA(Pizza[1:8],quali.sup = c(1))
#Lab3
PCA.Pizza<-PCA(Pizza[1:7].quali.sup = c(1),scale.unit = TRUE)

print(PCA.Pizza)
View(PCA.Pizza$eig)
PCA.Pizza$var$coord
PCA.Pizza$var$contrib
