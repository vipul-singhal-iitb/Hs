# final Factor Analysis 

Data__PCA_SVOD<-Data__PCA_SVOD[,-50]
PCA_non_reg <- prcomp(Data_non_reg[,-1],scores=TRUE, cor=TRUE)
summary(PCA_non_reg)
summary_pca<- summary(PCA_non_reg)
write.table(summary_pca$center,file="summary_pca_center.csv",sep=",",row.names=T)
write.table(summary_pca$importance,file="summary_pca_importance.csv",sep=",",row.names=T)
plot(PCA_non_reg)
screeplot(PCA_non_reg,type="lines",main="scree plot")

PCA_Final$scores # the principal components


factors_final_10 <- factanal(Data__PCA_SVOD[,-1], 10)
factors_final_30

loadings_svod <- loadings(factors_final_10) # pc loadings 
factors_final_30$uniquenesses
write.table(factors_final_10$uniquenesses,file="factors_svod_uniqueness_10.csv",sep=",",row.names=T)
write.table(loadings_svod,file="factors_loadings_svod_10.csv",sep=",",row.names=T)


factors_final_8 <- factanal(Data_final[,-1], 8, rotation="varimax")
factors_final_8
loadings_final_8 <- loadings(factors_final_8) # pc loadings 
factors_final_8$uniquenesses
write.table(factors_final_8$uniquenesses,file="factors_final_uniqueness_var.csv",sep=",",row.names=T)
write.table(loadings_final,file="factors_loadings_final_var.csv",sep=",",row.names=T)


data_final_scale <- scale(Data_final[,-1]) 
summary(data_final_scale)

#New
summary(Data_non_reg)
Data_non_reg[is.na(Data_non_reg)] <- 0
summary(Data_non_reg)