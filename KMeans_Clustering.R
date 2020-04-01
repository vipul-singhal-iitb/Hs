#final  Clustering

summary(Data_final)
Data_25_July[is.na(Data_25_July)] <- 0

summary(Data_25_July)


# Data sample
set.seed(12345)
data_train<- Browser.3.months[sample(1:nrow(Browser.3.months), 1000000,
                                     replace=FALSE),]
browsers_ID<-data_train[,1]
write.table(browsers_ID,file="D:/Analysis Vipul/Data for analysis/28 Mar/browsers_id_1mn.txt",sep="\t",row.names=F)

#clustering

Data_final_1 <- Data_final[c(-1,-200:-488)]
summary(Data_final_1)

set.seed(12387)

wssplot <- function(data, nc=40, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(Data_25_July[,-1])


k_20 <- kmeans(Data_25_July[,-1], 20,iter.max = 1000,nstart = 100,algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                                                                                "MacQueen"), trace=FALSE)

k_20$size

k_32 <- kmeans(Data_25_July[,-1], 32,iter.max = 1000,nstart = 100,algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                                                                                "MacQueen"), trace=FALSE)

k_32$size


k_22 <- kmeans(Data.for.group.shows.1Mn[,-1], 22,iter.max = 1000,nstart = 100,algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                                                                                            "MacQueen"), trace=FALSE)

k_22$size


k_15 <- kmeans(data_factor_cluster[,-1], 13,iter.max = 100,nstart = 50,algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                                                                                     "MacQueen"), trace=FALSE)

k_13$size

data_export_cluster <-Data_25_July[,1:2]
data_export_cluster$cluster_32 <- k_32$cluster
data_export_cluster$cluster_20 <- k_20$cluster
data_export_cluster$cluster_10 <- k_10$cluster
data_export_cluster <- data_export_cluster[,-2]
head(data_export_cluster)

write.table(data_export_cluster,file="data_export_cluster_svod.csv",sep=",",row.names=F)
