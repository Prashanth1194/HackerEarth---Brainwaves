train <- read.csv("E:/Hackathons/HackerEarth/BrainWaves/train.csv", stringsAsFactors=FALSE)


train1 = train

### 1 if there is an increase in price from previous day else 0 #############

for(i in 2:nrow(train1))
{
  
  for(j in 2:101)
  {
      train1[i,j] = ifelse(train[i,j]>train[i-1,j],1,0)
    
    
  }
  
}

train1[1,] = 0

train1 = t(train1)


train1 = train1[-c(1,102),]

##### Identifying the best no. of clusters to choose through various possible metrics ######

library(NbClust)

ind = c("kl", "ch","hartigan","cindex",
   "db", "silhouette", "duda", "pseudot2", "beale",
    "ball", "ptbiserial", "gap", "frey",
   "mcclain", "gamma", "gplus", "tau", "dunn",
   "hubert", "sdindex", "dindex", "sdbw")

values = list()
j = 1


for(i in ind)
{

values[j] = NbClust(data = train1, distance = "euclidean", min.nc = 5, max.nc = 15,method = "kmeans",index = i) 
 j = j + 1
}

###### K means clustering algorithm for 12 clusters ##########

fit <- kmeans(train1,12) 

aggregate(train1,by=list(fit$cluster),FUN=mean)

train2 <- data.frame(train1, fit$cluster)


train3 = train2[,c(1,3001)]
train3$Asset = row.names(train3)
train3 = train3[,-1]
train3 = train3[,c(2,1)]
names(train3) = c("Asset","Cluster")



write.csv(train3,"Asset Clustering/subm_12_OCt21_check.csv",row.names = F)

