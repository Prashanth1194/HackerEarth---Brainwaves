train <- read.csv("E:/Hackathons/HackerEarth/BrainWaves/train.csv", stringsAsFactors=FALSE)
test <- read.csv("E:/Hackathons/HackerEarth/BrainWaves/test.csv", stringsAsFactors=FALSE)





library(xgboost)

train1$Y = ifelse(train1$Y==-1,0,train1$Y)

xgb <- xgboost(data = data.matrix(train1[,c(2:101)]), 
               label = (train1$Y), 
               eta = 1,
               nround=25,
               set.seed = 1,
               eval_metric = "logloss",
               objective = "binary:logistic"
               
               
) 


pred_xgb = predict(xgb,data.matrix(train1[,c(2:101)]))

table(train1$Y,pred_xgb>0.5)

test1$Y = predict(xgb,data.matrix(test1[,c(2:101)]))

test1$Y = ifelse(test1$Y>0.5,1,-1)

table(test1$Y)

sub = test1[,c(1,102)]

write.csv(sub,"Predict the growth/xgboost_normal_25rounds_diff_.csv",row.names = F)

train1 = train

test1 =test

for(i in 2:nrow(train1))
{
  
  for(j in 2:101)
  {
    train1[i,j] = ifelse(train[i,j]>train[i-1,j],1,0)
    
    
  }
  
}


for(i in 2:nrow(test1))
{
  
  for(j in 2:101)
  {
    test1[i,j] = ifelse(test[i,j]>test[i-1,j],1,0)
    
    
  }
  
}


for(i in 2:nrow(train1))
{
  
  for(j in 2:101)
  {
    train1[i,j] = (train[i,j]-train[i-1,j]) #/train[i-1,j])*100
    
    
  }
  
}

for(i in 2:nrow(test1))
{
  
  for(j in 2:101)
  {
    test1[i,j] = (test[i,j]-test[i-1,j]) #/test[i-1,j])*100
    
    
  }
  
}


train1[1,] = 0
test1[1,] = 0

train1[1,1] = 1
train1[1,102] = 1
test1[1,1] = 3001
#test1[1,] = (test[1,] - train[3000,])/train[3000,] * 100

train1 = train1[,-c(1,102)]
test1 = test1[,-c(1,102)]

#train1$Time = train$Time
#train1$Y = train$Y

for(i in 1:nrow(train1))
{
  train1$pos_count[i] = sum(train1[i,])
  
}

for(i in 1:nrow(test1))
{
  test1$pos_count[i] = sum(test1[i,])
  
}



train1$neg_count = 100 - train1$pos_count
train1$diff = train1$pos_count - train1$neg_count

train2 = train1[,c(101,102,103)]

train3 = cbind(train,train2)


test1$neg_count = 100 - test1$pos_count
test1$diff = test1$pos_count - test1$neg_count

test2 = test1[,c(101,102,103)]

test3 = cbind(test,test2)

train4 = cbind(train1,train2)
test4 = cbind(test1,test2)

fit = glm(as.factor(Y)~.,data=train1[,-1],family =binomial)
summary(fit)

pred = predict(fit,type="response")

pred = ifelse(pred >=0.5,1,-1)

table(pred,train4$Y)

test1$Y = predict(fit,test1,type="response")

test1$Y = ifelse(test1$Y >=0.5,1,-1)

sub = test1[,c(1,102)]

write.csv(sub,"Predict the growth/subm_per_no_time_glm.csv",row.names = F)

library(randomForest)

fit_rf = randomForest(as.factor(Y)~.,data=train4)
pred_rf = predict(fit_rf,type="class")
table(pred_rf,train4$Y)

test4$Y = predict(fit_rf,test4,type="class")
#test3$Y = ifelse(test3$Y >=0.5,1,-1)
sub = test4[,c(1,102)]
write.csv(sub,"Predict the growth/subm_rf_per_pos_neg.csv",row.names = F)


############ VIF and PCA ###################

vif_func(train[,c(1:101)])

vif_names = c("X2" ,"X6"   ,"X10",  "X11",  "X12",  "X15"  ,"X18"  ,"X30" ,
              "X31" , "X37" , "X38" , "X41"  ,"X52" , "X57","X58",  "X64"  ,
              "X67" , "X79" , "X81" , "X92" , "X94" , "X98"  ,"X100")

train_vif = train[,vif_names]


prin_com <- prcomp(train_vif,scale.=T)
names(prin_com)

prin_com$rotation

std_dev <- prin_com$sdev

pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

train_pca = data.frame(Y=train$Y,prin_com$x[,1:20])

test_vif = test[,vif_names]
test_pca = predict(prin_com,test_vif)
test_pca = data.frame(test_pca)
test_pca = test_pca[,1:20]

library(randomForest)
fit_pca = randomForest(as.factor(Y)~.,data=train_pca)
pred_pca = predict(fit_pca,type="class")
table(train_pca$Y,pred_pca)

test_pca$Y = predict(fit_pca,test_pca)
table(test_pca$Y)

fit_pca_glm = glm(as.factor(Y)~.,data=train_pca,family =binomial)
summary(fit_pca_glm)

pred_pca_glm = predict(fit_pca_glm,type="response")

pred_pca_glm = ifelse(pred_pca_glm>0.5,1,-1)
table(train_pca$Y,pred_pca_glm)
test_pca$Y = predict(fit_pca_glm,test_pca,type="response")


test_pca$Y = ifelse(test_pca>0.5,1,-1)
table(test_pca$Y)

sub$Time = test$Time
sub$Y = test_pca$Y
write.csv(sub,"pca.csv",row.names = F)
