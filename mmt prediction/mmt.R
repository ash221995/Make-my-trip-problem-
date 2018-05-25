setwd("/home/ashok/Desktop/dataset")
library(plyr)
library(mice)
library(rpart)
library(rpart.plot)

train<-read.csv("/home/ashok/Desktop/dataset/train.csv")
test<-read.csv("/home/ashok/Desktop/dataset/test.csv")
sub<-read.csv("/home/ashok/Desktop/dataset/sample_submission.csv")
View(train)

#missing value treatment using mice package  
md.pattern(train_com)
imp.train<-mice(train, m=1,method = "cart",printFlag=FALSE)
xyplot(imp.train,B ~O)
densityplot(imp.train, ~B)
train_com<-complete(imp.train)

#model 
model<-rpart(P ~., data = train_com)
rpart.plot(model)

#test 
md.pattern(test_com)
imp.test<-mice(test, m=1,method = "cart",printFlag=FALSE)
xyplot(imp.test,B ~O)
densityplot(imp.test, ~B)
test_com<-complete(imp.test)

#prediction
a<-predict(model,newdata = test_com)
z<-ifelse(a>.4,1,0)
id<-as.vector(id<-test$id)
P<-as.vector(P<-z)
df<-data.frame(id,P)
head(df)

#writing into csv file 
write.csv(x = df,file = "sub.csv",row.names = F)
View(sub)

