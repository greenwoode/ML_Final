setwd("C:/Users/zhuw2/Desktop/ML_Data/Final Project")
library(rpart)
diabetes_data <- read.csv(file = "diabetes_data.csv", head = TRUE, sep = ",")

idxs <- sample(1:nrow(diabetes_data),as.integer(0.8*nrow(diabetes_data)))
trainData <- diabetes_data[idxs,]
testData <- diabetes_data[-idxs,]

#cfit <- rpart(as.factor(Species)~.,data=trainIris,method="class")
cfit <- rpart(class~.,data = trainData,method="class")
name1="decisionTREE_diabetes"
num=1
ext=".pdf"
name2=paste(name1,num,ext,sep='')
pdf(name2)
plot(cfit,uniform=T,main="Decision Tree for Diabetes data")
text(cfit,use.n=T,all=T,cex=.6)
dev.off()

pred1 <- predict(cfit,data=testData,type="class")
#print(pred1)
t <- table(predict(cfit, testData, type = "class"), testData$class)
print(t)
d <- sum(diag(t))
n <- sum(t)
overall_ac=d/n
print(overall_ac)
n1 <- sum(t[1,])
ns <- t[1,1]
Acc1=ns/n1
print(Acc1)
n2 <- sum(t[2,])
ns <- t[2,2]
Acc2=ns/n2
print(Acc2)
 