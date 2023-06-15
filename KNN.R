dia <- read.csv(file = "diabetes.csv", head = TRUE, sep = ",")

dia$Age <- (dia$Age - min(dia$Age))/(max(dia$Age) - min(dia$Age))

idxs <- sample(1:nrow(dia),as.integer(0.8*nrow(dia)))
trainDia <- dia[idxs,]
trainLabel <- trainDia$class

testDia <- dia[-idxs,]
testLabel <- testDia$class

#install.packages('class')
library(class)

output <- knn(trainDia, testDia, trainLabel, k=4)

acctable <- table(testLabel,output)
print(acctable)

t <- acctable

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

