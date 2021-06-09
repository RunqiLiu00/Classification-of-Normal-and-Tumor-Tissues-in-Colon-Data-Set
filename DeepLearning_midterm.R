#Import data and data preparation
library(readxl)
i2000 = read_excel("C:/Users/ADMIN/Desktop/i2000_1.xlsx",  col_names = FALSE)
colnames(i2000)=paste("X",1:2000,sep="")
#View(i2000)
tissues= read_excel("C:/Users/ADMIN/Desktop/tissues.xlsx",  col_names = FALSE)
colnames(tissues)=c("tissues")
tissues[tissues$tissues>0,1]=0
tissues[tissues$tissues!=0,1]=1 #0:normal tissues; 1: tumor
#View(tissues)
geneData=cbind(tissues,i2000)
#View(geneData)
anyNA(geneData)#no missing value
#split data
# train=geneData[1:40,]
# test=geneData[41:62,]
n=62
set.seed(666)
trainindex=sample(1:n,0.7*n)
train=geneData[trainindex,]
test=geneData[-trainindex,]
###logistic regression model
fit1 = glm(tissues~.,data=train,family='binomial')
summary(fit1)
#In the case where p>n, logistic regression does not work.

###lasso for the logistic regression

library('glmnet')
train_mat = data.matrix(train)
r0=glmnet(train_mat[,2:2001],train_mat[,1])
plot(r0) #Plot the shinkage of coefficient
set.seed(123)
cv=cv.glmnet(train_mat[,2:2001],train_mat[,1],nfolds=5)
plot(cv) #find the best lambda

bestLambda=cv$lambda.min
bestLambda

fit2=glmnet(train_mat[,2:2001],train_mat[,1],lambda=bestLambda,family = "binomial")#?should I add binomial
coef=fit2$beta[fit2$beta!=0]
coef=sort(coef,decreasing = T)
gene_index=match(coef,as.vector(fit2$beta))
cbind(gene_index,coef)# Selected genes and coefficients, in decreasing order

# model validation for the test data
test_mat=data.matrix(test)
ypredict=predict(fit2,type='response',newx = test_mat[,2:2001])

#assess the model
errorLasso=mean((test_mat[,1]-ypredict)^2)
errorLasso

# classification error
yclassified=round(ypredict)#classify
classificationError=mean(yclassified!=test_mat[,1])
classificationError
ytest=test_mat[,1]
plot(ypredict,ytest,pch=20,ylab="classes",xlab="predicted values")
points(ypredict,yclassified,col="red",pch=2)
legend(0.2, 0.9, pch=20, "true")
legend(0.2, 0.4, pch=2, "classified", col='red')

#Confusion matrix
library(caret)

confusionMatrix(factor(yclassified),factor(ytest))

###CART 还要调参
#https://blog.csdn.net/weixin_36372879/article/details/80493968
#https://www.cnblogs.com/karlpearson/p/6224148.html
library(rpart)
as.factor(train$tissues)
as.factor(test$tissues)

tc =rpart.control(minsplit=2,minbucket=5,maxdepth=10,xval=5,cp=0.005)
fit3=rpart(tissues~.,data=train,method="class", parms=list(split="gini"),control = tc)
printcp(fit3)
fit3$variable.importance#check variables' importance
plotcp(fit3)

library(rpart.plot)
rpart.plot(fit3,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="Decision tree")

#prune
fit3_pr=prune(fit3, cp= fit3$cptable[which.min(fit3$cptable[,"xerror"]),"CP"]) 
fit3_pr$cp

rpart.plot(fit3_pr,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="Decision tree")
fit3_pr$variable.importance#check variables' importance

#test, assess the model
ypredict2=predict(fit3_pr,test)
yclassified2=round(ypredict2[,2])
table(ytest,yclassified2,dnn=c("actual","predicted")) #confusion matrix

errorCart=mean((test_mat[,1]-ypredict2)^2)
errorCart
classificationError2=mean(yclassified2!=test_mat[,1])#Error rate
classificationError2

###kNN

library(kknn)
fit4=kknn(tissues~.,train,test,distance=2,kernel="rectangular",k=2)#还要调参

ypredict3=predict(fit4)

yclassified3=round(ypredict3)
table(ytest,yclassified3,dnn=c("actual","predicted")) #confusion matrix
#assess the model
errorKnn=mean((test_mat[,1]-ypredict3)^2)
errorKnn
classificationError3=mean(yclassified3!=test_mat[,1])#Error rate
classificationError3

###Random forest

library(randomForest)
set.seed(666)
fit5=randomForest(tissues~.,train,na.action = na.roughfix,importance=TRUE)
fit5
import=sort(importance(fit5,type=2),decreasing = T)[1:50] # importance measured by mean decrease Gini
gene_index2=match(import,importance(fit5,type=2))
cbind(gene_index2,import)# Selected important genes

#assess the model with test data
ypredict4=predict(fit5,test)
yclassified4=round(ypredict4)
table(test$tissues,yclassified4,dnn=c("actual","predict"))#confusion matrix
errorForest=mean((test_mat[,1]-ypredict4)^2)
errorForest
classificationError4=mean(yclassified4!=test_mat[,1])#Error rate
classificationError4
      








