# install.packages("caret")
# install.packages("glmnet")
# install.packages("car")
library(caret)
library(glmnet)
library(car)

### 1. logistic reg
rm(list = ls())
getwd()
setwd("D:\\tobigs2\\2weeks(reg,logistic)\\logistic")

# 1-1. 데이터 불러오기 









# # 1-2.전처리
# data$Survived <- as.factor(data$Survived)
# data$Pclass <- as.factor(data$Pclass)
# data$Pclass_1 <- as.factor(ifelse(data$Pclass=='1',1,0)) #Pclass = 1 이면 1, 아니면 0
# data$Pclass_2 <- as.factor(ifelse(data$Pclass=='2',1,0)) #Pclass = 2 이면 1, 아니면 0
# data <- data[,-which(names(data)=='Pclass')] #Pclass 지우기
# data$Sex <- as.factor(data$Sex)
# data$Embarked <- as.factor(data$Embarked)
# data$Embarked_C <- as.factor(ifelse(data$Embarked=='C',1,0)) #Embarked = C 이면 1, 아니면 0
# data$Embarked_Q <- as.factor(ifelse(data$Embarked=='Q',1,0)) #Embarked = Q 이면 1, 아니면 0
# data$Embarked_S <- as.factor(ifelse(data$Embarked=='S',1,0)) #Embarked = S 이면 1, 아니면 0
# data <- data[,-which(names(data)=='Embarked')] #Embarked 지우기

# 1-3.train set/ test set 나누기 
## createDataPartition 함수도 써보기! 모를 땐, ?createDataPartition 시행 후 예시 따라해보면 이해 쏙쏙 





# 1-4. glm 적용해보기 glm(y~.,data,family=binomial(link='logit'))









# 1-5. forward/ backward/ stepwise (full/not)glm 생성 후 step












# 1-6. roc 커브 그려보기
# install.packages("pROC")
library(pROC)
curve<-roc(test$Survived, pred_step, direction="<")
curve$auc
plot(curve)


### 2. Multinomial logistic reg

# 2-1. 데이터 불러오기
# install.packages("nnet")
library(nnet)
data(iris)
str(iris)
# 2-2. train set/ test set 나누기 createDataPartition(y,p=0.8,list=F)




# 2-3. multinom(y~.,data,link='logit')







### 3. ridge / lasso reg
# install.packages("mlbench")

# 3-1. 데이터 불러오기(train,test 나누기 까지)
library(mlbench)
data(Sonar)
str(Sonar)
idx<-sample(1:length(Sonar[,1]),length(Sonar[,1])*.8,replace = F)
train <- Sonar[idx,]
test <- Sonar[-idx,]

# 3-2. glm(y~.,data,family="binomial") 적용 --> 안 된다.


#Warning messages:
# 1: glm.fit: 알고리즘이 수렴하지 않았습니다 
# 2: glm.fit: 적합된 확률값들이 0 또는 1 입니다 
# 다중공선성 때문에 위의 말처럼 모든 값들이 0으로 예측하거나 1로 예측한다.


# 3-3. ridge 적용 cv.glmnet(x,y,family="binomail",alpha=0,nfolds=10) 하고 예측까지  [61번째]가 y


# 3-4. graph그리기 plot/ plot.glm





# 3-5 .동일하게 lasso 하는데 lambda를 여러번 조정해봐야 한다. 
# 적절한 변수 선택을 하기 위해서!(너무 중요한 변수가 사라질 수도 있는 걸 막기 위해서) 





# 3-6. 동시에
par(mfrow=c(2,2))