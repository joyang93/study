############################################################################
# <Data Description>                                                       #  
#1. Wife's now working?            (binary)           0=Yes, 1=No          #
#2. Wife's age                     (numerical)                             #
#3. Wife's education               (categorical)      1=low, 2, 3, 4=high  #
#4. Husband's education            (categorical)      1=low, 2, 3, 4=high  #
#5. Number of children ever born   (numerical)                             #
#6. Husband's occupation           (categorical)      1, 2, 3, 4           #
#7. Standard-of-living index       (categorical)      1=low, 2, 3, 4=high  #
############################################################################

#############################################################
#### 1. Data 부르기 및 전처리                            ####
#### 2. naivebayes::naive_bayes() model 만들고 구조보기  ####
#### 3. Option 조절                                      ####
#############################################################


######## 1.Data 부르기 및 전처리
rm(list=ls())

#### Path & Package

setwd("C:/Users/user/Desktop/tobigs/활동 2학기")
dir() # 현 디렉토리의 파일 리스트

WORK <- read.table('work.txt', header = TRUE, stringsAsFactors = TRUE)
for(i in c(1, 3, 4, 6, 7)) WORK[,i] <- factor(WORK[,i])
str(WORK)


#install.packages('caret')
library(caret)

#### Train / Test 

LABEL <- sample(c('A', 'B', 'C'), 1000, replace = TRUE)

propCompare <- function(Y, p) {
  N <- length(Y); nTrain <- round(p*N)
  
  set.seed(123)
  index.1 <- createDataPartition(Y, p = p, list = FALSE)
  index.2 <- sample(1:N, round(p*N), replace = FALSE)
  
  split.1 <- Y[index.1]; split.2 <- Y[index.2] 
  
  cat(' Original: ', table(Y)/N, '\n',
      'createDataPartition: ', table(split.1)/nTrain, '\n',
      'sample: ', table(split.2)/nTrain)
}

propCompare(LABEL, 0.7)
propCompare(LABEL, 0.5)

# createDataPartition을 쓰자

set.seed(123)
index <- createDataPartition(WORK[,1], p = 0.7, list = FALSE)
trainset <- WORK[index,]
testset <- WORK[-index,]

######## 2. naivebayes::naive_bayes() model 만들고 구조보기
#install.packages('naivebayes')
library(naivebayes)
library(caret) # http://topepo.github.io/caret/train-models-by-tag.html
library(dplyr)

#### 모수추정이 생각하는 것처럼 되나?
model_normal <- naive_bayes(trainset[,-1], trainset[,1], usekernel=FALSE, laplace = 0)

## 연속형 변수
model_normal$tables$wife_age
trainset %>% group_by(wife_work) %>% summarise(mean(wife_age), sd(wife_age))

## 범주형 변수
model_normal$tables$living_index
trainset %>% group_by(wife_work) %>% summarise(mean(children), sd(children))

## Prior
model_normal$prior
table(trainset$wife_work)/nrow(trainset)

######## option 조절

#### usekernel option
## Gaussian NB 
model_normal <- naive_bayes(trainset[,-1], trainset[,1], usekernel=FALSE, laplace = 0)
model_normal

## Flex NB 
model_kernel <- naive_bayes(trainset[,-1], trainset[,1], usekernel=TRUE, laplace = 0)
model_kernel

## 비교
library(ggplot2)
mu.1 <- model_normal$tables$children[,1][1]
sigma.1 <- model_normal$tables$children[,1][2]
mu.2 <- model_normal$tables$children[,2][1]
sigma.2 <- model_normal$tables$children[,1][2]
kernel_density.1 <- model_kernel$tables$children$`0`
kernel_density.2 <- model_kernel$tables$children$`1`

###
par(mfrow = c(1,2), lwd=2)

hist(trainset[trainset$wife_work==0, "children"], col = rgb(1,0,0,0.5), main = 'Non-working wife', 
     freq = FALSE, xlab = ''); par(new = T)
plot(kernel_density.1, axes=FALSE, ann=FALSE, lwd = 2, lty='dotted'); par(new = T)
plot(dnorm(seq(mu.1-3, mu.1+3, length.out=1e+4), mu.1, sigma.1), axes=FALSE, ann=FALSE, type='l')

hist(trainset[trainset$wife_work==1, "children"], col = rgb(0,0,1,0.5), main = 'Working wife', 
     freq = FALSE, xlab = ''); par(new = T)
plot(kernel_density.2, axes=FALSE, ann=FALSE, lty='dotted'); par(new = T)
plot(dnorm(seq(mu.2-3, mu.2+3, length.out=1e+4), mu.2, sigma.2), axes=FALSE, ann=FALSE, type='l')
###
dev.off() # plot 삭제 

#### laplace option


EX <- data.frame('합불' = c('합', '합', '불', '합', '불'), 
                 '서류' = c(10,8,8,7,9), 
                 '면접' = c(10,9,4,9,2), 
                 '석사' = B <- c('o', 'o', 'o', 'o', 'o'))


EX_laplace0 <- naive_bayes(EX[,-1], EX[,1], laplace = 0)
EX_laplace0$tables$'석사'

EX_laplace1 <- naive_bayes(EX[,-1], EX[,1], laplace = 1)
EX_laplace1$tables$'석사'

# 한번만 더해보자 laplace=2
# 분자에 2를 더하고 분자에 2X2=4를 더한다. 
# (2+2)/(2+4), (3+2)/(3+4) ==> (1/3, 5/7)
EX_laplace2 <- naive_bayes(EX[,-1], EX[,1], laplace = 2)
EX_laplace2$tables$'석사'


#### 성능
confusionMatrix(predict(model_normal, testset[,-1], type='class'), testset[,1])
confusionMatrix(predict(model_kernel, testset[,-1], type='class'), testset[,1])

##############################################################################
## 성능이 왜 안좋을까? ==> 과제 
