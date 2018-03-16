# 다음의 주어진 데이터로 1. Softmax 함수를 구현하세요!
# Beta 는 2. Cross-entropy 함수를 만들어서 이용하세요!
rm(list=ls())
library(UsingR)
data("iris")
str(iris)
x <-iris[,-5]
y <-iris[,5]

set.seed(1234)
index <- sort(sample(1:length(x[,1]),length(x[,1])*0.8,replace = F))
train_x <- x[index,]
train_y <- y[index]  
test_x <- x[-index,]
test_y <- y[-index]

# library(data.table)
# labeled_y2 = with(iris, data.frame(model.matrix(~Species-1,iris)))
# labeled_y2 = as.matrix(labeled_y2)
# colnames(labeled_y2)<-levels(y)



# x: 라벨링뗀 데이터  y: only 라벨링 데이터
Cross_entropy = function(x,y,iter,learning_rate ){

####### setting #######
  #one-hot encoding
  one_hot_label = matrix(0, nrow = length(y), ncol = length(levels(y)))
  colnames(one_hot_label) = levels(y)
  for (i in 1:length(levels(y))){
          one_hot_label[,i] = ifelse(y == levels(y)[i], 1, 0)
  }

  # X matrix 
  X = cbind(rep(1, nrow(x)), x) #  beta0를 한열벡터로 추가한 라벨이 떼진 데이터 
  colnames(X)[1] = 'beta0'
  X = as.matrix(X)
  
  #dim(beta) 수는 {(설명변수+1)개,y levels수}
  n_levels = length(levels(y)) # 맞출 범주의 수 
  
  # initial beta 난수로 생성
  Beta = matrix(runif((dim(x)[2]+1)*n_levels, min = 0.2, max = 0.25), nrow = dim(x)[2] + 1, ncol = n_levels) 
  eval_cost = c()
###################

   for(j in 1:iter){
#   if( is.nan(eval_cost[j]) == TRUE){Beta = abs(Beta)}
    hypothesis = exp(X %*% Beta)/rowSums(exp(X %*% Beta)) # 열마다 setosa vers virg 일 확률
#    hypothesis[is.nan(hypothesis) == T] = 0
      Cost = - sum(one_hot_label*log(hypothesis)) #cross-entropy 식 -> sigma i 없어진것은 onehot vector 이기 때문
#   Cost = - one_hot_label*log((hypothesis))
# Cost = sum(log( exp(X %*% Beta[,1])  + exp(X %*% Beta[,2]) + exp( X %*% Beta[,3]) )) -  sum(X %*% Beta) 

   #   if(is.nan(Cost) == T){ Cost = 0}
    Cost_diff = matrix(0,nrow = dim(x)[2]+1, ncol = n_levels) # 편미분결과 담을 matrix  

      for(i in 1:n_levels){
      idx = which(one_hot_label[,i] == 1) # setosa versicolor virginica 열순서로 돌면서 1의 위치 인덱스 반환 
      Cost_diff[,i] = colSums(-X[idx,]*(1 - hypothesis[idx,i])) # 1열-> setosa의 index를 돌며 미분값을 넣겠다 
    }
    Beta = Beta - learning_rate * Cost_diff
    eval_cost[j] = Cost

    

   
  }
  plot(1:iter, eval_cost)
  return(Beta)
}
a =(Cross_entropy(x,y,1e04,0.0005))


Softmax = function(x, y, test_x, test_y, iter=1e03, learning_rate = 0.0005){
        
        test_X = cbind( rep(1, length(test_x[,1])), test_x)
        colnames(test_X)[1] <- 'beta0'
        test_X = as.matrix(test_X)
        
        B = Cross_entropy(x, y,iter,learning_rate) # 추정한 beta를 불러옴
        
        # 1열,2열,3열 마다 각 후보 1, 2, 3 의 확률 
        hypothesis = exp(test_X %*% B)/rowSums(exp(test_X %*%B))
        
        result = c()
        for (i in 1:length(test_X[,1])){
                result[i] = which(max(hypothesis[i,]) == hypothesis[i,])
        }
        
        return(list("표" = table(test_y,levels(test_y)[result]), "분류결과" = result))
}

Softmax(train_x,train_y,test_x,test_y)

