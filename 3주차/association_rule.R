##### sample_data로 연습하기 #####

rm(list=ls())
# getwd()
# setwd("D:\\tobigs2\\3weeks(knn,clustering,association)\\association")
# install.packages("arules")
library(arules)

# 1.데이터 불러오기 (개인당 롤 플레이어 목록)




# 2.데이터 변환 * 꼭 "transaction" 으로 바꾸기 * #transaction 만들기 파트가 맨 뒤에 또 있음!




# 3.transaction 데이터 보기










# 4.apriori 함수 적용하기 
# apriori(data,parameter=list(support=0.1, confidence=0.8, minlen=1, maxlen=10, smax=1))
# support=최소지지도, confidence=최소신뢰도, minlen=최소물품수(lhs+rhs), maxlen=최대물품수(lhs+rhs), smax=최대지지도
# 별다른 지정을 안해주면 위의 default 값으로 저장된다.






# 5.시각화
# install.packages("arulesViz")
library(arulesViz)
plot(rules) 
plot(sort(rules, by = "lift"), method = "grouped")
plot(sort(rules, by = "confidence"), method = "grouped")
#빨갈수록 lift값이 크고, 원이 클수록 support값이 크다.
plot(rules, method = "graph", control = list(type="items"))
#{item}->{item} 연관규칙의 지지도는 원의 크기, 색깔은 향상도 
plot(rules,method="graph",interactive = T)   
plot(rules[1:5],method="graph",interactive = T)   
plot(rules,method="paracoord")

##### Groceries data로 연습하기 #####

# 1. 데이터 불러오기 
library(datasets)




# 2.transaction 데이터 보기








 

# 3. apriori 함수 적용하기 








# 3.2. 목표 제품 집중 분석
# whole milk 를 사는 고객들은 이전에 어떤 제품들을 구매했을까?
#{rice,sugar}/{canned fish,hygiene articles}/{root vegetables,butter,rice}/{root vegetables,whipped/sour cream,flour}/{butter,soft cheese,domestic eggs}




# whole milk 를 구매하는 고객들은 다음에 어떤 제품들을 구매할 것인가?
#{other vegetables}/{rolls/buns}/{yogurt}/{root vegetables}/{tropical fruit} 



# whole milk와 salt 를 구매한 고객은 어떤걸 구매할까?




# 4. 시각화
plot(rules, method="grouped")
plot(sort(rules, by = "confidence"), method = "grouped")
plot(rules[1:5],method="graph",interactive = T)
plot(rules,method="paracoord")

####################################################################
# transactions class는 arules 패키지내에 연관성 분석을 위한 class
# 기존의 데이터를 transactions class로 변환
# 모든 요소들이 팩터형이여야 한다.
## 1. matrix -> transactions class
matrix <- matrix(c(1,1,0,0,0,
                   1,0,1,0,0,
                   0,1,1,1,0,
                   1,0,0,0,1,
                   0,0,1,1,1), ncol=5, byrow=T)

dimnames(matrix)<-list(paste0("trans",1:5),letters[1:5])
trans.matrix <-as(matrix,"transactions")
summary(trans.matrix)
inspect(trans.matrix)

## 2. data.frame -> transactions class
df <- as.data.frame(matrix)
str(df)
df <- as.data.frame(sapply(df,as.logical))
df.trans <-as(df,"transactions")
summary(df.trans)
inspect(df.trans)

## 3. list -> transactions class
list <- list(tr1=c("a","b","c"),
             tr2=c("a","d"),
             tr3=c("b","e"),
             tr4=c("a","d","e"),
             tr5=c("b","c","d"))
list
trans.list <-as(list,"transactions")
summary(trans.list)
inspect(trans.list)

#범주형 자료를 이진화시켜서 분석 
cust_id <- c(1, 2, 3, 4, 5, 6)
gender <- c("FEMALE", "MALE", "FEMALE", "FEMALE", "MALE", "FEMALE")
age <- c(23, 28, 42, 34, 45, 36)
child_prd_yn <- c("NO", "NO", "NO", "YES", "NO", "YES")
mobile_app_use <- c("YES", "YES", "NO", "YES", "NO", "YES")
re_order <- c("YES", "NO", "NO", "YES", "NO", "YES")

cust_mart <- cbind(cust_id, gender, age, child_prd_yn, mobile_app_use, re_order)
cust_mart <- as.data.frame(cust_mart)
str(cust_mart)

cust_mart <- transform(cust_mart, cust_id = as.character(cust_id),
                       age=as.numeric(as.character(age)))
sapply(cust_mart,class)
str(cust_mart)

# age : custinuous data -> discretization
cust_mart <- within(cust_mart, { #with랑 비슷한 역할을 하는 함수 
  age_cd = character(0)
  age_cd[ age <= 29 ] = "age_20"
  age_cd[ age > 29 & age <= 39 ] = "age_30"
  age_cd[ age > 39 ] = "age_40"
  age_cd = factor(age_cd, level = c("age_20", "age_30", "age_40"))
})
# cust_mart$age_cd <- cut(cust_mart$age,c(0,29,39,100),labels = c("age_20", "age_30", "age_40")) 위의 within과 똑같은 효과 
cust_mart_ar <- subset(cust_mart, select=-c(cust_id, age))
#only factors
cust_mart_trans <-as(cust_mart_ar, "transactions")
inspect(cust_mart_trans)

