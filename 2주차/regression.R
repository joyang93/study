rm(list=ls())

set.seed(1)
x <- rnorm(100)
y <- x + rnorm(100)
model1 <- lm(y ~ x) #단순 선형 회귀모델 적합
summary(model1) # R-squared:  0.4674,	Adjusted R-squared:  0.4619

x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)

y <- x1 + rnorm(100)
# 의미없는 변수들을 추가해도 R-Square 값은 계속 증가한다

fit4 <- lm(y ~ x1 + x2 + x3 + x4)
fit3 <- lm(y ~ x1 + x2 + x3)
fit2 <- lm(y ~ x1 + x2)
fit1 <- lm(y ~ x1)

summary(fit4) # 0.4708
summary(fit3) # 0.4708
summary(fit2) # 0.4683
summary(fit1) # 0.4598


####################################################################
rm(list=ls())

fundata <- read.csv("fundata.csv")
str(fundata)
fundata

fundata <- fundata[,-1] #index 제거

fit1 <- lm(y1 ~ x1, fundata)
fit2 <- lm(y2 ~ x2, fundata)
fit3 <- lm(y3 ~ x3, fundata)
fit4 <- lm(y4 ~ x4, fundata)

fit1;fit2;fit3;fit4

par(mfrow=c(2,2)) #plots를 2X2으로 나누겠다

plot(fundata$x1, fundata$y1) #산점도
abline(fit1) #적합된 단순선형회귀식 그려서 비교하기
plot(fundata$x2, fundata$y2)
abline(fit2)
plot(fundata$x3, fundata$y3)
abline(fit3)
plot(fundata$x4, fundata$y4)
abline(fit4)

#####################################################################
#가변수 처리
#list.files()
##data 불러오기
data <- read.csv("education1960_70.csv",header=T,stringsAsFactors=FALSE)

#summary(data)
#names(data)

####################################################
# STATE : 주
# Y : 개인당 들어가는 교육 비용
# X1:수입
# X2:18세 이하 거주자 (천명)
# X3:도시 거주 인구(천명)
# Region: West,Northeast,North Central,South
# Year: 1960 or 1970
#######################################################

##data 탐색 및 전처리
#결측치 확인
sum(is.na(data)) #0
#있다면 data <- na.omit(data) 행별로 제거

#구조보기
par(mfrow=c(2,3))
hist(data$Y);summary(data$Y) 
hist(data$X1);summary(data$X1)
hist(data$X2);summary(data$X2)
hist(data$X3);summary(data$x3)
hist(data$Region);summary(data$Region)
hist(data$Year);summary(data$Year)

#Region 가변수 처리(4개-> 지시변수는 3개)
data$Z1<-ifelse(data$Region=="1",'1','0')
data$Z2<-ifelse(data$Region=="2",'1','0') 
data$Z3<-ifelse(data$Region=="3",'1','0')
names(data)
data$Z1 = as.factor(data$Z1)
data$Z2 = as.factor(data$Z2)
data$Z3 = as.factor(data$Z3)
unique(data$STATE)
data <- data[,c(-1,-6)]
names(data)
####################################################
# Y : 개인당 들어가는 교육 비용
# X1:수입
# X2:18세 이하 거주자 (천명)
# X3:도시 거주 인구(천명)
# Year: 1960 or 1970
# Z1=0 Z2=0 Z3=0 : Region = West
# Z1=1 Z2=0 Z3=0 : Region = Northeast
# Z1=0 Z2=1 Z3=0 : Region = North Central
# Z1=0 Z2=0 Z3=1 : Region = South
#######################################################

fit <- lm(Y~., data) #R-squared:  0.8982,	Adjusted R-squared:  0.8904
summary(fit) #대체로 모든 변수가 유의

##########################################################################################
rm(list=ls())
data <- read.table("data.txt", header = T)

str(data)
head(data)
###########################################################################################
#미국 금융기관에서 조사한 상사(supervisor) 직무수행평가 데이터
#보통 한 팀에 35명의 사원들과 1명의 상사가 있었는데 설문조사를 하여 만든 데이터라고 명함.
#변수설며
#Y : 상사의 직무수행에 대한 전반적인 평가
#X1: 피고용인의 불만처리
#X2: 특권을 허용하지 않음
#X3: 새로운 것을 배울 기회
#X4: 업무성과에 따른 승진
#X5: 과실에 대한 지나친 비판
#X6: 더 나은 일로의 진급 (상사가 아닌 나의 진급을 점수화)
###########################################################################################
fit.full <- lm(Y~., data)
summary(fit.full) # R-squared:  0.7326,	Adjusted R-squared:  0.6628
# 유의미한 변수 X1

## p-value를 기준으로 변수 제거

fit1 <- lm(Y~ . -X5, data)
summary(fit1) # R-squared:  0.7318,	Adjusted R-squared:  0.6759
fit2 <- lm(Y~ . -X5 -X4, data)
summary(fit2) # R-squared:  0.7293,	Adjusted R-squared:  0.686
fit3 <- lm(Y~ . -X5 -X4 -X2, data)
summary(fit3) # R-squared:  0.7256,	Adjusted R-squared:  0.6939
fit4 <- lm(Y~ . -X5 -X4 -X2 -X6, data)
summary(fit4) # R-squared:  0.708,	Adjusted R-squared:  0.6864 # X1, X3
fit5 <- lm(Y~ . -X5 -X4 -X2 -X6 -X3, data)
summary(fit5) # R-squared:  0.708,	Adjusted R-squared:  0.6864 # X1

## AIC기준으로 변수선택법(AIC가 낮을 수록 좋다)

# 설명변수를 넣지않은 모델
fit.con <- lm(Y ~ 1, data)
# 전진선택법
fit.forward <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "forward")
# 후진제거법
fit.backward <- step(fit.full, list(lower=fit.con, upper = fit.full), direction = "backward")
# 단계적회귀방법(stepwise)
fit.both <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "both")

summary(fit.backward) # R-squared:  0.708,	Adjusted R-squared:  0.6864 # X1, X3
summary(fit.both) # R-squared:  0.708,	Adjusted R-squared:  0.6864 # X1, X3

## 모델 비교
anova(fit5, fit.both) #p-value 0.1278로 크므로 유의미한 차이가 없다.

## PLOT
# Residuals vs Fitted : 독립성 판단에 사용. 만약 잔차와 예측값사이에 어떤 규칙이 발견된다면 그 규칙을 변수로 추가 할 수 있다. 
# Normal Q-Q : 정규성을 볼 수 있는 그림. 정규성을 만족하면 y=x 직선위에 모든점이 올라와야 된다. 
# Scale-Location : 등분산성을 판단하는 plot, 흩어짐의 정도가 일정해야 함 
#                  흩어짐의 정도가 치우쳐있음, 등분산성에 문제가 있다고 판단한다.
# Residual vs Leverage : 
# 개개의 관찰치에 대한 정보를 제공한다.이상치/큰 지레점/영향관측치를 확인할 수 있다
# 이상치(outlier): 회귀모형으로 잘 예측되지 않는 관측치(즉 아주 큰 양수/음수의 residual)
# 지레점(high leverage point): 예측변수측의 이상치
# 영향치(influential observation): 통계 모형 계수 결정에 불균형한 영향을 미치는 관측치
par(mfrow=c(2,2))
plot(fit.both)
# Residuals vs Fitted - 독립성과 이분산성 의심
# normal Q-Q - 정규성 의심
# qook distance 영향력관측 leverage 탐지 
#install.packages("car")
#vif, outlierTest 등
library(car)

##관측치
outlierTest(fit.both)
# Nullfor the Bonferonniadjusted outlier test is the observation is an outlier.
## Bonferonni p가 < .05이면 이상점으로 판단
## 이상점 없음

#influenceIndexPlot(fit.both, id.n=3)
# Cook's distance measures how much an observation influences the overall model or predicted values
#influencePlot(fit.both, id.n=6)
# Creates a bubble-plot combining the display of Studentizedresiduals, hat-values, and Cook's distance (represented in the circles).

e <-resid(fit.both) # 잔차

## shapiro.test 정규성검정 H0 : 자료가 정규성을 만족하지않는다. >0.05 이므로 h0을 기각 
shapiro.test(e) # p-value = 0.123 정규성 만족

predict(fit.both)

# 독립성
plot(predict(fit.both), e) # 딱히 패턴이 보이진 않는다.
plot(predict(fit.both), e, type = 'o')

# 다중 공선성 10이상있는지 CHECK 
vif(fit.full)


############################################################################################
