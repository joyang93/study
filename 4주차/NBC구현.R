################################################################################
## 0. 시행해 주세요
setwd('YOUR PATH')
work1 <- read.table('work1.txt', header = TRUE)
work2 <- read.table('work2.txt', header = TRUE)
work3 <- read.table('work3.txt', header = TRUE)
test <- read.table('test.txt', header = TRUE)

converting <- function(mydata) {
  for(i in c(1, 3, 4, 6, 7)) mydata[,i] <- factor(mydata[,i])
  return(mydata)
}

work1 <- converting(work1)
work2 <- converting(work2)
work3 <- converting(work3)
test <- converting(test)

head(test)
str(work1)

#################################################################################
## 1. 초기화
PARAMS <- list(
        
               # factor형 levels 4
               wife_edu_C1 = vector(mode = 'numeric', 4), 
               husband_edu_C1 = vector(mode = 'numeric', 4), 
               husband_job_C1 = vector(mode = 'numeric', 4),
               living_index_C1 = vector(mode = 'numeric', 4),
                
               # Mu, sigma 
               wife_age_C1 = c(0, 8.2272),
               children_C1 = c(0, 2.3585),
               
               
               wife_edu_C2 = vector(mode = 'numeric', 4),
               husband_edu_C2 = vector(mode = 'numeric', 4),
               husband_job_C2 = vector(mode = 'numeric', 4),
               living_index_C2 = vector(mode = 'numeric', 4),
               
               
               wife_age_C2 = c(0, 8.2272),
               children_C2 = c(0, 2.3585),
               
               # P(C_k)
               prior = vector(mode = 'numeric', 2),
               prev_cnt = c(0, 0)) # 이전 prior 누적해서 사용하는 장소 

##################################################################################
## 2. 모수 업데이트 함수 평균만 추정한다. 

ParamsUpdate <- function(data, PARAMS) {

  C1 <- data[data[,1] == 0,]  # C1
  C2 <- data[data[,1] == 1,]  # C2
  
  m_new_C1 <- nrow(C1) # m_new in C1 
  m_new_C2 <- nrow(C2) # m_new in C2
  
  m_old_C1 <- PARAMS[['prev_cnt']][1]  # m_old in C1
  m_old_C2 <- PARAMS[['prev_cnt']][2]  # m_old in C2
  
  ## 연속형 변수 업데이트;
  # C1의 경우
  PARAMS[['wife_age_C1']][1] <- (PARAMS[['wife_age_C1']][1]*m_old_C1 + sum(C1['wife_age']))/(m_old_C1 + m_new_C1)
  PARAMS[['children_C1']][1] <- (PARAMS[['children_C1']][1]*m_old_C1 + sum(C1['children']))/(m_old_C1 + m_new_C1)
  
  # C2의 경우
  PARAMS[['wife_age_C2']][1] <- (PARAMS[['wife_age_C2']][1]*m_old_C2 + sum(C2['wife_age']))/(m_old_C2 + m_new_C2)
  PARAMS[['children_C2']][1] <- (PARAMS[['children_C2']][1]*m_old_C2 + sum(C2['children']))/(m_old_C2 + m_new_C2)
  
  ## 범주형 변수 업데이트;
  # C1의 경우
  PARAMS[['wife_edu_C1']] <- (PARAMS[['wife_edu_C1']]*m_old_C1 + table(C1['wife_edu']))/(m_old_C1 + m_new_C1)
  PARAMS[['husband_edu_C1']] <- (PARAMS[['husband_edu_C1']]*m_old_C1 + table(C1['husband_edu']))/(m_old_C1 + m_new_C1)
  PARAMS[['husband_job_C1']] <- (PARAMS[['husband_job_C1']]*m_old_C1 + table(C1['husband_job']))/(m_old_C1 + m_new_C1)
  PARAMS[['living_index_C1']] <- (PARAMS[['living_index_C1']]*m_old_C1 + table(C1['living_index']))/(m_old_C1 + m_new_C1)
  
  # C2의 경우
  PARAMS[['wife_edu_C2']] <- (PARAMS[['wife_edu_C2']]*m_old_C2 + table(C2['wife_edu']))/(m_old_C2 + m_new_C2)
  PARAMS[['husband_edu_C2']] <- (PARAMS[['husband_edu_C2']]*m_old_C2 + table(C2['husband_edu']))/(m_old_C2 + m_new_C2)
  PARAMS[['husband_job_C2']] <- (PARAMS[['husband_job_C2']]*m_old_C2 + table(C2['husband_job']))/(m_old_C2 + m_new_C2)
  PARAMS[['living_index_C2']] <- (PARAMS[['living_index_C2']]*m_old_C2 + table(C2['living_index']))/(m_old_C2 + m_new_C2)
  
  ## prior 업데이트;
  total <- m_old_C1 + m_old_C2 + m_new_C1 + m_new_C2
  PARAMS[['prev_cnt']][1] <- PARAMS[['prev_cnt']][1] + nrow(C1)
  PARAMS[['prev_cnt']][2] <- PARAMS[['prev_cnt']][2] + nrow(C2)
  PARAMS[['prior']] <- PARAMS[['prev_cnt']]/total
  
  # 

  return(PARAMS)
}

#############################################################################################################################
## predict 함수
newdata=test; params = PARAMS_1

predict.1 <- function(test, params) {
  n <- nrow(test)
  mypred <- vector(mode='character', length=n)
  
  for(i in 1:n) {
    obs <- as.numeric(test[1,])
    
    # C1의 posterior
    x1_C1 <- dnorm(obs[1], params[['wife_age_C1']][1], params[['wife_age_C1']][2])
    x2_C1 <- params[['wife_edu_C1']][obs[2]]
    x3_C1 <- params[['husband_edu_C1']][obs[3]]
    x4_C1 <- dnorm(obs[4], params[['children_C1']][1], params[['children_C1']][2])
    x5_C1 <- params[['husband_job_C1']][obs[5]]
    x6_C1 <- params[['living_index_C1']][obs[6]]
    p_C1 <- params[['prior']][1]
    C1_x <- x1_C1 + x2_C1 + x3_C1 + x4_C1 + x5_C1 + x6_C1 + p_C1
    
    # C2의 posterior
    x1_C2 <- dnorm(obs[1], params[['wife_age_C2']][1], params[['wife_age_C2']][2])
    x2_C2 <- params[['wife_edu_C2']][obs[2]]
    x3_C2 <- params[['husband_edu_C2']][obs[3]]
    x4_C2 <- dnorm(obs[4], params[['children_C2']][1], params[['children_C2']][2])
    x5_C2 <- params[['husband_job_C2']][obs[5]]
    x6_C2 <- params[['living_index_C2']][obs[6]]
    p_C2 <- params[['prior']][2]
    
    C2_x <- x1_C2 + x2_C2 + x3_C2 + x4_C2 + x5_C2 + x6_C2 + p_C2
    
    if(C1_x > C2_x) mypred[i] <- 0
    else mypred[i] <- 1
  }
  return(mypred)
}


table(work1$wife_work)
table(work2$wife_work)
table(work3$wife_work)

###########################################################################################
## 결과 확인
library(naivebayes)

# 모수 추정이 같은가?
PARAMS_1 <- ParamsUpdate(work1, PARAMS)
model.1 <- naive_bayes(work1[,-1], work1[,1])

# 두 개가 같은가?
predict(model.1, newdata = test[,-1])
predict.1(test[,-1], PARAMS_3)

###
PARAMS_2 <- ParamsUpdate(work2, PARAMS_1)
work12 <- rbind(work1, work2)
naive_bayes(work12[,-1], work12[,1] ); PARAMS_2

PARAMS_3 <- ParamsUpdate(work3, PARAMS_2)
work123 <- rbind(work1, work2, work3)
naive_bayes(work123[,-1], work123[,1] ); PARAMS_3


