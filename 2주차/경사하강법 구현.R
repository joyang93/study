##################### 1.Gradient Descent 구현 과제입니다. ##########################
# 시각화용 코드입니다.
smoothing <- function(vec)
{
        vec1 <- c(vec[-1], vec[length(vec)])
        vec2 <- c(vec[1], vec[-length(vec)])
        return((vec1 + vec2 + vec) / 3)
}
visualize_loss <- function(loss_log)
{
        for(i in 1:100)
        {
                loss_log <- smoothing(loss_log)
                plot(loss_log)
                Sys.sleep(0.01)
        }
}


# 단순회귀 구현
x <- rnorm(1000, 0)
y <- 2 * x + 1
w <- 0.001
b <- 0.001
lr <- 0.01
loss_log <- c()
for(i in 1:1000){
        w = w - lr*sum((w*x+b-y)*x)/1000
        b = b - lr*sum(w*x+b-y)/1000
        loss = (sum(w*x+b-y)^2)/1000
        loss_log = c(loss_log,loss)
}
visualize_loss(loss_log)
if(max(abs(w-2), abs(b-1)) < 0.1)
{
        print("정답입니다!")
}else{
        print("모델을 수정하거나, 초기값, 파라미터를 수정해보세요!")
}
# > w
# [1] 1.99991
# > b
# [1] 0.9999584
#다중회귀 구현(변수 11개)
x <- as.data.frame(matrix(rnorm(5000,0), nrow = 500, ncol = 10))
y <- x$V1 * 1 + x$V2 * 2 + x$V3 * 3 + x$V4 * 4 + x$V5 * 5 + x$V6 * 6 + x$V7 * 7 + x$V8 * 8 + x$V9 * 9 + x$V10 * 10 + 11
w <- rnorm(10,0)
b <- rnorm(1,0)
lr <- 0.01
loss_log <- c()
x_mat = data.matrix(x)
for(i in 1:499)
{
        w = w - lr*t(x_mat) %*% (x_mat %*% w + b - y)/500
        b = b - lr*sum(x_mat %*% w + b - y)/500
        loss = (sum(x_mat %*% w + b - y)^2)/500
        loss_log = c(loss_log, loss)
}
visualize_loss(loss_log)
if(max(abs(w-1:10), abs(b-11)) < 0.5)
{
        print("정답입니다!")
}else{
        print("모델을 수정하거나, 초기값, 파라미터를 수정해보세요!")
}




# > w
# [,1]
# V1 1.001942
# V2 2.015418
# V3 2.903667
# V4 3.923738
# V5 4.937373
# V6 6.012185
# V7 6.830373
# V8 7.894939
# V9 8.827266
# V10 9.891744
# > b
# [1] 10.96345
#다중회귀 구현(변수 n개)
linear_regression <- function(n)
{
        x <- as.data.frame(matrix(rnorm(50*n*n,0), nrow = 50*n, ncol = n))
        y <- rep(0, 50*n)
        for(i in 1:(50*n))
        {
                y[i] <- sum(x[i,]*(1:n)) + (n+1)
        }
        w <- rnorm(n,0)
        b <- rnorm(1,0)
        lr <- 0.01
        loss_log <- c()
        x_mat = data.matrix(x)
        for(i in 1:nrow(x))
        {
                w = w - lr*t(x_mat) %*% (x_mat %*% w + b - y)/(50*n)
                b = b - lr*sum(x_mat %*% w + b - y)/(50*n)
                loss = (sum(x_mat %*% w + b - y)^2)/(50*n)
                loss_log = c(loss_log, loss)
        }
        visualize_loss(loss_log)
        if(max(abs(w-1:n), abs(b-n-1)) < 0.5)
        {
                print("정답입니다!")
        }else{
                print("모델을 수정하거나, 초기값, 파라미터를 수정해보세요!")
        }
        return(list(w = w, b = b))
}
linear_regression(10)
# > linear_regression(10)
# [1] "정답입니다!"
# $w
# [,1]
# V1 0.9953306
# V2 1.9808849
# V3 3.0331089
# V4 3.9299494
# V5 4.9905930
# V6 5.9685874
# V7 6.9742626
# V8 7.9749172
# V9 8.9602435
# V10 10.0045625
#
# $b
# [1] 10.9353
linear_regression(15)
# > linear_regression(15)
# [1] "정답입니다!"
# $w
# [,1]
# V1 1.000882
# V2 2.001569
# V3 3.011602
# V4 3.992253
# V5 4.997172
# V6 6.014246
# V7 6.966988
# V8 8.010798
# V9 8.979385
# V10 10.000806
# V11 10.993106
# V12 11.977982
# V13 12.979697
# V14 13.970464
# V15 14.983657
#
# $b
# [1] 15.97311
linear_regression(20)
# > linear_regression(20)
# [1] "정답입니다!"
# $w
# [,1]
# V1 0.9990956
# V2 2.0000945
# V3 3.0007125
# V4 4.0003216
# V5 4.9974911
# V6 5.9982369
# V7 6.9977754
# V8 8.0003634
# V9 9.0012612
# V10 9.9987929
# V11 10.9995157
# V12 12.0000694
# V13 12.9996222
# V14 13.9980336
# V15 14.9984284
# V16 15.9981385
# V17 16.9990511
# V18 17.9997220
# V19 18.9980097
# V20 20.0006212
#
# $b
# [1] 20.99902

