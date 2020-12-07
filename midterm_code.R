file1 <- "http://www.math.mcgill.ca/yyang/regression/data/abalone.csv"
abalone <- read.csv(file1, header = TRUE)

dim(abalone)
summary(abalone)
var(abalone)

hist(abalone$Rings, xlab="rings", 
     main='Distribution of no. of rings', 
     breaks=100)
abline(v=mean(abalone$Rings), col='red', lw=4)

hist(abalone$Height, xlab="height (mm)", 
     main='Distribution of height', 
     breaks=100)
abline(v=mean(abalone$Height), col='red', lw=4)

plot(abalone$Rings ~ abalone$Height, 
     main="Abalone's no. of rings ~ height", 
     xlab="Height (mm)",
     ylab="No. of rings")
ols_model <- lm(abalone$Rings ~ abalone$Height)
abline(ols_model, col='red', lw=3)

summary(ols_model)

log_rings <- log(abalone$Rings)
height <- abalone$Height

hist(log_rings,
     xlab="log(no. of rings)", 
     main='Distribution of log(no. of rings)',
     breaks=100)
abline(v=mean(log_rings), col='red', lw=4)

log_model <- lm(log_rings ~ height)

plot(log_rings ~ height, 
     main="log(no. of rings) ~ height", 
     xlab="Height (mm)",
     ylab="log(no. of rings)")
abline(log_model, col='red', lw=3)

summary(log_model)
exp(log_model$coefficients)

confint(log_model)

t_log = summary(log_model)$coefficients[2,3]

permutation_test <- function(output, input, M){
  t_vec <- rep(M)
  for(i in 1:M){
    y_permuted <- sample(output)
    model_permuted <- lm(y_permuted ~ input)
    t_vec[i] <- summary(model_permuted)$coefficients[2,3]
  }
  return(t_vec)
}

permuted_ts <- permutation_test(log_rings, height, 1000)
hist(permuted_ts, breaks=100,
     main="Distribution of t-statistics from permutation test",
     xlab='t value',
     xlim = c(-60, 60))
abline(v=t_log, col='red', lw=3)
abline(v=-t_log, col='red', lw=3)
p <- sum(permuted_ts > abs(t_log) | permuted_ts < -abs(t_log))/1000
p

predict(log_model, data.frame(height=0.132))

library(FNN)
height_test = data.frame(height_test = seq(min(height), max(height), by=0.0001))
pred_k1 = knn.reg(train=height, test=height_test, y=log_rings, k=1)
pred_k10 = knn.reg(train=height, test=height_test, y=log_rings, k=10)
pred_k100 = knn.reg(train=height, test=height_test, y=log_rings, k=100)
pred_k500 = knn.reg(train=height, test=height_test, y=log_rings, k=500)
pred_k1000 = knn.reg(train=height, test=height_test, y=log_rings, k=1000)
pred_k2000 = knn.reg(train=height, test=height_test, y=log_rings, k=2000)
pred_k4000 = knn.reg(train=height, test=height_test, y=log_rings, k=4000)

par(mfrow = c(3,2))

plot(log_rings ~ height, 
     main="k=1", 
     xlab="Height (mm)",
     ylab="log(no. of rings)")
lines(x = height_test$height_test, y = pred_k1$pred, col='orange')

plot(log_rings ~ height, 
     main="k=10", 
     xlab="Height (mm)",
     ylab="log(no. of rings)")
lines(x = height_test$height_test, y = pred_k10$pred, col='orange')

plot(log_rings ~ height, 
     main="k=100", 
     xlab="Height (mm)",
     ylab="log(no. of rings)")
lines(x = height_test$height_test, y = pred_k100$pred, col='orange')

plot(log_rings ~ height, 
     main="k=1000", 
     xlab="Height (mm)",
     ylab="log(no. of rings)")
lines(x = height_test$height_test, y = pred_k1000$pred, col='orange')

plot(log_rings ~ height, 
     main="k=2000", 
     xlab="Height (mm)",
     ylab="log(no. of rings)")
lines(x = height_test$height_test, y = pred_k2000$pred, col='orange')

plot(log_rings ~ height, 
     main="k=4000", 
     xlab="Height (mm)",
     ylab="log(no. of rings)")
lines(x = height_test$height_test, y = pred_k4000$pred, col='orange')
