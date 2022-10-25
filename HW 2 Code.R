# STA442 Homework 2 Code
library(nimble)
library(quantreg)


# Question 3
# part a
set.seed(442)
a <- rnorm(10000)
x <- rexp(10000, rate = 1)
z <- rep(NA, 10000)
for (i in 1:10000){
  if (a[i] > 0.5){
    z[i] <- x[i]
  } else{
    z[i] <- -x[i]
  }
}
k <- seq(-6,6, by = 0.5)
hist(z, prob = TRUE, main = " Q3(a)")
lines(k,ddexp(k), col = "blue", lty = 1, lwd = 2)

# PART (b)
set.seed(442)
n <- 50
cov_x <- runif(n, 0, 5)
df <- rep(NA,50)

for (i in 1:1000){
  a <- rnorm(50)
  k <- rexp(50, rate = 1)
  eps <- rep(NA, 50)
  for (j in 1:50){
    if (a[j] > 0.5){
      eps[j] <- k[j]
    } else{
      eps[j] <- -k[j]
    }
  }
  y <- 2 + cov_x + eps
  df <- cbind(df, y)
}
df <- df[,2:1001]

# (b)(i)
lm_mdl <- vector(mode = "list", length = 1000)
for (i in 1:1000){
 lm_mdl[[i]] <- lm(df[,i]~ cov_x)
}
# (b)(ii)
mad_mdl <- vector(mode = "list", length = 1000)
for (i in 1:1000){
  mad_mdl[[i]] <- rq(df[,i]~ cov_x, tau = 0.5)
}

# part (c)
mad_beta <- matrix(NA, nrow = 1000, ncol = 2)
lm_beta <- matrix(NA, nrow = 1000, ncol = 2)
for (i in 1:1000){
  lm_beta[i,1] <- summary(lm_mdl[[i]])$coefficients[2,1] - 1 # bias for the LS beta_1
  lm_beta[i,2] <- (summary(lm_mdl[[i]])$coefficients[2,2])^2 # variance for the LS beta_1
  mad_beta[i,1] <- summary(mad_mdl[[i]])$coefficients[2,1] -1 # bias for the MAD beta_1
  mad_beta[i,2] <- ((summary(mad_mdl[[i]])$coefficients[2,3]-
                       summary(mad_mdl[[i]])$coefficients[2,1])*sqrt(n)/1.96)^2 # variance for the MAD beta_1
}
summary(lm_beta[,2])
summary(mad_beta[,2])

# Question 4
# part a
set.seed(442)
theta <- 2
tau <- 1
sigma <- 3
n <- 20
avg_freq <- numeric(10000)
avg_baye <- numeric(10000)
for (i in 1:10000){
  mu <- rnorm(n, mean = theta, sd = tau)
  y <- rnorm(n, mean = mu, sd = sigma)
  mean_y <- mean(y)
  diff_freq <- (mu - mean_y)^2 # difference between mu and mean y
  avg_freq[i] <- mean(diff_freq)
  mu_bayes <- (sigma^2*theta + n*tau^2*mean_y)/(sigma^2+ n * tau^2)
  diff_baye <- (mu - mu_bayes)^2 # difference between mu and bayesian mu
  avg_baye[i] <- mean(diff_baye)
}
summary(avg_freq)
summary(avg_baye)
mean(avg_freq)
mean(avg_baye)

# part b
true_theta <- -3
true_tau <- 2
for (i in 1:10000){
  mu <- rnorm(n, mean = true_theta, sd = true_tau)
  y <- rnorm(n, mean = mu, sd = sigma)
  mean_y <- mean(y)
  diff_freq <- (mu - mean_y)^2 # difference between mu and mean y
  avg_freq[i] <- mean(diff_freq)
  mu_bayes <- (sigma^2*theta + n*tau^2*mean_y)/(sigma^2+ n * tau^2)
  diff_baye <- (mu - mu_bayes)^2 # difference between mu and bayesian mu
  avg_baye[i] <- mean(diff_baye)
}
summary(avg_freq)
summary(avg_baye)
mean(avg_freq)
mean(avg_baye)




# Question 5
# part b
set.seed(442)
n = 50
p = 3
X = matrix(runif(n*p), ncol = p)
err <- rnorm(50, mean = 0, sd = sqrt(5))
beta <- c(1,1,1)
# assume beta_0 = 0
y <- X %*% beta + err 
# (i) F test 
fit1 <- lm(y ~ X)
fstat = summary(fit)$fstatistic
pvalue1 = pf(q = fstat[1], df1 = fstat[2], df2 = fstat[3], lower.tail = F)
pvalue1
# (ii) T test
z <- X[,1] + X[,2] + X[,3]
fit2 <- lm(y ~ z)
summary(fit2)$coefficients[2,4]

# part (c)
num <- 100
pval_f <- numeric(num)
pval_t <- numeric(num)
set.seed(442)
X = matrix(runif(n*p), ncol = p)
for (i in 1:num){
  err <- rnorm(50, mean = 0, sd = sqrt(5))
  y <- X %*% beta + err 
  z <- X[,1] + X[,2] + X[,3]
  fit1 <- lm(y ~ X)
  fit2 <- lm(y ~ z)
  fstat = summary(fit1)$fstatistic
  pval_f[i] = pf(q = fstat[1], df1 = fstat[2], df2 = fstat[3], lower.tail = F)
  pval_t[i] = summary(fit2)$coefficients[2,4]
}
pow_f <- mean(pval_f < 0.05)
pow_f
pow_t <- mean(pval_t < 0.05)
pow_t

# part (e)
set.seed(442)
X = matrix(runif(n*p), ncol = p)
for (i in 1:num){
  err <- rnorm(50, mean = 0, sd = sqrt(5))
  y <- X %*% beta + err 
  z <- X[,1] + X[,2] + X[,3]
  fit1 <- lm(y ~ X)
  fit2 <- lm(y ~ z)
  fstat = summary(fit1)$fstatistic
  pval_f[i] = pf(q = fstat[1], df1 = fstat[2], df2 = fstat[3], lower.tail = F)
  pval_t[i] = summary(fit2)$coefficients[2,4]
}
pow <- mean(pval_f < 0.025|pval_t < 0.025)
pow
