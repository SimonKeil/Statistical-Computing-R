## bootstrap estimate of se and bias
times = c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

lambda_hat = 1/mean(times)
# bootstrap estimate of se and bias of the estimator 1/mean(x) for lambda
B <- 200
n <- length(times)
b <- numeric(B)
for(i in 1:B) {
  b[i] = 1/mean(sample(times, n, replace = T))
}
se = sd(b)
bias = mean(b) - lambda_hat
cbind(lambda_hat, se, bias)

# visualize distribution of bootstrap replicas
hist(b, las = 1, prob = T)
abline(v=lambda_hat,col="tomato",lwd=3)



## bootstrap confidence intervals
data = c(173, 183, 187, 179, 180, 186, 179, 196, 202, 198, 197, 185, 194, 185, 191, 182, 182, 187, 184, 186)
ecdf <- function(x) return(mean(data <= x))
x = (min(data)-5):(max(data)+5)
plot(x, sapply(x, FUN = ecdf), type = 'l', ylab = 'F_n(x)')

mean = mean(data)
# bootstrap for mean
B = 500
b = numeric(B)
n = length(data)
for(i in i:B) {
  b[i] = mean(sample(data, n, replace = T))
}
se = sd(b)

# standard normal bootstrap CI:
c(mean - qnorm(0.975)*se, mean + qnorm(0.975)*se)
# basic bootstrap CI
c(2*mean - quantile(data, c(0.975,0.025)))
# bootstrap percentile CI
c(quantile(data, c(0.025,0.975)))

## Bootsrap BCa CI
# import law data from bootstrap package
library(bootstrap)
data = law
n = nrow(data)
# conduct bootstrap for the correlation of LSAT and GPA in law
B = 2000
r = cor(data[,1],data[,2])
r_hat = numeric(B)
for(b in 1:B) {
  i = sample(1:n, n, replace = T)
  r_hat[b] = cor(data[i,1],data[i,2])
}

# compute BCa CI for the correlation r
z0 = qnorm(mean(r_hat < r))
j = numeric(n) # jackknife for correlation
for (i in 1:n) {
  j[i] = cor(law[-i,1],law[-i,2])
}
a = sum((mean(j)-j)^3)/(6*(sum((mean(j)-j)^2))^(3/2))

alpha=.05 #compute the percentiles to be used instead of .025 and .975
z1=qnorm(alpha/2)
alpha1=pnorm(z0+(z0+z1)/(1-a*(z0+z1)))
z2=qnorm(1-alpha/2)
alpha2=pnorm(z0+(z0+z2)/(1-a*(z0+z2)))
round(quantile(r_hat,c(alpha1,alpha2)),2) #95% BCa interval

# histogram of bootstrap and jackknife replicates
hist(r_hat, prob = T)
hist(j, prob = T)


## cross validation for linear model
library(DAAG)
groups = c(rep(1:26,2),26) # 25 groups with two elements, one with three
ironslag$groups = sample(groups)
errors = numeric(26)
for(i in 1:26) {
  data = ironslag[!ironslag$groups == i,]
  test = ironslag[ironslag$groups == i,]
  model = lm(magnetic ~ chemical, data = data)
  error = predict(model, newdata = test) - test$magnetic
  errors[i] = sum(error^2)
}
sum(errors)/53



