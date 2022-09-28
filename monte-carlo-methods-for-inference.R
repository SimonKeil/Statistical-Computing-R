## estimate mean of absolute difference of two standard normal rvs
# without loop
mean(abs(rnorm(1000)-rnorm(1000)))
  
# with loop
g <- numeric(n)
for(i in 1:n) {
  x <- rnorm(2)
  g[i] = abs(x[1]-x[2])
}
sqrt(var(g)/n) # standard error
mean(g)

## estimate Type I error in hypothesis testing
n <- 20
mu <- 500
sigma <- 100

m <- 1000
I <- numeric(m)
for(i in 1:m) {
  X <- rnorm(20, mu, sigma)
  I[i] = t.test(X, mu = mu, alternative = "greater")$p.value < .05
}
# should be .05
mean(I)

## estimate confidence of analytically derived confidence interval
m <- 1000
I <- numeric(m)
n <- 10
alpha <- 0.05
fac_l <- (n-1)/qchisq(1-alpha/2, df = n-1)
fac_u <- (n-1)/qchisq(alpha/2, df = n-1)
for(i in 1:m) {
  x <- rnorm(10)
  v = var(x)
  I[i] = 1 >= fac_l*v & 1 <= fac_u*v
}
conf = mean(I)
se = sqrt(alpha*(1-alpha)/m)
# estimated confidence and standard error
cbind(conf,se)

## MC for binomial hypothesis testing
# calculate significance of proposed test
alpha = pbinom(3,15,1/2)

theta = 1/2
m <- 10000
# rate of rejection
mean(rbinom(m,15,theta) <= 3)

# approximate power of the test
sim_power <- function(theta, m = 100) {
  x = mean(rbinom(m,15,theta) <= 3)
  return(cbind(power = x, se = sqrt(x*(1-x)/m)))
}

# analytically derived power
ana_power <- function(theta, m = 100) {
  power = pbinom(3,15,theta)
  return(cbind(power, se = sqrt(power*(1-power)/m)))
}

sim_power(.1)
sim_power(.4)

ana_power(.1)
ana_power(.4)

