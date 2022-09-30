## use the EM algorithm to estimate parameters of a Bernoulli-Normal-Mixture

# parameters
mu1 = -3
mu2 = 3
p = 0.8

# simulate data
n = 200
i <- rbinom(n, 1, p)
x <- numeric(n)
x[i == 1] = rnorm(sum(i), mu1, 1)
x[i != 1] = rnorm(n - sum(i), mu2, 1)
# check distribution
hist(x)

# we introduce auxiliary variables I
# for the non-observed Bernoulli variables of the mixture

# this function computes the expectancy of I conditional
# on the observed data and parameter estimates
expec_auxil <- function(x, mu1, mu2, p) {
  num <- dnorm(x,mu1,1)*p
  den <- num + dnorm(x,mu2,1)*(1-p)
  return(num/den)
}

# maximise the loglikelihood conditioned on the observed
# data and previous parameter estimates to obtain new estimates
# (formula derived analytically)
max_param <- function(x, mu1, mu2, p) {
  E_I <- expec_auxil(x, mu1, mu2, p)
  p_hat <- mean(E_I)
  mu1_hat <- sum(E_I*x)/sum(E_I)
  mu2_hat <- sum((1-E_I)*x)/sum(1-E_I)
  return(c(mu1_hat, mu2_hat, p_hat))
}

# initial parameter estimates
mu1_hat <- 0
mu2_hat <- 1
p_hat <- 0.5

# iterate EM steps
max_iter <- 1000
tol <- sqrt(.Machine$double.eps)
i <- 0
change <- 1
while(i <= max_iter & change > tol) {
  new_param = max_param(x, mu1_hat, mu2_hat, p_hat)
  # iteration limitation
  change <- sum(abs(new_param - c(mu1_hat, mu2_hat, p_hat)))
  i = i + 1
  # results of the step
  mu1_hat = new_param[1]
  mu2_hat = new_param[2]
  p_hat = new_param[3]
}

# result
cbind(mu1_hat, mu2_hat, p_hat, iterations = i-1)
# absolute errors
abs(c(mu1_hat, mu2_hat, p_hat) - c(mu1, mu2, p))




## use the ME algorithm to estimate mixing probabilities for
## a 2-component poisson mixture
# parameters
l = c(2, 6)
p = .65

# simulate data
n = 600
i <- sample(1:2, n, replace = T, prob = c(p, 1-p))
x <- rpois(n, l[i])
# check distribution
plot(table(x)/n) # simulated
den <- function(k) p*dpois(k,l[1])+(1-p)*dpois(k,l[2])
points(0:max(x), den(0:max(x))) # theoretical

# estimate parameters for data x using the given
# start values for l1, l2, p (in that order)
estimate_params <- function(x, param = c(1, 7, .4)) {
  expec <- function(x, param) {
    num <- param[1]^x*exp(-param[1])*param[3]
    den <- num + param[2]^x*exp(-param[2])*(1-param[3])
    return(num/den)
  }
  max <- function(x, param) {
    I <- expec(x, param)
    return(c(sum(x*I)/sum(I), sum(x*(1-I))/sum(1-I), mean(I)))
  }
  for(i in 1:200) {
    param = max(x, param)
  }
  return(param)
}

# result
estimate_params(x)









