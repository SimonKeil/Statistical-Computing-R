## generating t5-distribution using random walk Metropolis sampler

# density of t5-distribution without constants
t_dens <- function(x) {
  return((1+x^2/5)^(-3))
}

m = 2000
# random walk metropolis sampler
rw_sigma <- function(sigma = 1) {  
  X = numeric(m)
  U = runif(m)
  X[1] = 25
  k = 0 # count rejection
  for(i in 2:m) {
    Y = rnorm(1, X[i-1], sigma)
    if(U[i] <= t_dens(Y)/t_dens(X[i-1])) {
      X[i] = Y
    } else {
      k = k + 1
      X[i] = X[i-1]
    }
  }
  return(list(X = X, rej = k/m))
}

# check rejection rates for different sigma
rws <- sapply(c(.05, .5, 1.5, 10), FUN = rw_sigma)
# check for convergence by inspecting plots
sapply(c(1:4), FUN = function(k) plot(1:m, rws[1, k]$X, type = "line"))
# result: low variance yields high acceptance but poor convergence and vice-versa

sample = rws[1,3]$X[250:m] # best data
# histogram discarding burn in
hist(sample)
mean(sample)




## MCMC for Bayesian interference
set.seed(12345)
# generate data from a multinomial distribution where the probabilities depend on one parameter
p <- 0.2 # true parameter value
prob <- function(p) return(c(1, 1-p, 1-2*p, 2*p, p))
n <- 100
data <- tabulate(sample(1:5, n, replace = T, prob = prob(p)))

# target distribution (constants that will cancel later left out)
f <- function(p, data) {
  if(p < 0 || p >= .5) {
    return(0)
  }
  return(prod(prob(p)^data))
}

# markov chain
m <- 10000
X <- numeric(m)
X[1] <- .5
Z <- runif(m,-.25,.25) # proposal density
U <- runif(m) # for acceptance/rejection
for(i in 2:m) {
  Y = X[i-1] + Z[i]
  # prevent 0/0 issues, in this case always reject
  t1 = f(Y, data)
  t2 = f(X[i-1], data)
  if(t1 == 0 & t2 == 0) {
    t2 = 1
  }
  if(U[i] <= t1/t2) {
    X[i] = Y
  } else {
    X[i] = X[i-1]
  }
}
# generated chain
plot(1:m, X, type = "line")
hist(X[1000:m])
# posterior mean of the parameter after burn in
mean(X[1000:m])


