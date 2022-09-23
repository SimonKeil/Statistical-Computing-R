# generate a multivariate normal distribution from a univariate standard normal distribution
# returns a matrix with a N(mu, sigma) distributed sample in every row
rmvnorm <- function(n, mu, sigma) {
  m <- length(mu)
  Z <- matrix(rnorm(n*m), nrow = n)
  dim(Z)
  dim(chol(sigma))
  return(Z %*% chol(sigma) + rep(mu, each = n))
}

# test the function
mu=c(5,10)
sigma=matrix(c(1,.6,.6,1),nrow=2)
x = rmvnorm(1000, mu, sigma)
round(apply(x,2,mean),1)
round(cov(x),1)