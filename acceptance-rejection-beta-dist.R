# generate n random samples with a Beta(2,2) distribution
# using the acceptance-rejection method
beta2_2 <- function(n) {
  x <- integer(n)
  acc <- 0
  while(acc < n) {
    Y = runif(1)
    U = runif(1)
    if (U <= f(Y)*2/3) {
      acc = acc + 1
      x[acc] = Y
    }
  }
  return(x)
}

# pdf of Beta(2,2)
f <- function(x) {
  return(6*x*(1-x))
}

# generate random samples and compare quantiles
# to the built-in Beta distribution of R
x = beta2_2(1000)
p = seq(.1,.9,.1)
sim = quantile(x, p)
true = qbeta(p,2,2)
round(rbind(sim,true),3)