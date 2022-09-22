# generate random chisquared samples by convolution of standard normal distributed rvs
chisq <- function(n, df) {
  M <- matrix(rnorm(n*df), nrow = n)
  M <- M^2
  x <- apply(M, MARGIN = 1, FUN = sum)
  return(x)
}

# generate random samples and compare quantiles
# to the built-in Chisq distribution of R
x = chisq(1000,5)
p = seq(.1,.9,.1)
sim = quantile(x, p)
true = qchisq(p, df = 5)
round(rbind(sim,true),3)