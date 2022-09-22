# choose one of five Gamma distributions following a specified distribution
mixture_unif_gammas <- function(n) {
  prob = (1:5)/15
  # desired distribution
  x = sample(1:5, n, replace = TRUE, prob = prob)
  return(rgamma(n,3,1/x))
}

n = 50000
x = mixture_unif_gammas(n)
plot(density(x),lwd = 3, ylim = c(0,.28))
for(i in 1:5) {
  lines(density(rgamma(n, 3, 1/i)))
}
# true mean is 11
mean(x)