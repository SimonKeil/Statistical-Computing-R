## inverse transform for double exponential
rdexp <- function(n) {
  return(sapply(runif(n), FUN = inv_cdf))
}

inv_cdf <- function(x) {
  if(x <= 1/2) {
    return(log(2*x))  
  } else {
    return(-log(2-2*x))
  }
}

n <- 1000
x = rdexp(n)
# check the density
ddexp = function(x){.5*exp(-abs(x))}
plot(density(x), ylim = c(0,.5))
vals = seq(-6,6,0.1)
lines(vals,ddexp(vals))

# check the quantiles
p = seq(.1,.9,.1)
sim = quantile(x, probs = p)
true = sapply(p,inv_cdf)
diff = sim - true
sd = sqrt(p*(1-p)/(n*ddexp(true)^2)) # standard deviation of quantiles
rbind(sim,true,diff,sd)


## acceptance-rejection for N(0,1) using t(2)
# computation and verification of suitable constant
c = 1/gamma(3/2)*exp(-1/2)*(3/2)^(3/2)
vals = seq(-2,2,0.001)
plot(vals, dnorm(vals)/dt(vals, df = 2))
abline(h=c)

my_rnorm <- function(n) {
  x = numeric(n)
  acc = 0
  total = 0
  while(acc < n) {
    total = total + 1
    X = rt(1,df = 1)
    Y = runif(1)
    if(c*dt(X, df = 2)*Y <= dnorm(X)) {
      acc = acc + 1
      x[acc] = X
    }
  }
  return(x)
}

# compare
p = seq(0,1,.01)
sim = quantile(my_rnorm(1000), p)
true = qnorm(p)
plot(true,sim)
abline(a = 0, b = 1)


## transformation for F from chisq distribution
my_rf <- function(n,df1,df2) {
  return((rchisq(n,df1)/df1)/(rchisq(n,df2)/df2))
}

# compare
sim = quantile(my_rf(1000,1,3),p)
true = qf(p,1,3)
plot(sim,true)


## t(4) distribution as Normal-(inverse)gamma mixture
my_rt <- function(n, df) {
  # X ~ invgamma(df/2,df/2)
  X = 1/rgamma(n, df/2, df/2)
  return(rnorm(n, sd = sqrt(X)))
}

#compare
sim = quantile(my_rt(1000, 4),p)
true = qt(p,4)
plot(sim,true)












