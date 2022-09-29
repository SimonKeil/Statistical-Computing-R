## estimate integral of exp(-x) from 0 to 1
m = 100
x = mean(exp(-runif(m)))
true = 1-exp(-1)
print(x-true)

## monte carlo integration of e^-x on 0 to 0.5 exploring variance reduction techniques
x = matrix(0,100,3)
for(i in 0:100) {
  m = 100
  # using an uniform distribution
  x[i,1] = mean(exp(-runif(m,0,0.5)))/2
  # using an exponential distribution
  x[i,2] = mean(rexp(m,rate = 1) < .5)
  # using antithetical variables
  temp = runif(m/2,0,0.5)
  x[i,3] = mean(c(exp(-temp),exp(temp-.5)))/2
}
var(x[,1])
var(x[,2])
var(x[,3])

## importance sampling
# estimate the integral x^2/sqrt(2pi)exp(-x^2/2)) from 0 to inf
n = 1000
# sample from pdf f(x)=esp(-x+1) using inverse transform
x <- 1-log(runif(n))
x <- x^2*exp(-x^2/2+x)

# estimated value
1/(exp(1)*sqrt(2*pi))*mean(x) # include constants from density previously left out
# standard deviation
1/(exp(1)*sqrt(2*pi))*sd(x)