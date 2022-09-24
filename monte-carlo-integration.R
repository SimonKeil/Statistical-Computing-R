## estimate integral of exp(-x) from 0 to 1
m = 100
x = mean(exp(-runif(m)))
true = 1-exp(-1)
print(x-true)

## monte carlo integration of e^-x on 0 to 0.5
x = matrix(0,100,3)
for(i in 0:100) {
  m = 100
  x[i,1] = mean(exp(-runif(m,0,0.5)))/2
  x[i,2] = mean(rexp(m,rate = 1) < .5)
  temp = runif(m/2,0,0.5)
  x[i,3] = mean(c(exp(-temp),exp(temp-.5)))/2
}
var(x[,1])
var(x[,2])
var(x[,3])

## importance sampling
n = 1000
x <- 1-log(runif(n))
x <- x^2*exp(-x^2/2+x)
1/(exp(1)*sqrt(2*pi))*mean(x)
# standard deviation
1/(exp(1)*sqrt(2*pi))*sd(x)