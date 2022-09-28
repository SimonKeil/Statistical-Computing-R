data = chickwts
## permutation test for hypothesis "all 6 feeds have the same mean weight"
# calculate number of permutations for exact test
feed = table(data$feed)
n = sum(feed)
N = factorial(n)/prod(factorial(feed))
# alternative
N_alt = 1
for(i in 1:length(feed)) {
  N_alt = N_alt*choose(n - sum(feed[1:i-1]), feed[i])
}
cbind(N, N_alt)

# perform F-test for same mean weight
fit = aov(weight ~ feed, data)
summary(fit)
F_val = summary.lm(fit)$fstatistic[1]

# perform permutation test
n_reps = 1000
reps = numeric(n_reps)
for(i in 1:n_reps) {
  data$perm_weight = sample(data$weight)
  perm_fit = aov(perm_weight ~ feed, data)
  reps[i] = summary.lm(perm_fit)$fstatistic[1]
}
hist(reps)
ASL = mean(reps >= F_val) # comparison: p-value from F test was approx. 6e-10

## perform similar test for two groups
data = data[data$feed == 'casein' | data$feed == 'meatmeal', ]
fit = aov(weight ~ feed, data)
summary(fit)
F_val = summary.lm(fit)$fstatistic[1]

# perform permutation test
n_reps = 1000
reps = numeric(n_reps)
for(i in 1:n_reps) {
  data$perm_weight = sample(data$weight)
  perm_fit = aov(perm_weight ~ feed, data)
  reps[i] = summary.lm(perm_fit)$fstatistic[1]
}
hist(reps)
ASL = mean(reps >= F_val) # comparison: p-value from F test was approx 0.098







