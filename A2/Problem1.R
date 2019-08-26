n = 50

sample = vector("numeric", 50)

for (i in 1:50) {
    x = rlnorm(1000)
    sample[i] = mean(x^2*exp(9.5))
}

theta_RB = mean(sample)
theta_RB_var = var(sample)


sample2 = vector("numeric", 50)

for (i in 1:50) {
    x = rlnorm(1000)
    eps = rnorm(1000)
    sample2[i] = mean(exp(9+2*log(x)+eps))
}

theta_MC = mean(sample2)
theta_MC_var = var(sample2)
