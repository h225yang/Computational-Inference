x = c(-4.2,-2.85,-2.3,-1.02,0.7,0.98,2.72,3.5)
n = length(x)

log_likelihood = function(alpha, beta=0.1) {
    n*log(beta)-n*log(pi)-sum(log(beta^2+(x-alpha)^2))
}

samples = seq(-5,5, length.out = 100)
y = NA
for (i in 1:length(samples)) {
    y[i] = log_likelihood(samples[i])
}

plot(samples, y, 'l')

T_fn = function(i) {
    T0 = 10
    Tf = 10^-7
    n = 10^6
    return(T0*(Tf/T0)^((i-1)/n))
}

simulated_annealing = function(alpha_init, n) {
    alpha = alpha_init
    for (i in 1:n) {
        alpha_new = rnorm(1, alpha, 1)
        r = exp((log_likelihood(alpha_new) - log_likelihood(alpha))/T_fn(i))
        u = runif(1)
        if (r >= u) {
            alpha = alpha_new
        }
    }
    return(alpha)
}

simulated_annealing(-2.5, 10^6)
simulated_annealing(0, 10^6)
simulated_annealing(-0.30875, 10^6)

GA_10 = ga("real-valued", log_likelihood, lower = -4, upper = 4, popSize = 10)
GA_20 = ga("real-valued", log_likelihood, lower = -4, upper = 4, popSize = 20)
GA_30 = ga("real-valued", log_likelihood, lower = -4, upper = 4, popSize = 30)




