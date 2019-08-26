n = 50

rv = vector("numeric", n)

for (i in 1:50) {
    u = runif(500, -10, 10)
    theta = 20*exp(-u^2/2)
    theta_star = 20*(1-abs(u)/5)
    alpha = cov(theta, theta_star)/var(theta_star)
    rv[i] = mean(theta) + alpha * (mean(theta_star) - 0)
}

mean(rv)
var(rv)

#integrate(function(x) {exp(-x^2*0.5)}, -Inf, Inf)

n=50

rv2 = vector("numeric", n)

for (i in 1:50) {
    u = runif(500, -10, 10)
    theta = 20*exp(-u^2/2)
    theta_star = 20*(1-u^2/25)
    alpha = cov(theta, theta_star)/var(theta_star)
    rv2[i] = mean(theta) + alpha * (mean(theta_star) + 20/3)
}

mean(rv2)
var(rv2)
