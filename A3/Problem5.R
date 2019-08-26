x = c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,75,76,76,78,79,81)
y = c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,2,0,0,0,0,0)

p5log = function(alpha, beta) {
    sum(-log(1+exp(alpha+beta*x)) + y*(alpha+beta*x))
}

p5log_vector = function(theta) {
    alpha = theta[1]
    beta = theta[2]
    return(sum(-log(1+exp(alpha+beta*x)) + y*(alpha+beta*x)))
}

alpha_sample = seq(-5, 5, length.out = 100)
beta_sample = seq(-5, 5, length.out = 100)
z = matrix(nrow = length(alpha_sample), ncol = length(beta_sample))

for(i in 1:length(alpha_sample)) {
    for(j in 1:length(beta_sample))
        z[i,j] = p5log(alpha_sample[i], beta_sample[j])
}

persp(alpha_sample, beta_sample, z, theta = 45, phi = 30)

p5b = optim(c(0,0), p5log_vector, method= "Nelder-Mead", control=list(fnscale=-1))
summary(p5b)

p5c = newtonRaphson(p5log_vector, c(0,0), maxiter = 50000)
summary(p5c)

p5e = optim(c(0,0), p5log_vector, method= "BFGS", control=list(fnscale=-1))
summary(p5e)
