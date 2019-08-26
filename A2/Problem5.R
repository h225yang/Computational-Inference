alpha = 1.5
beta = 2

theta1 = 1.5
theta2 = 2

fz = function(z){
    z^(-3/2)*exp(-theta1*z-theta2/z+2*sqrt(theta1*theta2)+log(sqrt(2*theta2)))
}

n = 1001
path = vector("numeric", n)
path[1] = 1

for (i in 2:n){
    y = rgamma(1, alpha, beta)
    u = runif(1)
    x = path[i-1]
    rate = min(fz(y)*dgamma(x, alpha, beta)/fz(x)/dgamma(y, alpha, beta), 1)
    if (u <= rate){
        path[i] = y
    } else {
        path[i] = x
    }
}

fw = function(w){
    z = exp(w)
    return((z)*z^(-3/2)*exp(-theta1*z-theta2/z+2*sqrt(theta1*theta2)+log(sqrt(2*theta2))))
}

n = 1001
path = vector("numeric", n)
path[1] = 1

for (i in 2:n){
    x = path[i-1]
    eps = rnorm(1)
    u = runif(1)
    y = x + eps
    rate = min(fw(y)/fw(x), 1)
    if (u <= rate){
        path[i] = y
    } else {
        path[i] = x
    }
}

path = exp(path)

