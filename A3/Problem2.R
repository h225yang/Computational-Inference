# hyper-param
p = 0.7
lambda1 = 5
lambda2 = 5

init_value = c(p, lambda1, lambda2)

# obs
x_obs = c(24,18,21,5,5,11,11,17,6,7,20,13,4,16,19,21,4,22,8,17)

P2EM = function(init_value, iter){
    p = init_value[1]
    lambda1 = init_value[2]
    lambda2 = init_value[3]
    n = length(x_obs)
    w = vector("numeric", n)
    for (k in 1:iter) {
        for (i in 1:n){
            a = p * dpois(x_obs[i], lambda1)
            b = (1-p) * dpois(x_obs[i], lambda2)
            w[i] = a/(a+b)
        }
        # update hyper-param
        p = sum(w)/n
        lambda1 = sum(x_obs*w)/sum(w)
        lambda2 = sum(x_obs*(1-w))/sum(1-w)
    }
    
    return(c(p, lambda1, lambda2))
}

temp = init_value

temp = P2EM(temp,10000)


