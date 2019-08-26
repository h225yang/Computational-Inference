g = function(theta) {
    1/(pi^2*(1+(1.3-theta)^2)*(1+(15-theta)^2))
}

x = seq(-5,25,length.out = 500)

plot(x, g(x), 'l')


scal = 1

n = 10001
path = vector("numeric", n)
path[1] = 1

for (i in 2:n){
    x = path[i-1]
    y = rcauchy(1, x, scal)
    u = runif(1)
    rate = min(g(y)/g(x), 1)
    if (u <= rate){
        path[i] = y
    } else {
        path[i] = x
    }
}

hist(path)

g_new = function(theta, i) {
    1/(pi^2*(1+((1.3-theta)/i)^2)*(1+((15-theta)/i)^2))
}

n = 10001
path = vector("numeric", n)
i_vec = vector("numeric", n)
path[1] = 1
i_vec[1] = 1

T = seq(0.1, 1, 0.1)

for (i in 2:n) {
    x = path[i-1]
    y = rcauchy(1, x, T[i_vec[i-1]])
    u = runif(1)
    rate = g_new(y, T[i_vec[i-1]])/g_new(x, T[i_vec[i-1]])
    if (u <= rate){
        path[i] = y
    } else {
        path[i] = x
    }
    
    cur_i = 0
    case = 0
    
    if (i_vec[i-1] == 1) {cur_i = 2; case = 1}
    else if (i_vec[i-1] == 10) {cur_i = 9; case = 1}
    else {cur_i = i_vec[i-1] + sample(c(-1, 1), 1, TRUE, c(1/2, 1/2)); case = 2}

    top = dcauchy(path[i], x, T[cur_i]) * 1/10 * 1/case
    if(cur_i == 10 || cur_i == 10) {case = 1}
    bottom = dcauchy(path[i], x, T[i_vec[i-1]]) * 1/10 * 1/case
    
    r = top/bottom
    u = runif(1)
    if (r >= u) {i_vec[i] = cur_i} 
    else {i_vec[i] = i_vec[i-1]}
}

hist(path)
