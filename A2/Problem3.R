beta = 0.1
c = 0.5

g = function(a, b) {
    x = c(0.56, 2.26, 1.90, 0.94, 1.40, 1.39, 1.00, 1.45, 2.32, 2.08, 0.89, 1.68)
    return(a*b^beta*prod(x^(a-1))*exp(-b*(prod(x^a)+c)-a))
}


n = 10001
path_a = vector("numeric", n)
path_b = vector("numeric", n)
path_a[1] = 1
path_b[1] = 1

for (i in 2:n){
    a = path_a[i-1]
    b = path_b[i-1]
    u = runif(1)
    a_new = rexp(1, a)
    b_new = rexp(1, b)
    rate = g(a_new, b_new)*dexp(a, a_new)*dexp(b, b_new)/g(a, b)/dexp(a_new, a)/dexp(b_new, b)
    if (u <= rate){
        path_a[i] = a_new
        path_b[i] = b_new
    } else {
        path_a[i] = a
        path_b[i] = b
    }
}


CI_alpha = quantile(path_a[2000:10001], c(0.025, 0.975))
CI_ite = quantile(path_b[2000:10001], c(0.025, 0.975))
