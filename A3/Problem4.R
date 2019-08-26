h = function(x1,x2) {
    1/4*(x1^4) + 1/2*(x2^2) - x1*x2 +x1 - x2
}

h_gradient = function(x1, x2) {
    a = x1^3-x2+1
    b = x2-x1-1
    return(c(a,b))
}


FR_CG = function(init_value, n) {
    x1 = init_value[1]
    x2 = init_value[2]
    temp1 = h(x1,x2)
    p = -h_gradient(x1,x2)
    for (i in 1:n) {
        alpha = 0
        if(!isTRUE(all.equal(p, c(0,0), tolerance = 1e-3))) { 
            alpha = optimize(function(x) {h(x1 + x*p[1], x2 + x*p[2])}, c(-20,20))$minimum
        }
        else {break}
        old_x1 = x1
        old_x2 = x2
        x1 = x1 + (alpha*p)[1]
        x2 = x2 + (alpha*p)[2]
        beta = (h_gradient(x1,x2) %*% h_gradient(x1,x2)) /
            (h_gradient(old_x1,old_x2) %*% h_gradient(old_x1,old_x2))
        p = -h_gradient(x1,x2) + c(beta*p[1], beta*p[2])
    }
    return(c(x1,x2))
}

FR_CG(c(1,2), 100)
FR_CG(c(-1,0), 100)

FR_CG(c(5,10), 100)
FR_CG(c(-5,-10), 100)

> FR_CG(c(1,2), 100)
[1] 1 2

> FR_CG(c(-1,0), 100)
[1] -1  0

> FR_CG(c(5,10), 100)
[1] 1.000882 2.001866

> FR_CG(c(-5,-10), 100)
[1] -1.001005635 -0.002144307
