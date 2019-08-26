n = 10001
path_x = vector("numeric", n)
path_y = vector("numeric", n)
path_x[1] = 1
path_y[1] = 1

for (i in 2:n){
    x = path_x[i-1]
    y = path_y[i-1]
    x_new = rgamma(1, 3, 4+y^2)
    y_new = rnorm(1, 1/(x+1), sqrt(1/(2*(x+1))))
    path_x[i] = x_new
    path_y[i] = y_new
}


mean(path_x^2*path_y^3*exp(-path_x^2))
