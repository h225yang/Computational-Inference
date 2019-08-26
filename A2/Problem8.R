y = c(40, 10, 21, 16, 11, 7, 4, 4, 2, 3)
x = c(30.11, 13.91, 10.63, 8.68, 7.14, 5.99, 5.23, 4.37, 4.09, 3.54)

model = glm(y~x, family = poisson(link = "identity"))

var = summary(model)$cov.unscaled


para_boostrap = function(x, y) {
    x_star = sample(x, 1000, TRUE)
    y_star = vector("numeric", 1000)
    for (i in 1:1000) {
        y_star[i] = rpois(1, -2.312+1.5063*x_star[i])
    }
    model = glm(y_star~x_star, family = poisson(link = "identity"))

    return(summary(model)$cov.unscaled)
}


n = 10
for (i in 1:n) {
    rv = (rv + para_boostrap(x,y))/i
}


sample_idx = sample(c(1:10), 1000, TRUE)
x_star = x[sample_idx]
y_star = y[sample_idx]

model = glm(y_star~x_star, family = poisson(link = "identity"))

var = summary(model)$cov.unscaled
