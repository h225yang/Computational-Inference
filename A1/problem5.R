
fz = function(y) {(1/y-1)^2*sin(pi*(1/y-1))*exp(-(1/y-1)/2)*(-1/y^2)}

f = function(x) {2*x^2*sin(pi*x)}

temp1 = vector('numeric', 1000)
temp2 = vector('numeric', 1000)

for (i in 1:1000) {
    
    u = runif(10000)
    x = -2*log(u)
    y = -2*log(1-u)
    temp2[i] = (mean(f(x)) + mean(f(y)))/2
    
    uz = runif(10000)
    temp1[i] = mean(fz(uz))
}

var(temp1)
var(temp2)

mean(temp1)
mean(temp2)

