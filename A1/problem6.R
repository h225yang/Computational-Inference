n = 1000
y = runif(n)

f = function(y) {
    cos(pi/2*y)*sin(pi/2*y)*exp(-cos(pi/2*y)^2)*pi/2
}

mean(f(y))

temp = 1.96*sd(f(y))/sqrt(n)

mean(f(y))-temp
mean(f(y))+temp

mean(f(y))-temp - (mean(f(y))+temp)
