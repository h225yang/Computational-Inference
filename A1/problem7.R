temp1 = vector('numeric', 100)
temp2 = vector('numeric', 100)

for (i in 1:100) {
    
    n = 50
    
    mean1 = mean(rexp(n, 1))
    temp1[i] = mean1
    
    mean2 = mean(sample(c(1, 3, 9), n, TRUE, c(1/8, 1/8, 3/4)))
    temp2[i] = mean2
    
}

rv1_mean = mean(temp1)
rv2_mean = mean(temp2)

qqnorm(temp1)
qqnorm(temp2)

