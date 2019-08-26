N = 10000
B = 1000
theta = (qt(0.75, 3)-qt(0.25, 3))/1.34
len_a = vector("numeric", N)
len_b = vector("numeric", N)
len_c = vector("numeric", N)
cov_a = 0
cov_b = 0
cov_c = 0

for (i in 1:N) {
    x = rt(25, 3)
    jack = jackknife(x, function(x){(quantile(x,0.75)-quantile(x,0.25))/1.34})
    ci_a = mean(jack$jack.values)+c(-1,1)*qnorm(0.975)*jack$jack.se
    len_a[i] = ci_a[2] - ci_a[1]
    if(theta > ci_a[1] && theta < ci_a[2]) cov_a = cov_a + 1
    
    boots = boot(x, function(x, ind) {(quantile(x[ind], 0.75)-quantile(x[ind], 0.25))/1.34}, B,
                 parallel = "multicore", ncpus = 8)
    ci_b = boot.ci(boots, 0.95, "norm")
    len_b[i] = ci_b$normal[3] - ci_b$normal[2]
    if(theta > ci_b$normal[2] && theta < ci_b$normal[3]) cov_b = cov_b + 1  
    
    boots = boot(x, function(x, ind) {(quantile(x[ind], 0.75)-quantile(x[ind], 0.25))/1.34}, B, 
                 parallel = "multicore", ncpus = 8)
    ci_c = boot.ci(boots, 0.95, "perc")
    len_c[i] = ci_c$percent[5] - ci_c$percent[4]
    if(theta > ci_c$percent[4] && theta < ci_c$percent[5]) cov_c = cov_c + 1 
}

> mean(len_a)
[1] 1.238188

> mean(len_b)
[1] 1.343335

> mean(len_c)
[1] 1.323244

> cov_a
[1] 8617

> cov_b
[1] 9149

> cov_c
[1] 9751
