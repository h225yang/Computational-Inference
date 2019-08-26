N = 10000

vec.lower = vector("numeric", N)
vec.upper = vector("numeric", N)
theta = (qt(0.75, 3)-qt(0.25, 3))/1.34

for (counter in 1:N) {
    samples.minus.i = NA
    samples = rt(25, 3)
    q_sample = quantile(samples, c(0.25, 0.75), names = FALSE)
    theta_hat = (q_sample[1]-q_sample[2])/1.34
    for (i in 1:25) {
        q = quantile(samples[-i], c(0.25, 0.75), names = FALSE)
        samples.minus.i[i] = (q[1] - q[2])/1.34
    }
    se = sqrt((24/25)*sum(samples.minus.i-theta_hat)^2)
    vec.lower[counter] = theta_hat-qnorm(0.975)*se
    vec.upper[counter] = theta_hat+qnorm(0.975)*se
}

avg_ci = c(mean(vec.lower), mean(vec.upper))

# normal ci
#c(theta-qnorm(0.975)*avg_se, theta+qnorm(0.975)*avg_se)




# vec2 = vector("numeric", N)
# B = 1000
# 
# for (counter in 1:N) {
#     for (i in 1:B){
#         
#     }
# }
