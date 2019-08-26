n = 1000

P = matrix(c(0.2, 0.8, 0, 0, 0, 
             0.2, 0.2, 0.6, 0, 0, 
             0, 0.4, 0.2, 0.4, 0, 
             0, 0, 0.6, 0.2, 0.2, 
             0, 0, 0, 0.8, 0.2), nrow = 5, byrow = TRUE)

init_state = c(1, 0, 0, 0, 0)

path = vector("numeric", n)


cur_prob = init_state
for (i in 1:n) {
    cur_state = sample((1:5), 1, prob = cur_prob)
    path[i] = cur_state
    cur_prob = c(0, 0, 0, 0, 0)
    cur_prob[cur_state] = 1
    cur_prob = cur_prob %*% P
}

plot((1:n), path, 'l')

pi_hat = c(mean(path==1), mean(path==2), mean(path==3), mean(path==4), mean(path==5))

pi_stat = pi_hat %*% P

