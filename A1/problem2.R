theta1 = rgamma(10000, 219, 112)

rv = mean(pgamma(theta1, 68, 45))


xf = rnbinom(10000, 219, 112/113)

rv2 = mean(pnbinom(xf-1, 68, 45/46))

x1 = seq(0,10)

plot(x1, dnbinom(x1, 219, 112/113), 'l', col='red')
lines(x1, dnbinom(x1, 68, 45/46), col='green')
