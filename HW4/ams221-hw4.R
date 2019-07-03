set.seed(1)

#number of observations
n <- 1000

#parameter values
mu.0 <- 0
tau.0.2 <- 1
sigma.2 <- 4

#observed values
x <- rnorm(n, mu.0, sigma.2 + tau.0.2)

D <- sigma.2 + tau.0.2 

diff <- c()
for(i in 1:length(x)){
  diff[i] <- (tau.0.2/D)*(x[i]- mu.0)
}

c.V <- (D/tau.0.2^2)*(diff)^2
plot(density(c.V))

hist(diff, freq=F, breaks=30)
hist(diff^2, freq = F, breaks=30)
plot(density(diff^2), xlab="", main="")
plot(density(c.V), xlab="", main="")

# posterior values
A <- (sigma.2*mu.0)/D + (tau.0.2*x[1])/D
B <- (sigma.2*tau.0.2)/D
theta <- rnorm(n, A, B)


x[1]

-230/(6-A)^2

##################################################################################
alpha <- seq(1, 10, length=100)
u.0 <- c()
for (i in 1:100){
  u.0[i] <- (-1*(0.75)^(alpha[i]-1)) * (0.25)
}

u.1 <- c()
for(i in 1:100){
  u.1[i] <- -3*(0.5)^alpha[i]
}

data <- data.frame(u.0,u.1)

which(data$u.0>data$u.1)
data[49,]
data[50,]
alpha[50]


u.0 <- c()
for (i in 1:100){
  u.0[i] <- (-1*(0.75)^(alpha[i])) 
}

u.1 <- c()
for(i in 1:100){
  u.1[i] <- -3*(0.5)^alpha[i]
}

data <- data.frame(u.0,u.1)
alpha[20]
