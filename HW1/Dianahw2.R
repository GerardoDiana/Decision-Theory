# AMS221 Homework 2
# Diana's CODE

#Problem 1
u <- c(-5,-2,-1,0,1,2)
x <- c(-200, -100,-25, 100, 300, 600)
data <- data.frame(u,x)
plot(data$u~data$x, pch=19, xlab="x", ylab="u")
lines(spline(data$u~data$x, n=500))
abline(h= 0.51875)
abline(v = 191, col="darkgreen")

u <- c(0,2.5,3.75,5,6.25,7.5,8.75,10)
x <- c(0,200,600, 2000, 3000, 4000, 6000, 10000)
data <- data.frame(u,x)
plot(data$u~data$x, pch=19, xlab="x", ylab="u")
lines(spline(data$u~data$x, n=1000))

u <- c(-80,-40,-20,-10,-8.75, -7.5, -6.25,-5,-3.75,-2.5,-1.25,
       0, 1.25, 2.5, 3.75, 5, 6.25, 7.5, 8.75, 10)
x <- c(-10000,-7000,-5500,-4000,-3500,-3000,-2500,-2000,-1000,-550,-100,
       0, 500, 1000, 2000, 3000, 4000, 5000, 8000, 10000)
data <- data.frame(u,x)
points(data$u~data$x, pch=19, xlab="x", ylab="u")
lines(spline(data$u~data$x, n=500))
fit = (interpSpline(data$u~data$x))
plot(fit, xlab="x", ylab="u", ylim=c(-80,20))
abline(v=0, col="blue")
abline(h=0,col="blue")

par(mar=c(5,4,2,2))

(2.5+0)/2
#################################
p1 <- c(1/10, 1/2, 2/3, 9/10)
p <- c(.99975,0.9994,0.99915,0.99907)
c(1000*(1-p))
c(exp(-0.25)-(1-p)*exp(-1000))
################################
p <- c(.99975,0.9994,0.99915,0.99907)
z <- c(0.25, 0.60, 0.85, 0.93)

fit = (interpSpline(p~z))
plot(fit)
points(p~z)

fit = (interpSpline(p1~z))
plot(fit)
points(p1~z)

###############################
-a()
