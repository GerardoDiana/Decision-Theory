################################## Diana Gerardo #######################################
#################################### HW 3 Code #########################################

#### Problem 7.5
length = 200

prior1 <- seq(0, 1, length.out = length)
prior2 <- seq(0, 1, length.out = length)

# last 3 columns are the corresponding priors
# first 4 columns are the desicion rules
risk.matrix = matrix(NA, ncol = 7, nrow = (1 + length) * length / 2) 

row = 0

for (i in prior1) 
  {
    for (j in prior2) 
      {
        if (i + j > 1) next #we must satisfy the constraint
        k = 1 - i - j
        row = row + 1
        # 1st rule
        risk.matrix[row, 1] = 0 * i + 0 * j + 1 * k
        risk.matrix[row, 2] = 2 * i + 4 * j + 0 * k
        risk.matrix[row, 3] = 1.6 * i + 3.6 * j + 0.25 * k
        risk.matrix[row, 4] = 0.4 * i + 0.4 * j + 0.75 * k
        risk.matrix[row, 5] = i
        risk.matrix[row, 6] = j
        risk.matrix[row, 7] = k
    }
}

# inf Bayes risk 
minimized.risk = apply(risk.matrix[, 1:4], 1, min)

# sup inf Bayes risk
max(minimized.risk)
index = which(minimized.risk == max(minimized.risk))

# find the corresponding priors
risk.matrix[index, ]

##################################################################################

#under sample model 2
length = 200

prior1 <- seq(0, 1, length.out = length)
prior2 <- seq(0, 1, length.out = length)

# last 3 columns are the corresponding priors
# first 4 columns are the desicion rules
risk.matrix = matrix(NA, ncol = 7, nrow = (1 + length) * length / 2) 

row = 0

for (i in prior1) 
  {
    for (j in prior2) 
      {
        if (i + j > 1) next #we must satisfy the constraint
        k = 1 - i - j
        row = row + 1
        # 1st rule
        risk.matrix[row, 1] = 0 * i + 0 * j + 1 * k
        risk.matrix[row, 2] = 2 * i + 4 * j + 0 * k
        risk.matrix[row, 3] = 0.8 * i + 2.8 * j + 0.75 * k
        risk.matrix[row, 4] = 1.2 * i + 1.2 * j + 0.25 * k
        risk.matrix[row, 5] = i
        risk.matrix[row, 6] = j
        risk.matrix[row, 7] = k
    }
}

# inf Bayes risk 
minimized.risk = apply(risk.matrix[, 1:4], 1, min)

# sup inf Bayes risk
max(minimized.risk)
index = which(minimized.risk == max(minimized.risk))

# find the corresponding priors
risk.matrix[index, ]
##################################################################################
r.theta0 <- c(0, 1/9, 4/9, 4/9, 8/9, 5/9, 5/9, 1)
r.theta1 <- c(1, 3/4, 2/4, 3/4, 1/4, 2/4, 1/4, 0)

data <- data.frame(r.theta0, r.theta1)
row.names(data) <- c("d1", "d2", "d3",
                       "d4", "d5", "d6",
                       "d7", "d8")

plot(data$r.theta1~data$r.theta0, pch=19, 
     xlab = expression("R("~theta[0]~","~delta~")"),
     ylab = expression("R("~theta[1]~","~delta~")"))
segments(0, 1, 1/9, 3/4, lwd=2)
segments(0, 1, 4/9, 3/4, lwd=2)
segments(4/9, 3/4, 8/9, 1/4, lwd=2)
segments(8/9, 1/4, 1, 0, lwd=2)
segments(1/9, 3/4, 5/9, 1/4, lwd=2)
segments(5/9, 1/4, 1, 0, lwd=2)
abline(a = 0, b = 1)
abline(a = 0.25*4/3, b = -1/3, lty=5)
#points(0.411764705,0.411764705,pch=19, col="red")
segments(0.411764705,-1, 0.411764705,0.411764705, lty=3)
segments(-1,0.411764705, 0.411764705,0.411764705, lty=3)
#curve(-1/3*x+0.25*4/3, add=T)
identify(data$r.theta1~data$r.theta0,labels=row.names(data))

##################################################################################
alpha <-1
beta <- 5  
n <- 100
a <- alpha/(n + 1/beta)
b <- n/(n + 1/beta)

curve(x*1/n, xlab="", ylab="", xlim=c(0,10), lty=2, lwd=2,
      main=expression(alpha~"=1, "~beta~"=5, n=100" ))
curve(x^2 * (1-b)^2 + x*((b^2)/n - 2*a +2*a*b)+a^2,
      xlab="", ylab="" , col='red',add = T, lwd=2)
legend("topleft", legend = c("Bayes Estim.", "MLE"),
       lty=c(1,2),col=c("red","black"),lwd=2)

