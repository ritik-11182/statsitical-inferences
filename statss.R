set.seed(24)
n <- 40
lambda <- 0.2
nosim <- 1000

means = NULL
for (i in 1 : 1000) {
  means = c(means, mean(rexp(n,lambda)))
}

head(means)
theromean <- round(1/lambda)
theromean
actualMean <- round(mean(means),3)
actualMean
library(ggplot2)
sDf <- as.data.frame(means)
ggplot(sDf, aes(x=means))+ geom_histogram(binwidth = .2, color="black", fill="orange") +
  geom_vline(xintercept = theromean, color="red", size=2, lty=3, lwd = 2) +
  geom_vline(xintercept = actualMean, color="green", size=2, lty = 2, lwd = 2) +
  labs(x="mean of 40 simulated exponential sample", y= "frequency", 
       title="Simulated Exponential Samples Means")

therovar <- round((1/lambda)^2/n,3)
therovar
actualVar <- round(var(means),3)
actualVar
tab <- matrix(c(theromean, actualMean, 
                therovar,actualVar),
              ncol = 2, byrow=TRUE)
colnames(tab) <- c("theroetical","sample")
rownames(tab) <- c("mean","variance")
tab <- as.table(tab)
tab

ggplot(sDf, aes(x=means)) + geom_histogram(binwidth = .2, color="black", fill="orange" , aes(y=..density..))+
  stat_function(fun=dnorm, args=list(mean=theromean, sd=sd(means)), 
                color="red", size =2, geom = "line") +
  stat_density(geom = "line", color = "green", size =1.5)  +
  labs(x="mean of 40 simulated exponential sample", y= "density", 
       title="Density of Simulated Exponential Samples Means")

qqnorm(means, col = "blue")
qqline(means, col = "red", lwd = 2)
