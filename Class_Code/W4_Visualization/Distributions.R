rm(list=ls())

##a basic Gaussian generator 

sample(c(0,1),1) ## a coin toss

no.toss <- 300 ## number of tosses per person
no.people <- 300 ## number of people

Coin_toss <- NULL
i <- 1
for (i in 1:no.people){
  Coin_toss[i] <- sum(sample(c(0,1), no.toss, replace=TRUE))
  }                  
hist(Coin_toss)

sample(c(0,1),1)

##a basic log-normal generator 

sample(c(1,1.05),1) ## a raise pool toss

no.toss <- 300 ## number of iterations per person
no.people <- 300 ## number of people

Income_toss <- NULL
i <- 1
for (i in 1:no.people){
  Income_toss[i] <- prod(sample(c(1.05,1), no.toss, replace=TRUE))
}                  
hist(Income_toss)


## a more complex "salary simulator"
## try increasing the variance in the growth rate

start.value <- rep(50000,1000)
lifespan <- rnorm(1000, 40, 5)
growth.rate <- 1 + (rnorm(1000, 0.02, 0.00001))
z <- start.value*growth.rate^lifespan
hist(z)



