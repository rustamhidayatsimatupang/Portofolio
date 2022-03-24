library(GA)
thres <- 5
data <- rweibull(100, 3, 4) + thres
llik.weibull <- function(shape, scale, thres, x)
{ 
  length(x)*log(scale)-length(x)*scale*log(shape)+(scale*sum(x-thres))-1/shape^scale*sum((x-thres)^scale)
}
ll=llik.weibull(5,3,4,data)
ll

loglikelihood <- function(x1,x2,x3)
{
  min(abs(ll-llik.weibull(x1,x2,x3,data)))
}
GA <- ga(type = "real-valued", 
         fitness =  function(x) -loglikelihood(x[1], x[2], x[3]),
         lower = c(2.5, 3.5, 4.5), upper = c(3.5, 4.5, 5.5), 
         popSize = 50, maxiter = 100,
         optim = F)
summary(GA)


thres <- 5
data <- rweibull(200, 3, 4) + thres
GA <- ga(type = "real-valued", 
         fitness =  function(x) -loglikelihood(x[1], x[2], x[3]),
         lower = c(2.5, 3.5, 4.5), upper = c(3.5, 4.5, 5.5), 
         popSize = 50, maxiter = 100,
         optim = F)
summary(GA)

thres <- 5
data <- rweibull(500, 3, 4) + thres
GA <- ga(type = "real-valued", 
         fitness =  function(x) -loglikelihood(x[1], x[2], x[3]),
         lower = c(2.5, 3.5, 4.5), upper = c(3.5, 4.5, 5.5), 
         popSize = 50, maxiter = 100,
         optim = F)
summary(GA)

thres <- 5
data <- rweibull(1000, 3, 4) + thres
GA <- ga(type = "real-valued", 
         fitness =  function(x) -loglikelihood(x[1], x[2], x[3]),
         lower = c(2.5, 3.5, 4.5), upper = c(3.5, 4.5, 5.5), 
         popSize = 50, maxiter = 100,
         optim = F)
summary(GA)



setwd("D:")
write.csv(data, "abc.csv")