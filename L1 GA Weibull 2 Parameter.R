library(GA)
library(ExtDist)
d=rweibull(100,shape = 1,scale = 1)
ll=lWeibull(d, 100, shape = 1, scale = 1, params = list(shape = 1, scale = 1, logL = TRUE))
ll
loglikelihood <- function(x1, x2)
{
  min(abs(ll-lWeibull(d, w=100, shape = x1, scale = x2,params = list(shape = x1, scale = x2),logL = TRUE)))
}

GA <- ga(type = "real-valued", 
         fitness =  function(x) -loglikelihood(x[1], x[2]),
         lower = c(0.5, 0.5), upper = c(1.5, 1.5), 
         popSize = 50, maxiter = 100,
         optim = TRUE)
summary(GA)


d=rweibull(200,shape = 1,scale = 1)
ll=lWeibull(d, 200, shape = 1, scale = 1, params = list(shape = 1, scale = 1, logL = TRUE))
ll
loglikelihood <- function(x1, x2)
{
  min(abs(ll-lWeibull(d, w=200, shape = x1, scale = x2,params = list(shape = x1, scale = x2),logL = TRUE)))
}
GA <- ga(type = "real-valued", 
         fitness =  function(x) -loglikelihood(x[1], x[2]),
         lower = c(0.5, 0.5), upper = c(1.5, 1.5), 
         popSize = 50, maxiter = 100,
         optim = TRUE)
summary(GA)

d=rweibull(500,shape = 1,scale = 1)
ll=lWeibull(d, 500, shape = 1, scale = 1, params = list(shape = 1, scale = 1, logL = TRUE))
ll
loglikelihood <- function(x1, x2)
{
  min(abs(ll-lWeibull(d, w=500, shape = x1, scale = x2,params = list(shape = x1, scale = x2),logL = TRUE)))
}
GA <- ga(type = "real-valued", 
         fitness =  function(x) -loglikelihood(x[1], x[2]),
         lower = c(0.5, 0.5), upper = c(1.5, 1.5), 
         popSize = 50, maxiter = 100,
         optim = TRUE)
summary(GA)

d=rweibull(1000,shape = 1,scale = 1)
ll=lWeibull(d, 1000, shape = 1, scale = 1, params = list(shape = 1, scale = 1, logL = TRUE))
ll
loglikelihood <- function(x1, x2)
{
  min(abs(ll-lWeibull(d, w=1000, shape = x1, scale = x2,params = list(shape = x1, scale = x2),logL = TRUE)))
}
GA <- ga(type = "real-valued", 
         fitness =  function(x) -loglikelihood(x[1], x[2]),
         lower = c(0.5, 0.5), upper = c(1.5, 1.5), 
         popSize = 50, maxiter = 100,
         optim = TRUE)
summary(GA)

