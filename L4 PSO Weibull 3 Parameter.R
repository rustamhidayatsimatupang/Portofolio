thres <- 5
data <- rweibull(100, 3, 4) + thres
llik.weibull <- function(shape, scale, thres, x)
{ 
  length(x)*log(scale)-length(x)*scale*log(shape)+(scale*sum(x-thres))-1/shape^scale*sum((x-thres)^scale)
}
ll=llik.weibull(3,4,5,data)
ll

loglikelihood <- function(x1,x2,x3)
{
 ll-llik.weibull(x1,x2,x3,data)
}

weibullpso<- function(FUN, n, max.loop, w, c1, c2, xmin, xmax, vmax)
{
  d <- length(xmin)
  
  x <- matrix(nrow=n, ncol=d)
  for(i in 1:d)
    x[,i] <- runif(n, xmin[i],xmax[i])
  
  fitness <- abs(FUN(x))
  pbest <- x
  gbest <- matrix(x[which.min(fitness),], ncol=d)
  
  v <- matrix(runif(n*d, min=-vmax, max=vmax), ncol=d, nrow=n)
  
  g.mean <- c()
  g.best <- c()
  
  loop <- 1
  while(loop <= max.loop)
  {
    fitness <- abs(FUN(x))
    g.mean <- rbind(g.mean, mean(fitness))
    
    idx <- which(fitness <= abs(FUN(pbest)))
    pbest[idx,] <- x[idx,]
    Pbest.baru <- matrix(x[which.min(FUN(pbest)),],ncol=d)
    if(abs(FUN(Pbest.baru ))<= abs(FUN(gbest)))
      gbest <- Pbest.baru
    g.best <- rbind(g.best, FUN(gbest))
    cat(loop,'beta=',gbest[,1],'lambda=',gbest[,2],'alpha=',gbest[,3],'gbest =', abs(g.best[loop]))
    print('')
    for(i in 1:n) 
    {
      for(j in 1:d) 
      {
        r1 <- runif(1)
        r2 <- runif(1)
        
        v[i,j] <- w*v[i,j] + c1*r1*(pbest[i,j] - x[i,j]) +  c2*r2*(gbest[j]-x[i,j])
        
        if(v[i,j] > vmax[j] || v[i,j] < -vmax[j])
          v[i,j] <- vmax[j]
        
        x_prev <- x[i,j]
        
        x[i,j] <- x[i,j] + v[i,j]
        
        if(x[i,j] > xmax[j])
          x[i,j] <- x_prev
        if(x[i,j] < xmin[j])
          x[i,j] <- x_prev
      }
    }
    loop <- loop + 1
  }
  
  res <- list(sol = gbest, val=g.best[loop-1])
  print(gbest)
  cat('Fitness =', abs(g.best[max.loop]))
}

weibullpso(FUN= function(x) -loglikelihood(x[1], x[2], x[3]), n=50, max.loop=100, w=0.9, c1=0.2, c2=0.2,xmin= c(2.5, 3.5, 4.5), xmax= c(3.5, 4.5, 5.5), vmax=c(2,2,2))

thres <- 5
data <- rweibull(200, 3, 4) + thres
weibullpso(FUN= function(x) -loglikelihood(x[1], x[2], x[3]), n=50, max.loop=100, w=0.9, c1=0.2, c2=0.2,xmin= c(2.5, 3.5, 4.5), xmax= c(3.5, 4.5, 5.5), vmax=c(2,2,2))

thres <- 5
data <- rweibull(500, 3, 4) + thres
weibullpso(FUN= function(x) -loglikelihood(x[1], x[2], x[3]), n=50, max.loop=100, w=0.9, c1=0.2, c2=0.2,xmin= c(2.5, 3.5, 4.5), xmax= c(3.5, 4.5, 5.5), vmax=c(2,2,2))

thres <- 5
data <- rweibull(1000, 3, 4) + thres
weibullpso(FUN= function(x) -loglikelihood(x[1], x[2], x[3]), n=50, max.loop=100, w=0.9, c1=0.2, c2=0.2,xmin= c(2.5, 3.5, 4.5), xmax= c(3.5, 4.5, 5.5), vmax=c(2,2,2))

