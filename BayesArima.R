library(coda)
library(forecast)
BayesARIMA<-function(Y,iter=50,burnIn=1000,nc=1,p,d,q)
{
  model_arima<-arima(Y,order = c(p,d,q))
  par<-model_arima$coef
  para<-0
  paraprior<-0
  prior_total<-0
  l<-length(par)
  llike<-model_arima$loglik
  
  prior <- function(param){
    List <- list()
    for(j in 1:l){
      para[j]= param[j]
      paraprior[j] = dnorm(para[j], log = T)
      List[[j]] <- paraprior[j]
      prior_total<-prior_total+List[[j]]
    }
    return(prior_total)
  }
  
  posterior <- function(param){
    return (llike + prior(param))
  }
  
  proposalfunction <- function(param){
    return(rnorm(l,mean = param))
  }
  
  run_metropolis_MCMC <- function(startvalue, iterations){
    chain = array(dim = c(iterations+1,l))
    chain[1,] = startvalue
    for (i in 1:iterations){
      proposal = proposalfunction(chain[i,])
      probab = exp(posterior(proposal) - posterior(chain[i,]))
      if (runif(1) < probab){
        chain[i+1,] = proposal
      }else{chain[i+1,] = chain[i,]
      }
    }
    return(mcmc(chain))
  }
  
  List <- list()
  for(i in 1:nc){
    chain= run_metropolis_MCMC(par, iter)
    acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))
    List[[i]] <- chain
    list(summary(List[[1]]))
  }
  return( for(i in 1:nc){
    a=summary(List[[i]])
    cat(i)
    print(a)
    print(mcmc(chain))})
}
data=read.table("D://harga emas.csv")
data=as.ts(data)
data1=diff(data)
model=BayesARIMA(data1,p=1,d=0,q=2)
