library(coda)    ##package analisis output untuk MCMC
BayesARCH<-function(burnIn=1000,nc=1,Y, a0, b1,loglik)    ##Penamaan fungsi dan variabel-variabel 
{
  parameter<-c(mean(Y),a0,b1)   ##pendeklarasian nilai variabel parameter
  para<-0      ##nilai awal variabel parameter
  parameterprior<-0  ##nilai awal prior tiap parameter
  prior_total<-0   ##nilai awal jumlah prior
  ld<-length(parameter)  ##panjang parameter
  llike<-loglik  ##pendeklarasian nilai log-likelihood
  
  prior <- function(param){  ##fungsi untuk mencari nilai prior
    List <- list()     
    for(j in 1:l){   ##fungsi perulangan untuk mencari nilai prior dari 1 sampai panjang parameter
      para[j]= param[j]  
      parameterprior[j] = dnorm(para[j], log = T) ##mencari nilai prior tiap parameter menggunakan distribusi normal berbentuk logaritma
      List[[j]] <- parameterprior[j]    ##mengumpulkan nilai dari distribusi prior
      prior_total<-prior_total+List[[j]]  ##menghitung total prior 
    }
    return(prior_total)  ##mengakhri eksekusi dari fungsi mencari nilai prior, dan memberikan nilai akhir dari fungsi prior
  }
  
  posterior <- function(param){  ##fungsi untuk mencari nilai posterior
    return (llike + prior(param))  ##nilai dari posterior didapat dari penjumlahan (karena distribusi prior dan fungsi likelihood berbentuk logaritma) antara nilai log-likelihood dengan nilai prior yang telah didapat dari fungsi prior sebelumnya
  }
  
  proposalfunction <- function(param){  ##fungsi pembangkitan sampel random untuk parameter baru yang akan diuji
    return(rnorm(l,mean = param))   ##membangkitkan nilai acak berdistribusi normal sebanyak jumlah parameter (l) dengan rata-rata nilai parameter
  }
  
  run_metropolis_MCMC <- function(startvalue, iterations){  ##fungsi untuk menjalankan 
    chain = array(dim = c(iterations+1,l))  ##menentukan ukuran dimensi mcmc
    chain[1,] = startvalue   ##Inisialisasi nilai cain pertama
    for (i in 1:iterations){
      proposal = proposalfunction(chain[i,]) ##Pemanggilan fungsi proposal
      probab = exp(posterior(proposal) - posterior(chain[i,]))  ##Menentukan Nilai Kritis alpha
      if (runif(1) < probab){  ##Menentukan Nilai Parameter Baru
        chain[i+1,] = proposal 
      }else{chain[i+1,] = chain[i,]
      }
    }
    return(mcmc(chain))
  }
  
  List <- list()
  for(i in 1:nc){
    chain= run_metropolis_MCMC(parameter, 100) ##Melakukan MCMC sebanyak 100 iterasi
    acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))  ##Menentukan batas penerimaan
    List[[i]] <- chain
    list(summary(List[[1]])) 
  }
  return( for(i in 1:nc){
    print(mcmc(chain)) ##Menampilkan Hasil MCMC
    a=summary(List[[i]])
    print(a)}) ##MEnampilkan Hasil Estimasi Parameter
}
data=as.ts(read.table("D://Data Inflasi.csv")) ##Menginput Data
res=arima(data,order = c(3,1,0))$residuals[2:length(data)] ##Menentukan nilai Residual
BayesARCH(Y=res,a0=0, b1=0,loglik= -271.4201) ##Pemanggilan Fungsi untuk memperoleh parameter dengan metode bayes
