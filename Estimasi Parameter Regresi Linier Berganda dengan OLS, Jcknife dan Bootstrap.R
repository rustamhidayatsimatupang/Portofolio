library(readxl)
X1=rnorm(100,50,30)
X1
X2=rnorm(100,100,30)
X2
Residual=rnorm(100,3,30)
Residual
Residual5=c(rnorm(70,0,1),rnorm(30,30,30))
Residual5
b0=2
b1=9
b2=3
Y=c(b0+b1*X1+b2*X2+Residual5)
databangkitan=cbind(Y,X1,X2)

#Estimasi Parameter
Estimasi_parameter=function(data,B, d){
  Y<-data[,1]
  X1<-data[,2]
  X2<-data[,3]
  
  cat('Estimasi Parameter Regresi Linier\n\n')
  #Estimasi Parameter Dengan OLS
  b1_hat=(sum(X2^2)*sum(X1*Y)-sum(X1*X2)*sum(X2*Y))/(sum(X1^2)*sum(X2^2)-(sum(X1*X2))^2)
  b2_hat=(sum(X1^2)*sum(X2*Y)-sum(X1*X2)*sum(X1*Y))/(sum(X1^2)*sum(X2^2)-(sum(X1*X2))^2)
  b0_hat=mean(Y)-b1_hat*mean(X1)-b2_hat*mean(X2)
  Y_hat=b0_hat+b1_hat*X1+b2_hat*X2
  epsilon=Y-Y_hat
  
  
  #Menghitung MSE, RMSE, SE OLS
  MSE_OLS=sum((epsilon)^2)/length(epsilon)
  RMSE_OLS=sqrt(sum((epsilon)^2)/length(epsilon))
  SE_OLS=var(epsilon)/sqrt(length(epsilon))
  
  
  #Menampilkan Hasil Estimasi Parameter dan MSE, RMSE, SE OLS
  cat('Estimasi Parameter OLS\n')
  cat('---------------------------------------')
  cat("\n             Estimator OLS\n" )
  b_star_OLS=cbind(b0_hat,b1_hat,b2_hat)
  print(b_star_OLS)
  cat('\n---------------------------------------')
  cat("\nMSE OLS\n")
  cat(MSE_OLS)
  cat('\n--------------------------------------')
  cat("\nRMSE OLS\n")
  cat(RMSE_OLS)
  cat('\n--------------------------------------')
  cat("\nSE OLS\n")
  cat(SE_OLS)
  cat('\n--------------------------------------\n')
  
  
  #-------------------------------------------------------------------------------------------------------------#
  #Estimasi Parameter Dengan Bootstrap
  cat('\n\nEstimasi Bootstrap\n')
  for (BB in B){
    Bootstrap_residual=function(BB){
      
      #Bootstraping Residual
      Semua_Epsilon_star=c()
      Semua_b0_hat_star=c()
      Semua_b1_hat_star=c()
      Semua_b2_hat_star=c()
      for(i in 1:BB){
        for(i in 1:100){
          epsilon_star=sample(epsilon,100,replace = TRUE)
          Semua_Epsilon_star=c(Semua_Epsilon_star,mean(epsilon_star))
        }
        Y_star=b0_hat+b1_hat*X1+b2_hat*X2+Semua_Epsilon_star
        Semua_Epsilon_star=c()
        b1_hat_star=(sum(X2^2)*sum(X1*Y_star)-sum(X1*X2)*sum(X2*Y_star))/(sum(X1^2)*sum(X2^2)-(sum(X1*X2))^2)
        b2_hat_star=(sum(X1^2)*sum(X2*Y_star)-sum(X1*X2)*sum(X1*Y_star))/(sum(X1^2)*sum(X2^2)-(sum(X1*X2))^2)
        b0_hat_star=mean(Y_star)-b1_hat_star*mean(X1)-b2_hat_star*mean(X2)
        Semua_b0_hat_star=c(Semua_b0_hat_star, b0_hat_star)
        Semua_b1_hat_star=c(Semua_b1_hat_star, b1_hat_star)
        Semua_b2_hat_star=c(Semua_b2_hat_star, b2_hat_star)
        hasil=cbind(Semua_b0_hat_star,Semua_b1_hat_star,Semua_b2_hat_star)
      }
      
      #Estimator Bootstrap
      b0_star=mean(Semua_b0_hat_star)
      b1_star=mean(Semua_b1_hat_star)
      b2_star=mean(Semua_b2_hat_star)
      Y_star_hat=b0_star+b1_star*X1+b2_star*X2
      epsilon_B=Y-Y_star_hat
      
      #MSE, RMSE, SE Bootstrap
      MSE_Bootstap=sum((epsilon_B)^2)/length(epsilon_B)
      RMSE_Bootstap=sqrt(sum((epsilon_B)^2)/length(epsilon_B))
      SE_Bootstap=var(epsilon_B)/sqrt(length(epsilon_B))
      
      #Menampilkan Hasil Estimasi Parameter dan MSE, RMSE, SE Bootstrap
      cat('---------------------------------------\n')
      cat("Estimator Bootstrap B = ",BB,'\n' )
      b_star_bootrap=cbind(b0_star,b1_star,b2_star)
      print(b_star_bootrap)
      cat('---------------------------------------\n')
      cat("MSE Bootstrap\n")
      cat(MSE_Bootstap)
      cat('\n--------------------------------------')
      cat("\nRMSE Bootstap\n")
      cat(RMSE_Bootstap)
      cat('\n--------------------------------------')
      cat("\nSE Bootstap\n")
      cat(SE_Bootstap)
      cat('\n--------------------------------------\n')
    }
    Bootstrap_residual(BB) 
  }
  
  
  
  #-------------------------------------------------------------------------------------------------------------#
  #Estimasi Parameter Jackknife
  cat('\n\nEstimasi Parameter Jacknife\n')
  for (BJ in B){
    for (dj in d){
      Jackknife=function(BJ,dj){
        Semua_b0_hat_j=c()
        Semua_b1_hat_j=c()
        Semua_b2_hat_j=c()
        Semua_d=c()
        
        for(i in 1:dj){
          d_sampel=nrow(databangkitan)+1-i
          Semua_d=c(Semua_d, d_sampel)
        }
        
        #Resampling Jackknife
        for(i in 1:BJ){
          datajackknife=data.frame(databangkitan)[sample(1:nrow(databangkitan),100, replace=TRUE),]
          datajackknife=datajackknife[-c(Semua_d),]
          X1=datajackknife$X1
          X2=datajackknife$X2
          Y=datajackknife$Y
          b1_hat_j=(sum(X2^2)*sum(X1*Y)-sum(X1*X2)*sum(X2*Y))/(sum(X1^2)*sum(X2^2)-(sum(X1*X2))^2)
          b2_hat_j=(sum(X1^2)*sum(X2*Y)-sum(X1*X2)*sum(X1*Y))/(sum(X1^2)*sum(X2^2)-(sum(X1*X2))^2)
          b0_hat_j=mean(Y)-b1_hat_j*mean(X1)-b2_hat_j*mean(X2)
          Semua_b0_hat_j=c(Semua_b0_hat_j, b0_hat_j)
          Semua_b1_hat_j=c(Semua_b1_hat_j, b1_hat_j)
          Semua_b2_hat_j=c(Semua_b2_hat_j, b2_hat_j)
          hasil2=cbind(Semua_b0_hat_j,Semua_b1_hat_j,Semua_b2_hat_j)
        }
        
        #Estimator Jackknife
        b0_j=mean(Semua_b0_hat_j)
        b1_j=mean(Semua_b1_hat_j)
        b2_j=mean(Semua_b2_hat_j)
        Y_hat_j=b0_j+b1_j*X1+b2_j*X2
        epsilon_J=Y-Y_hat_j
        
        
        #MSE, RMSE, SE Jackknife
        MSE_Jacknife=sum((epsilon_J)^2)/length(epsilon_J)
        RMSE_Jacknife=sqrt(sum((epsilon_J)^2)/length(epsilon_J))
        SE_Jacknife=var(epsilon_J)/sqrt(length(epsilon_J))
        
        
        #Menampilkan Hasil Estimasi Parameter dan MSE, RMSE, SE Jackknife
        cat('---------------------------------------\n')
        cat(" Estimator Jackknife B = ",BJ, "d = ",dj ,"\n")
        b_hat_j=cbind(b0_j,b1_j,b2_j)
        print(b_hat_j)
        cat('---------------------------------------\n')
        cat("MSE Jackknife\n")
        cat(MSE_Jacknife)
        cat('\n--------------------------------------')
        cat("\nRMSE Jacknife\n")
        cat(RMSE_Jacknife)
        cat('\n--------------------------------------')
        cat("\nSE Jacknifep\n")
        cat(SE_Jacknife)
        cat('\n--------------------------------------\n')
      }
      Jackknife(BJ,dj)
    }
  }
}

Estimasi_parameter(data=databangkitan,B=c(10, 100, 500, 1000, 5000, 10000, 50000) ,d=c(1,2,3))
