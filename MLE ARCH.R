library(tseries)     ##package analisis output untuk time series
ARCHMLE<-function(data,p)  ##penamaan fungsi dan variabel-variabel
{
model=garch(data,order=c(p,0),trace=F)  ##estimasi model ARCH menggunakan MLE dengan data dan orde ARCH (orde p)
print(model)  ##menampilkan hasil estimasi model ARCH
res=model$residuals[2:length(data)]  ##menghitung nilai residual dari model ARCH
cat('mean =',mean(res),'Standard Deviasi =', sd(res))  ##menampilkan nilai mean dan varians dari residual
print(logLik(model))  ##menampilkan nilai log-likelihood dari model
}
data=as.ts(read.table("D://Tugas Akhir Mila//Data Inflasi.csv"))  ##input data
ARCHMLE(data,1)   ##pemanggilan fungsi ARCHMLE dengan orde 1
