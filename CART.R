library(rpart)
library(rpart.plot)
#Data Learning
Data1=read.csv(file.choose())
Data1

#CART parent Node
Cart1=rpart(Masa.Studi~Lama.Penyusunan.Skripsi,data=Data1,method="class",minsplit = 2,cp=0)
rpart.plot(Cart1,type=1,digit=5,fallen.leaves = FALSE)
print(Cart1)

#plot CART
Cart2=rpart(Masa.Studi~.,data=Data1,minsplit = 2,cp=0,xval=10,maxdepth=5)
rpart.plot(Cart2,type=1,digit=5,fallen.leaves = FALSE)
print(Cart2)

printcp(Cart2)
plotcp(Cart2)

#Pemangkasan
Cart2=rpart(Masa.Studi~.,data=Data1,method="class",minsplit = 3, cp=0)
rpart.plot(Cart2,type=5,digit=5,fallen.leaves = FALSE)

#Data Testing
Data2=read.csv(file.choose())
Data2

#Hasil Prediksi
Pred=predict(Cart2, newdata=Data2, type="class")
test=table(Data2$Masa.Studi,Pred)
print(test)
Pred

#Perhitungan Error
Akurasi=((test[1,1]+test[2,2])/31)
print(Akurasi)

Sensitivity=((test[1,1])/5)
print(Sensitivity)

Specifity=((test[2,2])/26)
print(Specifity)


