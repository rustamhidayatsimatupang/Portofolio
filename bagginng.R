library(ipred)
library(caret)
library(rpart)
library(rpart.plot)

datalearning=read.csv(file.choose())
testing1=read.csv(file.choose())

#mengubah data ke Faktor
datalearning$Gender=as.factor(datalearning$Gender)
datalearning$Pembiayaan.Kuliah=as.factor(datalearning$Pembiayaan.Kuliah)
datalearning$Status.Ketika.Mahasiswa=as.factor(datalearning$Status.Ketika.Mahasiswa)
datalearning$Lama.Penyusunan.Skripsi=as.factor(datalearning$Lama.Penyusunan.Skripsi)
datalearning$IPK=as.factor(datalearning$IPK)
datalearning$Masa.Studi=as.factor(datalearning$Masa.Studi)

model1=bagging(Masa.Studi~.,data=training1,nbagg = 10,control = rpart.control(minsplit = 2))
#Prediksi Berdasarkan Majority Vote
Prediksi1=predict(model1,testing1)
hasil1=table(Prediksi1, testing1$Masa.Studi)
print(hasil1)
caret::confusionMatrix(hasil1)

i=25
while(i<=100){
#Metode Bagging
sampellearning=sample(1:nrow(datalearning),123,replace = T)
training1=data.frame(datalearning)[sampellearning,]
model1=bagging(Masa.Studi~.,data=training1,nbagg = i,control = rpart.control(minsplit = 2))
#Prediksi Berdasarkan Majority Vote
Prediksi1=predict(model1,testing1)
hasil1=table(Prediksi1, testing1$Masa.Studi)
print(i)
print(hasil1)
caret::confusionMatrix(hasil1)
i=i+25
}