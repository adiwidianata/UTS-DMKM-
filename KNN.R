#read data
sonar <- read.csv("D:\\Matkul\\Semester 5\\DMKM\\UTS\\UTS\\sonar.csv", header = FALSE, sep = ";")
head(sonar)
#melihat apakah terdapat missing values
sum(is.na(sonar))
str(sonar)
set.seed(1234)
#membagi data menjadi data testing dan training
sampling <- sample(1:nrow(sonar), 0.8*nrow(sonar))
training_set <- sonar[sampling,]
test_set <- sonar[-sampling,]
sampling
#pembuatan model (split validation)
library(class)
split_knn <-knn(training_set[,-61], test_set[,-61], training_set$V61, k=5)
split_knn
library(MLmetrics)
uji <- function(en){
  cat("k\tAkurasi\t\tSensitivity\tPrecision\tRecall\tF-1 score\n")
  for ( i in 2:en){
    pred <- knn(training_set[,-61], test_set[,-61], training_set$V61, k=i)
    akurasi <- Accuracy(pred, test_set$V61)
    sensitif <- Sensitivity(pred, test_set$V61)
    presisi <- Precision(pred,test_set$V61)
    rec <- Recall(pred,test_set$V61)
    f <- F1_Score(pred, test_set$V61)
    cat(i, "\t", akurasi, "\t", sensitif,"\t",presisi,"\t",rec,"\t",f,"\n")
  }
}
uji(20)
#evaluasi model
split_knn <- knn(training_set[,-61], test_set[,-61], training_set$V61, k=5)
ConfusionMatrix(split_knn, test_set$V61)
Precision(split_knn, test_set$V61, positive="M")
Recall(split_knn, test_set$V61, positive = "M")
F1_Score(split_knn, test_set$V61, positive="M")
