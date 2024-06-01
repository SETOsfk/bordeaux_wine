# Load necessary libraries
library(caret)
library(xgboost)
library(randomForest)
library(themis)
library(dplyr)
library(pROC)
library(ROSE)

#Veri setini oluşturduk
wine<-BordeauxWines
str(wine) #Öncelikle karakter tiplerini görmek için veri setini inceliyoruz
#İlk dört sütun harici bütün değerlerin faktör olması gerekirken double olarak işaretlenmiş
#Bunun dönüşümü için ilk dört sütun atılıyor.
wine<-wine[,-1:-4]
#Ardından double olan değişkenler faktöre dönüştürülüyor
wine<- wine %>% mutate_if(is.double,as.factor)
str(wine)
#Bazı faktörler sadece 0 olarak işaretlenmiş modeli eğitmede bir katkıda bulunmayacağını düşündüğüm için
#veri setinden çıkardım.
cols_to_remove <- sapply(wine, function(x) is.factor(x) && length(levels(x)) == 1)
wine_main<-wine[,!cols_to_remove]
#Hepsi faktör ve 0,1 olmak üzre iki levele sahip
str(wine_main)
#Çıkarılan sütunlar da veri setine geri ekleniyor ve son olarak YIL değişkeninin sınıf dönüşümü yapılıyor.
birlestir<-BordeauxWines[,1:4]
wine_main<-data.frame(birlestir,wine_main)
wine_main$Year<-as.Date(wine_main$Year)

#Skor değişkeni 90dan fazla olanlar ve 89dan az olanlar şeklinde ikiye ayrılıyor.
Diagnose <- c(1:14349)
wine_main<-data.frame(wine_main,Diagnose)
wine_main <- wine_main %>%
  mutate(Diagnose = case_when(
    Score <=89  ~"X0",
    Score >=90 ~"X1",)) %>% 
  select(Diagnose,Score,Name,Year,Price,everything())
wine_main$Diagnose<-as.factor(wine_main$Diagnose)
wine_main<- wine_main %>% select(-Year,-Name,-Price,-Score) #olduğunca sade bir subset elde etmek amacıyla
#skora etkisi olmayan değişkenler veri setinden atılıyor.
str(wine_main$Diagnose)


#RFI ile boyut indirgeme için random forest modeli geliştirilmesi
rf_model <- randomForest(Diagnose ~ ., data = wine_main, importance = TRUE, ntree = 100)


print(rf_model)

# Önemli değişkenlerin kaydedilmesi ve özellikleri.
importance_scores <- importance(rf_model, type = 1)

head(importance_scores)

str(importance_scores)

# If importance_scores is a matrix, extract the correct column
# Typically the first column contains MeanDecreaseAccuracy or MeanDecreaseGini
if (is.matrix(importance_scores)) {
  importance_values <- importance_scores[, 1] # Change 1 if another column is needed
} else {
  importance_values <- importance_scores
}

#Azalan olarak önem düzeylerini sırala ve sonrasında ilk 50 tanesinin isimlerini kaydet.
sorted_importance <- sort(importance_values, decreasing = TRUE)
important_features <- names(sorted_importance)[1:50]





print(important_features)

#İsmleri ve DİAGNOSE sütununu farklı bir veri setine al
X_reduced <- wine_main[,c(important_features)]
Diagnose<-wine_main$Diagnose
X_reduced<- data.frame(Diagnose,X_reduced) 

# 2. Train-Test Split
trainIndex <- createDataPartition(X_reduced$Diagnose, p = .7, 
                                  list = FALSE, 
                                  times = 1)
train_data <- X_reduced[ trainIndex,]
test_data  <- X_reduced[-trainIndex,]




barplot(prop.table(table(X_reduced$Diagnose)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Sınıf Dağılımı")

#Sınıflar arası denge sorunu olduğu görülüyor

bothq <- ovun.sample(Diagnose~., data = train_data, method = "both")
train_data<-bothq$data
#Denge sorununu hem under hem oversampling yaparak çöz.

barplot(prop.table(table( train_data$Diagnose)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Sınıf Dağılımı")


# 3. Define Train Control
train_control <- trainControl(
  method = "cv",       # Cross-validation
  number = 5,          # 5-fold cross-validation
  classProbs = TRUE,   # Compute class probabilities
  summaryFunction = twoClassSummary # Summary function for classification
)

# 4. Logistic Regression Model
logistic_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "glm", 
  family = binomial, 
  trControl = train_control, 
  metric = "ROC"
)




rf_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "rf", 
  trControl = train_control, 
  metric = "ROC"
)

#svm model
svm_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "svmRadial", 
  trControl = train_control, 
  metric = "ROC"
)
#gbm model
gbm_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "gbm", 
  trControl = train_control, 
  metric = "ROC"
)
#adaboost model
ada_model<-train(Diagnose~.,data=train_data,method="AdaBoost.M1",
                 trControl=train_control,
                 metric="ROC")



# 7. Evaluate Models
# Logistic Regression
logistic_preds <- predict(logistic_model, newdata = test_data)
logistic_probs <- predict(logistic_model, newdata = test_data, type = "prob")
logistic_roc <- roc(response = test_data$Diagnose, predictor = logistic_probs[,2])
print(auc(logistic_roc))

plot(logistic_roc)
plot(auc(logistic_roc))
# Random Forest
rf_preds <- predict(rf_model, newdata = test_data)
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")
rf_roc <- roc(response = test_data$Diagnose, predictor = rf_probs[,2])
print(auc(rf_roc))




#SVM
svm_preds <- predict(svm_model, newdata = test_data)
svm_probs <- predict(svm_model, newdata = test_data, type = "prob")
svm_roc <- roc(response = test_data$Diagnose, predictor = rf_probs[,2])
print(auc(svm_roc))

#GBM
gbm_preds <- predict(gbm_model, newdata = test_data)
gbm_probs <- predict(gbm_model, newdata = test_data, type = "prob")
gbm_roc <- roc(response = test_data$Diagnose, predictor = rf_probs[,2])
print(auc(gbm_roc))

ada_preds <- predict(ada_model, newdata = test_data)
ada_probs <- predict(ada_model, newdata = test_data, type = "prob")
ada_roc <- roc(response = test_data$Diagnose, predictor = rf_probs[,2])
print(auc(gbm_roc))

# Confusion Matrices
print(confusionMatrix(logistic_preds, test_data$Diagnose))
print(confusionMatrix(rf_preds, test_data$Diagnose))

print(confusionMatrix(svm_preds, test_data$Diagnose))
print(confusionMatrix(gbm_preds, test_data$Diagnose))
print(confusionMatrix(ada_preds, test_data$Diagnose))





results <- resamples(list(LOG = logistic_model, RF = rf_model, GBM = gbm_model, 
                          SVM = svm_model,ADAB=ada_model))
summary(results)
dotplot(results)


varimp_rf <- varImp(rf_model)
plot(varimp_ada, main="rf icin degisken onem duzeyleri")

varimp_svm <- varImp(svm_model)
plot(varimp_svm, main="svm icin degisken onem duzeyleri")

varimp_ada<-varImp(ada_model)
plot(varimp_ada, main="adaboost icin degisken onem duzeyleri")











