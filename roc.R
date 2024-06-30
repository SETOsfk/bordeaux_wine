library(caret)
library(randomForest)
library(dplyr)
library(tidyverse)
library(ROSE)
library(ada)
library(pROC)

#Creating the dataset
wine<-read_csv("/kaggle/input/bordeaux-wine-reviews-from-2000-to-2016/BordeauxWines.csv")
#To seperate first four columns so we can do the transformation.
merge<-wine[,1:4]
wine<-wine[,-1:-4]
#Transforming double's to factors
wine<- wine %>% mutate_if(is.double,as.factor)

#To remove which only has one level.
cols_to_remove <- sapply(wine, function(x) is.factor(x) && length(levels(x)) == 1)
wine_main<-wine[,!cols_to_remove]

#Creating a column called "Diagnose" for classes                         
Diagnose <- c(1:14349)
wine_main<-data.frame(wine_main,Diagnose,merge)
wine_main$Diagnose
wine_main <- wine_main %>%
  mutate(Diagnose = case_when(
    wine_main$Score >=90 ~"X1",
  TRUE ~"X0")) %>% 
  select(Diagnose,Score,Wine,Year,Price,everything())
wine_main$Diagnose<-as.factor(wine_main$Diagnose)
wine_main<- wine_main %>% select(-Year,-Wine,-Price,-Score)


#Random forest for feature selection.
rf_model <- randomForest(Diagnose ~ ., data = wine_main, importance = TRUE, ntree = 100)
importance_scores <- importance(rf_model, type = 1)

if (is.matrix(importance_scores)) {
  importance_values <- importance_scores[, 1] # Change 1 if another column is needed
} else {
  importance_values <- importance_scores
}


sorted_importance <- sort(importance_values, decreasing = TRUE)
important_features <- names(sorted_importance)[1:50]

X_reduced <- wine_main[,c(important_features)]
Diagnose<-wine_main$Diagnose
X_reduced<- data.frame(Diagnose,X_reduced)
trainIndex <- createDataPartition(X_reduced$Diagnose, p = .7, 
                                  list = FALSE, 
                                  times = 1)
train_data <- X_reduced[ trainIndex,]
test_data  <- X_reduced[-trainIndex,]

barplot(prop.table(table(X_reduced$Diagnose)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Ratios")

bothq <- ovun.sample(Diagnose~., data = train_data, method = "both")
train_data<-bothq$data

barplot(prop.table(table( train_data$Diagnose)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Ratios")

train_control <- trainControl(
  method = "cv",       # Cross-validation
  number = 5,          # 5-fold cross-validation
  classProbs = TRUE,   # Compute class probabilities
  summaryFunction = twoClassSummary # Summary function for classification
)                         

logistic_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "glm", 
  family = binomial, 
  trControl = train_control, 
  metric = "ROC"
)
# Random Forest
rf_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "rf", 
  trControl = train_control, 
  metric = "ROC"
)

# GBM
gbm_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "gbm", 
  trControl = train_control, 
  verbose = FALSE,
  metric = "ROC"
)

# SVM
svm_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "svmRadial", 
  trControl = train_control, 
  metric = "ROC"
)

# AdaBoost
ada_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "ada", 
  trControl = train_control, 
  metric = "ROC"
)


# CART
cart_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "rpart", 
  trControl = train_control,
    metric = "ROC"
)

# LDA
lda_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "lda", 
  trControl = train_control, 
  metric = "ROC"
)

# 5. Resample Comparison
results <- resamples(list(
  LOG = logistic_model, 
  RF = rf_model, 
  GBM = gbm_model, 
  SVM = svm_model,
  ADAB = ada_model, 
  CART = cart_model,
  LDA = lda_model
))

# 6. Summary of the results
summary(results)
dotplot(results)

# Random Forest
rf_preds <- predict(rf_model, newdata = test_data)
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")
rf_roc <- roc(response = test_data$Diagnose, predictor = rf_probs[,2])
print(auc(rf_roc))
# Random Forest
rf_preds <- predict(rf_model, newdata = test_data)
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")
rf_roc <- roc(response = test_data$Diagnose, predictor = rf_probs[,2])
print(auc(rf_roc))

#GBM
gbm_preds <- predict(gbm_model, newdata = test_data)
gbm_probs <- predict(gbm_model, newdata = test_data, type = "prob")
gbm_roc <- roc(response = test_data$Diagnose, predictor = rf_probs[,2])
print(auc(gbm_roc))

# Confusion Matrices
print(confusionMatrix(rf_preds, test_data$Diagnose))
print(confusionMatrix(svm_preds, test_data$Diagnose))
print(confusionMatrix(gbm_preds, test_data$Diagnose))


#Necessary packages.
library(caret)
library(randomForest)
library(dplyr)
library(tidyverse)
library(ROSE)
library(ada)
library(pROC)
Loading required package: ggplot2

Loading required package: lattice


Attaching package: ‘caret’


The following object is masked from ‘package:httr’:

    progress


randomForest 4.6-14

Type rfNews() to see new features/changes/bug fixes.


Attaching package: ‘randomForest’


The following object is masked from ‘package:ggplot2’:

    margin



Attaching package: ‘dplyr’


The following object is masked from ‘package:randomForest’:

    combine


The following objects are masked from ‘package:stats’:

    filter, lag


The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union


── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
✔ forcats   1.0.0     ✔ stringr   1.5.1
✔ lubridate 1.9.3     ✔ tibble    3.2.1
✔ purrr     1.0.2     ✔ tidyr     1.3.0
✔ readr     2.1.4     
── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::combine()       masks randomForest::combine()
✖ dplyr::filter()        masks stats::filter()
✖ dplyr::lag()           masks stats::lag()
✖ purrr::lift()          masks caret::lift()
✖ randomForest::margin() masks ggplot2::margin()
✖ caret::progress()      masks httr::progress()
ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
Loaded ROSE 0.0-4


Loading required package: rpart

Type 'citation("pROC")' for a citation.


Attaching package: ‘pROC’


The following objects are masked from ‘package:stats’:

    cov, smooth, var


#Creating the dataset
wine<-read_csv("/kaggle/input/bordeaux-wine-reviews-from-2000-to-2016/BordeauxWines.csv")
#To seperate first four columns so we can do the transformation.
merge<-wine[,1:4]
wine<-wine[,-1:-4]
#Transforming double's to factors
wine<- wine %>% mutate_if(is.double,as.factor)
Rows: 14349 Columns: 989
── Column specification ────────────────────────────────────────────────────────
Delimiter: ","
chr   (2): Wine, Price
dbl (987): Year, Score, BLOOD ORANGE, CITRUS, CITRUS PEEL, CITRUS ZEST, CLEM...

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
First we need to check data types for better analysis. All of the columns except first four column coded as double but they need to be two levelled factors.Some factors only has one level which is "0". This creates zero variance so that is not gonna effect the model because of that, we will remove them. What we are looking for is what makes a wine good. So we will group scores as 1 and 0 which means 90+ score wines and 90- score wines. To do this we will create a column called "Diagnose"

#To remove which only has one level.
cols_to_remove <- sapply(wine, function(x) is.factor(x) && length(levels(x)) == 1)
wine_main<-wine[,!cols_to_remove]

#Creating a column called "Diagnose" for classes                         
Diagnose <- c(1:14349)
wine_main<-data.frame(wine_main,Diagnose,merge)
wine_main$Diagnose
wine_main <- wine_main %>%
  mutate(Diagnose = case_when(
    wine_main$Score >=90 ~"X1",
  TRUE ~"X0")) %>% 
  select(Diagnose,Score,Wine,Year,Price,everything())
wine_main$Diagnose<-as.factor(wine_main$Diagnose)
wine_main<- wine_main %>% select(-Year,-Wine,-Price,-Score)
123456789101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899100101102103104105106107108109110111112113114115116117118119120121122123124125126127128129130131132133134135136137138139140141142143144145146147148149150151152153154155156157158159160161162163164165166167168169170171172173174175176177178179180181182183184185186187188189190191192193194195196197198199200⋯1415014151141521415314154141551415614157141581415914160141611416214163141641416514166141671416814169141701417114172141731417414175141761417714178141791418014181141821418314184141851418614187141881418914190141911419214193141941419514196141971419814199142001420114202142031420414205142061420714208142091421014211142121421314214142151421614217142181421914220142211422214223142241422514226142271422814229142301423114232142331423414235142361423714238142391424014241142421424314244142451424614247142481424914250142511425214253142541425514256142571425814259142601426114262142631426414265142661426714268142691427014271142721427314274142751427614277142781427914280142811428214283142841428514286142871428814289142901429114292142931429414295142961429714298142991430014301143021430314304143051430614307143081430914310143111431214313143141431514316143171431814319143201432114322143231432414325143261432714328143291433014331143321433314334143351433614337143381433914340143411434214343143441434514346143471434814349
Dataset has lots of columns which might cause an overfitting problem or lots of computational time. We need to reduce column size which known as "dimension reduction". We will use RFI for this. Basicly it's gonna find the most effective columns, we can use other methods such as NZV (near zero variance). I've tried NZV it gives the same result with RFI.

#Random forest for feature selection.
rf_model <- randomForest(Diagnose ~ ., data = wine_main, importance = TRUE, ntree = 100)
importance_scores <- importance(rf_model, type = 1)

if (is.matrix(importance_scores)) {
  importance_values <- importance_scores[, 1] # Change 1 if another column is needed
} else {
  importance_values <- importance_scores
}


sorted_importance <- sort(importance_values, decreasing = TRUE)
important_features <- names(sorted_importance)[1:50]

X_reduced <- wine_main[,c(important_features)]
Diagnose<-wine_main$Diagnose
X_reduced<- data.frame(Diagnose,X_reduced)
trainIndex <- createDataPartition(X_reduced$Diagnose, p = .7, 
                                  list = FALSE, 
                                  times = 1)
train_data <- X_reduced[ trainIndex,]
test_data  <- X_reduced[-trainIndex,]
We'll split dataset %70 training and %30 test. But with createDataPartion function we will easily split dataset according to class ratio in "Diagnose" column which is our key column. So basicly, in main dataset diagnose column has two levels which is 1 and 0. Ratio between em is 0.7 "1" class and 0.3 "1" class so we will keep this ratio in our training and test datasets too.

We have unbalanced classes which may cause low sensitiviy. To solve this we will use "both" method from "ROSE" package.

barplot(prop.table(table(X_reduced$Diagnose)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Ratios")
No description has been provided for this image
bothq <- ovun.sample(Diagnose~., data = train_data, method = "both")
train_data<-bothq$data

barplot(prop.table(table( train_data$Diagnose)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Ratios")
No description has been provided for this image
What "both" does is; method combines these two approaches by simultaneously over-sampling the minority class and under-sampling the majority class. This balanced approach aims to mitigate the disadvantages of both methods when used independently.

train_control <- trainControl(
  method = "cv",       # Cross-validation
  number = 5,          # 5-fold cross-validation
  classProbs = TRUE,   # Compute class probabilities
  summaryFunction = twoClassSummary # Summary function for classification
)
#Logistic Regression
logistic_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "glm", 
  family = binomial, 
  trControl = train_control, 
  metric = "ROC"
)
# Random Forest
rf_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "rf", 
  trControl = train_control, 
  metric = "ROC"
)

# GBM
gbm_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "gbm", 
  trControl = train_control, 
  verbose = FALSE,
  metric = "ROC"
)

# SVM
svm_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "svmRadial", 
  trControl = train_control, 
  metric = "ROC"
)

# AdaBoost
ada_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "ada", 
  trControl = train_control, 
  metric = "ROC"
)


# CART
cart_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "rpart", 
  trControl = train_control,
    metric = "ROC"
)

# LDA
lda_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "lda", 
  trControl = train_control, 
  metric = "ROC"
)
line search fails -1.917239 0.005370928 1.244651e-05 -6.50073e-06 -1.8522e-08 9.329662e-09 -2.911839e-13
Warning message in method$predict(modelFit = modelFit, newdata = newdata, submodels = param):
“kernlab class prediction calculations failed; returning NAs”
Warning message in method$prob(modelFit = modelFit, newdata = newdata, submodels = param):
“kernlab class probability calculations failed; returning NAs”
Warning message in data.frame(..., check.names = FALSE):
“row names were found from a short variable and have been discarded”
line search fails -1.951916 0.03288434 1.277875e-05 -6.873433e-06 -1.992422e-08 1.017287e-08 -3.245293e-13
Warning message in method$predict(modelFit = modelFit, newdata = newdata, submodels = param):
“kernlab class prediction calculations failed; returning NAs”
Warning message in method$prob(modelFit = modelFit, newdata = newdata, submodels = param):
“kernlab class probability calculations failed; returning NAs”
Warning message in data.frame(..., check.names = FALSE):
“row names were found from a short variable and have been discarded”
line search fails -1.876663 -0.05272459 1.133491e-05 -5.538233e-06 -1.540632e-08 7.216411e-09 -2.145954e-13
Warning message in method$predict(modelFit = modelFit, newdata = newdata, submodels = param):
“kernlab class prediction calculations failed; returning NAs”
Warning message in method$prob(modelFit = modelFit, newdata = newdata, submodels = param):
“kernlab class probability calculations failed; returning NAs”
Warning message in data.frame(..., check.names = FALSE):
“row names were found from a short variable and have been discarded”
line search fails -1.948997 0.01694277 2.051767e-05 -1.153854e-05 -3.071045e-08 1.672382e-08 -8.230753e-13
Warning message in method$predict(modelFit = modelFit, newdata = newdata, submodels = param):
“kernlab class prediction calculations failed; returning NAs”
Warning message in method$prob(modelFit = modelFit, newdata = newdata, submodels = param):
“kernlab class probability calculations failed; returning NAs”
Warning message in data.frame(..., check.names = FALSE):
“row names were found from a short variable and have been discarded”
Warning message in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo, :
“There were missing values in resampled performance measures.”
# 5. Resample Comparison
results <- resamples(list(
  LOG = logistic_model, 
  RF = rf_model, 
  GBM = gbm_model, 
  SVM = svm_model,
  ADAB = ada_model, 
  CART = cart_model,
  LDA = lda_model
))

# 6. Summary of the results
summary(results)
dotplot(results)
Call:
summary.resamples(object = results)

Models: LOG, RF, GBM, SVM, ADAB, CART, LDA 
Number of resamples: 5 

ROC 
          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
LOG  0.9090527 0.9114828 0.9115026 0.9144897 0.9195272 0.9208835    0
RF   0.9208736 0.9219875 0.9242501 0.9252130 0.9268115 0.9321424    0
GBM  0.9050904 0.9133316 0.9148965 0.9151222 0.9152632 0.9270295    0
SVM  0.9169237 0.9169980 0.9170723 0.9189410 0.9199496 0.9228269    2
ADAB 0.8816706 0.8877027 0.8888276 0.8896599 0.8932728 0.8968259    0
CART 0.7736844 0.7799290 0.7814261 0.7883699 0.8007964 0.8060134    0
LDA  0.9028536 0.9034437 0.9064532 0.9094940 0.9164589 0.9182606    0

Sens 
          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
LOG  0.8424726 0.8454636 0.8525896 0.8528709 0.8554337 0.8683948    0
RF   0.8693918 0.8834661 0.8843470 0.8837720 0.8883350 0.8933200    0
GBM  0.8047809 0.8255234 0.8305085 0.8333390 0.8464606 0.8594217    0
SVM  0.8364905 0.8369890 0.8374875 0.8391492 0.8404786 0.8434696    2
ADAB 0.8215354 0.8315055 0.8325025 0.8377183 0.8426295 0.8604187    0
CART 0.8384845 0.8494516 0.8514457 0.8530689 0.8595618 0.8664008    0
LDA  0.8386454 0.8454636 0.8514457 0.8524748 0.8574277 0.8693918    0

Spec 
          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
LOG  0.8081511 0.8220676 0.8270378 0.8258449 0.8320080 0.8399602    0
RF   0.8439364 0.8538767 0.8598410 0.8592445 0.8628231 0.8757455    0
GBM  0.8170974 0.8349901 0.8369781 0.8337972 0.8379722 0.8419483    0
SVM  0.8648111 0.8712724 0.8777336 0.8737575 0.8782306 0.8787276    2
ADAB 0.7455268 0.7524851 0.7564612 0.7562624 0.7604374 0.7664016    0
CART 0.6113320 0.6252485 0.6282306 0.6463221 0.6779324 0.6888668    0
LDA  0.7862823 0.7932406 0.8031809 0.7986083 0.8031809 0.8071571    0
No description has been provided for this image
Results are fine but they may lead to misinformation, because we used "both" function to create and eliminate some data. To see the real results we will use test dataset.

# 7. Evaluate Models

# Random Forest
rf_preds <- predict(rf_model, newdata = test_data)
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")
rf_roc <- roc(response = test_data$Diagnose, predictor = rf_probs[,2])
print(auc(rf_roc))
Setting levels: control = X0, case = X1

Setting direction: controls < cases

Area under the curve: 0.8727
#SVM
svm_preds <- predict(svm_model, newdata = test_data)
svm_probs <- predict(svm_model, newdata = test_data, type = "prob")
svm_roc <- roc(response = test_data$Diagnose, predictor = rf_probs[,2])
print(auc(svm_roc))
Setting levels: control = X0, case = X1

Setting direction: controls < cases

Area under the curve: 0.8727
#GBM
gbm_preds <- predict(gbm_model, newdata = test_data)
gbm_probs <- predict(gbm_model, newdata = test_data, type = "prob")
gbm_roc <- roc(response = test_data$Diagnose, predictor = rf_probs[,2])
print(auc(gbm_roc))
Setting levels: control = X0, case = X1


# Confusion Matrices
print(confusionMatrix(rf_preds, test_data$Diagnose))
print(confusionMatrix(svm_preds, test_data$Diagnose))
print(confusionMatrix(gbm_preds, test_data$Diagnose))
Confusion Matrix and Statistics



                         
