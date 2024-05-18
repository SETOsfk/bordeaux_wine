library(dplyr)
library(ggplot2)
library(caret)
library(RANN)
library(tictoc)
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
    Score <=89  ~0,
    Score >=90 ~1,)) %>% 
  select(Diagnose,Score,Name,Year,Price,everything())
wine_main$Diagnose<-as.factor(wine_main$Diagnose)
wine_main<- wine_main %>% select(-Year,-Name,-Price,-Score) #olduğunca sade bir subset elde etmek amacıyla
#skora etkisi olmayan değişkenler veri setinden atılıyor.
str(wine_main$Diagnose)


wine_0<- wine_main %>% filter(Diagnose==0)#0 ve 
wine_1<- wine_main %>% filter(Diagnose==1)#1 ler ayrı veri setine alınıyor



# Convert the data frame to a tibble for easier manipulation with dplyr
#dplyr ile daha kolay bir şekilde manipüle edilmesi için veri seti tibblea dönüştürüldü.
data_tbl_0 <- as_tibble(wine_0)
data_tbl_1 <- as_tibble(wine_1)
data_tbl<-as_tibble(wine_main)

num_ones_tbl <- data_tbl %>%
  summarise(across(everything(), ~ sum(. == 1))) #0 ve 1ler iiçeren veri setinde
#her sütun için kaç defa birlerin gözüktüğünü hesaplatan kod

# Print the result
num_ones_tbl<-t(num_ones_tbl)

# Calculate the number of 1s in each column
num_ones_tbl_1 <- data_tbl_1 %>%
  summarise(across(everything(), ~ sum(. == 1)))
#90+ skora sahip verilerde her sütun için kaç defa bir kullanıldığını gösteriyor
#ne kadar fazla görülürse o kadar iyi böylelikle bir şarabın 90+
#skor almasını sağlayan kelimeleri çok daha net bir şekilde görebiliriz.

num_ones_tbl_1<-t(num_ones_tbl_1)

num_ones_tbl_0 <- data_tbl_0 %>%
  summarise(across(everything(), ~ sum(. == 1))) #aynısı sıfır için de yapılıyor

# Print the result
num_ones_tbl_0<-t(num_ones_tbl_0)


# Filter columns
filtered_wine_main_1 <- data_tbl_1 %>%
  select(where(~ sum(. == 1) > 305))
filtered_wine_main_1<-as.data.frame(filtered_wine_main_1) #1ler için ilk elli sütun seçiliyor.

filtered_wine_main <- data_tbl %>%
  select(where(~ sum(. == 1) > 690))
filtered_wine_main<-as.data.frame(filtered_wine_main)#genel veri seti için ilk elli sütun seçiliyor.


tictoc::tic()
model_knn = train(Diagnose ~ ., data=filtered_wine_main, method='knn')
tictoc::toc()
model_knn
model_knn$results
plot(model_knn, main="Model Dogruluk Orani")












