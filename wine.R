library(dplyr)
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

Grouped_Score <- c(1:14349)
wine_main<-data.frame(wine_main,Grouped_Score)
wine_main <- wine_main %>%
  mutate(Grouped_Score = case_when(
    Score >= 50 & Score <= 74  ~ "Tavsiye Edilmiyor",
    Score >= 75 & Score <= 79  ~ "Vasat",
    Score >= 80 & Score <= 84  ~ "İyi",
    Score >= 85 & Score <= 89 ~"Çok İyi",
    Score >= 90 & Score <= 94 ~"Seçkin",
    Score >= 95 & Score <= 100 ~ "Klasik")) %>% 
    select(1:3,Grouped_Score,Price,everything())

# Assuming 'data' is your DataFrame and 'Category' needs ordering
wine_main$Grouped_Score <- factor(wine_main$Grouped_Score, 
                        levels = c("Tavsiye Edilmiyor", "Vasat", "İyi","Çok İyi","Seçkin", "Klasik"), 
                        ordered = TRUE)
str(wine_main$Grouped_Score)
wine_main<- wine_main %>% group_by(Grouped_Score)
wine_main %>% count()






