

#Veri setini oluşturduk
wine<-BordeauxWines
str(wine) #Öncelikle karakter tiplerini görmek için veri setini inceliyoruz
#İlk dört sütun harici bütün değerlerin faktör olması gerekirken double olarak işaretlenmiş
#Bunun dönüşümü için ilk dört sütun atılıyor.
wine<-wine[,-1:-4]
wine<-as.factor(wine)

#Ardından double olan değişkenler faktöre dönüştürülüyor
wine<- wine %>% mutate_if(is.double,as.factor)
str(wine)
#Bazı faktörler sadece 0 olarak işaretlenmiş modeli eğitmede bir katkıda bulunmayacağını düşündüğüm için
#veri setinden çıkardım.
cols_to_remove <- sapply(wine, function(x) is.factor(x) && length(levels(x)) == 1)
wine_filtered<-wine[,!cols_to_remove]
#Hepsi faktör ve 0,1 olmak üzre iki levele sahip
str(wine_filtered)
#Çıkarılan sütunlar da veri setine geri ekleniyor ve son olarak YIL değişkeninin sınıf dönüşümü yapılıyor.
birlestir<-BordeauxWines[,1:4]
wine_tamam<-data.frame(birlestir,wine_filtered)
wine_tamam$Year<-as.Date(wine_tamam$Year)
