data("AirPassengers")
AP<-AirPassengers
AP
class(AP)
start(AP); end(AP); frequency(AP)
plot(AP,ylab= "Passenger  (1000's)")
# Grafige bakildiginda zamanla yolcu sayisi artmistir
#Sezonsallik oldugu artan bir ivmeyle oldugu icin carpimsallik oldugu gorunuyor


layout(1:2)
plot(aggregate(AP))
boxplot(AP~ cycle(AP))
# plot a bakildiginda yıllara gore artan bir trend var
#boxplot a bakildiginda da sezonsallik etkisi gorunuyor

#Ders kodları

kings<- scan("https://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings

kingstimeseries<- ts(kings)
kingstimeseries
#Kralların ölüm yaslarina iliskin veri seti

plot.ts(kingstimeseries)


births<-scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

birthstimeseries<-ts(births,frequency = 12 , start=c(1946,1))
birthstimeseries
plot.ts(birthstimeseries)
#aylık doğum sayısında mevsimsellik var




install.packages("TTR")
library(TTR)

kingstimeseriesSMA8<- SMA(kingstimeseries, n=8)
plot.ts(kingstimeseriesSMA8)
#Grafik yorumu olarak ilk 20 kralda ölüm yaşı  azalmıs


birthstimeseriescomponents<- decompose(birthstimeseries)
birthstimeseriescomponents$seasonal
#mevsimsel bilesenin tahmini degerlerini elde ettik

plot(birthstimeseriescomponents)
#Observed (gözlenen seri) iyi ayrıstırıldıysa etkileri düzgün olarak görülmeli

birthstimeseriesseasonallyadjusted<- birthstimeseries-birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)

#Toplamsal oldugu icin cikarttik, carpimsal olsa bolecektik
#Mevsimsel etkiyi arindirdiktan sonra plotta mevsimsel etki gorulmemeli
#Eger mevsimsel etki gorulurse iyi ayristirma yapilmamistir


souvenir<-scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries<-ts(souvenir,frequency = 12, start=c(1987,1))

souvenirtimeseriescomponents<-decompose(souvenirtimeseries, type="multiplicative")
plot(souvenirtimeseriescomponents)
#Mevsimsel etkiyi ayristirinca dalga siddetlerinin esit olmasi beklenir

souvenirtimeseriesseasonallyadjusted<-souvenirtimeseries/souvenirtimeseriescomponents$seasonal
plot(souvenirtimeseriesseasonallyadjusted)
#Mevsimsel etkiden arindirilmis zaman serisi 
#Dogru bir ayrim yapmisiz, grafikte sadece trend ve random etki gorunuyor

#Airpassengers

AP<-AirPassengers
plot(AP)
plot(decompose(AP))
#Multiplicative ve sezonsallık var
DAP<- (decompose(AP, type="multiplicative"))
plot(DAP$seasonal)
APTrend<- window(DAP$trend, start=1950 , end=c(1958,1))
NewAP<-DAP$random*DAP$seasonal*DAP$trend
plot(NewAP)
lines(AP,col=4)
APdiff<-NewAP-AP
plot(APdiff)
#apdiff plotu  sifir etrafinda olmasi beklenir, sayilar cok kucuk cikmis
# son donemde bu fark artmis, son doneme agirlik veren bir yapi degil


rain<- scan ("http://robjhyndman.com/tsdldata/hurst/precip1.dat", skip=1)
rainseries<- ts(rain,start=c(1813))
plot.ts(rainseries)
#¦Trend yok, mevsimsellik yok, simple exponential smoothing uygulanabilir
#Simple exponential smoothing uygulamak icin beta ve gamma false yapilir

rainseriesforecasts<- HoltWinters(rainseries, beta=F, gamma=F)
rainseriesforecasts
#alpha degeri 0.024 verdi, bu sifira cok yakin oldugu icin tahminlerin son gozlemlere agirlik vermedigini gosterir

rainseriesforecasts$fitted
#y cap leri gostermek icin
rainseriesforecasts$SSE
#toplam hata karelerini verir, diger modellerle karsilastirmak icin gerekli

HoltWinters(rainseries, beta=F , gamma=F , l.start=23.56)
#baslangic degerini 23.56 aldik
install.packages("forecast")
library(forecast)
rainseriesforecast2<- forecast::: forecast.HoltWinters(rainseriesforecasts, h=8)
rainseriesforecast2
#önümüzdeki 8 dönem icin forecast yaptirdik
#1920 icin %80 güven düzeyinde yagis miktari 19.16 ile 30.19 arasindadir
#1920 icin %95 guven duzeyinde yagis miktari 16.24 ile 33.11 arasindadir

forecast:::plot.forecast(rainseriesforecast2)
#Koyu olan %80 lik, acik renk olan alan ise %95 güven düzeyindeki tahmin araligi


#Holt Trend Üssel Düzeltme Yöntemi

skirts<- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries<-ts(skirts,start=c(1866))
plot.ts(skirtsseries)
#Önce artış sonra azalış var


skirtsseriesforecasts<- HoltWinters(skirtsseries,gamma=F)
skirtsseriesforecasts
#Alfayı 0.83 almis, beta trendi gosterdigi icin false demedik
# alfa 1 e yakin oldugu icin son gozlemlere agirlik vermis

plot(skirtsseriesforecasts)

HoltWinters(skirtsseries,gamma=F, l.start=608, b.start=9)
skirtsseriesforecasts2<-forecast:::forecast.HoltWinters(skirtsseriesforecasts,h=19)
forecast:::plot.forecast(skirtsseriesforecasts2)
#Koyu olan %80 lik guveni, acik renk olan %95 lik tahmin araligini gosterir
#Nokta kestirimi mavi cizgi seklinde lineer artan bir sekilde yapmis

#Holtwinters Yöntemi

logsouvenirtimeseries<-log(souvenirtimeseries)
souvenirtimeseriesforecasts<-HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts
# alfası 0.41 cikmis, son gozlemlere az agirlik verildigi soylenebilir
#beta 0 cikmis, trendi az cikmis log aldigimiz icinde olabilir
#gamma 0.95 cikmis, mevsimsellik etkisi var

plot(souvenirtimeseriesforecasts)
souvenirtimeseriesforecasts2<-forecast:::forecast.HoltWinters(souvenirtimeseriesforecasts,h=48)
forecast:::plot.forecast(souvenirtimeseriesforecasts2)
#yine koyu renkliler %80, acik renkliler %95 lik tahmin araligini gosterir

install.packages("TSA")
library(TSA)
win.graph(width = 4.875, height = 2.5, pointsize=8)
data("larain"); plot(larain, ylab='Inches',xlab='Year', type='o')


win.graph(width = 3,height = 3,pointsize = 8)
plot(y=larain,x=zlag(larain),ylab='Inches',xlab='previous Year Inches')
# Sag en altta ki nokta 1883 deki 40 inchlik yagis miktarini gosteriyor
# 1884 de ise 12 inchlik bir yagis gozlenmis 

data(color)
plot(color,ylab='Color Property',xlab='Batch',type='o')
plot(y=color, x=zlag(color),ylab='Color Property',xlab='Previous Batch Color Property')
#Bu scatterplottan elde edilen sonuc, trend olması yonunde


data(hare); plot(hare,ylab='Abundance',xlab='Year',type='o')
plot(y=hare,x=zlag(hare),ylab='Abundance',
     xlab='Previous Year Abundance')

data(tempdub); plot(tempdub,ylab='Temperature',type='o')
data(oilfilters); plot(oilfilters,type='o',ylab='Sales')
# iki veride de sezonsallik oldugu goruluyor
plot(oilfilters,type='l',ylab='Sales')
points(y=oilfilters,x=time(oilfilters),
         pch=as.vector(season(oilfilters)))

data(rwalk) 
plot(rwalk,type='o',ylab='Random Walk')


data(AirPassengers)
AP <- AirPassengers
AP.decom <- decompose(AP, "multiplicative")
plot(ts(AP.decom$random[7:138]))
acf(AP.decom$random[7:138])

sd(AP[7:138])
sd(AP[7:138] - AP.decom$trend[7:138])   
sd(AP.decom$random[7:138])   
