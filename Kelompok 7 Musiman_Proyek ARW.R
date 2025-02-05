#Persiapan#

#1 Mengaktifkan package
library(fpp)
library(dplyr)
library(ggplot2)
library(TSA)
library(portes)

#2 Membaca Data
datakurs <- read.csv("Data Historis USD_IDR.csv")
glimpse(datakurs)

#3 Mengganti nama kolom
colnames(datakurs)[1] <- "Tanggal"
colnames(datakurs)[2] <- "Nilai Tukar"

#4 Memisahkan data
kurs <- (datakurs[c(1,2)])

#5 Mengatur format tanggal
kurs$Tanggal <- strptime(as.character(kurs$Tanggal), "%d/%m/%Y")
format(kurs$Tanggal, "%Y-%m-%d")
kurs$Tanggal <- as.Date(kurs$Tanggal)
View(kurs)

#6 Deskripsi data
summary(kurs)

#Pemodelan
#0 Mengubah data ke bentuk time series
kurs.ts <- ts(kurs$`Nilai Tukar`,start = c(2018,1,1),frequency = 262)
kurs2018_2019 <- window(kurs.ts,start= c(2018,1),end = c(2019,262))

#1 Plot Data
autoplot(kurs2018_2019,color=c("#00868B"),size=1)+
  labs (x="Tahun",
        title = "Nilai Tukar US Dollar terhadap Rupiah",
        subtitle = "Periode: 01 Januari 2018 - 30 Desember 2020")

#2 Spesifikasi Model
acf(kurs2018_2019)
pacf(kurs2018_2019)
eacf(kurs2018_2019)

#3 Uji Stasioner
BoxCox.lambda(kurs2018_2019)
adf.test(kurs2018_2019)

#3a Menstasionerkan data dengan diff=1
kurs.ts.s <- diff(kurs2018_2019, differences = 1)
autoplot(kurs.ts.s,color=c("#00868B"),size=1)+
  labs (x="Tahun",
        title = "Nilai Tukar US Dollar terhadap Rupiah",
        subtitle = "Periode: 01 Januari 2018 - 30 Desember 2020")
acf(kurs.ts.s)
pacf(kurs.ts.s)
eacf(kurs.ts.s)
BoxCox.lambda(kurs.ts.s)
adf.test(kurs.ts.s)

#3b Menstasionerkan data dengan diff=2
kurs.ts.s2 <- diff(kurs2018_2019,differences = 2)
autoplot(kurs.ts.s2,color=c("#00868B"),size=1)+
  labs (x="Tahun",
        title = "Nilai Tukar US Dollar terhadap Rupiah",
        subtitle = "Periode: 01 Januari 2018 - 30 Desember 2020")
acf(kurs.ts.s2)
pacf(kurs.ts.s2)
eacf(kurs.ts.s2)
BoxCox.lambda(kurs.ts.s2)
adf.test(kurs.ts.s2)

#4 Penaksiran model
#diff=1
model <- auto.arima(kurs.ts.s)
model
#diff=2
model2 <- auto.arima(kurs.ts.s2)
model2

#5 Diagnostik Model
#diff=1
LjungBox(residuals(auto.arima(kurs.ts.s)),lags=seq(1,5),order=0,squared.residuals=FALSE)
tsdisplay(residuals(model),main='ARIMA(0,0,1) Model Residuals')

#diff=2
LjungBox(residuals(auto.arima(kurs.ts.s2)),lags=seq(1,5),order=0,squared.residuals=FALSE)
tsdisplay(residuals(model2),main='ARIMA(2,0,0) Model Residuals')

#6 Peramalan Model dengan ARIMA Box-Jenkins
#diff=1
kurs2020 <- window(kurs.ts,start = c(2020,1),end = c(2020,241))
arima_kurs<-Arima(y=kurs2018_2019,order = c(0,0,1))
ramalan_2020<-forecast(arima_kurs,241)
kurs2018_2019 %>% autoplot(series="train test")+
  autolayer(kurs2020,series = "test data")+
  autolayer(ramalan_2020,series = "forecast")

#diff=2
arima_kurs2<-Arima(y=kurs2018_2019,order = c(2,0,0))
ramalan2_2020<-forecast(arima_kurs2,241)
kurs2018_2019 %>% autoplot(series="train test")+
  autolayer(kurs2020,series = "test data")+
  autolayer(ramalan2_2020,series = "forecast")

#6a Pemilihan Model Terbaik
accuracy(ramalan_2020$mean,kurs2020)
accuracy(ramalan2_2020$mean,kurs2020)