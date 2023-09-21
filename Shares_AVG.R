library(tidyquant)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

getSymbols("ADS.DE", from = '2017-01-01',
           to = "2023-09-20",warnings = FALSE,
           auto.assign = TRUE)


ADS<-na.omit(data.frame(ADS.DE[,4])) 


for(i in 25:dim(ADS)[1])
{ADS[i,2]<-mean(ADS[(i-24):(i),1])}

for(i in 50:dim(ADS)[1])
{ADS[i,3]<-mean(ADS[(i-49):(i),1])}

for(i in 200:dim(ADS)[1])
{ADS[i,4]<-mean(ADS[(i-199):(i),1])}

names(ADS)<-c("Kurs", "GD25", "GD50", "GD200")

ADS<-na.omit(ADS)

matplot(ADS)


n<-dim(ADS)[1]
ADS$Empfehlung30_200<-""
ADS$Empfehlung30<-""
ADS$Empfehlung200<-""

for(i in 1:n){
        if(ADS$Kurs[i]>ADS$GD30[i] && ADS$Kurs[i]>ADS$GD200[i]){ADS$Empfehlung30_200[i]<-"Kauf"}
        if(ADS$Kurs[i]>ADS$GD30[i]){ADS$Empfehlung30[i]<-"Kauf"}
        if(ADS$Kurs[i]>ADS$GD200[i]){ADS$Empfehlung200[i]<-"Kauf"}
}