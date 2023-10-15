library(tidyverse)

setwd("C:\\Users\\danie\\Documents\\shares_avg\\downloads\\")
pfad="C:\\Users\\danie\\Documents\\shares_avg\\downloads\\"

download_files<-function(Startdatum, Enddatum, Referenzdatum, Symbol){
        
        Start <- strptime(Startdatum, "%d.%m.%Y")
        Ende <- strptime(Enddatum, "%d.%m.%Y")
        Referenz <- strptime(Referenzdatum, "%d.%m.%Y")
        
        Beginn<-as.numeric(Start-Referenz,units="secs")
        Ende<-as.numeric(Ende-Referenz,units="secs")
        
        URL<- paste("https://query1.finance.yahoo.com/v7/finance/download/", Symbol, "?period1=", Beginn, "&period2=", Ende, "&interval=1d&events=history", sep = "")
        download.file(URL, paste(pfad, paste(Symbol, ".csv", sep=""), sep = ""), mode = "wb")
        
}


Startdatum <- "01.01.2022"
Enddatum <- "11.10.2023"
Referenzdatum<- "01.01.1970"

Symbol <- c("ADS.DE", "AIR.DE", "ALV.DE", "BAS.DE", "BAYN.DE", "BEI.DE", "BMW.DE",
            "BNR.DE", "CBK.DE", "CON.DE", "1COV.DE", "DTG.DE", "DBK.DE", "DB1.DE",
            "DHL.DE", "DTE.DE", "EOAN.DE", "FRE.DE", "HNR1.DE", "HEI.DE", "HEN3.DE",
            "IFX.DE", "MBG.DE", "MRK.DE", "MTX.DE", "MUV2.DE","P911.DE", "PAH3.DE",
            "QIA.DE", "RHM.DE", "RWE.DE", "SAP.DE", "SRT3.DE", "SIE.DE", "ENR.DE",
            "SHL.DE", "SY1.DE", "VOW3.DE", "VNA.DE", "ZAL.DE")

for(i in 1:(length(Symbol))){
        download_files(Startdatum, Enddatum, Referenzdatum, Symbol[i])
}

#download_files(Startdatum, Enddatum, Referenzdatum, Symbol)





filenames <- list.files(path=, pattern=".csv", all.files=FALSE, full.names=FALSE)

i<-1
for(i in 1:40){
        
        infile<-read.csv(filenames[i])
        shares<-data.frame(cbind(infile$Date, infile$Close))
        names(shares)<-c("Date", "Close")
        
        shares$Close<-as.numeric(shares$Close)
        
        shares<-na.omit(shares)
        
        for(j in 25:dim(shares)[1])
        {shares[j,3]<-mean(shares[(j-24):(j),2])}
        
        for(j in 50:dim(shares)[1])
        {shares[j,4]<-mean(shares[(j-49):(j),2])}
        
        for(j in 200:dim(shares)[1])
        {shares[j,5]<-mean(shares[(j-199):(j),2])
        shares[j,6]<-j-199}
        
        names(shares)<-c("Date", "Kurs", "GD25", "GD50", "GD200", "Laufindex")
        shares<-na.omit(shares)
        
        #shares<-shares[4000:5865,]
        #shares$Laufindex<-c(1:(dim(shares)[1]))
        row.names(shares)<-c(1:(dim(shares)[1]))

        
        p= ggplot(data = shares, aes(x = shares$Laufindex)) +
                geom_line(aes(y = shares$Kurs, colour = "Kurs")) +
                geom_line(aes(y = shares$GD25, colour = "GD25")) +
                geom_line(aes(y = shares$GD50, colour = "GD50")) +
                geom_line(aes(y = shares$GD200, colour = "GD200"))+
                ggtitle(filenames[i])
        
        print(p)
}



