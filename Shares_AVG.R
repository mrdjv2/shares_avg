library(tidyverse)



setwd("C:\\Users\\danie\\Documents\\shares_avg\\downloads\\")

pfad="C:\\Users\\danie\\Documents\\shares_avg\\downloads\\"

filenames <- list.files(path=, pattern=".csv", all.files=FALSE, full.names=FALSE)

i<-1
for(i in 1:1){
        
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
                geom_line(aes(y = shares$GD200, colour = "GD200"))
        
        print(p)
}



