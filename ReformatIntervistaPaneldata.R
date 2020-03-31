# import libraries
library(tidyverse)
library(reshape2)
options(scipen = 1000000)
options(digits = 6)


dir_name <- "./Download"
#dir.create(dir_name)

# Metadaten
downloader::download("https://www.intervista.ch/media/2020/03/Download_Mobilit%C3%A4ts-Monitoring_Covid-19.zip",
                     dest=paste0(dir_name,"/Download.zip"), 
                     mode="wb")
unzip(paste0(dir_name,"/Download.zip"), exdir = dir_name)
# import data
dat <-  read.csv("./Download/Mittelwerte_und_Median_pro_Tag.csv", header=T, sep=",", stringsAsFactors=FALSE, encoding="ANSI_X3.4-1986") 


#remove umlauts from colnames
colnames(dat)<-sub("ä", "ae", colnames(dat), ignore.case = T)
colnames(dat)<-sub("ü", "ue", colnames(dat), ignore.case = T)
colnames(dat)<-sub("ö", "oe", colnames(dat), ignore.case = T)
colnames(dat)<-sub(".", "", colnames(dat), fixed=T)
# long format with first three vars staying
dat2<-reshape2::melt(dat, id.vars=1:3)
#Regional vars subsetted
datreg<-droplevels(subset(dat2, variable%in%c("Kanton_Zuerich_Ja", "Kanton_Zuerich_Nein", "Laendlich", "Staedtisch", "Total")))
#pasting variable name from tages , Beschreibung (rad/dist) Typ (mean/median)


datreg$variable2<-with(datreg, tolower(paste("tages", Beschreibung, Typ, sep="_")))
#spatial reference in location
datreg$location=datreg$variable
# data values for the whole of switzerland, socially segmented

datch<-droplevels(subset(dat2, variable%in%c("Alter_1529","Alter_3064", "Alter_6579", "Maennlich", "Weiblich", "Erwerbstaetig", "In_Ausbildung", "Nicht_Erwerbstaetig")))
datch$variable2<-with(datch, tolower(paste("tages", Beschreibung, Typ, variable, sep="_")))
datch$location<-"CH"
#recode location

#values for all of switzerland, without social segments
dattot<-droplevels(subset(dat2, variable%in%c("Total")))
dattot$variable2<-with(dattot, tolower(paste("tages", Beschreibung, Typ, sep="_")))
dattot$location<-"CH"
#bind them all together
datall<-rbind(datreg, datch, dattot)
datall$unit<-"km"

datall$location<-recode_factor(datall$location, "Kanton_Zuerich_Ja" = "ZH",
                               "Kanton_Zuerich_Nein" = "CH ohne ZH",
                               "Staedtisch" = "CH: Städtischer Raum",
                               "Laendlich"  = "CH: Ländlicher Raum",
                               "CH" = "CH", 
                               "Total" = "CH")

#Long and german variablenames merge
codvars<-read.csv("coding_vars_intervista.csv",  fileEncoding = "UTF-8")
datall<-merge(datall, codvars, by.x="variable2", by.y="var", all.x=T)

#Create the final dataset

intervista<-data.frame(date=as.POSIXct(paste(datall$Datum, "00:00:00", sep=" ")),
                       value=datall$value,
                       topic="Mobilität",
                       variable_short=datall$variable2,
                       variable_long=datall$variable_long,
                       location=datall$location,
                       unit="km",
                       source="intervista Tracking-panel",
                       update="täglich",
                       public="ja",
                       description="https://github.com/statistikZH/covid19monitoring_mobility_intervista")

#only median values distances and without restschweiz for simplicity 
#!!! Mistakes in coding vars intervista where means are concerned!!!
mobility_intervista<-subset(intervista, grepl("median", intervista$variable_short)==T & grepl("distanz", intervista$variable_short)==T & location!="CH ohne ZH")

mobility_intervista<-mobility_intervista[order(mobility_intervista$date),]

#write the final file for publication
write.table(mobility_intervista, "Mobility_intervista.csv", sep=",", fileEncoding="UTF-8", row.names = F)



