# import libraries
#library(tidyverse)
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
dat<-dat
#remove umlauts from colnames
colnames(dat)<-sub("ä", "ae", colnames(dat), ignore.case = T)
colnames(dat)<-sub("ü", "ue", colnames(dat), ignore.case = T)
colnames(dat)<-sub("ö", "oe", colnames(dat), ignore.case = T)
colnames(dat)<-sub(".", "", colnames(dat), fixed=T)
colnames(dat)<-sub("..", "_", colnames(dat), fixed=T)
# long format with first three vars staying
dat2<-reshape2::melt(dat, id.vars=1:3)
#Regional vars subsetted
datreg<-droplevels(subset(dat2, variable%in%c("Kanton_Zuerich_Ja", "Kanton_Zuerich_Nein", "Laendlich", "Staedtisch", "DCH", "FCH", "ICH")))
#pasting variable name from tages , Beschreibung (rad/dist) Typ (mean/median)
datreg$variable2<-with(datreg, tolower(paste("tages", Beschreibung, Typ, sep="_")))
#spatial reference in location
datreg$location<-datreg$variable

# data values for the whole of switzerland, socially segmented
datch<-droplevels(subset(dat2, variable%in%c("Alter_1529","Alter_3064", "Alter_6579", "Maennlich", "Weiblich", "Erwerbstaetig", "In_Ausbildung", "Nicht_Erwerbstaetig", "Abotyp_GA", "Abotyp_HalbtaxStrecken..Verbundabo", "Abotyp_kein_Abo", 
                                             "Auto_Ja", "Auto_Nein", "Haushaltsgroesse_1_Person", "Haushaltsgroesse_2_Personen", 
                                             "Haushaltsgroesse_3_Personen", "Kinder_Ja", "Kinder_Nein")))
datch$variable2<-with(datch, tolower(paste("tages", Beschreibung, Typ, variable, sep="_")))
datch$location<-"CH"

#values for all of switzerland, without social segments
dattot<-droplevels(subset(dat2, variable%in%c("Total")))
dattot$variable2<-with(dattot, tolower(paste("tages", Beschreibung, Typ, sep="_")))
dattot$location<-"CH"
#bind them all together
datall<-rbind(datreg, datch, dattot)
datall$unit<-"km"

levels(datall$location)<-c("ZH", "CH ohne ZH", "CH: Städtischer Raum", "CH: Ländlicher Raum", "Deutschschweiz", "Romandie", "italienische Schweiz", "CH")

#Long and german variablenames merge
codvars<-read.csv("coding_vars_intervista.csv",  fileEncoding = "UTF-8")
datall<-merge(datall, codvars, by.x="variable2", by.y="var", all.x=T)

#Create the first part of the dataset

intervista<-data.frame(date=as.Date(datall$Datum),
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

#Only distances not radii
mobility_intervista<-droplevels(subset(intervista, grepl("distanz", intervista$variable_short)==T))
#CH ohne ZH remove
mobility_intervista<-droplevels(subset(mobility_intervista, location!="CH ohne ZH"))
mobility_intervista<-mobility_intervista[order(mobility_intervista$date),]


# Distanzkategorien----
dat <-  read.csv("./Download/Distanzkategorien_in_Prozent_pro_Tag.csv", header=T, sep=",", stringsAsFactors=FALSE, encoding="ANSI_X3.4-1986") 
#remove umlauts from colnames
colnames(dat)<-sub("ä", "ae", colnames(dat), ignore.case = T)
colnames(dat)<-sub("ü", "ue", colnames(dat), ignore.case = T)
colnames(dat)<-sub("ö", "oe", colnames(dat), ignore.case = T)
colnames(dat)<-sub(".", "", colnames(dat), fixed=T)
# long format with first three vars staying
dat2<-reshape2::melt(dat, id.vars=1:3)
#Regional vars subsetted
dat2<-droplevels(subset(dat2, Beschreibung=="Distanz"))
dat2$Auspraegung<-factor(dat2$Auspraegung, labels=c("10-20km", "100pluskm", "2-10km", 
                                                    "20-50km", "50-100km", "weniger2km"))

datreg<-droplevels(subset(dat2, variable%in%c("Kanton_Zuerich_Ja", "Kanton_Zuerich_Nein", "Laendlich", "Staedtisch")))
#pasting variable name from tages , Beschreibung (rad/dist) Typ (mean/median)

datreg$variable2<-with(datreg, tolower(paste("tages", Beschreibung, Auspraegung, sep="_")))
#spatial reference in location
datreg$location=datreg$variable
# data values for the whole of switzerland, socially segmented

datch<-droplevels(subset(dat2, variable%in%c("Alter_1529","Alter_3064", "Alter_6579", "Maennlich", "Weiblich", "Erwerbstaetig", "In_Ausbildung", "Nicht_Erwerbstaetig")))
datch$variable2<-with(datch, tolower(paste("tages", Beschreibung, Auspraegung, variable, sep="_")))
datch$location<-"CH"
#recode location

#values for all of switzerland, without social segments
dattot<-droplevels(subset(dat2, variable%in%c("Total")))
dattot$variable2<-with(dattot, tolower(paste("tages", Beschreibung, Auspraegung, sep="_")))
dattot$location<-"CH"
#bind them all together
datall<-rbind(datreg, datch, dattot)
datall$unit<-"Anteil in %"

levels(datall$location)<-c("ZH", "CH ohne ZH", "CH: Städtischer Raum", "CH: Ländlicher Raum", "CH")

#Long and german variablenames merge
codvars<-read.csv("distkat_intervista.csv",  fileEncoding = "UTF-8")
datall<-merge(datall, codvars, by.x="variable2", by.y="variable", all.x=T)

#Create the final dataset

intervista_distkat<-data.frame(date=as.Date(datall$Datum),
                       value=datall$value,
                       topic="Mobilität",
                       variable_short=datall$variable2,
                       variable_long=datall$variable_long,
                       location=datall$location,
                       unit="Anteil in %",
                       source="intervista Tracking-panel",
                       update="täglich",
                       public="ja",
                       description="https://github.com/statistikZH/covid19monitoring_mobility_intervista")

intervista_distkat<-droplevels(subset(intervista_distkat, grepl("alter", intervista_distkat$variable_short)==T | nchar(as.character(intervista_distkat$variable_short))<25))
intervista_distkat<-droplevels(subset(intervista_distkat, location=="CH"))
intervista_distkat<-intervista_distkat[order(intervista_distkat$date),]

##
#Modal und zwecksplit

# import data
zweck <-  read.csv("./Download/Mobilit„tszweck_pro_Tag.csv", header=T, sep=",", stringsAsFactors=FALSE, encoding="ANSI_X3.4-1986") 
names(zweck)<-c("date", "total", "tages_distanz_mittelwert_zweck_pendeln", "tages_distanz_mittelwert_zweck_freizeit", "tages_distanz_mittelwert_zweck_einkauf", "tages_distanz_mittelwert_zweck_andere")
zweck<-melt(zweck, id.vars="date")


mode <-  read.csv("./Download/Modalsplit_pro_Tag.csv", header=T, sep=",", stringsAsFactors=FALSE, encoding="ANSI_X3.4-1986") 
names(mode)<-c("date", "total", "tages_distanz_mittelwert_modus_miv", "tages_distanz_mittelwert_modus_oev", "tages_distanz_mittelwert_modus_fuss", "tages_distanz_mittelwert_modus_andere_vkm")

mode<-melt(mode, id.vars="date")

zwmod<-rbind(zweck, mode)
zwmod<-droplevels(subset(zwmod, variable!="total"))
zwmodvars<-read.csv("zweck_mod_vars.csv",  fileEncoding = "UTF-8")
zwmod<-merge(zwmod, zwmodvars, by.x="variable", by.y="var", all.x=T)

intervista_zwmod<-data.frame(date=as.Date(zwmod$date),
                               value=zwmod$value,
                               topic="Mobilität",
                               variable_short=zwmod$variable,
                               variable_long=zwmod$variable_long,
                               location="CH",
                               unit="km",
                               source="intervista Tracking-panel",
                               update="täglich",
                               public="ja",
                               description="https://github.com/statistikZH/covid19monitoring_mobility_intervista")

#write the final file for publication

mobility_intervista<-rbind(mobility_intervista, intervista_distkat, intervista_zwmod)


write.table(mobility_intervista, "Mobility_intervista.csv", sep=",", fileEncoding="UTF-8", row.names = F)

range(mobility_intervista$date)

