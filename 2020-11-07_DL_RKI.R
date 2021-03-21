library("readr")
library(shiny)
library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(magrittr)
library(tidyr)
library(forcats)
#rki_csv<-read_csv("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv")

download.file("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv","/opt/shiny-server/samples/sample-apps/corona/www/rki.csv")
rki_csv<-read_csv("/opt/shiny-server/samples/sample-apps/corona/www/rki.csv")
bev<-readRDS("/opt/shiny-server/samples/sample-apps/corona/www/bev_lk.rds")

###für experimente lokal
# names(rki_csv)
# glimpse(rki_csv)
# bev<-readRDS("corona_2/www/bev_lk.rds")

input<-vector()
input$dauer_g<-20
input$dauer_t<-7
input$dauer_t_3<-10




agg<-rki_csv%>%
  ##Datumsformate setzen
  mutate(Meldedatum=ymd_hms(Meldedatum))%>%
  mutate(Date=as.Date(Meldedatum))%>%
  ##Übernahme ursprünglicher bennenung
  mutate(Anz_conf=AnzahlFall,Anz_dea=AnzahlTodesfall,Anz_rec=AnzahlGenesen)%>%
  #hinzufügen der Bevölkerungsdichte
  left_join(bev)%>%
  ##gruppieren nach Bundesländern
  group_by(Bundesland)%>%
  mutate(cfr_1=Anz_dea/Anz_conf)%>%
  mutate(cfr_2=Anz_dea/(Anz_dea+Anz_rec))%>%
  ###Datenvorverarbeitung: Absolute Änderungen ermitteln
  mutate(Weekday=factor(weekdays(Date),labels=c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag")))%>%
  arrange(Date)%>%
  mutate(Anz_dea_1=Anz_dea-lag(Anz_dea,default=0))%>%
  mutate(Anz_conf_1=Anz_conf-lag(Anz_conf,default=0))  %>%
  mutate(Anz_rec_1=Anz_rec-lag(Anz_rec,default=0))  %>%
  ### Kennzahlenvorverarbeitung
  mutate(anzahl_neu_tod_avd=lag(Anz_dea_1,input$dauer_g-input$dauer_t,default=0))%>%
  mutate(anzahl_neu_conf_avd=lag(Anz_conf_1,input$dauer_t,default=0))%>%
  mutate(anzahl_neu_conf_avd_3=lag(Anz_conf_1,input$dauer_t_3,default=0))%>%
  #  mutate(cfr_advanced=cumsum(anzahl_neu_tod_avd)/cumsum(anzahl_neu_tod_avd+Anz_rec_1))%>%
  mutate(cfr_advanced_2=cumsum(Anz_dea_1)/cumsum(anzahl_neu_conf_avd))%>%
  #  mutate(cfr_advanced_3=cumsum(Anz_dea_1)/cumsum(anzahl_neu_conf_avd_3))%>%
  mutate(est_quality=ifelse(cumsum(Anz_dea_1)>20,"mittel","gering"))%>%
  mutate(anz_krank=Anz_conf-Anz_dea-Anz_rec)%>%
  mutate(Anz_krank_1=anz_krank-lag(anz_krank,default=0))  %>%
  mutate(Verd_3d= log(8,base=Anz_conf/lag(Anz_conf,3)))%>%
  gather("estimator","Value",c("cfr_1","cfr_2","cfr_advanced_2"))

agg<-agg%>%
  group_by(Bundesland, Landkreis,Date,estimator,Einwohner)%>%
  summarise(Anz_conf=sum(Anz_conf),Anz_dea=sum(Anz_dea),Anz_rec=sum(Anz_rec),
            Anz_conf_1=sum(Anz_conf_1),Anz_dea_1=sum(Anz_dea_1),Anz_rec_1=sum(Anz_rec_1))
###add file with trends


##saveRDS(agg,"corona_2/www/rki.RDS")
saveRDS(agg,"/opt/shiny-server/samples/sample-apps/corona/www/rki.rds")
