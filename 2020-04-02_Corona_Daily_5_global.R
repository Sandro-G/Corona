library(tidyverse)
library(readxl)
library(lubridate)
library(purrr)
library(grid)
library(gridExtra)

setwd("C:/Users/Sandro/Documents/R/Skripte/Corona")
options(encoding = "UTF-8")
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
              destfile = "Input/jhu_confirmed.txt")
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
              destfile = "Input/jhu_death.txt")
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
              ,destfile = "Input/jhu_recoverd.txt")


jhu_conf<-read_delim("Input/jhu_confirmed.txt",delim=",")
jhu_death<-read_delim("Input/jhu_death.txt",delim=",")
jhu_rec<-read_delim("Input/jhu_recoverd.txt",delim=",")

jhu_conf_tidy<-jhu_conf%>%
  gather("Date","Anz_conf",-c(1:4))
jhu_death_tidy<-jhu_death%>%
  gather("Date","Anz_dea",-c(1:4))
jhu_rec_tidy<-jhu_rec%>%
  gather("Date","Anz_rec",-c(1:4))

binom.test(682, 682 + 243)

input<-vector()
input$dauer_g<-20
input$dauer_t<-7
input$dauer_t_3<-10

jhu_clean<-jhu_conf_tidy%>%
  left_join(jhu_death_tidy)%>%
  left_join(jhu_rec_tidy)%>%
  mutate(Date=mdy(Date))%>%  
  ##Zusammenführung einzelner Regionen zu Ländern
  group_by(`Country/Region`,Date)%>%
  summarise(Anz_conf=sum(Anz_conf),Anz_dea=sum(Anz_dea),Anz_rec=sum(Anz_rec))%>%
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

##Auwahl Liste mit Länder mit aktuell höchsten Anzahl bestätigter Fälle
list_top_c<-jhu_clean%>%  
  top_n(1,Date)%>%
  ungroup()%>%
  dplyr::filter(estimator=="cfr_1")%>%
  top_n(10,Anz_conf)%>%
  select(`Country/Region`)%>%
  unique()

###Plots für den PDF-Output
plot1<-jhu_clean%>%
    filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
    filter(estimator=="cfr_1")%>%
    gather("keze","Menschen",c("Anz_conf","Anz_dea","Anz_rec","anz_krank"))%>%
    ggplot(aes(x=Date,y=Menschen,col=keze))+  scale_color_manual("keze",values = c("blue","red","green","grey"), breaks = c("Anz_conf", "Anz_dea","Anz_rec","anz_krank"),labels = c("Bestätigt", "Gestorben","Genesen","Krank"))+
    geom_line()+facet_wrap(~`Country/Region`) +
    scale_y_continuous(labels=scales::comma)+
    ggtitle("Gesamtzahlen")


plot1a<-jhu_clean%>%
  filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
  filter(estimator=="cfr_1")%>%
  filter(!(`Country/Region`=="China"&Date==ymd("2020-02-13")))%>%
  gather("keze","Menschen",c("Anz_conf_1","Anz_dea_1","Anz_rec_1"))%>%
  ggplot(aes(x=Date,y=Menschen,col=keze))+  scale_color_manual("keze",values = c("blue","red","green"), breaks = c("Anz_conf_1", "Anz_dea_1","Anz_rec_1"),labels = c("Bestätigt", "Gestorben","Genesen"))+
  geom_line()+facet_wrap(~`Country/Region`,scales="free")+
  ggtitle("Tägliche Veränderung") 

plot1aa<-jhu_clean%>%
  filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
  filter(estimator=="cfr_1")%>%
  filter(!(`Country/Region`=="China"&Date==ymd("2020-02-13")))%>%
  gather("keze","Menschen",c("Anz_krank_1"))%>%
  ggplot(aes(x=Date,y=Menschen,col=keze))+  scale_color_manual("keze",values = c("grey"), breaks = c("Anz_krank_1"),labels = c("Krank"))+
  geom_line()+facet_wrap(~`Country/Region`,scales="free")+
  ggtitle("Veränderung Kranke") 


plot1b<-jhu_clean%>%
  filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
  filter(estimator=="cfr_1")%>%
  group_by(`Country/Region`)%>%
  arrange(Date)%>%
  mutate(g_conf=lead(Anz_conf)/Anz_conf-1)%>%
  ggplot(aes(x=Date,y=g_conf))+  
  geom_line()+facet_wrap(~`Country/Region`,scales="free_y") +scale_y_continuous(labels=scales::percent,limits=c(0,1))+ylab("Infektionsrate pro Tag")+
#  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
  geom_smooth(method = "loess", span=0.5)+
  ggtitle("Neuinfektionen pro Bestätigte Fälle")

plot1ba<-jhu_clean%>%
  filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
  filter(estimator=="cfr_1")%>%
  group_by(`Country/Region`)%>%
  arrange(Date)%>%
  mutate(g_conf=Anz_conf_1/lag(anz_krank))%>%
  ggplot(aes(x=Date,y=g_conf))+  
  geom_line()+facet_wrap(~`Country/Region`,scales="free_y") +scale_y_continuous(labels=scales::percent, limits=c(0,1))+ylab("Infektionsrate pro Tag")+
  #  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
  geom_smooth(method = "loess", span=0.5)+
  ggtitle("Neuinfektionen pro erkrankte Fälle")

est_text<-"Alle Schätzer betrachten nur die bestätigten Fälle. Die Frage: Was passiert, wenn ich mich infiziere? lässt sich daraus nur abschätzen.\n 
Die blaue Linie ist in einer exponentiellen Wachstumsphase in der Regel zu niedrig . Die orange is zu hoch, wenn die Genesung länger dauert als der Tod\n
Der Advanced Estimator2 berücksichtigt Annahmen zur Sterbedauer (7 Tage) und Geneseung (20 Tage) um diesen Effekt zu reduzieren. Die Dauern sind auf Basis chinesischer Daten ebenfalls geschätzt. Der Wert ist nicht immer konstant.\n
Vergleichbarkeit zwischen Ländern ist aufgrund unterschiedlicher Dunkelziffern und Testsystematik schwierig. "


plot2<-jhu_clean%>%
filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
ggplot(aes(x=Date,y=Value,col=estimator,linetype=est_quality))+geom_line()+facet_wrap(~`Country/Region`,scales="free_y")+
 scale_color_manual("estimator",values = c("blue","orange","grey"), breaks = c("cfr_1","cfr_2","cfr_advanced_2"),labels = c("Tote pro Bestätigte Fälle","Tote pro Abgeschlossene Fälle","Advanced Estimator"))+
  scale_y_continuous(labels=scales::percent,limits=c(0,0.5))+
 scale_linetype_manual(values=c("dotted", "solid"))+ggtitle("Schätzung der Todesfälle pro bestätigtem Fall anhand der Beobachtungen")
  


##Table mit liste der Aktuellen Schätzer

update<-jhu_clean%>%
  ungroup()%>%
  summarise(upd=max(Date))%>%
  as.vector()

aktu_data<-jhu_clean%>%  
  top_n(1,Date)%>%
  filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
  spread(estimator,Value)%>%
  arrange(desc(Anz_conf))%>%
  mutate(Bestätigte_Gesamt=Anz_conf)%>%
  mutate(Gestorben_Gesamt=Anz_dea)%>%
  mutate(Genesen_Gesamt=Anz_rec)%>%
  mutate(Bestätigte_Tag=Anz_conf_1)%>%
  mutate(Gestorben_Tag=Anz_dea_1)%>%
  mutate(Genesen_Tag=Anz_rec_1)%>%
  mutate(Tote_pro_Bestätigte=round(cfr_1,2))%>%
  mutate(Tote_pro_Abgeschlossene=round(cfr_2,2))%>%
#  mutate(Adv_Estimator=round(cfr_advanced,2))%>%
  mutate(Adv_Estimator_2=round(cfr_advanced_2,2))%>%
#  mutate(Adv_Estimator_3=round(cfr_advanced_3,2))%>%
  mutate(Verdopplungszeit=round(Verd_3d,1))%>%
  select(1:2,Bestätigte_Gesamt:Verdopplungszeit)


plot3<-jhu_clean%>%
  filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
  filter(estimator=="cfr_1")%>%
  filter(!(`Country/Region`=="China"&Date==ymd("2020-02-13")))%>%
  gather("keze","Menschen",c("Anz_conf_1","Anz_dea_1"))%>%
  filter(keze=="Anz_conf_1")%>%
  filter(Menschen>0)%>%## Damit im Boxplot nicht die Phase vor der Welle dominiert
  ggplot(aes(x=Weekday,y=Menschen,col=Weekday))+  
  geom_boxplot()+facet_wrap(~`Country/Region`,scales="free")+
  ggtitle("Täglich bestätige Fälle: Gibt es Wochenendbetrieb in den Testlaboren?") 




###jhu_clean_prog, Prognoserechnung zur Erreichung der Kapazitätsgrenze

# jhu_prog<-jhu_clean%>%
#   filter(`Country/Region`=="Germany",estimator=="cfr_1")%>%
#   mutate(Kapazität=820*34, Kapazität_bei_Ausbau=820*34+10000)%>%
#   mutate(Prognose_Intensivfälle=round((Anz_conf)*0.05))%>%
#   select(Date,Prognose_Intensivfälle,Kapazität,Kapazität_bei_Ausbau,Anz_conf,Anz_krank)%>%
#   mutate(Status="Beobachtung")%>%
#   mutate(Prognose_Intensivfälle_Szenario_1=Prognose_Intensivfälle)%>%
#   mutate(Prognose_Intensivfälle_Szenario_2=Prognose_Intensivfälle)%>%
#   mutate(Prognose_Intensivfälle_Szenario_3=Prognose_Intensivfälle)  

rate_1<-0.4
rate_2<-0.15
rate_3<-0.05
tage<-30
jhu_synth<-data.frame(Date=max(jhu_prog$Date)+ddays(1:tage),Anz_conf_prog_1=max(jhu_prog$Anz_conf)*(1+rate_1)**(1:tage),
                      Anz_conf_prog_2=max(jhu_prog$Anz_conf)*(1+rate_2)**(1:tage),Anz_conf_prog_3=max(jhu_prog$Anz_conf)*(1+rate_3)**(1:tage)                    )%>%
  mutate(`Country/Region`="Germany")%>%
  mutate(Kapazität=820*34, Kapazität_bei_Ausbau=820*34+10000)%>%
  mutate(Prognose_Intensivfälle_Szenario_1=round((Anz_conf_prog_1)*0.05))%>%
  mutate(Prognose_Intensivfälle_Szenario_2=round((Anz_conf_prog_2)*0.05))%>%
  mutate(Prognose_Intensivfälle_Szenario_3=round((Anz_conf_prog_3)*0.05))%>%
  mutate(Status="Prognose")
##2. Variante nur kranke
jhu_synth_krank<-data.frame(Date=max(jhu_prog$Date)+ddays(1:tage),Anz_conf_prog_1=max(jhu_prog$Anz_krank)*(1+rate_1)**(1:tage),
                      Anz_conf_prog_2=max(jhu_prog$Anz_krank)*(1+rate_2)**(1:tage),Anz_conf_prog_3=max(jhu_prog$Anz_krank)*(1+rate_3)**(1:tage)                    )%>%
  mutate(`Country/Region`="Germany")%>%
  mutate(Kapazität=820*34, Kapazität_bei_Ausbau=820*34+10000)%>%
  mutate(Prognose_Intensivfälle_Szenario_1=round((Anz_conf_prog_1)*0.05))%>%
  mutate(Prognose_Intensivfälle_Szenario_2=round((Anz_conf_prog_2)*0.05))%>%
  mutate(Prognose_Intensivfälle_Szenario_3=round((Anz_conf_prog_3)*0.05))%>%
  mutate(Status="Prognose")


jhu_sum<-bind_rows(jhu_prog,jhu_synth)%>%
  select(-Anz_conf)%>%
  gather(Kat,Anzahl,c("Prognose_Intensivfälle_Szenario_1","Prognose_Intensivfälle_Szenario_2","Prognose_Intensivfälle_Szenario_3"))

kap_text<-"Annahmen:\n
            - etwa 5% der Bestätigten Fälle benötigen ein Bett auf einer Intensivstation
            - gelb: Die Bettanzahl liegt bei 34 Betten pro 100.000 Einwohnern
            . rot: Die Bettanzahl konnte um 10.000 Betten ausgebaut werden"



plot1c<-ggplot(jhu_sum,aes(x=Date,y=Anzahl,linetype=Status))+geom_line(col="blue",cex=1.2)+  scale_linetype_manual(values=c("solid", "dotted"))+
  facet_wrap(~Kat,scales="free_y",labeller = labeller(Kat = 
                                                        c("Prognose_Intensivfälle_Szenario_1" = paste0("Prognose Intensivfälle bei ",rate_1*100,"% Wachstum"),
                                                          "Prognose_Intensivfälle_Szenario_2" = paste0("Prognose Intensivfälle bei ",rate_2*100,"% Wachstum"),
                                                          "Prognose_Intensivfälle_Szenario_3" = paste0("Prognose Intensivfälle bei ",rate_3*100,"% Wachstum"))))+
  geom_hline(aes(yintercept = jhu_sum$Kapazität),col="yellow",cex=1.2)+
  geom_hline(yintercept = jhu_sum$Kapazität_bei_Ausbau,col="red",cex=1.2)+
  scale_y_log10(breaks = c(1000,10000, 20000,50000,100000,1000000),labels=scales::comma)+
  ggtitle("Ereichung der Kapazitätsgrenze bei Intensivbetten")
###Ende Prognose
##Prognose gesunde Fälle
cfr_country<-aktu_data%>%mutate(cfr_ang=(Tote_pro_Bestätigte+Adv_Estimator_2)/2)%>%
  select(cfr_ang)
jhu_clean<-jhu_clean%>%
  inner_join(cfr_country)%>%
  group_by(`Country/Region`,estimator)%>%
  arrange(Date)%>%
  mutate(Anz_rec_1_est=(1-cfr_ang)*lag(Anz_conf_1,input$dauer_g,default = 0))%>%
  mutate(Genesene_Gesamt_Prognose=cumsum(Anz_rec_1_est))%>%
  mutate(Aktuell_Krank_Prognose=Anz_conf-Anz_dea-Genesene_Gesamt_Prognose)

##Ende Prognose gesunde Fälle
##Plot Genesene
plot4<-jhu_clean%>%
  filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
  filter(estimator=="cfr_1")%>%
  gather("Kennzahl","Menschen",c("Genesene_Gesamt_Prognose","Aktuell_Krank_Prognose","Anz_dea","Anz_conf"))%>%
ggplot(aes(x=Date,y=Menschen, col=Kennzahl))+geom_line()+facet_wrap(~`Country/Region`,scales = "free_y")+
scale_color_manual("Kennzahl",values = c("red","blue","green","grey"), breaks = c("Anz_dea","Anz_conf","Genesene_Gesamt_Prognose","Aktuell_Krank_Prognose"),
                      labels = c("Bestätigt", "Gestorben","Genesen Prognose","Aktuell Krank Prognose"))+
  ggtitle("Gesamtzahlen inklusive Prognose Genesene")
##
gen_text<-"Annahme:\n
        Krankheitsdauer 2 Wochen\n
        Wahrscheinlichkeit zu genesen wird anhand der aktuellen Sterberate geschätzt"


footnote<-grid.text(paste0(est_text),  gp=gpar(fontsize=8))
footnote_kap<-grid.text(paste0(kap_text),  gp=gpar(fontsize=8))
footnote_gen<-grid.text(paste0(gen_text),  gp=gpar(fontsize=8))
lay <- rbind(c(1),
             c(1),
             c(1),
             c(2))

         
pdf(paste0("corona_report_",update$upd,".pdf"),width=13)
plot1
plot1a
plot1aa
plot1b
plot1ba
grid.arrange(plot1c,footnote_kap,layout_matrix=lay)
grid.arrange(plot2,footnote,layout_matrix=lay)
grid.newpage()
grid.table(aktu_data,theme=ttheme_default(base_size=8,rect=element_rect(fill="blue")))
plot3
#grid.arrange(plot4,footnote_gen,layout_matrix=lay)
dev.off()






#Versandmodul
##Probleme mit dem Workspace
library(RDCOMClient)
pdf.name<-paste0("C:\\Users\\Sandro\\Documents\\R\\Skripte\\Corona\\corona_report_",update$upd,".pdf")
versand<-"sandrogrieger@gmx.de;oliver.weisbecker@gmx.de;elena.grieger@gmail.com;carolinegrieger@gmx.de;dorisgrieger@gmail.com;helgrieger@googlemail.com;t.kerschbaumer@processand.com;martinbloos@gmx.de"
#versand<-"sandro.grieger@ruv.de"
#versand<-"sandrogrieger@gmx.de"
### Outlook Nutzen für Ausgabe
OutApp<-COMCreate("Outlook.Application")
outMail<-OutApp$CreateItem(0)
outMail[["bcc"]]<-versand
outMail[["subject"]]<-paste0("Corona Update ",as.character(Sys.Date()))
outMail[["body"]]<-"Quelle: Johns Hopkins University. Seit 03.04. wieder inklusive geneneser Fälle"
###Attachment dynamisch
outMail[["attachments"]]$Add(pdf.name)
outMail$Send()

