library(tidyverse)
library(readxl)
library(lubridate)
library(purrr)
library(grid)
library(gridExtra)

#setwd("C:/Users/Sandro/Documents/R/Skripte/Corona")
options(encoding = "UTF-8")
p<-"C:/Users/Sandro/Documents/R/Skripte/Corona/"
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
              destfile = paste0(p,"Input/jhu_confirmed.txt"))
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
              destfile = paste0(p,"Input/jhu_death.txt"))
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
              ,destfile = paste0(p,"Input/jhu_recoverd.txt"))


jhu_conf<-read_delim(paste0(p,"Input/jhu_confirmed.txt"),delim=",")
jhu_death<-read_delim(paste0(p,"Input/jhu_death.txt"),delim=",")
jhu_rec<-read_delim(paste0(p,"Input/jhu_recoverd.txt"),delim=",")

jhu_conf_tidy<-jhu_conf%>%
  gather("Date","Anz_conf",-c(1:4))
jhu_death_tidy<-jhu_death%>%
  gather("Date","Anz_dea",-c(1:4))
jhu_rec_tidy<-jhu_rec%>%
  gather("Date","Anz_rec",-c(1:4))


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

##Liste für manuelle Auswahl

list_man <-jhu_clean%>%
  ungroup()%>%
  select(`Country/Region`)%>%
  unique%>%
  filter(`Country/Region` %in% c("Austria","Korea, South","Netherlands","Croatia","Germany","Sweden"))
#list_top_c<-list_man

###Standardtheme erstellen
tt_stand<-theme(text=element_text(family="Bookman",size=14),plot.background = element_rect(fill = "grey25"),
            panel.background = element_rect(fill="white"),
            title=element_text(colour="white"),
            axis.text = element_text(colour="white"),
            axis.ticks = element_line(colour="white"),
            axis.line = element_line(colour="white"),
            panel.grid = element_line(colour="lightgray"),
            legend.title = element_text(colour = "black"),
            legend.position = "bottom",
            plot.subtitle = element_text(size = 8),
            plot.caption = element_text(size = 8)
            )



###Plots für den PDF-Output
plot1<-jhu_clean%>%
    filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
    filter(estimator=="cfr_1")%>%
    gather("keze","Menschen",c("Anz_conf","Anz_dea","Anz_rec","anz_krank"))%>%
    ggplot(aes(x=Date,y=Menschen,col=keze))+  scale_color_manual("keze",values = c("blue","red","green","grey"), breaks = c("Anz_conf", "Anz_dea","Anz_rec","anz_krank"),labels = c("Bestätigt", "Gestorben","Genesen","Krank"))+
    geom_line()+facet_wrap(~`Country/Region`,scales="free_y") +
    scale_y_continuous(labels=scales::comma)+
  coord_cartesian(ylim=c(0,NA))+  
    ggtitle("Gesamtzahlen")+
    labs(caption=paste0("Quelle Johns-Hopkins-University, Update: ",now()),subtitle = "Täglich aktualisierte Coronafälle in der zeitlichen Entwicklung")+
    tt_stand


plot1a<-jhu_clean%>%
  filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
  filter(estimator=="cfr_1")%>%
  filter(!(`Country/Region`=="China"&Date==ymd("2020-02-13")))%>%
  gather("keze","Menschen",c("Anz_conf_1","Anz_dea_1","Anz_rec_1"))%>%
  ggplot(aes(x=Date,y=Menschen,col=keze))+  scale_color_manual("keze",values = c("blue","red","green"), breaks = c("Anz_conf_1", "Anz_dea_1","Anz_rec_1"),labels = c("Bestätigt", "Gestorben","Genesen"))+
  geom_line()+facet_wrap(~`Country/Region`,scales="free_y")+
  scale_y_continuous(labels=scales::comma)+
  coord_cartesian(ylim=c(0,NA))+
  ggtitle("Tägliche Veränderung") +
  labs(caption="Quelle Johns-Hopkins-University",subtitle = "Täglich aktualisierte Veränderung der Coronafälle in der zeitlichen Entwicklung")+
  tt_stand

plot1aa<-jhu_clean%>%
  filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
  filter(estimator=="cfr_1")%>%
  filter(!(`Country/Region`=="China"&Date==ymd("2020-02-13")))%>%
  gather("keze","Menschen",c("Anz_krank_1"))%>%
  ggplot(aes(x=Date,y=Menschen,col=keze))+  scale_color_manual("keze",values = c("grey"), breaks = c("Anz_krank_1"),labels = c("Krank"))+
  geom_line()+facet_wrap(~`Country/Region`,scales="free_y")+
  scale_y_continuous(labels=scales::comma)+
  geom_hline(yintercept = 0)+
  ggtitle("Tächliche Veränderung Kranke") +
  labs(caption=paste0("Quelle Johns-Hopkins-University, Update: ",now()),subtitle = "Täglich aktualisierte Veränderung der aktuell erkrankten Coronafälle in der zeitlichen Entwicklung")+
  tt_stand

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
  ggtitle("Wird abgelöst: Neuinfektionen pro Bestätigte Fälle")+
  labs(caption="Quelle Johns-Hopkins-University",subtitle = "Wachstum der bestätigten Fälle in der zeitlichen Entwicklung")+
  tt_stand

plot1ba<-jhu_clean%>%
  filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
  filter(estimator=="cfr_1")%>%
  group_by(`Country/Region`)%>%
  arrange(Date)%>%
  mutate(g_conf=Anz_conf_1/lag(anz_krank))%>%
  ggplot(aes(x=Date,y=g_conf))+  
  geom_line()+facet_wrap(~`Country/Region`,scales="free_y") +scale_y_continuous(labels=scales::percent)+ylab("Infektionsrate pro Tag")+
  coord_cartesian(ylim=c(0,0.5))+
  #  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
  geom_smooth(method = "loess", span=0.2,se=FALSE)+
  ggtitle("Neuinfektionen pro erkrankte Fälle")+
  labs(caption="Quelle Johns-Hopkins-University",subtitle = "Neuinfektionen je aktuellem Krankheitsfall in der zeitlichen Entwicklung")+
  tt_stand

est_text<-"Alle Schätzer betrachten nur die bestätigten Fälle. Die Frage: Was passiert, wenn ich mich infiziere? lässt sich daraus nur abschätzen.\n 
Die blaue Linie ist in einer exponentiellen Wachstumsphase in der Regel zu niedrig . Die orange is zu hoch, wenn die Genesung länger dauert als der Tod\n
Der Advanced Estimator2 berücksichtigt Annahmen zur Sterbedauer (7 Tage) und Geneseung (20 Tage) um diesen Effekt zu reduzieren. Die Dauern sind auf Basis chinesischer Daten ebenfalls geschätzt. Der Wert ist nicht immer konstant.\n
Vergleichbarkeit zwischen Ländern ist aufgrund unterschiedlicher Dunkelziffern und Testsystematik schwierig. "


plot2<-jhu_clean%>%
filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
ggplot(aes(x=Date,y=Value,col=estimator,linetype=est_quality))+geom_line()+facet_wrap(~`Country/Region`,scales="free_y")+
 scale_color_manual("estimator",values = c("blue","orange","grey"), breaks = c("cfr_1","cfr_2","cfr_advanced_2"),labels = c("Tote pro Bestätigte Fälle","Tote pro Abgeschlossene Fälle","Advanced Estimator"))+
  scale_y_continuous(labels=scales::percent)+
  coord_cartesian(ylim=c(0,0.5))+
 scale_linetype_manual(values=c("dotted", "solid"))+ggtitle("Schätzung der Fallsterblichkeit")+
  labs(caption="Quelle Johns-Hopkins-University",subtitle = "3 verschiedene Schätzer zeigen die hohe Schätzunsicherheit auf")+
  tt_stand
  


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
  mutate(Tote_pro_Bestätigte=round(cfr_1,3))%>%
  mutate(Tote_pro_Abgeschlossene=round(cfr_2,3))%>%
#  mutate(Adv_Estimator=round(cfr_advanced,2))%>%
  mutate(Adv_Estimator_2=round(cfr_advanced_2,3))%>%
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


#plot mit cfr je Land
#cluster gesundheitssystem in kontrolle

aktu_extrakt<-function(list_top_c,n){
  aktu_data<-jhu_clean%>%  
    spread(estimator,Value)%>%
    top_n(n,Date)%>%
    filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
    arrange(desc(Anz_conf))%>%
    mutate(Bestätigte_Gesamt=Anz_conf)%>%
    mutate(Gestorben_Gesamt=Anz_dea)%>%
    mutate(Genesen_Gesamt=Anz_rec)%>%
    mutate(Bestätigte_Tag=Anz_conf_1)%>%
    mutate(Gestorben_Tag=Anz_dea_1)%>%
    mutate(Genesen_Tag=Anz_rec_1)%>%
    mutate(Tote_pro_Bestätigte=round(cfr_1,3))%>%
    mutate(Tote_pro_Abgeschlossene=round(cfr_2,3))%>%
    #  mutate(Adv_Estimator=round(cfr_advanced,2))%>%
    mutate(Adv_Estimator_2=round(cfr_advanced_2,3))%>%
    #  mutate(Adv_Estimator_3=round(cfr_advanced_3,2))%>%
    mutate(Verdopplungszeit=round(Verd_3d,1))%>%
    select(1:2,Bestätigte_Gesamt:Verdopplungszeit)%>%
    filter(weekdays(Date)==weekdays(today()-1))%>%
    arrange(Date)
  aktu_last<-aktu_data%>%
    group_by(`Country/Region`)%>%
    top_n(1,Date)
  
  return(list(aktu_data,aktu_last))
}

aktu_extrakt(list_top_c,15)
list_top_10<-jhu_clean%>%  
  top_n(1,Date)%>%
  ungroup()%>%
  dplyr::filter(estimator=="cfr_1")%>%
  top_n(10,Anz_conf)%>%
  select(`Country/Region`)%>%
  unique()
list_man <-jhu_clean%>%
  ungroup()%>%
  select(`Country/Region`)%>%
  unique%>%
  filter(`Country/Region` %in% c("Austria","Korea, South","Netherlands","Croatia","Germany","Sweden","Belgium"))
list_sel<-bind_rows(list_top_10,list_man)

plot7<-ggplot(aktu_extrakt(list_sel,42)[[1]],aes(x=Verdopplungszeit,y=Adv_Estimator_2,col=`Country/Region`,size=Bestätigte_Gesamt))+
  geom_path(size=0.5,linetype="dashed",arrow=arrow())+geom_point(shape=16,alpha=0.5)+
  #geom_text(col="blue",check_overlap = TRUE)+
  tt_stand+ylab("Geschätzte Sterberate (CFR)")+scale_x_log10()+
  coord_cartesian(ylim=c(0,0.20))+
  geom_text(data=aktu_extrakt(list_sel,42)[[2]],aes(x=Verdopplungszeit,y=Adv_Estimator_2,col=`Country/Region`,label=`Country/Region`),size=4,check_overlap = TRUE)+
  scale_y_continuous(labels=scales::percent)+
  labs(caption="Quelle Johns-Hopkins-University",subtitle = "Simulation auf Basis der aktuell erkrankten Fälle mit Einbeziehung von Genesung")+
  ggtitle("Ländervergleich Sterberate und Verdopplungszeit in der Entwicklung")+
  labs(subtitle = "Wöchentliche Veränderung, Größe der Bubbles = Anzahl Bestätigte Fälle")+
  theme(legend.position = "none")


###jhu_clean_prog, Prognoserechnung zur Erreichung der Kapazitätsgrenze

 jhu_prog<-jhu_clean%>%
  filter(`Country/Region`=="Germany",estimator=="cfr_1")%>%
  mutate(Kapazität=820*34, Kapazität_bei_Ausbau=820*34+10000)%>%
  mutate(Prognose_Intensivfälle=round((Anz_conf)*0.05))%>%
  select(Date,Prognose_Intensivfälle,Kapazität,Kapazität_bei_Ausbau,Anz_conf)%>%
  mutate(Status="Beobachtung")%>%
  mutate(Prognose_Intensivfälle_Szenario_1=Prognose_Intensivfälle)%>%
  mutate(Prognose_Intensivfälle_Szenario_2=Prognose_Intensivfälle)%>%
  mutate(Prognose_Intensivfälle_Szenario_3=Prognose_Intensivfälle)

 jhu_prog_krank<-jhu_clean%>%
   filter(`Country/Region`=="Germany",estimator=="cfr_1")%>%
   mutate(Kapazität=820*34, Kapazität_bei_Ausbau=820*34+10000)%>%
   mutate(Prognose_Intensivfälle=round((anz_krank)*0.05))%>%
   select(Date,Prognose_Intensivfälle,Kapazität,Kapazität_bei_Ausbau,anz_krank)%>%
   mutate(Status="Beobachtung")%>%
   mutate(Prognose_Intensivfälle_Szenario_1=Prognose_Intensivfälle)%>%
   mutate(Prognose_Intensivfälle_Szenario_2=Prognose_Intensivfälle)%>%
   mutate(Prognose_Intensivfälle_Szenario_3=Prognose_Intensivfälle) 

rate_1<-0.15
rate_2<-0.10
rate_3<-0.05
tage<-50
jhu_synth<-data.frame(Date=max(jhu_prog$Date)+ddays(1:tage),Anz_conf_prog_1=max(jhu_prog$Anz_conf)*(1+rate_1)**(1:tage),
                      Anz_conf_prog_2=max(jhu_prog$Anz_conf)*(1+rate_2)**(1:tage),Anz_conf_prog_3=max(jhu_prog$Anz_conf)*(1+rate_3)**(1:tage)                    )%>%
  mutate(`Country/Region`="Germany")%>%
  mutate(Kapazität=820*34, Kapazität_bei_Ausbau=820*34+10000)%>%
  mutate(Prognose_Intensivfälle_Szenario_1=round((Anz_conf_prog_1)*0.05))%>%
  mutate(Prognose_Intensivfälle_Szenario_2=round((Anz_conf_prog_2)*0.05))%>%
  mutate(Prognose_Intensivfälle_Szenario_3=round((Anz_conf_prog_3)*0.05))%>%
  mutate(Status="Prognose")

jhu_sum<-bind_rows(jhu_prog,jhu_synth)%>%
  select(-Anz_conf)%>%
  gather(Kat,Anzahl,c("Prognose_Intensivfälle_Szenario_1","Prognose_Intensivfälle_Szenario_2","Prognose_Intensivfälle_Szenario_3"))



####neue Funktion, Prognose
##Inputs: rate, Aktueller Stand als DF mit 20 einträgen inkl. Datum, Prognose-Horizont
prognos<-function(rate,vector,ph){
  data<-vector%>%
    mutate(Status="Beobachtung")
  for (i in 1:ph){
  lastline<-slice(data,n())
  prevline<-slice(data,n()-1)
  lagline<-slice(data,n()-12)
  newline<-data.frame(Date=lastline$Date+ddays(1),anz_krank=pmax(lastline$anz_krank*(1+rate)-lagline$Anz_conf_1,0),Anz_conf_1=lastline$anz_krank*(rate),Status="Prognose",stringsAsFactors = FALSE)
  print(i)
  print(newline)
  data<-bind_rows(data,newline)
  }
return(data)
}
test<-jhu_clean%>%
  ungroup()%>%
  filter(`Country/Region`=="Germany",estimator=="cfr_1")%>%
  select(Date,anz_krank,Anz_conf_1)

###3facher Durchlauf für 3 raten
synth_krank<-prognos(rate_1,test,tage)%>%
  inner_join(prognos(rate_2,test,tage),by=c("Date","Status"),suffix=c("_rate_1","_rate_2"))%>%
  inner_join(prognos(rate_3,test,tage),by=c("Date","Status"))%>%  
  mutate(`Country/Region`="Germany")%>%
  mutate(Kapazität=820*34, Kapazität_bei_Ausbau=820*34+10000)%>%
  mutate(Prognose_Intensivfälle_Szenario_1=round((anz_krank_rate_1)*0.05))%>%
  mutate(Prognose_Intensivfälle_Szenario_2=round((anz_krank_rate_2)*0.05))%>%
  mutate(Prognose_Intensivfälle_Szenario_3=round((anz_krank)*0.05))


jhu_sum_krank<-synth_krank%>%
  select(-anz_krank)%>%
  mutate(Status=factor(Status))%>%
  gather(Kat,Anzahl,c("Prognose_Intensivfälle_Szenario_1","Prognose_Intensivfälle_Szenario_2","Prognose_Intensivfälle_Szenario_3"))%>%
  mutate(Kat=factor(Kat))

kap_text<-"Annahmen:\n
            - etwa 5% der Bestätigten Fälle benötigen ein Bett auf einer Intensivstation
            - gelb: Die Bettanzahl liegt bei 34 Betten pro 100.000 Einwohnern
            . rot: Die Bettanzahl konnte um 10.000 Betten ausgebaut werden"

kap_text_krank<-"Die Simulation geht davon aus, dass die aktuell erkrankten 12 Tage ansteckend sind (Wert angepasst). \n
                Die Ansteckungsrate ist konstant. Es werden 3 Szenarien betrachtet\n
Weitere Annahmen:\n
            - etwa 5% der aktuell erkrankten Fälle benötigen ein Bett auf einer Intensivstation
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
  ggtitle("Ereichung der Kapazitätsgrenze bei Intensivbetten")+
  labs(caption="Quelle Johns-Hopkins-University",subtitle = "Simulation auf Basis der Bestätigen Fälle")+
  tt_stand

plot1ca<-ggplot(jhu_sum_krank,aes(x=Date,y=Anzahl,linetype=Status))+geom_line(col="blue",cex=1.2)+  scale_linetype_manual(values=c("solid", "dotted"))+
  facet_wrap(~Kat,scales="free_y",labeller = labeller(Kat = 
                                                        c("Prognose_Intensivfälle_Szenario_1" = paste0("Prognose Intensivfälle bei ",rate_1*100,"% Wachstum"),
                                                          "Prognose_Intensivfälle_Szenario_2" = paste0("Prognose Intensivfälle bei ",rate_2*100,"% Wachstum"),
                                                          "Prognose_Intensivfälle_Szenario_3" = paste0("Prognose Intensivfälle bei ",rate_3*100,"% Wachstum"))))+
  geom_hline(yintercept = jhu_sum$Kapazität,col="yellow",cex=1.2)+
  geom_hline(yintercept = jhu_sum$Kapazität_bei_Ausbau,col="red",cex=1.2)+
  scale_y_log10(breaks = c(1000,10000, 20000,50000,100000,1000000),labels=scales::comma)+
  ggtitle("Ereichung der Kapazitätsgrenze bei Intensivbetten")+
  labs(caption="Quelle Johns-Hopkins-University",subtitle = "Simulation auf Basis der aktuell erkrankten Fälle mit Einbeziehung von Genesung")+
  tt_stand

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
footnote_kap_krank<-grid.text(paste0(kap_text_krank),  gp=gpar(fontsize=8))
footnote_gen<-grid.text(paste0(gen_text),  gp=gpar(fontsize=8))
lay <- rbind(c(1),
             c(1),
             c(1),
             c(2))



library(rvest)
##Extraktor webscraping
url<-"https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html"
rki<-read_html(url)
table<-html_table(rki)
df<-table[[1]]
rki_names<-c("Bundesland",df[1,2],df[1,3],df[1,4],df[1,5])
names(df)<-rki_names
df<-df%>%
  slice(-1)%>%
  mutate(update=Sys.time())
write_csv2(df,paste0(p,"rki_",Sys.Date(),".csv"))
url_2<-"https://soziales.hessen.de/gesundheit/infektionsschutz/corona-hessen/taegliche-uebersicht-der-bestaetigten-sars-cov-2-faelle-hessen"
he<-read_html(url_2)
table<-html_table(he)
df_he<-table[[1]]
df_he[1:2,]
he_names<-paste(df_he[1,],df_he[2,],sep = " ")
#he_names<-paste(names(df_he),df_he[1,],sep = " ")
names(df_he)<-he_names
df_he<-df_he%>%
  slice(-c(1:2))%>%
  mutate(update=Sys.time())
write_csv2(df_he,paste0(p,"web/hessische_landkreise_",Sys.Date(),".csv"))


##rlp
url_3<-"https://msagd.rlp.de/de/unsere-themen/gesundheit-und-pflege/gesundheitliche-versorgung/oeffentlicher-gesundheitsdienst-hygiene-und-infektionsschutz/infektionsschutz/informationen-zum-coronavirus-sars-cov-2/"
rlp<-read_html(url_3)
table<-html_table(rlp)
df_rlp<-table[[1]]
rlp_names<-df_rlp[1,]
names(df_rlp)<-rlp_names
df_rlp<-df_rlp%>%
  slice(-c(1,26))%>%
  mutate(update=Sys.time())
write_csv2(df_rlp,paste0(p,"rlp_landkreise_",Sys.Date(),".csv"))

###
df_he<-df_he%>%
  mutate(`letzte 7 Tage Inzidenz`=as.numeric(`letzte 7 Tage Inzi-denz`))

plot8<-ggplot(df_he,aes(x=fct_reorder(` Kreis/Stadt`,`letzte 7 Tage Inzidenz`,max),y=`letzte 7 Tage Inzidenz`))+
   geom_col()+coord_flip()+tt_stand+ggtitle("Kurzfristige Entwicklung je Landkreis")+
    xlab("Kreis")+ylab("Fälle pro 100.000 in den letzten 7 Tagen")+
  labs(caption="Quelle Land Hessen")
n<-names(df)
n[3]<-"diff"
n[2]<-"Anzahl"
names(df)<-n
df<-df%>%
  mutate_each(as.numeric,-c(Bundesland,update))%>%
  mutate(einwohner_mio=Anzahl/`Fälle/ 100.000 Einw.`*100)%>%
  mutate(diff_pro_100=diff/einwohner_mio/10)

df_rlp<-df_rlp%>%
  mutate_each(as.numeric,-c(Landkreis,update)) 
  
plot9<-ggplot(df,aes(x=fct_reorder(Bundesland,`Fälle/ 100.000 Einw.`,max),y=`Fälle/ 100.000 Einw.`))+
  geom_col()+coord_flip()+tt_stand+ggtitle("Situation je Bundesland")+
  xlab("Bundesland")+ylab("Fälle pro 100.000 kumuliert")+
  labs(caption="Quelle: RKI")
plot10<-ggplot(df,aes(x=fct_reorder(Bundesland,diff_pro_100,max),y=diff_pro_100))+
  geom_col()+coord_flip()+tt_stand+ggtitle("Veränderung pro 100.000 je Bundesland")+
  xlab("Bundesland")+ylab("Tägliche Neuinfektionen")+
  labs(caption="Quelle: RKI")

###Verarbeitung He_LK
import_ordner<-function(ordner){
  dateien<-list.files(ordner,recursive = F)
  ###leeren DF erzeugen
  start<-read_csv2(paste0(ordner,"/",dateien[1]))
  col_n<-names(start)
  for (name in dateien ) {
    print(name)
    new<-read_csv2(paste0(ordner,"/",name),col_names = col_n,skip=1)
    start<-bind_rows(start,new)
  }
  return(start)}

he_lk<-import_ordner(paste0(p,"web/"))

###Ende Verarbeitung He_LK
##
plot11<-filter(he_lk,`Kreis/Stadt`!="Gesamtergebnis")%>%
  filter(date(update) %in% c(today(),today()-7))%>%
  mutate(`Kreis/Stadt`=fct_reorder(`Kreis/Stadt`,`letzte 7 Tage Inzidenz`))%>%
ggplot(aes(x=`Kreis/Stadt`,y=`letzte 7 Tage Inzidenz`,group=`Kreis/Stadt`))+
  geom_path(col="grey30",size=0.5,arrow=arrow(type='closed',angle=15,length=unit(0.15,"inches")))+
  coord_flip()+
  tt_stand+ggtitle("Kurzfristige Entwicklung je Landkreis",subtitle = "Veränderung zur Vorwoche")+
  xlab("Kreis")+ylab("Fälle pro 100.000 in den letzten 7 Tagen") +
  labs(caption="Quelle Land Hessen")
he_lk%>%
  filter(str_detect(`Kreis/Stadt`,'Wiesbaden'))%>%
  ggplot(aes(x=update,y=`kumuliert bestätigte Fälle`))+geom_point()+
  coord_cartesian(ylim=c(0,NA))
class(he_lk)
names(he_lk)
he_lk%>%
  #  filter(str_detect(`Kreis/Stadt`,'Wiesbaden'))%>%
  unique()%>%
  ggplot(aes(x=update,y=`letzte 7 Tage bestätigte Fälle`))+geom_point()+
  coord_cartesian(ylim=c(0,NA))+facet_wrap(~`Kreis/Stadt`,scales="free_y")


##Modell mir purr: schätze lm, broom
##data_prep
he_lk<-he_lk%>%
  unique()%>%
  group_by(`Kreis/Stadt`)%>%
  arrange(update)%>%
  mutate(Anz_conf_1=`kumuliert bestätigte Fälle`-lag(`kumuliert bestätigte Fälle`)) 
test<-he_lk%>%group_by(`Kreis/Stadt`)%>%
  unique()%>%
  mutate(zeit=row_number())%>%
  nest()%>%
  mutate(lm_modell= map(data,~lm(`letzte 7 Tage bestätigte Fälle` ~zeit, data=.x)))%>%
  mutate(lm_tidy = map(lm_modell, broom::tidy)) %>%
  ungroup() %>%
  transmute(`Kreis/Stadt`,data,lm_tidy)%>%
  unnest(cols=c(lm_tidy))%>%
  select(-c(std.error,statistic,p.value))%>%
  spread(term,estimate)%>%
  mutate(delta_zeit=zeit)%>%
  select(-zeit)%>%
  unnest(data)%>%
  filter(!str_detect(`Kreis/Stadt`,'esamt'))%>%
  mutate(kat=cut(delta_zeit,breaks=c(-10,-1,1,10)))
plot12<-test%>%
  ggplot(aes(x=as.Date(update),y=`letzte 7 Tage bestätigte Fälle`,col=kat))+geom_point()+
  coord_cartesian(ylim=c(0,NA))+
  #  geom_abline(data=test,aes(slope = delta_zeit,intercept =`(Intercept)` ,col=`Kreis/Stadt`))+
  facet_wrap(~`Kreis/Stadt`,scales="free_y")+
  scale_colour_manual(values=c("green","blue","red"),labels=c("sinkend","gleichbleibend","steigend"))+
  tt_stand+
  scale_x_date(date_breaks = "1 week", date_labels = "%b-%d")+
  labs(caption=paste0("Quelle Land Hessen, Update: ",now()),title="Trends je Landkreis",subtitle = "Letzte 7 Tage in der zeitlichen Entwicklung")+
  xlab("Datum")


##
source(file=paste0(p,"2020-05-24_bayern.R"),encoding="UTF-8")
source(file=paste0(p,"2020-05-24_übersterblichkeit.R"),encoding="UTF-8")
         
pdf(paste0(p,"corona_report_",update$upd,".pdf"),width=13)
#pdf(paste0("corona_report_manuelle_Liste",update$upd,".pdf"),width=13)
#plot9
#plot10
#plot8
#plot11
print(plot12)
print(plot7)
print(plot1)
print(plot1a)
print(plot1aa)
#plot1b
print(plot1ba)
#grid.arrange(plot1c,footnote_kap,layout_matrix=lay)
grid.arrange(plot1ca,footnote_kap_krank,layout_matrix=lay)
print(plot_14)
print(plot_15)
grid.arrange(plot2,footnote,layout_matrix=lay)
grid.newpage()
grid.table(aktu_data,theme=ttheme_default(base_size=7,rect=element_rect(fill="blue")))
#plot3
#grid.arrange(plot4,footnote_gen,layout_matrix=lay)
dev.off()
pdf.name<-paste0("C:\\Users\\Sandro\\Documents\\R\\Skripte\\Corona\\corona_report_",update$upd,".pdf")


source(file=paste0(p,"2020-05-10_rlp_report.R"),encoding="UTF-8")
pdf2.name<-paste0("C:\\Users\\Sandro\\Documents\\R\\Skripte\\Corona\\corona_report_rlp_",update$upd,".pdf")



#Versandmodul
##Probleme mit dem Workspace
library(RDCOMClient)

versand<-"sandrogrieger@gmx.de;oliver.weisbecker@gmx.de;elena.grieger@gmail.com;carolinegrieger@gmx.de;dorisgrieger@gmail.com;helgrieger@googlemail.com;t.kerschbaumer@processand.com;martinbloos@gmx.de;rene@reneklein.de"
#versand<-"sandro.grieger@ruv.de"
#versand<-"sandrogrieger@gmx.de"
### Outlook Nutzen für Ausgabe
OutApp<-COMCreate("Outlook.Application")
outMail<-OutApp$CreateItem(0)
outMail[["bcc"]]<-versand
outMail[["subject"]]<-paste0("Corona Update ",as.character(Sys.Date()))
outMail[["body"]]<-"Quellen: RKI, Landesministerien, Johns-Hopkins-University."
###Attachment dynamisch
outMail[["attachments"]]$Add(pdf.name)
outMail[["attachments"]]$Add(pdf2.name)
outMail$Send()

##Experimentieren

