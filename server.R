
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)
library(magrittr)
library(tidyr)
library(forcats)
#library(grid)
#library(gridExtra)

#setwd("C:/Users/Sandro/Documents/R/Skripte/Corona")

wd<-getwd()

library(shiny)

server<-function(input,output){  
plot_14<-readRDS("Altersvergleich.RDS")
he_lk<-read.csv2("/opt/shiny-server/samples/sample-apps/corona/he.csv")
#he_lk<-read.csv2("he.csv")
stand<-he_lk%>%select(update)%>%mutate(update=ymd_hms(update))%>%summarise(update=max(update))
      
  output$text_wd<-renderText(wd)
  options(encoding = "UTF-8")
#  he_lk$update<-ymd_hms(he_lk$update)
  jhu_conf<-read_delim("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",delim=",")
  jhu_death<-read_delim("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
                        ,delim=",")
  jhu_rec<-read_delim("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
                ,delim=",")
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
    left_join(jhu_death_tidy,by=c("Province/State","Country/Region","Date"))%>%
    left_join(jhu_rec_tidy,by=c("Province/State","Country/Region","Date"))%>%
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
    filter(`Country/Region` %in% c("Austria","Netherlands","Croatia","Germany","Sweden","Italy"))
  list_top_c<-bind_rows(list_man,list_top_c)
  
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
    ggplot(aes(x=Date,y=Value,col=estimator))+geom_line()+facet_wrap(~`Country/Region`,scales="free_y")+
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

  ##
  gen_text<-"Annahme:\n
        Krankheitsdauer 2 Wochen\n
        Wahrscheinlichkeit zu genesen wird anhand der aktuellen Sterberate geschätzt"

  # output$he<-renderPlot(filter(he_lk,`Kreis_Stadt`!="gesamt")%>%
  #   filter(date(update) %in% c(today(),today()-7))%>%
  #   mutate(`Kreis_Stadt`=fct_reorder(`Kreis_Stadt`,`letzte_7_Tage_Inzidenz`))%>%
  #   ggplot(aes(x=`Kreis_Stadt`,y=`letzte_7_Tage_Inzidenz`,group=`Kreis_Stadt`))+
  #   geom_path(col="grey30",size=0.5,arrow=arrow(type='closed',angle=15,length=unit(0.15,"inches")))+
  #   coord_flip()+
  #   tt_stand+ggtitle("Kurzfristige Entwicklung je Landkreis",subtitle = "Veränderung zur Vorwoche")+
  #   xlab("Kreis")+ylab("Fälle pro 100.000 in den letzten 7 Tagen") +
  #   labs(caption="Quelle Land Hessen"))
  output$he<-renderPlot(filter(he_lk,Kreis_Stadt!="gesamt")%>%
    filter(date(update) %in% c(today(),today()-7))%>%
    mutate(Kreis_Stadt=fct_reorder(Kreis_Stadt,letzte_7_Tage_Inzidenz))%>%
    ggplot(aes(x=Kreis_Stadt,y=letzte_7_Tage_Inzidenz,group=Kreis_Stadt))+
    geom_path(col="grey30",size=0.5,arrow=arrow(type='closed',angle=15,length=unit(0.15,"inches")))+
    coord_flip()+
    tt_stand+ggtitle("Kurzfristige Entwicklung je Landkreis",subtitle = "Veränderung zur Vorwoche")+
    xlab("Kreis")+ylab("Fälle pro 100.000 in den letzten 7 Tagen") +
    labs(caption=paste0("Quelle Land Hessen, update: ",stand[1,1]))
  )
  output$plot_lk_2<-renderPlot(
    filter(he_lk, Kreis_Stadt != "gesamt") %>%
#      filter(date(update) %in% c(today(), today() - 7)) %>%
#      mutate(Kreis_Stadt = fct_reorder(Kreis_Stadt, letzte_7_Tage_Inzidenz)) %>%
      ggplot(aes(x = update, y = letzte_7_Tage_Inzidenz,group=Kreis_Stadt),height=1000) +
      geom_line(
        size = 0.5
      ) +
      tt_stand + ggtitle("Kurzfristige Entwicklung je Landkreis") +
      facet_wrap(~Kreis_Stadt,ncol=2)+
      xlab("Datum") + ylab("Fälle pro 100.000 in den letzten 7 Tagen") +
      labs(caption = paste0("Quelle Land Hessen, update: ", stand[1, 1]))
    
  )
  

output$plot7<-renderPlot(plot7)
# print(plot7)
  output$plot1<-renderPlot(plot1)
  output$plot1a<-renderPlot(plot1a)
  output$plot1aa<-renderPlot(plot1aa)
  output$table1<-DT::renderDataTable(aktu_data)
  output$plot2<-renderPlot(plot2)
  output$plot14<-renderPlot(plot_14)
# #plot1b
# print(plot1ba)
# #grid.arrange(plot1c,footnote_kap,layout_matrix=lay)
# grid.arrange(plot1ca,footnote_kap_krank,layout_matrix=lay)
# print(plot_14)
# print(plot_15)
# grid.arrange(plot2,footnote,layout_matrix=lay)
# grid.newpage()
# grid.table(aktu_data,theme=ttheme_default(base_size=7,rect=element_rect(fill="blue")))
#plot3
}

