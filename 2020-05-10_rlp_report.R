library(readxl)
library(httr)
library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
url<-"https://msagd.rlp.de/fileadmin/msagd/Gesundheit_und_Pflege/GP_Dokumente/Informationen_zum_Coronavirus/Listen_Corona_RLP.xlsx"

download.file(url,
              destfile = "C:/Users/Sandro/Documents/R/Skripte/Corona/Input/rlp.xlsx",mode="wb")
rlp_conf<-read_xlsx("C:/Users/Sandro/Documents/R/Skripte/Corona/Input/rlp.xlsx",sheet="Infektionen RLP",skip=3,n_max = 39)
rlp_death<-read_xlsx("C:/Users/Sandro/Documents/R/Skripte/Corona/Input/rlp.xlsx",sheet="Todeszahlen RLP",skip=3,n_max = 39)

rlp_conf_tidy<-rlp_conf%>%
  gather("Date","Anz_conf",-c(1))%>%
  mutate(Date=as.Date(as.numeric(Date),origin="1899-12-30"))
rlp_death_tidy<-rlp_death%>%
  gather("Date","Anz_dea",-c(1))%>%
  mutate(Date=as.Date(as.numeric(Date),origin="1899-12-30"))

rlp<-rlp_conf_tidy%>%
  left_join(rlp_death_tidy)%>%
  mutate(Anz_conf=replace_na(Anz_conf,0))%>%
  mutate(Anz_dea=replace_na(Anz_dea,0))%>%
  filter(!Landkreis %in% c("Summe","Stadt"))
anreichern<-function(df)
{
input<-vector()
input$dauer_g<-20
input$dauer_t<-7
input$dauer_t_3<-10

df %>%
  ##Zusammenführung einzelner Regionen zu Ländern
  group_by(Landkreis,Date)%>%
  summarise(Anz_conf=sum(Anz_conf),Anz_dea=sum(Anz_dea))%>%
  mutate(cfr_1=Anz_dea/Anz_conf)%>%
  ###Datenvorverarbeitung: Absolute Änderungen ermitteln
  mutate(Weekday=factor(weekdays(Date),labels=c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag")))%>%
  arrange(Date)%>%
  mutate(Anz_dea_1=Anz_dea-lag(Anz_dea,default=0))%>%
  mutate(Anz_conf_1=Anz_conf-lag(Anz_conf,default=0))  %>%
  ### Kennzahlenvorverarbeitung
  mutate(anzahl_neu_tod_avd=lag(Anz_dea_1,input$dauer_g-input$dauer_t,default=0))%>%
  mutate(anzahl_neu_conf_avd=lag(Anz_conf_1,input$dauer_t,default=0))%>%
  mutate(anzahl_neu_conf_avd_3=lag(Anz_conf_1,input$dauer_t_3,default=0))%>%
  #  mutate(cfr_advanced=cumsum(anzahl_neu_tod_avd)/cumsum(anzahl_neu_tod_avd+Anz_rec_1))%>%
  mutate(cfr_advanced_2=cumsum(Anz_dea_1)/cumsum(anzahl_neu_conf_avd))%>%
  #  mutate(cfr_advanced_3=cumsum(Anz_dea_1)/cumsum(anzahl_neu_conf_avd_3))%>%
  mutate(est_quality=ifelse(cumsum(Anz_dea_1)>20,"mittel","gering"))%>%
  mutate(Verd_3d= log(8,base=Anz_conf/lag(Anz_conf,3)))%>%
  mutate(letzte_7=Anz_conf-lag(Anz_conf,7,default=0))%>%
  gather("estimator","Value",c("cfr_1","cfr_advanced_2"))

}
test<-anreichern(rlp)

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

# last_14<-test%>%
#   filter(max(Date)-Date<=14)%>%
#   mutate(anz_l14=sum(Anz_conf_1))%>%
#   select(anz_l14)%>%
#   unique()

list_top_lk<-test%>%  
  top_n(1,Date)%>%
  ungroup()%>%
  dplyr::filter(estimator=="cfr_1")%>%
  top_n(10,Anz_conf)%>%
  select(Landkreis)%>%
  unique()
list_bottom_lk<-test%>%  
  top_n(1,Date)%>%
  ungroup()%>%
  dplyr::filter(estimator=="cfr_1")%>%
  top_n(-10,Anz_conf)%>%
  select(Landkreis)%>%
  unique()


plot1_rlp_top_10<-test%>%
  filter(Landkreis %in% (list_top_lk$Landkreis))%>%
  filter(estimator=="cfr_1")%>%
  gather("keze","Menschen",c("Anz_conf","Anz_dea"))%>%
  ggplot(aes(x=Date,y=Menschen,col=keze))+  scale_color_manual("keze",values = c("blue","red"), breaks = c("Anz_conf", "Anz_dea"),labels = c("Bestätigt", "Gestorben"))+
  geom_line()+facet_wrap(~Landkreis,scales="free_y") +
  scale_y_continuous(labels=scales::comma)+
  coord_cartesian(ylim=c(0,NA))+  
  ggtitle("Gesamtzahlen | Top 10 Landkreise")+
  labs(caption=paste0("Quelle Land RLP, Update: ",now()),subtitle = "Täglich aktualisierte Coronafälle in der zeitlichen Entwicklung")+
  tt_stand

plot1_rlp_bottom_10<-test%>%
  filter(Landkreis %in% (list_bottom_lk$Landkreis))%>%
  filter(estimator=="cfr_1")%>%
  gather("keze","Menschen",c("Anz_conf","Anz_dea"))%>%
  ggplot(aes(x=Date,y=Menschen,col=keze))+  scale_color_manual("keze",values = c("blue","red"), breaks = c("Anz_conf", "Anz_dea"),labels = c("Bestätigt", "Gestorben"))+
  geom_line()+facet_wrap(~Landkreis,scales="free_y") +
  scale_y_continuous(labels=scales::comma)+
  coord_cartesian(ylim=c(0,NA))+  
  ggtitle("Gesamtzahlen | Bottom 10 Landkreise")+
  labs(caption=paste0("Quelle Land RLP, Update: ",now()),subtitle = "Täglich aktualisierte Coronafälle in der zeitlichen Entwicklung")+
  tt_stand

plot1a_rlp<-test%>%
#  filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
  filter(estimator=="cfr_1")%>%
  gather("keze","Menschen",c("Anz_conf_1","Anz_dea_1"))%>%
  ggplot(aes(x=Date,y=Menschen,col=keze))+  scale_color_manual("keze",values = c("blue","red"), breaks = c("Anz_conf_1", "Anz_dea_1"),labels = c("Bestätigt", "Gestorben"))+
  geom_line()+facet_wrap(~Landkreis,scales="free_y")+
  scale_y_continuous(labels=scales::comma)+
  coord_cartesian(ylim=c(0,NA))+
  ggtitle("Tägliche Veränderung") +
  labs(caption=paste0("Quelle Land RLP, Update: ",now()),subtitle = "Täglich aktualisierte Veränderung der Coronafälle in der zeitlichen Entwicklung")+
  tt_stand



aktu_data_rlp<-test%>%  
  top_n(1,Date)%>%
  spread(estimator,Value)%>%
  arrange(desc(Anz_conf))%>%
  ungroup()%>%
  mutate(Lfd_nr=row_number())%>%
  filter(Landkreis %in% (list_top_lk$Landkreis)|Landkreis %in% (list_bottom_lk$Landkreis))%>%
  mutate(Bestätigte_Gesamt=Anz_conf)%>%
  mutate(Gestorben_Gesamt=Anz_dea)%>%
  mutate(Bestätigte_Tag=Anz_conf_1)%>%
  mutate(Gestorben_Tag=Anz_dea_1)%>%
  mutate(Tote_pro_Bestätigte=round(cfr_1,3))%>%
  #  mutate(Adv_Estimator=round(cfr_advanced,2))%>%
  mutate(Adv_Estimator_2=round(cfr_advanced_2,3))%>%
  #  mutate(Adv_Estimator_3=round(cfr_advanced_3,2))%>%
  mutate(Verdopplungszeit=round(Verd_3d,1))%>%
  select(Lfd_nr,1:2,Bestätigte_Gesamt:Verdopplungszeit)
row.names(aktu_data_rlp)<-aktu_data_rlp$Lfd_nr
aktu_data_rlp<-select(aktu_data_rlp,-Lfd_nr)

update<-test%>%
  ungroup()%>%
  summarise(upd=max(Date))%>%
  as.vector()

test<-anreichern(rlp)

##Modell mir purr: schätze lm, broom
##data_prep
test<-test%>%
  dplyr::filter(estimator=="cfr_1")%>%  
  group_by(Landkreis)%>%
  unique()%>%
  arrange(desc(Date))%>%
  slice(1:10)%>%
  arrange(Date)%>%
  mutate(zeit=row_number())%>%
  nest()%>%
  mutate(lm_modell= map(data,~lm(`letzte_7` ~zeit, data=.x)))%>%
  mutate(lm_tidy = map(lm_modell, broom::tidy)) %>%
  ungroup() %>%
  transmute(`Landkreis`,data,lm_tidy)%>%
  unnest(cols=c(lm_tidy))%>%
  select(-c(std.error,statistic,p.value))%>%
  spread(term,estimate)%>%
  mutate(delta_zeit=zeit)%>%
  select(-zeit)%>%
  unnest(data)%>%
  mutate(kat=cut(delta_zeit,breaks=c(-10,-1,1,10)))
plot13<-test%>%
  ggplot(aes(x=Date,y=`letzte_7`,col=kat))+geom_point()+
  coord_cartesian(ylim=c(0,NA))+
  #  geom_abline(data=test,aes(slope = delta_zeit,intercept =`(Intercept)` ,col=`Kreis/Stadt`))+
  facet_wrap(~`Landkreis`)+
  scale_colour_manual(values=c("green","blue","red"),labels=c("sinkend","gleichbleibend","steigend"),drop=FALSE)+
  tt_stand+
  scale_x_date(date_breaks = "1 week", date_labels = "%b-%d")+
  labs(caption=paste0("Quelle Land Hessen, Update: ",now()),title="Trends je Landkreis",subtitle = "Letzte 7 Tage in der zeitlichen Entwicklung")+
  xlab("Datum")






pdf(paste0("C:/Users/Sandro/Documents/R/Skripte/Corona/corona_report_rlp_",update$upd,".pdf"),width=13)
print(plot13)
print(plot1_rlp_top_10)
print(plot1_rlp_bottom_10)
grid.newpage()
grid.table(aktu_data_rlp,theme=ttheme_default(base_size=8,rect=element_rect(fill="blue")))
dev.off()
