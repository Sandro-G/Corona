kw<-paste0(rep("KW ",52),1:52)
n<-c("Bundesland","Jahr","Alter",kw)
destatis_data<-read_xlsx("sonderauswertung-sterbefaelle.xlsx",sheet="BL_2016_2020_Wochen_AG",col_names =n,skip=10,n_max=264)

destatis_data<-destatis_data%>%
  mutate_each(as.numeric,-c(Bundesland, Jahr, Alter))%>%
  gather("KW","Anzahl",-c(Bundesland, Jahr, Alter))

url_nl<-"https://www.cbs.nl/en-gb/news/2020/15/mortality-rising-further"
d<-read_html(url_nl)
e<-html_table(d)
nl<-e[[1]]
names(nl)<-c("KW","2017","2018","2019","2020")
nl<-nl%>%
  mutate(Bundesland="Niederlande")%>%
  select(c(6,1:5))%>%
  gather("Jahr","Anzahl",c("2017","2018","2019","2020"))%>%
  mutate_each(as.numeric,-c(Bundesland,Jahr))%>%
  mutate(Alter="Insgesamt")

sui<-read_delim("ts-d-14.03.04.03-wr.csv",delim = ";")%>%
  mutate(KW=Woche)%>%
  group_by(KW)%>%
  summarise(Erwartung=sum(Erwartung),Jahr_2020=sum(AnzTF_HR))%>%
  gather("Jahr","Anzahl",-KW)%>%
  mutate(Jahr=str_replace(Jahr,"Jahr_",""))%>%
  mutate(Bundesland="Schweiz")%>%
  mutate(Alter="Insgesamt")

gb<-read_xlsx("datadownload.xlsx",sheet="Data",skip=6,n_max=26)
gb_df<-gb%>%
  gather("Jahr","Anzahl",-`Week no.`)%>%
  filter(str_detect(Jahr,"All deaths"))%>%
  mutate(KW=str_replace(`Week no.`,"Week ",""))%>%
  mutate(KW=as.numeric(KW))%>%
  arrange(KW)%>%
  mutate(Bundesland="Great Britain")%>%
  mutate(Alter="Insgesamt")%>%
  select(-`Week no.`)


ausw_dest<-destatis_data%>%
  mutate(KW=as.numeric(str_replace(KW,"KW ","")))%>%
  mutate(Jahr=as.character(Jahr))%>%
  bind_rows(nl)%>%
  bind_rows(sui)%>%
  bind_rows(gb_df)

plot_15<-ausw_dest%>%
  filter(KW<=17)%>%
  mutate(Jahr=factor(Jahr))%>%
  mutate(Region=Bundesland)%>%
  group_by(Jahr,KW,Region)%>%
  filter(Alter=="Insgesamt")%>%
  filter(!(Jahr %in% c(2016,2017)))%>%
ggplot(aes(x=KW,y=Anzahl,col=Jahr,group=Jahr))+geom_line()+facet_wrap(~Region,scales="free")+
 coord_cartesian(ylim=c(0,NA))+
  tt_stand+
  labs(title = "Sterblichkeit nach Regionen",subtitle = "Die Grippewelle 2018 forderte in Deutschland mehr Tote, in den besonders stark betroffenen Ländern BY und BW sieht man auch Corona-Auswirkungen. In den Niederlanden ist die Übersterblichkeit deutlich erhöht" ,
       caption="Quellen: Statistisches Bundesamt (destatis.de),
       Centraal Bureau for de Statistics (cbs.nl),Office of National Statistics (ons.gov.uk),Bundesamt für Statistik (bfs.admin.ch)")+
  xlab("Kalenderwoche") 
    
