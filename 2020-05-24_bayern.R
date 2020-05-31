library(readxl)
library(httr)
library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
url<-"https://www.lgl.bayern.de/gesundheit/infektionsschutz/infektionskrankheiten_a_z/coronavirus/karte_coronavirus/index.htm"
b<-read_html(url)
c<-html_table(b)
for (i in 1:7){
  print(head(c[[i]]))
}
head(c[[2]])
tested<-c[[5]]
death<-c[[6]]
ges<-bind_rows(list(tested=tested,death=death),.id="source")

ber<-function(x){
  return(ifelse(trunc (x)==x,x,x*1000 ))
}
ges<-ges%>%
  mutate_each(ber,weiblich,männlich,unbekannt)
ausw<-ges%>%
  filter(!str_detect(`Altersgruppe (Jahre)`,"esamt"))%>%
  gather("Geschlecht","Anzahl",c("weiblich","männlich","unbekannt"))%>%
  spread(source,Anzahl)%>%
  mutate(Quote=death/tested)%>%
  filter(Geschlecht!="unbekannt")%>%
  filter(`Altersgruppe (Jahre)`!="Unbekannt")
plot_14<-ggplot(ausw,aes(x=`Altersgruppe (Jahre)`,y=Quote,col=Geschlecht,group=Geschlecht))+geom_point()+geom_line()+
  tt_stand+
  scale_y_continuous(labels=scales::percent)+
  labs(title="Case Fatalaty Rate nach Altersklassen",subtitle = "Auswertungen aus bayrischen Daten")+
  ylab("Todesfälle pro bestätigte Fälle")

lk_by<-c[[3]]

ausw_lk<-lk_by%>%
  mutate(fp100=as.numeric(str_replace(`Fallzahl pro 100.000 Einwohner`,",",".")))%>%
  ##tirschenreuth noch nicht richtig verarbeitet, punkt - comma problem
  mutate(tote=as.numeric(str_replace(`Anzahl der Todesfälle`,"-","0")))%>%
  mutate(anz=ber(`Anzahl der Fälle`))%>%
  mutate(quote=tote/anz)%>%
  mutate(ausbruchsstärke=cut(fp100,breaks=c(0,500,750,1500)))%>%
  group_by(ausbruchsstärke)%>%
  summarise(quote_ges=sum(tote,na.rm = TRUE)/sum(anz,na.rm = TRUE))
ggplot(ausw_lk,aes(x=ausbruchsstärke,y=quote_ges))+geom_point()+ylim(0,0.25)+geom_smooth()

write.csv(tested,paste0("C:/Users/Sandro/Documents/R/Skripte/Corona/bayern/anz_test_",Sys.Date(),".csv"))
write.csv(death,paste0("C:/Users/Sandro/Documents/R/Skripte/Corona/bayern/anz_death_",Sys.Date(),".csv"))
write.csv(lk_by,paste0("C:/Users/Sandro/Documents/R/Skripte/Corona/bayern/lk_by_",Sys.Date(),".csv"))
