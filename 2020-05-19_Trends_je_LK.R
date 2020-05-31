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
plot_12<-test%>%
ggplot(aes(x=as.Date(update),y=`letzte 7 Tage bestätigte Fälle`,col=kat))+geom_point()+
  coord_cartesian(ylim=c(0,NA))+
#  geom_abline(data=test,aes(slope = delta_zeit,intercept =`(Intercept)` ,col=`Kreis/Stadt`))+
  facet_wrap(~`Kreis/Stadt`)+
  scale_colour_manual(values=c("green","blue","red"),labels=c("sinkend","gleichbleibend","steigend"))+
  tt_stand+
  scale_x_date(date_breaks = "1 week", date_labels = "%b-%d")+
  labs(caption=paste0("Quelle Land Hessen, Update: ",now()),title="Trends je Landkreis",subtitle = "Letzte 7 Tage in der zeitlichen Entwicklung")+
  xlab("Datum")
  
