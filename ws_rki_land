library("rvest")
library("tidyverse")
##Extraktor webscraping
url<-"https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html"
rki<-read_html(url)
table<-html_table(rki)
df<-table[[1]]
rki_names<-c("Bundesland",df[1,2],df[1,3],df[1,4],df[1,5],df[1,6])
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
