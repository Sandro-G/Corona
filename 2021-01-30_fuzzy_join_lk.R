###extract bevölkerung nach LK und matche zu den Infektionen
library("rvest")
library("dplyr")
###Exktraktor für Wiki Landkreise
url<-"https://de.wikipedia.org/wiki/Liste_der_Landkreise_in_Deutschland"
rki<-read_html(url)
table<-html_table(rki)
df<-table[[1]]

##Extraktor für wiki städte
url<-"https://de.wikipedia.org/wiki/Liste_der_kreisfreien_Städte_in_Deutschland"
rki<-read_html(url)
table<-html_table(rki)
df_sk<-table[[2]]%>%
  mutate(Stadt=str_extract(Stadt,"^\\w+"))


###liste rki
rki<-readRDS("corona_2/www/rki.RDS")
rki_csv<-read_csv("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv")


rki_lk<-rki%>%
  filter(str_detect(Landkreis,"^LK"))%>%
  mutate(LK=str_replace(Landkreis,"^LK ",""))%>%
  select(Landkreis,LK)%>%
  unique()

###unscharfes matching
install.packages("fuzzyjoin")
library(fuzzyjoin)
library(tidyverse)
library(glue)
joined <- stringdist_join(
  df,
  rki_lk,
  by = c("Landkreis/Kreis" = "LK"),
  method="qgram",
  max_dist = 1,
  distance_col = "distance",
  ignore_case = TRUE
)

resume<-joined%>%
  select(`Landkreis/Kreis`,Landkreis,distance)%>%
  unique()%>%
  mutate(distance=as.factor(distance))
trans<-function(x){
x%>%unlist()%>%glue_collapse(" ")
}
##iterieren über die noch offenen
rki_lk_2<-rki_lk%>%
  anti_join(joined)%>%
  mutate(stem=str_extract_all(LK,"\\w{3,}"))%>%
  mutate(stem=map_chr(rki_lk_2$stem,trans))

df_2<-df%>%
  anti_join(joined)%>%
  mutate(stem=str_extract_all(`Landkreis/Kreis`,"\\w{3,}"))%>%
  mutate(stem=map_chr(df_2$stem,trans))

joined_2 <- stringdist_join(
  df_2,
  rki_lk_2,
  by = "stem",
  method="qgram",
  max_dist = 4,
  distance_col = "distance",
  ignore_case = TRUE
)

resume<-joined_2%>%
  select(`Landkreis/Kreis`,Landkreis,distance)%>%
  unique()%>%
  mutate(distance=as.factor(distance))

rki_lk_3<-rki_lk_2%>%
  anti_join(joined_2)%>%
  mutate(f_LK=str_extract(LK,"^\\w+"))

df_3<-df_2%>%
  anti_join(joined_2)%>%
  mutate(f_LK=str_extract(`Landkreis/Kreis`,"^\\w+"))

joined_3 <- stringdist_join(
  df_3,
  rki_lk_3,
  by = c("f_LK"="f_LK"),
  method="qgram",
  max_dist = 1,
  distance_col = "distance",
  ignore_case = TRUE
)
rki_lk_4<-rki_lk_3%>%
  anti_join(joined_3)

df_4<-df_3%>%
  anti_join(joined_3)


joined_lk<-bind_rows(joined,joined_2,joined_3)

#### Wiederholen für die Städte...
rki_sk<-rki%>%
  filter(str_detect(Landkreis,"^SK"))%>%
  mutate(SK=str_replace(Landkreis,"^SK ",""))%>%
  select(Landkreis,SK)%>%
  unique()

joined <- stringdist_join(
  df_sk,
  rki_sk,
  by = c("Stadt"="SK"),
  method="qgram",
  max_dist = 0,
  distance_col = "distance",
  ignore_case = TRUE
)
rki_sk_2<-rki_sk%>%
  anti_join(joined)

df_sk_2<-df_sk%>%
  anti_join(joined)

resume<-joined%>%
  select(Stadt,SK,distance)%>%
  unique()%>%
  mutate(distance=as.factor(distance))

exp<-joined_lk%>%
  transmute(Landkreis,Einwohner=Einw.,Dichte=`Bev.D.Ew./km²`)
exp_2<-joined%>%
  transmute(Landkreis,Einwohner=EWjetzt,Dichte=`Bev.-dichte(EW/km²)`)

exp_ges<-bind_rows(exp,exp_2)
exp_ges<-readRDS("bev_lk.rds")

exp_ges<-exp_ges%>%
  mutate(Einwohner=str_extract(Einwohner,"^\\d+\\.\\d+"))%>%
  mutate(Einwohner=str_remove(Einwohner,"[.]"))%>%
  mutate(Einwohner=as.numeric(Einwohner))

###es geht auch ohne fuzzy, sogar besser, da die ids dabei sind
exp<-df%>%
  transmute(IdLandkreis=KrS,Einwohner=Einw.,Dichte=`Bev.D.Ew./km²`)
exp_2<-df_sk%>%
  transmute(IdLandkreis=`Stadtkreis ID`,Einwohner=EWjetzt,Dichte=`Bev.-dichte(EW/km²)`)

exp_ges<-bind_rows(exp,exp_2)

exp_ges<-exp_ges%>%
  mutate(Einwohner=str_extract(Einwohner,"^\\d+\\.\\d+"))%>%
  mutate(Einwohner=str_remove(Einwohner,"[.]"))%>%
  mutate(Einwohner=as.numeric(Einwohner))%>%
  mutate(IdLandkreis=str_pad(IdLandkreis,5,"left","0"))

saveRDS(exp_ges,"corona_2/www/bev_lk.rds")
