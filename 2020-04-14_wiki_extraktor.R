library(httr)
library(pageviews)
library(tidyverse)
library(lubridate)

url<-"https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia.org/all-access/all-agents/Elton_John/daily/20170101/20200101"

klick_wiki<-function(url)
{
  # Make a GET request to url and save the results
  pageview_response <- GET(url)
  
  # Call content() to retrieve the data the server sent back
  pageview_data <- content(pageview_response,as="text")
  ##parse to json
  elton_raw<-jsonlite::fromJSON(pageview_data)
  elton<-elton_raw$items%>%
    select(article,timestamp,views)%>%
    mutate(timestamp=ymd(substr(timestamp,1,8)))
  
  return(elton)
}


wiki_list<-function(list,c="de")
  {
df<-data.frame(views=1,article="",timestamp=ymd("2000-01-01"),stringsAsFactors = FALSE)%>%
  filter(views!=1)
for (l in list){
  url_queen<-paste0("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",c,".wikipedia.org/all-access/all-agents/",l,"/daily/20070101/20200414")
#  url_queen<-paste0("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia.org/all-access/all-agents/",l,"/daily/20070101/20200414")
  
  queen<-klick_wiki(url_queen)
  df<-bind_rows(df,queen)
}
return(df)
}
list_en<-list("Elton_John","Freddie_Mercury","Donald_Trump","Angela_Merkel","Boris_Johnson","Heiko_Maas")
list<-list("Stundung","Kurzarbeit","Corona","COVID-19-Pandemie_in_Deutschland","COVID-19")

auswertung<-wiki_list(list)
auswertung<-wiki_list(list_en,c="en")

auswertung%>%
ggplot(aes(x=timestamp,y=views,col=article))+geom_line()+scale_y_continuous(labels=scales::comma)+facet_wrap(~article,scales="free")+
    tt_stand+
  ggtitle("Klickzahlen der Wikipedia")












