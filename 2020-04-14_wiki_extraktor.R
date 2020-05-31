library(httr)
library(tidyverse)
library(lubridate)

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
  queen<-klick_wiki(url_queen)
  df<-bind_rows(df,queen)
}
return(df)
}
list_en<-list("Elton_John","Freddie_Mercury","Donald_Trump","Angela_Merkel","Boris_Johnson","Heiko_Maas")
list<-list("Stundung","Kurzarbeit","Corona","COVID-19-Pandemie_in_Deutschland","COVID-19","Mund-Nasen-Schutz_(Medizin)","Schulpflicht")


#######Block zum Test und zur Visiualisierung. Nicht essentiell
auswertung<-wiki_list(list)
auswertung<-wiki_list(list_en,c="en")

wiki_list("Schulpflicht")
auswertung%>%
ggplot(aes(x=timestamp,y=views,col=article))+geom_line()+scale_y_continuous(labels=scales::comma)+facet_wrap(~article,scales="free")+
  ggtitle("Klickzahlen der Wikipedia")
list<-c("Weihnachten","Karneval","Ostern","Pfingsten","Christi_Himmelfahrt","Fronleichnam")
feste<-wiki_list(list)

#######Block Ende


#####Dashboard
library(shiny)
ui<-fluidPage(
  includeCSS("styles.css"),
  titlePanel("Wiki_Extraktor by BE-Data-Sciene", windowTitle="Wiki_Extraktor by BE-Data-Sciene"),
  
    sidebarLayout(
    sidebarPanel(
      
    textInput("wiki","Gib den Suchbegriff ein"),
    selectInput("lang","Sprache",choices=c("de","en"),selected = "de"),
    actionButton("send","Ausführen")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title="Eigene Auswahl",value="tab1",
        plotOutput("plot2")
        )
        ,
        tabPanel(title="Feste",value="tab3",
                 plotOutput("feste")  
        )
        ,
        tabPanel(title="Forderungsmanagement",value="tab2",
                 plotOutput("plot")                 
        )
      )
    )
  )
  
)

server<-function(input,output){
  output$plot<-renderPlot(auswertung%>%
                            ggplot(aes(x=timestamp,y=views,col=article))+geom_line()+scale_y_continuous(labels=scales::comma)+facet_wrap(~article,scales="free")+
                            ggtitle("Klickzahlen der Wikipedia")+
                            tt_stand)
  output$feste<-renderPlot(feste%>%
                            ggplot(aes(x=timestamp,y=views,col=article))+geom_line()+scale_y_continuous(labels=scales::comma)+facet_wrap(~article,scales="free")+
                            ggtitle("Klickzahlen der Wikipedia")+
                            tt_stand)
##to do: gestalte reactive so, dass nur bei bestätigungsbutton der get-request abgefeuert wird  
    wiki_call<-eventReactive(input$send,{wiki_list(input$wiki,input$lang)})
    output$plot2<-renderPlot(wiki_call()%>%
                            ggplot(aes(x=timestamp,y=views,col=article))+geom_line()+scale_y_continuous(labels=scales::comma)+facet_wrap(~article,scales="free")+
                            ggtitle("Klickzahlen der Wikipedia")+
                              tt_stand)
}
shinyApp(ui,server)











