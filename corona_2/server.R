##new server für corona_2
library(shiny)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)
library(magrittr)
library(tidyr)
library(forcats)
library(stringr)

###Standardtheme erstellen
{tt_stand<-theme(text=element_text(size=14),plot.background = element_rect(fill = "grey25"),
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
)}


#Dataload
##JHU-Data

options(encoding = "UTF-8")

input<-vector()
input$dauer_g<-20
input$dauer_t<-7
input$dauer_t_3<-10





##RKI-Data


agg<-readRDS("corona_2/www/rki.RDS")



plot_tm<-function(land)   { agg%>%
    filter(Bundesland == land)%>%
    filter(estimator=="cfr_1")%>%
    gather("keze","Menschen",c("Anz_conf","Anz_dea","Anz_rec"))%>%
    group_by(Date,Bundesland,keze)%>%
    summarise(Menschen=sum(Menschen))%>%
    ggplot(aes(x=Date,y=Menschen,col=keze),height=600)+  
    scale_color_manual("keze",values = c("blue","red","green"), breaks = c("Anz_conf", "Anz_dea","Anz_rec"),
                                                                 labels = c("Bestätigt", "Gestorben","Genesen"))+
    geom_line()+
    scale_y_continuous(labels=scales::comma)+
    coord_cartesian(ylim=c(0,NA))+  
    ggtitle("Tägliche Veränderung")+
    labs(caption=paste0("Quelle RKI, Update: ",now()),subtitle = "Täglich aktualisierte Coronafälle in der zeitlichen Entwicklung")+
    tt_stand+
    facet_grid(keze~.,scales="free_y")
}
plot_tm_d<-function(land)   { agg%>%
        filter(Bundesland == land)%>%
        filter(estimator=="cfr_1")%>%
        gather("keze","Menschen",c("Anz_conf_1","Anz_dea_1","Anz_rec_1"))%>%
        group_by(Date,Bundesland,keze)%>%
        summarise(Menschen=sum(Menschen))%>%
        ggplot(aes(x=Date,y=Menschen,col=keze))+  scale_color_manual("keze",values = c("blue","red","green"), breaks = c("Anz_conf_1", "Anz_dea_1","Anz_rec_1"),labels = c("Bestätigt", "Gestorben","Genesen"))+
        geom_col()+
        scale_y_continuous(labels=scales::comma)+
        coord_cartesian(ylim=c(0,NA))+  
        ggtitle("Tägliche Veränderung") +
        labs(caption="Quelle Johns-Hopkins-University",subtitle = "Täglich aktualisierte Veränderung der Coronafälle in der zeitlichen Entwicklung")+
        tt_stand
}

##Table mit liste der Aktuellen Schätzer

# update<-jhu_clean%>%
#     ungroup()%>%
#     summarise(upd=max(Date))%>%
#     as.vector()
# 
# aktu_data<-jhu_clean%>%  
#     top_n(1,Date)%>%
#     filter(`Country/Region` %in% (list_top_c$`Country/Region`))%>%
#     spread(estimator,Value)%>%
#     arrange(desc(Anz_conf))%>%
#     mutate(Bestätigte_Gesamt=Anz_conf)%>%
#     mutate(Gestorben_Gesamt=Anz_dea)%>%
#     mutate(Genesen_Gesamt=Anz_rec)%>%
#     mutate(Bestätigte_Tag=Anz_conf_1)%>%
#     mutate(Gestorben_Tag=Anz_dea_1)%>%
#     mutate(Genesen_Tag=Anz_rec_1)%>%
#     mutate(Tote_pro_Bestätigte=round(cfr_1,3))%>%
#     mutate(Tote_pro_Abgeschlossene=round(cfr_2,3))%>%
#     #  mutate(Adv_Estimator=round(cfr_advanced,2))%>%
#     mutate(Adv_Estimator_2=round(cfr_advanced_2,3))%>%
#     #  mutate(Adv_Estimator_3=round(cfr_advanced_3,2))%>%
#     mutate(Verdopplungszeit=round(Verd_3d,1))%>%
#     select(1:2,Bestätigte_Gesamt:Verdopplungszeit)

#he_lk<-read.csv2("/opt/shiny-server/samples/sample-apps/corona/he.csv")
#he_lk<-read.csv2("he.csv")
#stand<-he_lk%>%select(update)%>%mutate(update=ymd_hms(update))%>%summarise(update=max(update))




plot_lk<-function(lk){
    filter(he_lk, str_detect(tolower(Kreis_Stadt),tolower(lk)) )%>%

        ggplot(aes(x = update, y = letzte_7_Tage_Inzidenz,group=Kreis_Stadt),height=300) +
        geom_line(
            size = 0.5
        ) +
        tt_stand + ggtitle("Kurzfristige Entwicklung je Landkreis") +
        xlab("Datum") + ylab("Fälle pro 100.000 in den letzten 7 Tagen") +
        labs(caption = paste0("Quelle Land Hessen, update: ", stand[1, 1]))
    
}

plot_lk_rki<-function(lk){
    filter(agg, str_detect(tolower(Landkreis),tolower(lk)) )%>%
    filter(estimator=="cfr_1")%>%
    group_by(Landkreis,Date)%>%
    summarise(Bestätigt=sum(Anz_conf),Gestorben=sum(Anz_dea),Genesen=sum(Anz_rec))%>%
    gather("Dimension","Anzahl",Bestätigt:Genesen)%>%
        ggplot(aes(x = Date, y = Anzahl,col=Dimension),height=300) +
        geom_line(
            size = 0.5
        ) +
        tt_stand + ggtitle("Tägliche Fälle im Landkreis") +
        xlab("Datum") + ylab("Anzahl") +
        labs(caption = paste0("Quelle RKI, update: "))+
        facet_grid(Dimension~.,scales = "free_y")
    
}
plot_lk_rki_7d<-function(lk){
    filter(agg, str_detect(tolower(Landkreis),tolower(lk)) )%>%
        filter(estimator=="cfr_1")%>%
        group_by(Landkreis,Date)%>%
        summarise(Bestätigt=sum(Anz_conf),Gestorben=sum(Anz_dea),Genesen=sum(Anz_rec))%>%
        gather("Dimension","Anzahl",Bestätigt:Genesen)%>%
        ggplot(aes(x = Date, y = Anzahl,col=Dimension),height=300) +
        geom_line(
            size = 0.5
        ) +
        tt_stand + ggtitle("Kumulierte Fälle im Landkreis") +
        xlab("Datum") + ylab("Anzahl") +
        labs(caption = paste0("Quelle RKI, update: "))+
        facet_wrap(~Dimension,scales = "free_y")
    
}







#plot_lk("rheingau")
#plot_lk_rki("rheingau")
#plot_tm_d("Hamburg")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
output$plot_gesamt<-renderPlot(
    plot_tm(input$land)
)
# output$plot_delta<-renderPlot(
#     plot_tm_d(input$land)
# )
# output$he<-renderPlot(filter(he_lk,Kreis_Stadt!="gesamt")%>%
#                           filter(date(update) %in% c(today(),today()-7))%>%
#                           mutate(Kreis_Stadt=fct_reorder(Kreis_Stadt,letzte_7_Tage_Inzidenz))%>%
#                           ggplot(aes(x=Kreis_Stadt,y=letzte_7_Tage_Inzidenz,group=Kreis_Stadt))+
#                           geom_path(col="grey30",size=0.5,arrow=arrow(type='closed',angle=15,length=unit(0.15,"inches")))+
#                           coord_flip()+
#                           tt_stand+ggtitle("Kurzfristige Entwicklung je Landkreis",subtitle = "Veränderung zur Vorwoche")+
#                           xlab("Kreis")+ylab("Fälle pro 100.000 in den letzten 7 Tagen") +
#                           labs(caption=paste0("Quelle Land Hessen, update: ",stand[1,1]))
# )

# output$plot_lk_2<-renderPlot(plot_lk(input$lk))
output$plot_lk_2<-renderPlot(plot_lk_rki(input$lk))

# output$table1<-DT::renderDataTable(aktu_data)
})
