library(shiny)
ui<-fluidPage(
  title="Corona-Dashboard von Sandro",
  theme = "bootstrap.css",
  tags$head(
    tags$style(HTML("ul a {color: white;}")
               )
    ),
  
  
  tags$div( style="background-color:blue; color: white; font-size: 30px","Corona-Reporting von Sandro Grieger" ),
  tags$div( style="background-color:grey; color: white;",
  tabsetPanel(
    tabPanel("Länderentwicklung",
  plotOutput("plot7")),
  tabPanel("Bestätigte Fälle",
           plotOutput("plot1")),
  tabPanel("Bestätigte Fälle täglich",
           plotOutput("plot1a")),
  tabPanel("Entwicklung aktuell Infizierte",
           plotOutput("plot1aa")),
  tabPanel("Tabelle",
           DT::dataTableOutput("table1")),
  tabPanel("Hessen",
           plotOutput("he"),
           textOutput("text_wd")),
  tabPanel("Hessen_LK_Timeline",
           plotOutput("plot_lk_2",height=1000)),
  tabPanel("Case fatality rate", plotOutput("plot2")),
  tabPanel("CFR Altersgruppen", plotOutput("plot14"))
  
  )
  )
)

