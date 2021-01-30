
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = "bootstrap.css",

    # Sidebar with a slider input for number of bins
    navbarPage("Sandros Corona-Dashboard",
            tabPanel("Entwicklung Deutschland",
                     
                     sidebarLayout(
                         sidebarPanel("Auswahl",
                                      selectInput("land","Bundesland auswählen",choices =c("Baden-Württemberg", "Bayern","Berlin"                
                                                  ,"Brandenburg", "Bremen" , "Hamburg"               
                                                  , "Hessen"  , "Mecklenburg-Vorpommern" ,"Niedersachsen"         
                                                  , "Nordrhein-Westfalen" ,"Rheinland-Pfalz"  ,"Saarland"              
                                                  , "Sachsen"  , "Sachsen-Anhalt" , "Schleswig-Holstein"    
                                                  , "Thüringen") ,selected = "Hessen")
                                      ),
                         mainPanel(
                             plotOutput("plot_gesamt",height = 600),
                             plotOutput("plot_delta")
                         )
                     )),
            tabPanel("Entwicklung Landkreise",                     
                sidebarLayout(
                    sidebarPanel("Auswahl",
                                 selectInput("lk","Landkreis auswählen",choices = c("Rheingau","Wiesbaden","Frankfurt","SK Mainz","Westerwald"),selected = "Rheingau")
                    ),
                    mainPanel(
                        tags$h2("Entwicklung der Corona-Zahlen je Landkreis"),
                        # plotOutput("he"),
                         plotOutput("plot_lk_2")
                        )
                    )
                ),
            
            tabPanel("Tabelle",
                     DT::dataTableOutput("table1"))
    )
))
