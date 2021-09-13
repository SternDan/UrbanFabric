#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(raster)
library(lwgeom)
library(rgdal)
library("sf")
library(ggplot2)
library(plotly)
library(ggthemes)
library(OpenStreetMap)
library(maps)
library(leaflet)
library(jsonlite)
library(geojsonio)
library(geojsonR)
library(dplyr)
library(readr)
library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(colorspace)
library(rsconnect)
library(fontawesome)

#library(plyr)

#here::here("UrbanFabric



source("maphelper.R")
source("helpers.R")




ui <- fluidPage(
    
    titlePanel("Urbaner Boden und versiegelte Oberflächen in Berlins Nachbarschaften - Prototyp/Entwurf"),
    
    sidebarLayout(
        
        sidebarPanel(
            verbatimTextOutput("wahlKiez"),
            leafletOutput("kiezmap"),
            h6("Diese App wurde erstellt um den gegenwärtigen Bodenversiegelungszustand in Berlins Nachbarschaften 
        abzubilden und zu ermitteln wieviel Boden bzw. Pflanzensubstrat erforderlich sein wird um mit grüner 
        Infrastruktur klimaangepasstere Nachbarschaften zu gestalten. 
        In der gegenwärtigen Version ist ausschließich der Planungsraum Wrangelkiez in Berlin Kreuzberg erkundbar.
        Die Karten und Diagramme basieren auf Open Data Daten aus dem Geoportal Berlin/[Straßenbefahrung 2014]. Sie 
                Wurden im Rahmen einer vermessungstechnischen Straßenbefahrung in den Jahren 2014 und 2015 erhoben."),
            
            uiOutput("URLtechdetailsStrassenbefahrung"),
            
            
            h6("Autorin: Moreen Willaredt, Kontakt: moreen.willaredt@campus.tu-berlin.de")
            
            ),
            
            
           
           
    mainPanel( 
      
          tabsetPanel( #over all tab set panel
            
            
            tabPanel("Urbaner Boden", icon= icon("hand-holding-heart"),
                     h1(""),
                     h6("Wird zur Zeit bearbeitet -- Überall, wo keine gebaute Infrastruktur den Boden versiegelt, finden wir urbanen Boden: In Baumschreiben, in Grünanlagen, in Parks und Grünflächen."), 
                     h6("Status quo:"), textOutput("urbBodenAnteilstatus"),
                     h1(""),
                     splitLayout(cellWidths = c("50%", "50%"), leafletOutput("UrbaneBodenkarte") ,plotOutput("urbbodenPlot")),
                     
                     #splitLayout(cellWidths = c("50%", "50%"),textOutput("BS_transFaktor"), textOutput("begr")),
                     h1(""),
                     h1(""),
                      textOutput("urbBodenAnteiltrans"),
                     h1(""),
           
            
               tabsetPanel(
                       
                       #    tabPanel("Flächenentsiegelung", icon = icon("road"),),
                          
                       
                       #Layout, Baumscheiben transform ---------------------------------------------              
                       
                       tabPanel("Flächentransformationen - Baumscheiben vergrößern",icon = icon("leaf"),
                                h1(""),
                                splitLayout(cellWidths = c("45%", "45%"),       
                                            sliderInput("BSfaktor", "Baumscheibengröße in % (100% ist Ausgangsgröße):",
                                                        min =100, max = 300,
                                                        value =100, step = 10),
                                            sliderInput("BodTiefe", "Aushubtiefe nach Entsiegelung in m",
                                                        min =0.1, max = 1,
                                                        value =0.5, step = 0.1)
                                            
                                      ), #splitLayout
                                
                                h1(""), #horizontal whitespace
                                actionButton(
                                  inputId = "transformBS",
                                  label = "transform",
                                  icon("leaf"),
                                  class = "btn btn-success",
                                  width = "33%"
                                ),
                               
                                 h1(""), #horizontal whitespace
                                
                                      tableOutput("BSextendBodendemand") 
                                
                               
                                
                       ), # tabPanel Baumscheiben
                       
                   #Layout Dachbegrünung trans---------------------    
                   h1(""),
                       tabPanel("Flächentransformation Dächer begrünen",
                                splitLayout(cellWidths = c("45%", "45%"),    
                                            sliderInput("DachTeil", "Anzahl der Gebäude mit Gründach:",
                                                        min = round(length(Surfaces_subset$Gebaeude$GRUENDACH[Surfaces_subset$Gebaeude$GRUENDACH == "vorhanden"]),digits = 1), max = length(Surfaces_subset$Gebaeude$GRUENDACH),
                                                        value = round(length(Surfaces_subset$Gebaeude$GRUENDACH[Surfaces_subset$Gebaeude$GRUENDACH == "vorhanden"]),digits = 1), step = 5),
                                            sliderInput("Substrathoehe", "Substrathöhe in cm:",
                                                        min = 2, max = 50,
                                                        value = 2, step = 2) 
                                            
                                        ), #splitLayout
                                
                                h1(""),
                                 
                                           
                                          
                                actionButton(
                                  inputId = "transformGD",
                                  label = "transform",
                                  icon("leaf"),
                                  class = "btn btn-success",
                                  width = "20%"
                                ),
                                
                                h1(""),
                                tableOutput("GDextendBodendemand")
                                #tableOutput("GD_transFaktorTab") 
                                
                       ), #TabPanel Gründach
                       
                       tabPanel("Flächentransformation Fassaden begrünen",
                                h6("in Bearbeitung"),
                                sliderInput("FassadenZahl", "Anzahl Gebäude für Fassadenbegrünung:",
                                            min = 1, max = length(Surfaces_subset$Gebaeude$gml_id),
                                            value = 1, step = 1),
                                
                                
                                sliderInput("FassadeVolumen", "Bodenvolumen pro Fassadenbegrünung in m³:",
                                            min = 0.8, max = 1.5,
                                            value = 1, step = 0.1),
                                
                                
                                
                                tableOutput("Fass_transFaktor")  )#Tabpanel Fassade
                   ), #tabSetPanel transformers
            
          ), #tabPanel Urbaner Boden
                       
            
            
              tabPanel("Flächeninventur in Diagrammen", icon = icon("bar-chart-o"),
                       h1(""),
                       textOutput("anleitungHover"),
                       h1(""),
                  splitLayout(cellWidths = c("50%", "50%"),  leafletOutput("erkunden",height="100vh"),  plotOutput("gesamtFlaechenPlot")),
                  splitLayout(cellWidths = c("50%", "50%"),  plotOutput("oeffRaumFlaechenPlot") , plotOutput("parkenPlot"))
                      
              ), #tabPanel Flächeninvent
              
              tabPanel("Versiegelung", icon = icon("road"), 
                       
                   splitLayout(cellWidths = c("50%", "50%"),   leafletOutput("versiegelt",height="100vh"),    plotOutput("versiegelungsPlot2")) ,
                   splitLayout(cellWidths = c("50%", "50%"), plotOutput("versiegelungsPlot") ,plotOutput("versiegelungsPlot3")),
                               verbatimTextOutput("VGklassen")
                       
              ), #tabpanel versiegelung
          
              tabPanel("Straßenbäume", icon = icon("leaf"),
                       h1(""),
                       h6("in Bearbeitung"),
                       h1(""),
                       plotOutput("baeumePlot"),
                       plotOutput("baumartenPlot"),
                       plotOutput("baeumeproStrPlot"),
                       plotlyOutput("Baeumealter_strPlot")
              
              #tabPanel("Versiegelung", plotOutput("versiegelungsPlot")),
               
              
              
            #  tabPanel("Transformation",icon = icon("forward"),
                 #      h6("Wird zur Zeit bearbeitet")
             # )
              
              #verbatimTextOutput("trans_summary"))
              #verbatimTextOutput("summary")
            
         
         
          
       
          
          )#tab panel straßenbäume
         
            
           
               
              )#over all tab set panel
    
   
             )#main panel
          )#side bar layout
     )#fluid page


# Define server logic ----
server <- function(input, output) {
  
  BS_select <-Surfaces_subset$Baumscheiben #select all
  BS_trans <-  st_transform(BS_select, crs=5243)#ETRS89 / LCC Germany (E-N)
  
  
  
    
  ##inputs -------------------------------------------
  ## processing - Baumscheiben transform----------------------------
  toListen <- reactive({
    list(input$transformBS,input$transformGD) })
  
  observeEvent(toListen(), {
    
   
    ### slider inputs ----------
     BSsliderFaktor <-  reactive({ input$BSfaktor })
     BS_Bodentiefe <- reactive({ input$BodTiefe  })    
    
    
    
    
           # output$BS_transFaktor <- renderText({ paste("Baumscheiben vergrößern um", BSsliderFaktor()-100, "%" ,sep=" ")
           # })
              
            BS_extend <- st_buffer(BS_trans, sqrt(((BSsliderFaktor()-100)/100*st_area(BS_trans)+pi*st_area(BS_trans)/pi)/pi)-sqrt(st_area(BS_select)/pi))
            BS_extend_trans <-st_transform(BS_extend, crs= 4326)
            total_extendedBS <- sum(as.numeric(round(st_area(BS_extend_trans),digits=1)))- sum(as.numeric(st_area(Surfaces_subset$Baumscheiben)))
            
            urbBoden_anteile$area[urbBoden_anteile$Group == "Baumscheiben"] <- urbBoden_anteile$area[urbBoden_anteile$Group == "Baumscheiben"]+ total_extendedBS
            
    # Bodenbedarf berechnen-----------------------
            output$BSextendBodendemand <- renderTable({ 
                          Boden_extendedBS <- data.frame("prozentuale Vergrößerung" = BSsliderFaktor()-100,
                                       "Tiefe" = BS_Bodentiefe(),
                                       "zu entsiegelnde Gesamtfläche" = total_extendedBS,
                                       "Bodenbedarf in m³" =  BS_Bodentiefe()*total_extendedBS,
                                       "LKW-Ladungen" = 0.9*BS_Bodentiefe()*total_extendedBS/30)
                            })
        
        output$urbbodenPlot <- renderPlot({
          
          ggplot(data=urbBoden_anteile, aes(reorder(Group,-area),area/1000)) + 
            geom_col(color="black",aes(fill = class),alpha=0.5)+
            ggtitle("Flächen mit unversiegeltem Boden")+
            geom_text(aes(
              label= paste(round(area/1000, digits = 1),sep="")),
              position = position_dodge(0.9),
              vjust = -1)+
            theme_bw()+
            theme(axis.text.x=element_text(angle=45, hjust=1),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
            labs(x="",y= "Fläche in 1000 m²") +
            scale_y_continuous(limits=c(0,urbBoden_anteile$area[urbBoden_anteile$Group=="Gesamtfläche"]*1.1/1000),breaks = seq(0, urbBoden_anteile$area[urbBoden_anteile$Group=="Gesamtfläche"]*1.1/1000, by = 100))+
            scale_fill_manual(name = "",values = c("0"= VG_class_0, "1"= VG_class_1,"2"= VG_class_2,  "3"=VG_class_3, "4"= VG_class_4, "NA"=VG_class_NA))
                })
        
          #output$urbBodenAnteiltrans <- renderText({ paste("nach Transformation: Anteil unversiegelter Fläche an Gesamtfläche:", round( sum(urbBoden_anteile$area[urbBoden_anteile$Group!="Gesamtfläche"])/urbBoden_anteile$area[urbBoden_anteile$Group=="Gesamtfläche"]*100 ,digits=1), "%" ,sep=" ") 
            #      })
   
        
  
         output$UrbaneBodenkarte <- renderLeaflet({
           
             UrbaneBodenkarte%>%
             leaflet::addPolygons(
               data =BS_extend_trans,  # LAD polygon data from geojson
               weight = 1,  # line thickness
               opacity = 1,  # line transparency
               color = "green",  # line colour
               fillOpacity = 0.8,
               fillColor = treeCol,
               label =paste("vergrößert", round(st_area(BS_extend_trans),digits=1), "m²", sep=" ")
             ) })
         
     #   })
    
         
         
         
         
         
   
  ## processing Gruendach transform-------------------------------       
    
   #observeEvent(input$transformGD, {   
    
     #### slider input-----
      GD_DachTeil <- reactive({
        input$DachTeil - length(Surfaces_subset$Gebaeude$GRUENDACH[Surfaces_subset$Gebaeude$GRUENDACH == "vorhanden"])
              })
      
      GD_Substrathoehe <- reactive({
        input$Substrathoehe/100
      })
     
      output$GD_transFaktorTab <- renderTable({
        
        data.frame(
          Name = c("Substratdicke",
                   "Anzahl gewählte Gebäude"),
          Value = c(GD_Substrathoehe(),
                    GD_DachTeil()
          ),
          stringsAsFactors = FALSE)
        
       })
     
      begruenbar <- Surfaces_subset$Gebaeude[Surfaces_subset$Gebaeude$GRUENDACH=="nicht vorhanden", ] 
      begruenbar_select <-  begruenbar[sample(nrow(begruenbar),GD_DachTeil() ), ]
      
      total_extendedGD <- sum(as.numeric(st_area(begruenbar_select)))
      
      
      #output$begr <- renderText( paste("Anzahl zu begrünender Gebäudedächer:", {length(begruenbar_select$GRUENDACH)}),sep="")
         
      urbBoden_anteile$area[urbBoden_anteile$Group == "Gruendach"] <- urbBoden_anteile$area[urbBoden_anteile$Group == "Gruendach"]+ total_extendedGD*0.3
      
      output$UrbaneBodenkarte <- renderLeaflet({
        
        UrbaneBodenkarte%>%
          leaflet::addPolygons(
            data =begruenbar_select,  # LAD polygon data from geojson
            weight = 1,  # line thickness
            opacity = 1,  # line transparency
            color = "green",  # line colour
            fillOpacity = 0.8,
            fillColor = treeCol,
            label =paste("Zur begruenung gewähltes Gebäude mit potentieller Dachfläche", 0.3*round(st_area(begruenbar_select),digits=1), "m²", sep=" ")
          ) 
        })
      
      
      output$urbbodenPlot <- renderPlot({
        
        ggplot(data=urbBoden_anteile, aes(reorder(Group,-area),area/1000)) + 
          geom_col(color="black",aes(fill = class),alpha=0.5)+
          ggtitle("Flächen mit unversiegeltem Boden")+
          geom_text(aes(
            label= paste(round(area/1000, digits = 1),sep="")),
            position = position_dodge(0.9),
            vjust = -1)+
          theme_bw()+
          theme(axis.text.x=element_text(angle=45, hjust=1),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
          labs(x="",y= "Fläche in 1000 m²") +
          scale_y_continuous(limits=c(0,urbBoden_anteile$area[urbBoden_anteile$Group=="Gesamtfläche"]*1.1/1000),breaks = seq(0, urbBoden_anteile$area[urbBoden_anteile$Group=="Gesamtfläche"]*1.1/1000, by = 100))+
          scale_fill_manual(name = "",values = c("0"= VG_class_0, "1"= VG_class_1,"2"= VG_class_2,  "3"=VG_class_3, "4"= VG_class_4, "NA"=VG_class_NA))
      })
   
  
      output$urbBodenAnteiltrans <- renderText({ paste("Anteil unversiegelter Fläche an Gesamtfläche:", 
                                                       round( sum(urbBoden_anteile$area[urbBoden_anteile$Group!="Gesamtfläche"])/urbBoden_anteile$area[urbBoden_anteile$Group=="Gesamtfläche"]*100 ,digits=1),"%,", 
                                                       "Anzahl zu begrünender Gebäudedächer:", length(begruenbar_select$GRUENDACH), ",",
                                                        "Baumscheiben vergrößern um", BSsliderFaktor()-100, "%" ,sep=" ")})
      
     
      
# Bodenbedarf berechnen----------------
      
      output$GDextendBodendemand <- renderTable({ 
        Boden_extendedGD <- data.frame("Anzahl begrünter Gebäude" = length(begruenbar_select$GRUENDACH),
                                       "Substratdicke" = GD_Substrathoehe(),
                                       "begrünbare Dachfläche" = total_extendedGD*0.3,
                                       "Bodenbedarf in m³" =  GD_Substrathoehe()*total_extendedGD*0.3,
                                       "LKW-Ladungen" = 0.9*GD_Substrathoehe()*total_extendedGD*0.3/30) # 30t pro LKW
      })
      
      
          
      
})
  
###Outputs---------------
  output$UrbaneBodenkarte <- renderLeaflet({
   
    UrbaneBodenkarte
  })
  
  output$urbbodenPlot <- renderPlot({
    
    urbbodenPlot
    
  })
    

  
 
  
  Fassadengebaeudezahl <- reactive({
    input$FassadenZahl
    
      })
  
  bodVolumenFassade <- reactive({
    input$FassadeVolumen
    
  })
  
 

  
    #outputs text------------------------------------------
  
  urlStr2014 <- a("PDF Straßenbefahrung", href= "https://fbinter.stadt-berlin.de/fb_daten/beschreibung/datenformatbeschreibung/Datenformatbeschreibung_Straßenbefahrung_2014.pdf")
    output$URLtechdetailsStrassenbefahrung <- renderUI({tagList("Technische Beschreibung der Daten aus der Straßenbefahrung:", urlStr2014)})  
     
    
    
    output$wahlKiez <- renderText({ paste("Wähle eine Nachbarschaft aus der Karte:","(In der gegenwärtigen Version ausschließlich Wrangelkiez möglich)" ,kiez_oi$PLANUNGSRAUM,sep="\n")
    })
    
    output$anleitungHover <- renderText({paste("Fahre mit der Maus über die Karte um dir anzeigen zu lassen wie die Nachbarschaft beschaffen ist" ,sep=" ")
    })
    
  
   
    output$urbBodenAnteilstatus <- renderText({ paste("ca",round(sum(urbBoden_anteile$area[urbBoden_anteile$Group!="Gesamtfläche"])/urbBoden_anteile$area[urbBoden_anteile$Group=="Gesamtfläche"]*100, digits= 1),  " % der Fläche beherbergt unversiegelten Boden und kann bodenspezifische Ökosystemdienstleistungen erbringen",  sep=" ")
    })
    
  
 
    
    output$Fass_transFaktor <- renderText({ paste("Fassadenbegrünung an", round(Fassadengebaeudezahl()/length(Surfaces_subset$Gebaeude$gml_id)*100 , digits = 1), "% der Gebäude im Kiez" ,sep=" ")
    })
    
    
    output$VGklassen <- renderText({ paste("Versiegelungsklassen nach Timm et al. 2018" ,sep=" ")
    })
    
    #outputs karten------------------------------------------  
    
    output$kiezmap <- renderLeaflet({
        
        kiezmap
        
        
        
    })
    
    output$erkunden <- renderLeaflet({
        karte
        
    })
    
    output$versiegelt <- renderLeaflet({
      Versiegelungskarte
      
    }) 
    
    
  
    
    
  
    #output$BS_transform <- renderLeaflet({
      
  
      #default in current version: Alle baumscheiben

      #total_extendedBS <- sum(round(st_area(BS_extend_trans),digits=1))- st_area(Surfaces_subset$Baumscheiben)

  
      #check how much bigger
      #st_area(BS_extend)/st_area(BS_select)
      
      
      #visualize
      # kiez_BS_transform <- map %>%
      #   leaflet::addPolygons(
      #     data =kiez_oi,  # LAD polygon data from geojson
      #     weight = .7,  # line thickness
      #     opacity = 1,  # line transparency
      #     color = "grey",  # line colour
      #     fillOpacity = 0.2,
      #     fillColor = "gray",
      #     label = paste("Transformationsraum", kiez_oi$PLANUNGSRAUM,  ", Gesamtfläche der Baumscheiben:", sum(round(st_area(BS_extend_trans),digits=1)), "m²","entspricht", round(sum(round(st_area(BS_extend_trans),digits=1))/(68*105) ,digits = 1),"Fussballfeldern" ,sep = " ")
      #   )%>%
      #   leaflet::addPolygons(
      #     data =BS_extend_trans,  # LAD polygon data from geojson
      #     weight = 1,  # line thickness
      #     opacity = 1,  # line transparency
      #     color = "green",  # line colour
      #     fillOpacity = 0.8,
      #     fillColor = treeCol,
      #     label =paste("vergrößert", round(st_area(BS_extend_trans),digits=1), "m²", sep=" ")
      #   )%>%
      #   leaflet::addPolygons(
      #     data =Surfaces_subset$Baumscheiben,  # LAD polygon data from geojson
      #     weight = 1,  # line thickness
      #     opacity = 1,  # line transparency
      #     color = "green",  # line colour
      #     fillOpacity = 0.5,
      #     fillColor = "green",
      #     label = paste("orginal", round(Surfaces_subset$Baumscheiben$flaeche,digits=1), "m²", sep=" ")  # LAD name as a hover label
      #   )
      # 
      # kiez_BS_transform
      
      
     
  #  })
    
    
    
    
    
    
    #LKWanzahl <- reactive({0.9*BS_Bodentiefe()})
    
    # versuch die anzahl der LKW lieferungen als icons darzustellen
    # output$truck2 <-   DT::renderDataTable({
     #                         data.frame("LKW" = c(rep(as.character(icon("child")), 3 ))
     #                                    )
     #                   }, escape=FALSE)
                              
       
      
     
     
    
    #outputs plots------------------------------------------
    output$gesamtFlaechenPlot <- renderPlot({ 
        gesamtFlaechenPlot 
        
    })
    
    output$oeffRaumFlaechenPlot <- renderPlot({ 
        oeffRaumFlaechenPlot
        
    })
    
    
    output$parkenPlot <- renderPlot({ 
        parkenPlot
        
    })
    
    output$baeumePlot <- renderPlot({ 
        baeumePlot
        
    })
    
    output$baumartenPlot <- renderPlot({ 
        baumartenPlot
        
    })
    output$baeumeproStrPlot <- renderPlot({ 
        baeumeproStrPlot
        
    })
    
    output$Baeumealter_strPlot <- renderPlotly({ 
      
      ggplotly(baeumealterPlot_str)
  
    })
    
    output$versiegelungsPlot <- renderPlot({ 
      versiegelungsPlot
        
    })
    
    output$versiegelungsPlot2 <- renderPlot({ 
        versiegelungsPlot2
        
    })
    
    output$versiegelungsPlot3 <- renderPlot({ 
      versiegelungsPlot3
      
    }) 

    
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
