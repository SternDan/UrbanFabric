
Kieze <- st_read("data/lor_planungsraeume.geojson")
Surfaces_subset <- readRDS("Surfaces_subset.rds")
surfaceMaterial2 <- read_delim("data/material2.txt", ";", escape_double = FALSE, trim_ws = TRUE)
kiez_oi <- subset(Kieze, PLANUNGSRAUM == "Wrangelkiez" )
Surfaces_subset[["Baustelle"]][["material"]] <- 17
Surfaces_subset[["Gruenanlagen"]][["material"]] <- 16
Surfaces_subset[["Gruenflaeche"]][["material"]] <- 15
Surfaces_subset[["Gehweg"]][["VersFarbe"]] <- surfaceMaterial2$farbe[match(Surfaces_subset$Gehweg$material,surfaceMaterial2$material)]


#defaults

#BS_extend_trans <- Surfaces_subset$Baumscheiben





#Farbgebung--------------------------------



parkCol <-rgb(33, 94, 143, maxColorValue = 255)
radCol <- rgb(255,0,0, maxColorValue = 255)
treeCol <- rgb(107,171,5, maxColorValue = 255)

hausCol <- rgb(128,0,128, maxColorValue = 255)
baustCol <- rgb(255,0,0 ,maxColorValue = 255)
fahrbCol <- rgb(128,128,128 ,maxColorValue = 255)
gehwCol <- rgb(255,255,0 ,maxColorValue = 255)
kiezCol <-  rgb(0,128,0, maxColorValue = 255)
spielpCol <-  rgb(255,192,203, maxColorValue = 255)
gruenanlCol <- rgb(0,255,0 ,maxColorValue = 255)
gruenflCol <-rgb(0,255,0 ,maxColorValue = 255)
gruendCol <-rgb(0,255,0 ,maxColorValue = 255)
oeffplCol <- rgb(128,128,128 ,maxColorValue = 255)
gehwuebCol <- rgb(255,165,0,maxColorValue = 255)
gehwCol <-rgb(255,255,0,maxColorValue = 255)

flaechTypFarben <- list(parkCol = rgb(33, 94, 143, maxColorValue = 255),
                        radCol = rgb(255,0,0, maxColorValue = 255))

farbliste <- list(FTfarbe = flaechTypFarben  )


mapOp <- 0.5

# VG_class_0 <-rgb(242,240,247, maxColorValue = 255)
# VG_class_1 <-rgb(203,201,226, maxColorValue = 255)
# VG_class_2 <-rgb(158,154,200, maxColorValue = 255)
# VG_class_3 <-rgb(117,107,177, maxColorValue = 255)
# VG_class_4 <-rgb(84,39,143, maxColorValue = 255)

VG_class_0 <-treeCol  #1    
VG_class_1 <-"darkolivegreen1" #2
VG_class_2 <-'yellow'#4
VG_class_3 <-'orange2' #4
VG_class_4 <-'coral3' #5
VG_class_NA <- "gray69"


#VGklassenFarben<- list(VG_class_0 = treeCol , #1    
 #                      VG_class_1 ='#80cdc1' #2
#)


labeldata <- surfaceMaterial2[order(surfaceMaterial2$order),] 
#labeldata <- labeldata[order(newdata$V_grad),] 



pal_VG <- colorFactor(
  c("#6BAB05",  "darkolivegreen3",   "darkolivegreen1", "darkolivegreen2"  ,"yellow3",      "yellow2",  
              "yellow"  ,  "lightgoldenrod1",   "orange",  "orange1" ,  "orange2", "orange3" , 
              "coral2" ,    "coral3",     "coral1",   "coral",   "orangered3"  , "gray69"),
  domain = labeldata$order
) 


VGartFarben <- list(
col_mat_geb = "orangered3",  # 0;  Gebaeude;4
col_mat_bet = "coral3", # 1;   Beton;4
col_mat_asph = "coral2", # 2;   Asphalt;4
col_mat_mos = "yellow3", # 3;   Mosaikpflaster (ca. 50/50mm);2
col_mat_klStpfl = "yellow2", # 4;   Kleinsteinpflaster (ca. 90/90 mm);2
col_mat_grStpfl = "lightgoldenrod1" ,# 5;   Grosssteinpflaster (ca. 160/160-220mm bzw. 120/120-180mm);2
col_mat_betPf = "yellow" ,# 6;   Betonpflaster;2
col_mat_gehwPlB = "orange", # 7;   Gehwegplatten Beton (350/350mm);3
col_mat_gewhPlN =  "orange1" ,# 8;   Gehwegplatten Naturstein;3
col_mat_grGranPl =  "orange2", # 9;   Granitplatte Grossformat (alt/neu);3
col_mat_grBetPl =  "orange3", # 10;  Betonplatte Grossformat (>350/350mm);3
col_mat_aspAP = "coral", # 11;  Asphaltueberzug auf Pflaster;4
col_mat_aspAB = "coral1", # 12;  Asphaltueberzug auf Beton;4
col_mat_wasgebD = "darkolivegreen2", # 13;  wassergebunden Decke;1
col_mat_unvers = "darkolivegreen3", # 14;  unversiegelt (Sand etc.);0
col_mat_gruen = rgb(107,171,5, maxColorValue = 255), # 15;  Gruen;0
col_mat_befMisch = "darkolivegreen1", # 16;  befestigte Mischflaeche;1
col_mat_NA = "gray69" # 17;  sonstiges Material;NA
)







#tiles:----------------------------------------

map <- leaflet::leaflet() %>%
  leaflet::addProviderTiles(providers$OpenStreetMap)


#explorer map-----------------------------------------

karte <- map %>%
  leaflet::addPolygons(
    data =kiez_oi,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = 0.2,
    fillColor = "green",
    label = paste("Wrangelkiez, Gesamtfläche",round(st_area(kiez_oi)), "m²",sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gebaeude,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "purple",  # line colour
    fillOpacity = mapOp,
    fillColor = hausCol,
    label = paste("Gebaeude, Gründach", Surfaces_subset$Gebaeude$GRUENDACH,  sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baustelle,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = baustCol,
    label = "Baustelle"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Radweg,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "blue",  # line colour
    fillOpacity = 0.8,
    fillColor = radCol,
    label = paste("Radweg aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Radweg$material, surfaceMaterial2$material)] ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data = Surfaces_subset$Fahrbahn,  # polygon data from geojson
    weight = 0,  # line thickness
    opacity = 1,  # line transparency
    color = "Purple",  # line colour
    fillOpacity = mapOp,
    fillColor = fahrbCol,
    label= paste("Fahrbahn aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Fahrbahn$material, surfaceMaterial2$material)], Surfaces_subset$Adressen$str_name[st_nearest_feature(Surfaces_subset$Fahrbahn,Surfaces_subset$Adressen)], sep = " ")
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gehweg,  # LAD polygon data from geojson
    weight = 0.5,  # line thickness
    opacity = 0.8,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = gehwCol,
    label = paste("Gehweg aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Gehweg$material, surfaceMaterial2$material)],sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gehwegsueberfahrt,  # LAD polygon data from geojson
    weight = 0.5,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = gehwuebCol,
    label = paste("Gehwegsüberfahrt aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Gehwegsueberfahrt$material, surfaceMaterial2$material)],sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Parkplatzflaeche,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = parkCol,
    label = paste("Parkplatz aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Parkplatzflaeche$material, surfaceMaterial2$material)],round(Surfaces_subset$Parkplatzflaeche$flaeche/12), "Stellplätze" , sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$oeffentlicherPlatz,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = oeffplCol,
    label = paste("öffentlicher Platz, Belag aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$oeffentlicherPlatz$material, surfaceMaterial2$material)] ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruenanlagen,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = gruenanlCol,
    label = "Grünanlagen"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruenflaeche,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = gruenflCol,
    label = "Grünfläche"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruendach_Geb,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = gruendCol,
    label = paste("Gründach:", Surfaces_subset$Gruendach_Geb$GRUEN_KAT ,sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Spielplaetze,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "pink",  # line colour
    fillOpacity = mapOp,
    fillColor = spielpCol,
    label = "Spielplatz"  # LAD name as a hover label
  )%>%
  leaflet::addCircles(
    data =Surfaces_subset$Baeume,  # LAD polygon data from geojson
    radius= Surfaces_subset$Baeume$kronedurch/2,
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol,
    label = paste(Surfaces_subset$Baeume$art_dtsch, ",", "Standalter" ,Surfaces_subset$Baeume$standalter, "Jahre" ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addCircles(
    data =Surfaces_subset$Baeume_anl,  # LAD polygon data from geojson
    radius= Surfaces_subset$Baeume_anl$kronedurch/2,
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol,
    label = paste(Surfaces_subset$Baeume_anl$art_dtsch,",", "Standalter" ,Surfaces_subset$Baeume_anl$standalter, "Jahre" ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baumscheiben,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "yellow",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol,
    label = paste("Baumscheibe", round(st_area(Surfaces_subset$Baumscheiben),digits = 1),"m²" ,sep = " ")  # LAD name as a hover label
  )

#übersichts und auswahl map
kiezmap <- map %>%
  leaflet::addPolygons(
    data =Kieze,  # LAD polygon data from geojson
    weight = .7,  # line thickness
    opacity = 1,  # line transparency
    color = "grey",  # line colour
    fillOpacity = 0.2,
    fillColor = "red",
    label = paste(Kieze$BEZIRKSNAME, Kieze$PLANUNGSRAUM, sep = " "),
    layerId = ~PLANUNGSRAUM
  )%>%
  leaflet::addPolygons(
    data =kiez_oi,  # LAD polygon data from geojson
    weight = .7,  # line thickness
    opacity = 1,  # line transparency
    color = "grey",  # line colour
    fillOpacity = 0.7,
    fillColor = "yellow",
    label = paste("vorausgewählt:", kiez_oi$PLANUNGSRAUM, sep=" "))

#Versiegelungkarte----------------------------------------
#Karte mit Flächen in der Farbe der Versiegelungsarten

Versiegelungskarte <- map %>%
  leaflet::addPolygons(
    data =kiez_oi,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = 0.1,
    fillColor = "gray",
    label = paste("Wrangelkiez, Gesamtfläche",round(st_area(kiez_oi)), "m²",sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gebaeude,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "purple",  # line colour
    fillOpacity = 0.5,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gebaeude$material,surfaceMaterial2$material)]) ,
    label = paste("Gebaeude, Gründach", Surfaces_subset$Gebaeude$GRUENDACH,  sep = " ")  # LAD name as a hover label
    )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baustelle,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Baustelle$material,surfaceMaterial2$material)]),
    label = "Baustelle"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Radweg,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "blue",  # line colour
    fillOpacity = 0.5,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Radweg$material,surfaceMaterial2$material)]),
    label = paste("Radweg aus", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Radweg$material, surfaceMaterial2$material)] ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data = Surfaces_subset$Fahrbahn,  # polygon data from geojson
    weight = 0,  # line thickness
    opacity = 1,  # line transparency
    color = "Purple",  # line colour
    fillOpacity = mapOp,
    fillColor =  pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Fahrbahn$material,surfaceMaterial2$material)]),
    label= paste("Fahrbahn aus", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Fahrbahn$material, surfaceMaterial2$material)] ,Surfaces_subset$Adressen$str_name[st_nearest_feature(Surfaces_subset$Fahrbahn,Surfaces_subset$Adressen)], sep = "\n")
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gehweg,  # LAD polygon data from geojson
    weight = 0.5,  # line thickness
    opacity = 0.8,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor =  pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gehweg$material,surfaceMaterial2$material)]),
    label = paste("Gehweg aus", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Gehweg$material, surfaceMaterial2$material)] ,sep="\n")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gehwegsueberfahrt,  # LAD polygon data from geojson
    weight = 0.5,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gehwegsueberfahrt$material,surfaceMaterial2$material)]),
    label = paste("Gehwegsüberfahrt aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Gehwegsueberfahrt$material, surfaceMaterial2$material)],sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Parkplatzflaeche,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Parkplatz$material,surfaceMaterial2$material)]),
    label = paste("Parkplatz aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Parkplatzflaeche$material, surfaceMaterial2$material)],round(Surfaces_subset$Parkplatzflaeche$flaeche/12), "Stellplätze" , sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$oeffentlicherPlatz,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = "gray",
    label = paste("Versiegelungsklasse", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$oeffentlicherPlatz$material, surfaceMaterial2$material)] ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruenanlagen,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gruenanlagen$material,surfaceMaterial2$material)]),
    label = "Grünanlagen"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruenflaeche,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gruenflaeche$material,surfaceMaterial2$material)]),
    label = "Grünfläche"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruendach_Geb,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = gruendCol,
    label = paste("Gründach:", Surfaces_subset$Gruendach_Geb$GRUEN_KAT ,sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Spielplaetze,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Spielplaetze$material,surfaceMaterial2$material)]),
    label = "Spielplatz"  # LAD name as a hover label
  )%>%
  leaflet::addCircles(
    data =Surfaces_subset$Baeume,  # LAD polygon data from geojson
    radius= 0.5,
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol,
    label = paste(Surfaces_subset$Baeume$art_dtsch, "Standalter: " ,Surfaces_subset$Baeume$standalter, " Jahre" ,sep="")  # LAD name as a hover label
  )%>%
  leaflet::addCircles(
    data =Surfaces_subset$Baeume_anl,  # LAD polygon data from geojson
    radius= .5,
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol,
    label = paste(Surfaces_subset$Baeume_anl$art_dtsch,",", "Standalter" ,Surfaces_subset$Baeume_anl$standalter, "Jahre" ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baumscheiben,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Baumscheiben$material,surfaceMaterial2$material)]),
    label = paste("Baumscheibe", round(st_area(Surfaces_subset$Baumscheiben),digits = 1),"m²" ,sep = " ")  # LAD name as a hover label
  )
#Versiegelungskarte


# Urbaner Boden Karte-------------------------------------------


UrbaneBodenkarte <- map %>%
  leaflet::addPolygons(
    data =Surfaces_subset$Boden,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = 0.2,
    fillColor = "gray",
    label = paste("Bodentyp in diesem Block:" ,Surfaces_subset$Boden$btyp, "Bodenausgangsmaterial:", Surfaces_subset$Boden$ausgangsm  ,sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gebaeude,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = 0.5,
    fillColor = "gray" 
    #label = paste("Gebaeude, Gründach", Surfaces_subset$Gebaeude$GRUENDACH,  sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baustelle,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = mapOp,
    fillColor = "gray",
    label = "Baustelle"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Radweg,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = 0.5,
    fillColor = "gray"
    #label = paste("Radweg aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Radweg$material, surfaceMaterial2$material)] ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data = Surfaces_subset$Fahrbahn,  # polygon data from geojson
    weight = 0,  # line thickness
    opacity = 1,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = mapOp,
    fillColor =  "gray"
    #label= paste("Fahrbahn aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Fahrbahn$material, surfaceMaterial2$material)], Surfaces_subset$Adressen$str_name[st_nearest_feature(Surfaces_subset$Fahrbahn,Surfaces_subset$Adressen)], sep = " ")
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gehweg,  # LAD polygon data from geojson
    weight = 0.5,  # line thickness
    opacity = 0.8,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = mapOp,
    fillColor =  "gray"
   # label = paste("Gehweg aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Gehweg$material, surfaceMaterial2$material)], surfaceMaterial2$farbe[match(Surfaces_subset$Gehweg$material,surfaceMaterial2$material)],sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gehwegsueberfahrt,  # LAD polygon data from geojson
    weight = 0.5,  # line thickness
    opacity = 1,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = mapOp,
    fillColor = "gray"
    # label = paste("Gehwegsüberfahrt aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Gehwegsueberfahrt$material, surfaceMaterial2$material)],sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Parkplatzflaeche,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = mapOp,
    fillColor = "gray"
    # label = paste("Parkplatz aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Parkplatzflaeche$material, surfaceMaterial2$material)],round(Surfaces_subset$Parkplatzflaeche$flaeche/12), "Stellplätze" , sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$oeffentlicherPlatz,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = "gray",
   # label = paste("öffentlicher Platz, Belag aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$oeffentlicherPlatz$material, surfaceMaterial2$material)] ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruenanlagen,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gruenanlagen$material,surfaceMaterial2$material)]),
    label = "teilverisegelter Urbaner Boden in Grünanlagen"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruenflaeche,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gruenflaeche$material,surfaceMaterial2$material)]),
    label = "Urbaner Boden in Grünfläche"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruendach_Geb,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = gruendCol,
    label = paste("urbanes Bodensusbtart für Gründach:", Surfaces_subset$Gruendach_Geb$GRUEN_KAT ,sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Spielplaetze,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Spielplaetze$material,surfaceMaterial2$material)]),
    label = "Urbaner Boden auf Spielplatz"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baumscheiben,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Baumscheiben$material,surfaceMaterial2$material)]),
    label = paste("Urbaner Boden in Baumscheibe", round(st_area(Surfaces_subset$Baumscheiben),digits = 1),"m²" ,sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addCircles(
    data =Surfaces_subset$Baeume,  # LAD polygon data from geojson
    radius= 0.8,
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol
   # label = paste(Surfaces_subset$Baeume$art_dtsch, ",", "Standalter" ,Surfaces_subset$Baeume$standalter, "Jahre" ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addCircles(
    data =Surfaces_subset$Baeume_anl,  # LAD polygon data from geojson
    radius= .8,
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol
    #label = paste(Surfaces_subset$Baeume_anl$art_dtsch,",", "Standalter" ,Surfaces_subset$Baeume_anl$standalter, "Jahre" ,sep=" ")  # LAD name as a hover label
  )
#UrbaneBodenkarte
