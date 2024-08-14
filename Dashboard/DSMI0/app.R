# Les Packages 
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(ggthemes)
library(readr)
library(jpeg)
library(sf)
library(DT)
library(sp)
library(raster)
library(dplyr)


##################### Définition de l'interface utilisateur ###########
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Présentation de la zone', tabName = 'menu1', icon = icon('flag')),
      menuItem('Présentation des Résultats', tabName = 'menu3', icon = icon('square-poll-vertical'),
               menuSubItem('Analyse Topographique', tabName = 'submenu2', icon = icon('mound')),
               menuSubItem('Analyse Hydrologique', tabName = 'submenu3', icon = icon('water')),
               menuSubItem('Localisation des points', tabName = 'submenu4', icon = icon('location-dot'))
      )
    )
  ),
  
  ############### Corps du tableau de bord #############################
  dashboardBody(
    tags$head(tags$style(HTML("
        #map {
          height: 600vh;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = 'menu1',
              h2('Présentation de la zone'), leafletOutput("map"),
              fluidRow(
                box(
                  title = "Quartiers DSM",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  dataTableOutput("quartiers")
                )
              ),
              fluidRow(
                box(
                  title = "Occupation du sol",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("barplot")
                )
              ),  
              fluidRow(
                box(
                  title = "", width = 9, solidHeader = TRUE, status = "primary",
                  div(class = "center-left", plotOutput("occupation_1988"))
                )
              )
      ),
      
      tabItem(tabName = 'menu3', h2('Présentation des Résultats')),
      tabItem(tabName = 'submenu2', h2('La Topographie'),
              fluidRow(
                box(
                  title = "Topographie",
                  width = 9,
                  solidHeader = TRUE,
                  status = "primary",
                  img(src = "Pente.jpg", width = "100%")
                )
              ),
              h2("Élévation"),
              plotOutput("mnt_plot")
      ),
      
      tabItem(tabName = 'submenu3', h2('Analyse Hydrologique')),
      tabItem(tabName = 'submenu4', h2('Localisation des points'),
              leafletOutput("point_map")),
      tabItem(tabName = 'menu5', h2('Détection des zones inondées'))
    )
  )
)



######################Définition du Serveur#########################"
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addMarkers(lng = -17.352540491056484,lat = 14.767247041989215,popup = "Diamaguene 
                 Sicap Mbao")%>%
      setView(lng = -17.352540491056484, lat = 14.767247041989215, zoom = 12)
  })
  
  #Lecture du fichier de l'occupation du sol
  DSM_1988<-read.csv2("C:/Users/pc gz/Desktop/Data/Occup_1988.csv")
  #View(DSM_1988)
  #création du barplot de l'occupation du sol 1988
  output$barplot<-renderPlot({
    ggplot(data=DSM_1988,aes(x=Classe,y=Perimetre))+
      geom_bar(stat = "identity",width = 0.2,position = "dodge",fill="cyan3")+
      labs(x="Occupation du sol",y="Périmètres occupés",fill="Classe")+
      theme_minimal()+
      theme(axis.text= element_text(hjust = 0.5,size = 15,face = "bold"),
            legend.text = element_text(size = 12,face = "bold"),
            axis.title = element_text(size = 15,face = "bold"))
  })
  #Lecture du fichier shp
  occup_1988<-st_read("C:/Users/pc gz/Desktop/Data/Occup_1988.shp")
  plot(st_geometry(occup_1988))
  occupation<-st_transform(occup_1988,crs = 4326)
  #visualisation de l'occupation du sol
  output$occupation_1988<-renderPlot({
    ggplot(data = occupation)+
      geom_sf(aes(fill=Classe),color="black")+
      scale_fill_viridis_d(option = "E")+
      theme_minimal()+
      labs(title = "Occupation du sol en 1988",
           fill="Classe")+
      theme(plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
            legend.title = element_text(size = 20,face = "bold"),
            legend.text = element_text(size = 15,face = "bold"),
            axis.text = element_text(size = 15,face = "bold"),
            panel.background = element_rect(fill = "lightgrey",color="grey",size = 1))
  })
  
  ########################Pente#################################################
  
  
  
  #######################Altitudes##############################################4

  
  #######################Localisation des points d'eau##########################
  point_eau<-st_read("C:/Users/pc gz/Desktop/SdAfrique/Projet/myrepo/Data/Points_d'eau_DSM.shp")
  plot(st_geometry(point_eau))
  table_point_eau<-read.csv2("Data/table_points_o.csv")
  View(table_point_eau)
  
  #Visualisation des quartiers dans la partie presentation de la zone
  quartiers_DSM<-read.csv2("Data/Quartiers.csv")
  View(quartiers_DSM)
  quartiers_DSM1<-as.data.frame(quartiers_DSM)
  quartiers_DSM1
  t#Lecture de la table dans le Server
  output$quartiers <- DT::renderDataTable({
    DT::datatable(quartiers_DSM1)
    })

  #verification des coordonnées
  coords <- st_coordinates(point_eau)
  coords
  #Affichage des points d'eau
  output$point_map<-renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$Esri.WorldImagery)%>%
      addCircleMarkers(data = point_eau,
                        lng = ~coords[,1],
                        lat = ~coords[,2],
                        radius = 5,
                        color = "red",
                        stroke = FALSE,
                        fillOpacity = 0.8)
  })
  
  #Traitement du MNT
  MNT<-raster("Data/MNT_DSM.tif")
  plot(MNT)
  View(MNT)
  
  # Convertir le raster en data.frame pour ggplot
  mnt_DK <- as.data.frame(rasterToPoints(MNT), stringsAsFactors = FALSE)
  colnames(mnt_DK) <- c("x", "y", "elevation")
  
  #Creation du plot du Modèle Dakar
  output$mnt_plot <- renderPlot({
    ggplot(mnt_DK, aes(x = x, y = y, fill = elevation)) +
      geom_tile() +
      scale_fill_viridis_c(option = "C") +
      coord_equal() +
      labs(title = "Modèle Numérique de Terrain Diamaguene Sicap Mbao",
           fill = "Elevation (m)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
            axis.title = element_text(size = 15, face = "bold"),
            legend.title = element_text(size = 15, face = "bold"),
            legend.text = element_text(size = 12, face = "bold"))
  })
  
  
 
}
shinyApp(ui, server)
*