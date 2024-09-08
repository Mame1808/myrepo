library(shiny)
library(leaflet)
library(sf)
library(readr)

# Définition de l'interface utilisateur
ui <- fluidPage(
  titlePanel(h2("DSM O Inondation")),
  
  sidebarLayout(
    sidebarPanel(
      h3("Localisation eau stagnante"),
      textInput("quartier", "Rechercher un Quartier:", ""),
      actionButton("add_point", "Ajouter un nouveau Point"),
      actionButton("save_points", "Sauvegarder les Points"),
      actionButton("edit_point", "Modifier un Point"),
      actionButton("delete_point", "Supprimer un Point"),
      verbatimTextOutput("user_location")  # Affichage de la localisation actuelle
    ),
    
    mainPanel(
      leafletOutput("map", width = 900, height = 600)
    )
  )
)

# Définition de la logique serveur
server <- function(input, output, session) {
  
  # Lecture des fichiers
  point_eau <- st_read("C:/Users/pc gz/Desktop/SdAfrique/Projet/myrepo/Data/Points_d'eau_DSM.shp")
  
  if (file.exists("Data/Points_stag.csv")) {
    points_stag <- read.csv2("Data/Points_stag.csv")
  } else {
    points_stag <- data.frame(lat = numeric(0), lng = numeric(0), quartier = character(0))
  }
  
  new_points <- reactiveVal(points_stag)
  user_location <- reactiveVal(NULL)
  
  # Initialiser la carte
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = -17.352540491056484, lat = 14.767247041989215, zoom = 12) %>%
      addCircleMarkers(data = point_eau, radius = 5, color = "red", stroke = FALSE, fillOpacity = 0.8) %>%
      addCircleMarkers(data = new_points(), lng = ~lng, lat = ~lat, radius = 5, color = "blue", stroke = FALSE, fillOpacity = 0.8)
  })
  
  # Ajouter de nouveaux points sur la carte à partir de la position de l'utilisateur
  observeEvent(input$add_point, {
    if (!is.null(user_location())) {
      new_point <- data.frame(lat = user_location()$lat, lng = user_location()$lng, quartier = input$quartier)
      new_points(rbind(new_points(), new_point))
      
      leafletProxy("map") %>%
        addMarkers(lng = user_location()$lng, lat = user_location()$lat, popup = paste("Quartier:", input$quartier, "<br>Lat:", user_location()$lat, "<br>Lng:", user_location()$lng)) %>%
        setView(lng = user_location()$lng, lat = user_location()$lat, zoom = 15)  # Zoom sur la localisation de l'utilisateur
    }
  })
  
  # Fonction pour obtenir la localisation de l'utilisateur
  observe({
    session$sendCustomMessage(type = 'getLocation', message = NULL)
  })
  
  observeEvent(input$user_location, {
    user_location(input$user_location)
    output$user_location <- renderPrint({
      if (!is.null(user_location())) {
        paste("Latitude:", user_location()$lat, "Longitude:", user_location()$lng)
      } else {
        "Localisation non disponible."
      }
    })
  })
  
  # Sauvegarder les points ajoutés dans le fichier CSV
  observeEvent(input$save_points, {
    write.csv2(new_points(), "Data/Points_stag.csv", row.names = FALSE)
    
    showModal(modalDialog(
      title = "Points Enregistrés",
      paste(nrow(new_points()), "points ont été enregistrés dans le fichier CSV.")
    ))
  })
}

# JavaScript pour obtenir la position de l'utilisateur
js_code <- '
shiny.addCustomMessageHandler("getLocation", function(message) {
  if (navigator.geolocation) {
    navigator.geolocation.getCurrentPosition(function(position) {
      Shiny.setInputValue("user_location", {lat: position.coords.latitude, lng: position.coords.longitude}, {priority: "event"});
    }, function(error) {
      Shiny.setInputValue("user_location", {lat: null, lng: null}, {priority: "event"});
    });
  } else {
    Shiny.setInputValue("user_location", {lat: null, lng: null}, {priority: "event"});
  }
});
'

# Lancement de l'application
shinyApp(
  ui = ui,
  server = server,
  onStart = function() {
    shiny::addResourcePath("www", tempdir())
    cat(js_code, file = file.path(tempdir(), "location.js"))
  }
)
