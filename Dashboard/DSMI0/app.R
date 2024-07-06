##Ui.R##
library(shinydashboard)
library(leaflet)
#La composition minimale d'un tableau de bord avec Shinydashboard
dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Présentation de la zone',tabName = 'menu1',icon = icon('flag')),
      menuItem('Equipe du Projet',tabName = 'menu4',icon = icon('people-group')),
      menuItem('Mission du Projet',tabName = 'menu2',icon = icon('laptop')),
      menuItem('Présentation des Résultats',tabName = 'menu3',icon = icon('square-poll-vertical'),
               menuSubItem('Occupation du sol',tabName = 'submenu1',icon = icon('map')),
               menuSubItem('La Topographie',tabName = 'submenu2',icon = icon('mound')),
               menuSubItem('Analyse Hydrologique',tabName = 'submenu3',icon = icon('water')),
               menuSubItem('Localisation des points',tabName = 'submenu4',icon = icon('location-dot'))
               ),
      menuItem('Détection zones inondables',tabName = 'menu5',icon = icon('fa-sharp fa-solid fa-house-water'))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
        #map {
          height: 600vh;
        }
      "))
  ),

      tabItems(
        tabItem(tabName = 'menu1',
                h2('Présentation de la zone')),
        tabItem(tabName = 'menu4',h2('Equipe du Projet')),
        tabItem(tabName = 'menu2',h2('Mission du Projet')),
        tabItem(tabName = 'menu3',h2('Présentation des Résultats')),
        tabItem(tabName = 'submenu1',h2('Occupation du sol')),
        tabItem(tabName = 'submenu2',h2('La Topographie')),
        tabItem(tabName = 'submenu3',h2('Analyse Hydrologique')),
        tabItem(tabName = 'submenu4',h2('Localisation des points'))
      )
  ))
##Server##
ui<-dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Présentation de la zone',tabName = 'menu1',icon = icon('flag')),
      menuItem('Equipe du Projet',tabName = 'menu4',icon = icon('people-group')),
      menuItem('Mission du Projet',tabName = 'menu2',icon = icon('laptop')),
      menuItem('Présentation des Résultats',tabName = 'menu3',icon = icon('square-poll-vertical'),
               menuSubItem('Occupation du sol',tabName = 'submenu1',icon=icon('map')),
               menuSubItem('La Topographie',tabName = 'submenu2',icon = icon('mound')),
               menuSubItem('Analyse Hydrologique',tabName = 'submenu3',icon = icon('water')),
               menuSubItem('Localisation des points',tabName = 'submenu4',icon = icon('location-dot'))
               ),
      menuItem('Détection des zones inondables',tabName = 'menu5',icon = icon('water'))
    )
  ),
  ##########Cotenu du tableau de bord avec un peu de HTML pour la forme
  dashboardBody(
    tags$head(
      tags$style(HTML("
        #map {
          height: 600vh;
          width: 100vw;
        }
      "))
    ),
      tabItems(
        tabItem(tabName = 'menu1',h2('Présentation de la zone'),
                leafletOutput("map")),
        tabItem(tabName = 'menu4',h2('Equipe du Projet')),
        tabItem(tabName = 'menu2',h2('Mission du Projet')),
        tabItem(tabName = 'menu3',h2('Présentation des Résultats')),
        tabItem(tabName = 'submenu1',h2('Occupation du sol')),
        tabItem(tabName = 'submenu2',h2('La Topographie')),
        tabItem(tabName = 'submenu3',h2('Analyse Hydrologique')),
        tabItem(tabName = 'submenu4',h2('Localisation des points'))
      )
    )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -17.3284, lat = 14.7745, zoom = 8)
  })
}
shinyApp(ui, server)