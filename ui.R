library(shiny)
library(leaflet)
library(markdown)

# create list for input
vars <- c(
  "Paris" = "paris",
  "Arrondisements" = "arrondissements",
  "Quartiers Administratifs" = "quartiers"
)


vars_paris <- c(
  "Heatmap Décile" = "decile",
  "Heatmap Airbnb" = "heat_airbnb",
  "Heatmap Hôtels" = "heat_hotel"
)

vars_arr <- c(
  "Population/km2" = "popkm",
  "Airbnb/km2" = "airkm",
  "Hotels/km2" = "hotkm",
  "Airbnb/habitant" = "airhab",
  "Hotels/habitant" = "hothab",
  "Prix moyen" = "prixmoy"
)

vars_quar <- c(
  "Population/km2" = "popkm",
  "Airbnb/km2" = "airkm",
  "Hotels/km2" = "hotkm",
  "Airbnb/habitant" = "airhab",
  "Hotels/habitant" = "hothab",
  "Prix moyen" = "prixmoy",
  "Marge" = "marge"
)


# navigation page
navbarPage("Projet Medas", id="nav",
           
           tabPanel("Carte Intéractive",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Paramètres"),
                                      
                                      selectInput("localisation", "Localisation", vars),
                                      conditionalPanel("input.localisation == 'paris'",
                                                       selectInput("info", "Carte", vars_paris)
                                                       ),
                                      conditionalPanel("input.localisation == 'arrondissements'",
                                                       selectInput("info2", "Carte", vars_arr)
                                                      ),
                                      conditionalPanel("input.localisation == 'quartiers'",
                                                       selectInput("info3", "Carte", vars_quar)
                                                      ),
                                      

                                      checkboxInput( "marker","Marqueurs"),
                                      checkboxInput( "marker_ill","Annonces Illégales"),
                                      
                                      conditionalPanel("input.localisation == 'paris'",
                                                       plotOutput("histo", height = 200)
                                                       
                                      )
                                      
                               
                                                       
                                      )
                        )
                    )
           )


