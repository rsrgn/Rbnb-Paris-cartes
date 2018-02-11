library(shiny)
#-----Chargement des librairies----
library(rsconnect)
library(leaflet)
library(sf)
library(htmltools)
library(leaflet.extras)
library(rmarkdown)

load('data')


function(input, output, session) {
  
  
  ### Interactive Map ###########################################
  
  ###-- Create the map ####
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%  
      addProviderTiles("CartoDB.DarkMatter", group = 'paris') %>%  
      setView(lng = 2.349014, lat = 48.864716, zoom = 12)
      
  })
  
  # hist price distribution ####
  
  output$histo <- renderPlot({
  
    hist(
      as.numeric(df$price),
      breaks = 200,
      main = "Prix des offres Airbnb",
      xlab = "Prix",
      xlim = c(0,400),
      col = '#00DD00',
      border = 'white')
  })
  
  # barplot of number of offers ####
  
  output$hotels_plot <- renderPlot({
    par(las=2)
    par(mar=c(5,12,4,2))
    barplot(arrondissements_paris_sf$hotels_c_nb ,
            names.arg=arrondissements_paris_sf$l_ar,
            horiz=TRUE,
            main = "Nombre de chambres d'hôtels par arrondissements",
            axis.lty=1,
            col = '#00DD00',
            border = 'white')})
  
  output$airbnb_plot <- renderPlot({
    par(las=2)
    par(mar=c(5,12,4,2))
    barplot(arrondissements_paris_sf$airbnb_nb ,
            names.arg=arrondissements_paris_sf$l_ar,
            horiz=TRUE,
            main = "Nombre d'offre Airbnb par arrondissements",
            axis.lty=1,
            col = '#00DD00',
            border = 'white')})
  
  
  
  
    

  
# carte decile  ####
  observe({
    if ((input$localisation == "paris") && (input$info == "decile")) 
        {
      leafletProxy("map") %>% clearShapes() %>% clearControls()  %>% clearGroup('decilem') %>% clearWebGLHeatmap() %>% 
        addProviderTiles("CartoDB.DarkMatter", group = 'paris') %>%
            addCircleMarkers(data = listings2,
                             ~longitude, 
                             ~latitude, 
                             radius = 0.2 ,
                             color = ifelse(listings2$price > 160, "white", "orange"),
                             fillOpacity = 0.01,
                             group = 'decilem')
    } 
  })
 #carte heatmap aribnb  ####  
  observe({

    if ((input$localisation == "paris") && (input$info == "heat_airbnb"))
    {
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>%
        addProviderTiles("CartoDB.DarkMatter", group = 'paris') %>%
        addWebGLHeatmap(lng=df$longitude,
                        lat=df$latitude,
                        size="100",
                        units="m")
    }
  })
# carte heatmap hotel ####
  observe({
    
    if ((input$localisation == "paris") && (input$info == "heat_hotel"))
    {
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>%
        addProviderTiles("CartoDB.DarkMatter", group = 'paris') %>%
        addWebGLHeatmap(data=hotel_paris,
                        size="100",
                        intensity = hotel_paris$nombre_de_chambres)
    }
  })
  
  # carte arrondissement popkm2 ####
  observe({
    
    if ((input$localisation == "arrondissements") && (input$info2 == "popkm"))
    {
      
      #label of legend
      lab0 <- sprintf(
        "<strong>%s</strong><br/>%g habitants par km²",
        arrondissements_paris_sf$l_ar, arrondissements_paris_sf$density_hab_km
      ) %>% lapply(htmltools::HTML)
      
      #color palette
      pal0 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = arrondissements_paris_sf$density_hab_km)
      
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        addPolygons(data =arrondissements_paris_sf,
                    color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    fillOpacity = 0.9,
                    fillColor = ~pal0(density_hab_km),
                    highlight = highlightOptions(color = "#000",
                                                 weight = 3,
                                                 bringToFront = TRUE),
                    label = lab0,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal0,
                  values = arrondissements_paris_sf$density_hab_km,
                  opacity = 1.0,
                  title = "Nombre d'habitants<br/>par km²<br/>par arrondissements",
                  position = "bottomleft")
    }
  })
  # carte arr airbnbkm2 ####
  observe({
    
    if ((input$localisation == "arrondissements") && (input$info2 == "airkm"))
    {
      lab1 <- sprintf(
        "<strong>%s</strong><br/>%g annonces Airbnb",
        arrondissements_paris_sf$l_ar, arrondissements_paris_sf$airbnb_density
      ) %>% lapply(htmltools::HTML)
      
      pal1 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = arrondissements_paris_sf$airbnb_density)
      
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        addPolygons(data =arrondissements_paris_sf,
                    color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    fillOpacity = 0.9,
                    fillColor = ~pal1(airbnb_density),
                    highlight = highlightOptions(color = "#000",
                                                 weight = 3,
                                                 bringToFront = TRUE),
                    label = lab1,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal1,
                  values = arrondissements_paris_sf$airbnb_density,
                  opacity = 1.0,
                  title = "Nombre d'annonces<br/>AirBnB par km²",
                  position = "bottomleft")
    }
    
  })
  # carte arr hotkm2 ####
  observe({
    
    if ((input$localisation == "arrondissements") && (input$info2 == "hotkm"))
    {
      lab2 <- sprintf(
        "<strong>%s</strong><br/>%g Nombre de<br/>chambres d'hotel par km²",
        arrondissements_paris_sf$l_ar, arrondissements_paris_sf$hotels_c_density
      ) %>% lapply(htmltools::HTML)
      pal2 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = arrondissements_paris_sf$hotels_c_density)

      
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        addPolygons(data =arrondissements_paris_sf,
                    color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    fillOpacity = 0.9,
                    fillColor = ~pal2(hotels_c_density),
                    highlight = highlightOptions(color = "#000",
                                                 weight = 3,
                                                 bringToFront = TRUE),
                    label = lab2,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal2,
                  values = arrondissements_paris_sf$hotels_c_density,
                  opacity = 1.0,
                  title = "Nombre de chambres<br/>d'hotel par km²",
                  position = "bottomleft")
    }
  })
  
  
  
  observe({
    
    if ((input$localisation == "arrondissements") && (input$info2 == "airhab"))
    {
      lab3 <- sprintf(
        "<strong>%s</strong><br/>%g Nombre d'annonces<br/>RBNB par habitant",
        arrondissements_paris_sf$l_ar, arrondissements_paris_sf$airbnb_density_hab
      ) %>% lapply(htmltools::HTML)
      pal3 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = arrondissements_paris_sf$airbnb_density_hab)
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        
        addPolygons(
          data =arrondissements_paris_sf,
          color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    fillOpacity = 0.9,
                    highlight = highlightOptions(color = "#000", weight = 3,
                                                 bringToFront = TRUE),
                    fillColor = ~pal3(airbnb_density_hab),
                    label = lab3,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal3,
                  values = arrondissements_paris_sf$airbnb_density_hab,
                  opacity = 1.0,
                  title = "Nombre d'annonces<br/>AirBnB par habitant<br/>par arrdt",
                  position = "bottomleft")
    }
  })
  
  
  observe({
    
    if ((input$localisation == "arrondissements") && (input$info2 == "hothab"))
    {
      lab4 <- sprintf(
        "<strong>%s</strong><br/>%g Chambres<br/>d'hôtels par habitant",
        arrondissements_paris_sf$l_ar, arrondissements_paris_sf$hotels_c_density_hab
      ) %>% lapply(htmltools::HTML)
      pal4 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = arrondissements_paris_sf$hotels_c_density_hab)
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        addPolygons(data =  arrondissements_paris_sf,
                    color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    highlight = highlightOptions(color = "#000", weight = 3,
                                                 bringToFront = TRUE),
                    fillOpacity = 0.9,
                    fillColor = ~pal4(hotels_c_density_hab),
                    label = lab4,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal4,
                  values =  arrondissements_paris_sf$hotels_c_density_hab,
                  opacity = 1.0,
                  title = "Nombre de chambres<br/>d'hôtels par habitant<br/>par arrdt",
                  position = "bottomleft")
    }
  })
  
  observe({
    
    if ((input$localisation == "arrondissements") && (input$info2 == "prixmoy"))
    {
      pal8 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = arrondissements_paris_sf$prix_moyen)
      lab8 <- sprintf(
        "<strong>%s</strong><br/>Prix moyen Airbnb : %g €",
        arrondissements_paris_sf$l_ar, arrondissements_paris_sf$prix_moyen
      ) %>% lapply(htmltools::HTML)
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        addPolygons(data = arrondissements_paris_sf,
                    color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    highlight = highlightOptions(color = "#000", weight = 3,
                                                 bringToFront = TRUE),
                    fillOpacity = 0.9,
                    fillColor = ~pal8(prix_moyen),
                    label = lab8,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal8,
                  values = arrondissements_paris_sf$prix_moyen,
                  opacity = 1.0,
                  title = "Prix moyen Airbnb nuit/personne",
                  position = "bottomleft"
                  )
    }
  })
  observe({
    
    if ((input$localisation == "quartiers") && (input$info3 == "popkm"))
    {
      
      lab0 <- sprintf(
        "<strong>%s</strong><br/>%g habitants par km²",
        quartiers_paris$l_qu, quartiers_paris$density_hab_km
      ) %>% lapply(htmltools::HTML)
      pal0 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = quartiers_paris$density_hab_km)
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        addPolygons(data = quartiers_paris,
                    color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    highlight = highlightOptions(color = "#000", weight = 3,
                                                 bringToFront = TRUE),
                    fillOpacity = 0.9,
                    fillColor = ~pal0(density_hab_km),
                    label = lab0,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal0,
                  values = quartiers_paris$density_hab_km,
                  opacity = 1.0,
                  title = "Nombre d'habitants<br/>par km²<br/>par quartier",
                  position = "bottomleft")
    }
  })
  # carte quar airbnbkm2 ####
  observe({
    
    if ((input$localisation == "quartiers") && (input$info3 == "airkm"))
    {
      lab5 <- sprintf(
        "<strong>%s</strong><br/>%g annonces Airbnb",
        quartiers_paris$l_qu, quartiers_paris$airbnb_density
      ) %>% lapply(htmltools::HTML)
      pal5 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = quartiers_paris$airbnb_density)
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        
        addPolygons(
          data = quartiers_paris,color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    fillOpacity = 0.9,
                    highlight = highlightOptions(color = "#000", weight = 3,
                                                 bringToFront = TRUE),
                    fillColor = ~pal5(airbnb_density),
                    label = lab5,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal5,
                  values = quartiers_paris$airbnb_density,
                  opacity = 1.0,
                  title = "Nombre d'annonces<br/>AirBnB par km²",
                  position = "bottomleft")
    }
    
  })
  # carte quar hotkm2 ####
  observe({
    
    if ((input$localisation == "quartiers") && (input$info3 == "hotkm"))
    {
      lab6 <- sprintf(
        "<strong>%s</strong><br/>%g Nombre de<br/>chambres d'hotel par km²",
        quartiers_paris$l_qu, quartiers_paris$hotels_c_density
      ) %>% lapply(htmltools::HTML)
      pal6 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = quartiers_paris$hotels_c_density)
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        
        addPolygons(data = quartiers_paris,color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    highlight = highlightOptions(color = "#000", weight = 3,
                                                 bringToFront = TRUE),
                    fillOpacity = 0.9,
                    fillColor = ~pal6(hotels_c_density),
                    label = lab6,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal6,
                  values = quartiers_paris$hotels_c_density,
                  opacity = 1.0,
                  title = "Nombre de chambres<br/>d'hotel par km²",
                  position = "bottomleft")
    }
  })
  
  
  
  observe({
    
    if ((input$localisation == "quartiers") && (input$info3 == "airhab"))
    {
      lab3 <- sprintf(
        "<strong>%s</strong><br/>%g Nombre d'annonces<br/>RBNB par habitant",
        quartiers_paris$l_qu, quartiers_paris$airbnb_density_hab
      ) %>% lapply(htmltools::HTML)
      pal3 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = quartiers_paris$airbnb_density_hab)
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        
        addPolygons(data = quartiers_paris,color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    highlight = highlightOptions(color = "#000", weight = 3,
                                                 bringToFront = TRUE),
                    fillOpacity = 0.9,
                    fillColor = ~pal3(airbnb_density_hab),
                    label = lab3,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal3,
                  values = quartiers_paris$airbnb_density_hab,
                  opacity = 1.0,
                  title = "Nombre d'annonces<br/>AirBnB par habitant<br/>par quartier",
                  position = "bottomleft"
                  )
    }
  })
  
  
  observe({
    
    if ((input$localisation == "quartiers") && (input$info3 == "hothab"))
    {
      lab4 <- sprintf(
        "<strong>%s</strong><br/>%g Hotellerie<br/> par habitant",
        quartiers_paris$l_qu, quartiers_paris$hotels_c_density_hab
      ) %>% lapply(htmltools::HTML)
      pal4 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = quartiers_paris$hotels_c_density_hab)
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        
        addPolygons(data = quartiers_paris,color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    fillOpacity = 0.9,
                    highlight = highlightOptions(color = "#000", weight = 3,
                                                 bringToFront = TRUE),
                    fillColor = ~pal4(hotels_c_density_hab),
                    label = lab4,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal4,
                  values = quartiers_paris$hotels_c_density_hab,
                  opacity = 1.0,
                  title = "Nombre de chambres<br/>d'hôtels par habitant<br/>par quartier",
                  position = "bottomleft")
    }
  })
  
  observe({
    
    if ((input$localisation == "quartiers") && (input$info3 == "prixmoy"))
    {
      pal7 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = quartiers_paris$prix_moyen)
      lab7 <- sprintf(
        "<strong>%s</strong><br/>Prix moyen : %g €",
        quartiers_paris$l_qu, quartiers_paris$prix_moyen
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        
        addPolygons(data = quartiers_paris,color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    fillOpacity = 0.9,
                    highlight = highlightOptions(color = "#000", weight = 3,
                                                 bringToFront = TRUE),
                    fillColor = ~pal7(prix_moyen),
                    label = lab7,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal7,
                  values =quartiers_paris$prix_moyen,
                  opacity = 1.0,
                  title = "Prix moyen nuit/personne",
                  position = "bottomleft")
    }
  })
  
  observe({
    
    if ((input$localisation == "quartiers") && (input$info3 == "marge"))
    {
      lab10 <- sprintf(
        "<strong>%s</strong><br/>Prix du m² en location<br/>Airbnb : %g €<br/><br/>Prix du m² en location<br/>classique: %g €<br/><br/>La location Airbnb est <b>%s<br/> plus rentable</b> que la location classique.",
        marge_airbnb$nom_quartier, marge_airbnb$prix_nuite_airbnb_80_de_365, marge_airbnb$loyer_mensuel_x_80_de_12, marge_airbnb$rapport_entre_prix_nuite_airbnb_365_et_loyer_annuel
      ) %>% lapply(htmltools::HTML)
      pal10 <- colorNumeric(palette = c('#e7e1ef','#c994c7','#dd1c77'), domain = marge_airbnb$rapport_entre_prix_nuite_airbnb_365_et_loyer_annuel)
      leafletProxy("map") %>%  clearShapes() %>% clearControls()  %>% clearMarkers() %>% clearWebGLHeatmap() %>% clearGroup("paris") %>%
        
        addPolygons(data =  marge_airbnb,color="white",
                    dashArray = "3",
                    weight=2,
                    smoothFactor = 0.3,
                    highlight = highlightOptions(color = "#000", weight = 3,
                                                 bringToFront = TRUE),
                    fillOpacity = 0.9,
                    fillColor = ~pal10(rapport_entre_prix_nuite_airbnb_365_et_loyer_annuel),
                    label = lab10,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "10px",
                      direction = "auto")) %>%
        addLegend(pal = pal10,
                  values = marge_airbnb$rapport_entre_prix_nuite_airbnb_365_et_loyer_annuel,
                  opacity = 1.0,
                  title = "Marge Airbnb",
                  position = "bottomleft")
    }
  })
  
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("markerill")
    if (input$marker_ill) {
      mlabs <- sprintf(
        "<strong>HostID : %s</strong><br/>Liste des offres : %s<br/>Estim. revenu mensuel : %g €",
        loueurs_suspects$host_id, loueurs_suspects$calculated_host_listings_count, loueurs_suspects$estimated_monthly_income
      ) %>% lapply(htmltools::HTML)
      
      
      leafletProxy("map") %>%
        addMarkers( data = loueurs_suspects,
                    group = "markerill",
          
                   lng = ~loueurs_suspects$longitude,
                   lat = ~loueurs_suspects$latitude,
                   clusterOptions = markerClusterOptions(),
                   label = mlabs,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "10px",
                     direction = "auto")
                   
                   )
    }
    })
  
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("marker2")
    if (input$marker) {
      leafletProxy("map") %>% addMarkers(data = df, 
                                         group = "marker2",
                                         
                                         lng = ~df$longitude,
                                         lat = ~df$latitude,
                                         clusterOptions = markerClusterOptions())
    }
  })
  
  
  
   
}
