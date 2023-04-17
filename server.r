source("libraries.r")

##Load data
solardach <- sfarrow::st_read_parquet("data-processed/solardach.parquet") |>
  select(STROMERTRAG, STROMERTRAG_SOMMERHALBJAHR, STROMERTRAG_WINTERHALBJAHR, GWR_EGID, geom) |>
  na.omit() |>
  group_by(GWR_EGID) |>
  summarise(STROMERTRAG = sum(STROMERTRAG)) |>
  st_cast()

#sisslerfeld <- st_read("data-processed/prepared_data.gpkg", "sisslerfeld")
cars <- read_delim("data-processed/electric_cars.csv", delim = ";")

##Server
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() |>
      setView(7.97170, 47.54841, 14) %>%
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(data = solardach, 
                  group = "solardach",
                  fillOpacity = 0.5,
                  layerId = solardach$GWR_EGID,
                  color = "#320385",
                  opacity = 1,
                  weight = 1,
                  highlightOptions = highlightOptions(color = "red",
                                                      weight = 2, 
                                                      fillColor = 'red', 
                                                      bringToFront = TRUE)
                  )
  })
  
  output$tab <- renderTable({
    table <- solardach |>
      filter(solardach$GWR_EGID == solardach_index()) |>
      st_drop_geometry() |>
      mutate(across(.fns = as.character)) |>
      pivot_longer(everything(), names_to = "Attribute", values_to = "Value") |>
      mutate(Attribute = str_to_title(str_replace(Attribute, "_", " ")))
    
    table
  })
  
  solardach_index <- reactive({
    if(!is.null(input$map_shape_click)){
      idx <- input$map_shape_click$id
      
      selected_solardach <- solardach |>
        filter(solardach$GWR_EGID == idx) 
      
      leafletProxy("map", data = selected_solardach, session) |>
        clearGroup("selected_solardach") |>
        addPolygons(
          fillOpacity = 0.5,
          layerId = selected_solardach$GWR_EGID,
          color = "red",
          opacity = 1,
          weight = 1,
          group = "selected_solardach"
        )
      idx
    } else {
      1
    } 
  })
  
  ##Generate isoline data
  observeEvent(input$rayshade, {
    car <- cars |> filter(model == input$car) #Get point of selected location
    
    selected_solardach <- solardach |>
      filter(solardach$GWR_EGID == solardach_index()) |>
      st_centroid()

      ##TODO Berechung stimmt noch nicht
      range_per_day_km <- (car$range_km / car$capacity_kwh) * (selected_solardach$STROMERTRAG / 365 / 100)
      range_calc <- c(5, 3, 1) * ceiling(range_per_day_km)
      range_calc <- ifelse(range_calc >= car$range_km, car$range_km, range_calc)
      
      isolines <- hereR::isoline(
        selected_solardach,
        datetime = Sys.time(),
        traffic = FALSE,
        range = range_calc * 100,
        range_type = "distance",
        routing_mode = "short",
        transport_mode = "car") |>
        cbind(title = c("1 day", "3 day", "5 day"))

      myPalette <- c("#ff8800", "#ffb700", "#fff700")
      colorsSchema <- setNames(myPalette, c("1 day", "3 day", "5 day"))
      pal <- colorFactor(colorsSchema, isolines$title)
      bounds <- st_bbox(isolines) |> as.vector()
      
      leafletProxy("map", data = isolines, session) |>
        flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4]) |>
        clearGroup("range_calc") |>
        clearControls() |>
        addPolygons(
          fillColor = ~ pal(isolines$title),
          fillOpacity = 0.2,
          stroke = FALSE,
          group = "range_calc"
        ) |>
        addLegend(
          "bottomright",
          title = "Charging time (days)",
          pal = pal,
          values = isolines$title,
          opacity = 1,
          group = "range_calc"
        ) |>
        addLayersControl(
          overlayGroups = c("range_calc"),
          options = layersControlOptions(collapsed = FALSE)
        )
  })
}