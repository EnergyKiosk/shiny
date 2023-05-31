source("utils.r")

##Server
server <- function(input, output, session) {
  
  #Prepare and show manual
  output$slickr <- renderSlickR({
    imgs <- list.files("www/manual/", pattern=".PNG", full.names = TRUE)
    slickR(imgs,
           height = "100%",
           width = "auto") +
    settings(
        dots = TRUE,
        arrows = TRUE
    )
  })
  observeEvent(input$manualbutton, {
    runjs("openNav(id = 'overlay_question');")
  })
  
  #Show information
  observeEvent(input$infobutton, {
    runjs("openNav(id = 'overlay_info');")
  })

  #Map of solar roofs
  output$map <- renderLeaflet(basemap)
  
  # Get selected roof
  selected_solardach <- reactive({
    if(is.null(input$map_shape_click$id)) {
      idx <- 581928 #TODO Currently hardcoded (Change so that the index does not matter until first one is selected)
    } else {
      idx <- input$map_shape_click$id
    }
    solardach |>
      filter(solardach$GWR_EGID == idx) 
  })
  
  # Hightlight selected roof
  observe({
      leafletProxy("map", data = selected_solardach(), session) |>
        clearGroup("selected_solardach") |>
        addPolygons(
          fillOpacity = 0.5,
          layerId = selected_solardach()$GWR_EGID,
          color = "red",
          opacity = 1,
          weight = 1,
          group = "selected_solardach"
        )
  })
  
  #########################################################################################################################
  # Solarpanel tab
  
  ##Gemeindeinformationen
  output$plot_municipality <- renderPlot({
    color_highlight = c("#fcdc00", "#009946", "#0094de", "#f00001")
    img_url = c("https://upload.wikimedia.org/wikipedia/commons/thumb/2/26/Wappen_Eiken.svg/1200px-Wappen_Eiken.svg.png", 
                "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/CHE_Sisseln_COA.svg/120px-CHE_Sisseln_COA.svg.png",
                "https://upload.wikimedia.org/wikipedia/commons/thumb/8/89/CHE_M%C3%BCnchwilen_COA.svg/1200px-CHE_M%C3%BCnchwilen_COA.svg.png", 
                "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d3/CHE_Stein_COA.svg/120px-CHE_Stein_COA.svg.png")
    
    plots <- solardach |>
      st_drop_geometry() |>
      group_by(Municipality, Has_Solar) |>
      summarize(Electricity_Yield = sum(Electricity_Yield)) |>
      pivot_wider(names_from = Has_Solar, values_from  = Electricity_Yield) |>
      rename(No_Solar = `FALSE`, Has_Solar = `TRUE`) |>
      mutate(Percent = Has_Solar / No_Solar) |>
      na.omit() |>
      cbind(color_highlight, img_url) |>
      rename(color_highlight = "...5", img_url = "...6") |>
      pmap(~big_number_donut_plot(value = ..4, municipality_text_label = ..1, img_url = ..6, font_family = "Inter", highlight_color = ..5))
    
    ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], ncol = 2, nrow = 2)
  })
  
  ###################################################
  #Where I am the champ
  observeEvent(input$distance_king, {
    champ_distance <- solardach |>
      filter(Electricity_Yield >= selected_solardach()$Electricity_Yield) |>
      mutate(distance_m = st_distance(geom, selected_solardach()) |> as.data.frame()) |>
      arrange(distance_m) |>
      mutate(Rank = row_number()) |> 
      filter(Rank == 2) |>
      st_drop_geometry()
      
    champ_distance_circle <- st_buffer(selected_solardach(), dist = as.integer(round(champ_distance$distance_m[[1]], 0)))
      
    leafletProxy("map", data = champ_distance_circle, session) |>
      clearGroup("ranking") |>
      clearGroup("ranking_top_3") |>
      clearGroup("champ_distance") |>
      addPolygons(
        fillOpacity = 0.5,
        layerId = champ_distance_circle$GWR_EGID,
        color = "orange",
        opacity = 1,
        weight = 1,
        group = "champ_distance"
      ) 
  })
  
  ###################################################
  #Ranking Nachbarschaft
  ranking_list <- reactive({
    if(input$rankingtype == "Street") {
      ranking <- solardach |>
        filter(Street == selected_solardach()$Street, ZIP == selected_solardach()$ZIP) |>
        arrange(desc(Electricity_Yield)) |>
        mutate(Rank = row_number())
    } 
    else if(input$rankingtype == "Bounding box") {
      bounds <- input$map_bounds
      bbox <- c(bounds$west, bounds$south, bounds$east, bounds$north)
      coords <- matrix(c(bbox[1], bbox[2], bbox[1], bbox[4], bbox[3], bbox[4], bbox[3], bbox[2], bbox[1], bbox[2]), ncol = 2, byrow = TRUE)
      bbox_polygon <- st_sfc(st_polygon(list(coords)), crs = 4326)
      
      solardach$is_in_polygon = st_within(solardach, bbox_polygon) |> sapply(\(x)length(x)>0)
      
      ranking <- solardach |> 
        filter(is_in_polygon) |> 
        arrange(desc(Electricity_Yield)) |>
        mutate(Rank = row_number())
    }
    
    #Hightlight all solar roofs in bbox or street
    leafletProxy("map", data = ranking, session) |>
      clearGroup("ranking") |>
      clearGroup("ranking_top_3") |>
      clearGroup("champ_distance") |>
      addPolygons(
        fillOpacity = 0.5,
        layerId = ranking$GWR_EGID,
        color = "orange",
        opacity = 1,
        weight = 1,
        group = "ranking"
      ) 
    
    #Hightlight the top 3 with gold, silver, bronze
    ranking_top_3 <- ranking |> head(3)
    leafletProxy("map", data = ranking_top_3, session) |>
      clearGroup("ranking_top_3") |>
      addPolygons(
        fillOpacity = 1,
        layerId = ranking_top_3$GWR_EGID,
        color = unname(colorsSchema[names(colorsSchema) %in% ranking_top_3$Rank]),
        opacity = 1,
        weight = 1,
        group = "ranking_top_3"
      ) 
    
    #Add icon for top 3
    ranking_top_3_points <- ranking_top_3 |>
      st_centroid()
    leafletProxy("map", data = ranking_top_3_points, session) |>
      addAwesomeMarkers(
        icon = icons[ranking_top_3_points$Rank],
        group = "ranking_top_3"
      )
    
    #Check if the selected roof is already part of the top 3 otherwise add it as well for the plot
    ranking_top_3 <- ranking_top_3 |> mutate(Rank_label = as.character(Rank))
    if(!any(ranking_top_3$GWR_EGID == selected_solardach()$GWR_EGID)) {
      you_ranking <- ranking |> filter(GWR_EGID == selected_solardach()$GWR_EGID)
      you_ranking$Rank_label <- "You"
      ranking_top_3 <- ranking_top_3 |>
        rbind(you_ranking)
    } else {
      ranking_top_3 <- ranking_top_3 |> 
        mutate(Rank_label = replace(Rank_label, GWR_EGID == selected_solardach()$GWR_EGID, "You"))
    }
    
    ranking_top_3
  })
  
  #Show plot of ranking
  output$plot_ranking <- renderPlotly({
    ranking_list() |>
      st_drop_geometry() |>
      mutate(Adress = paste(Street, Number)) |>
      plot_ly(
        x = ~ reorder(Adress, Rank),
        y = ~ Electricity_Yield,
        text = ~ ifelse(Rank_label == as.character(Rank), Rank_label, paste(as.character(Rank), "-" , Rank_label)), 
        textposition = 'auto',
        type = "bar",
        marker = list(color = ~ color_scale(Rank_label),
                      line = list(color = 'black', width = 1.5))
      ) |>
      config(displayModeBar = FALSE) |>
      layout(hovermode = TRUE,
             showlegend = FALSE,
             xaxis = list(title = "Solar roof"),
             yaxis = list(title = "Electricity Yield"))
  })

  ######################################################################################################################
  #Outputs for car tab
  
  selected_car <- reactive({
    cars |> filter(model == input$car)
  })
  
  #Update slider with max hours of charging time
  observeEvent(input$car, {
    times <- seq(from = 0, to = selected_car()$battery_charge_time, by = 0.5)
    slider_labels <- lapply(times, function(hours) {
      decimal_part <- hours - floor(hours)
      minutes <- decimal_part * 60
      slider_labels <- paste0(floor(hours), ":", sprintf("%02d", minutes))
      slider_labels
    })
    updateSliderTextInput(session = session, inputId = "charginghours", choices = slider_labels)
  })

  ##Car information
  output$vehicle_info <- renderTable({
    selected_car() |>
      select(model, max_range_km, battery_capacity_kwh) |>
      mutate(max_range_km = formatC(max_range_km, format="f", big.mark=",", digits=1),
             battery_capacity_kwh = formatC(battery_capacity_kwh, format="f", big.mark=",", digits=1)) |>
      rename("Model" = model, 
            "Max range (km)" = max_range_km, 
            "Battery Capacity (kwh)" = battery_capacity_kwh)
  })
  output$carimage <- renderUI({
    url <- cars |> filter(model == input$car) |> pull(url)
    tags$img(src = url, width = "100%", height="auto")
  })
  
  ##Weather information
  output$tab_weather <- function() {
    table <- weather_data |> 
      t() |> 
      kable(format = "html", escape = F) |>
      kable_styling(bootstrap_options = "striped")
  }
  
  output$currentTime <- renderText({
    invalidateLater(as.integer(8.64e+7), session)
    paste("Current time:", format(Sys.time(), "%a, %d %b %Y %H:%M"))
  })
  
  ##Generate isoline for car
  observeEvent(input$rayshade, {
    selected_solardach <- selected_solardach() |>
      st_centroid() #Get point of selected location
    
    charging_hours <- as.numeric(hm(input$charginghours), "hours")

    range_per_day_km <- ceiling((selected_car()$max_range_km / selected_car()$battery_capacity_kwh) * (selected_solardach$Electricity_Yield / 365 / 24 * charging_hours))
    range_calc <- ifelse(range_per_day_km >= selected_car()$max_range_km, selected_car()$max_range_km, range_per_day_km)
    range_calc <- range_calc |> append(selected_car()$max_range_km)
      
    isolines <- hereR::isoline(
      selected_solardach,
      datetime = Sys.time(),
      traffic = FALSE,
      range = range_calc * 1000,
      range_type = "distance",
      routing_mode = "short",
      transport_mode = "car") |>
      cbind(title = c("Charging time range", "Max range"))

    myPalette <- c("#ff8800", "#fff700")
    colorsSchema <- setNames(myPalette, c("Charging time range", "Max range"))
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
        title = NULL,
        pal = pal,
        values = isolines$title,
        opacity = 1,
        group = "range_calc"
      )
      # addLayersControl(
      #   overlayGroups = c("range_calc"),
      #   options = layersControlOptions(collapsed = FALSE)
      # )
  })
}
