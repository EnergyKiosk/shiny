source("utils.r")

##Server
server <- function(input, output, session) {
  
  #Prepare and show manual
  output$slickr <- renderSlickR({
    imgs <- sort(list.files("www/manual/2560x1440/", pattern=".PNG", full.names = TRUE), decreasing = TRUE)
    slickR(imgs)
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
        addMapPane("selected_solardach", zIndex = 320) |>
        addPolygons(
          fillOpacity = 0.5,
          layerId = selected_solardach()$GWR_EGID,
          color = "red",
          opacity = 1,
          weight = 1,
          group = "selected_solardach",
          options = pathOptions(pane = "selected_solardach")
        )
    
    selected_solardach <- selected_solardach() |>
      st_centroid() #Get point of selected location
    
    leafletProxy("map", data = selected_solardach, session) |>
      addMapPane("selected_solardach_icon", zIndex = 320) |>
      addAwesomeMarkers(
        icon = makeAwesomeIcon(
          icon = 'house',
          iconColor = 'black',
          library = 'fa',
          markerColor = "red"
        ),
        group = "selected_solardach",
        options = pathOptions(pane = "selected_solardach_icon")
      )
  })
  
  #########################################################################################################################
  # Solarpanel tab
  
  ##Gemeindeinformationen
  # output$plot_municipality <- renderPlot({
  #   color_highlight = c("#fcdc00", "#009946", "#0094de", "#f00001")
  #   img_url = c("https://upload.wikimedia.org/wikipedia/commons/thumb/2/26/Wappen_Eiken.svg/1200px-Wappen_Eiken.svg.png", 
  #               "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/CHE_Sisseln_COA.svg/120px-CHE_Sisseln_COA.svg.png",
  #               "https://upload.wikimedia.org/wikipedia/commons/thumb/8/89/CHE_M%C3%BCnchwilen_COA.svg/1200px-CHE_M%C3%BCnchwilen_COA.svg.png", 
  #               "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d3/CHE_Stein_COA.svg/120px-CHE_Stein_COA.svg.png")
  #   
  #   plots <- solardach |>
  #     st_drop_geometry() |>
  #     group_by(Municipality, Has_Solar) |>
  #     summarize(Electricity_Yield = sum(Electricity_Yield)) |>
  #     pivot_wider(names_from = Has_Solar, values_from  = Electricity_Yield) |>
  #     rename(No_Solar = `FALSE`, Has_Solar = `TRUE`) |>
  #     mutate(Percent = Has_Solar / No_Solar) |>
  #     na.omit() |>
  #     cbind(color_highlight, img_url) |>
  #     rename(color_highlight = "...5", img_url = "...6") |>
  #     pmap(~big_number_donut_plot(value = ..4, municipality_text_label = ..1, img_url = ..6, font_family = "Inter", highlight_color = ..5))
  # 
  #   ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], ncol = 4, nrow = 1) + bgcolor("#f5f5f5")
  # })
  
  output$plot_sunburst <- renderPlotly({
    df <- solardach |>
      mutate(selected_area = ifelse(GWR_EGID %in% included_ranked_solarroofs()$GWR_EGID, TRUE, FALSE)) |>
      mutate(selected_roof = ifelse(GWR_EGID == selected_solardach()$GWR_EGID, TRUE, FALSE)) |>
      st_drop_geometry() |>
      group_by(Municipality, Has_Solar, selected_area, selected_roof) |>
      summarize(Electricity_Yield = sum(Electricity_Yield, na.rm = TRUE)) |>
      ungroup() |>
      na.omit() |>
      tidyr::complete(Municipality, Has_Solar, selected_area, selected_roof, fill = list(Electricity_Yield = 0)) |>
      pivot_wider(names_from = c(Has_Solar, selected_area, selected_roof), values_from = Electricity_Yield) |>
      rename(Not_used_Municipality = `FALSE_FALSE_FALSE`, 
             Not_used_Neighbourhood = `FALSE_TRUE_FALSE`, 
             Not_used_Roof = `FALSE_TRUE_TRUE`, 
             Used_Municipality = `TRUE_FALSE_FALSE`, 
             Used_Neighbourhood = `TRUE_TRUE_FALSE`,
             Used_Roof = `TRUE_TRUE_TRUE`) |>
      select(Municipality, Not_used_Municipality, Not_used_Neighbourhood, Not_used_Roof, Used_Municipality, Used_Neighbourhood, Used_Roof) |> 
      rowwise() |>
      mutate(
        Not_used_Neighbourhood = Not_used_Neighbourhood + Not_used_Roof,
        Used_Neighbourhood = Used_Neighbourhood + Used_Roof) |>
      mutate(Not_used_Municipality = Not_used_Municipality + Not_used_Neighbourhood,
             Used_Municipality = Used_Municipality + Used_Neighbourhood) |>
      pivot_longer(
        cols = -Municipality,
        names_to = "labels",
        values_to = "values"
      ) |>
      mutate(level = sapply(labels, extract_level),
             labels = case_when(
               grepl("^Used", labels) ~ "Used",
               grepl("^Not_used", labels) ~ "Not Used",
               TRUE ~ labels
             )) |>
      select(Municipality, labels, level, values)
    
    df <- df |>
      rbind(df |>
            filter(level == "2") |>
            group_by(Municipality) |>
            summarize(values = sum(values)) |>
            mutate(labels = "Total",
                   level = "1") |>
            select(Municipality, labels, level, values)
    )
     df <- df |> 
      rbind(df |>
              filter(labels == "Total") |>
              group_by(labels) |>
              summarize(values = sum(values)) |>
              mutate(labels = "Total",
                     level = "0",
                     Municipality = "Total") |>
              select(Municipality, labels, level, values)
      )

    transformed_df <- as.data.frame(t(apply(df, 1, transform_data)))
    colnames(transformed_df) <- c("labels", "values", "parents", "ids")
    
    transformed_df <- transformed_df |>
      rowwise() |>
      mutate(percentage = round(as.integer(values) / as.integer(transformed_df$values[transformed_df$labels == "Total"]) * 100, 2))
    
    plot_ly(data = transformed_df, 
            ids = ~ids, 
            labels= ~labels, 
            parents = ~parents, 
            values= ~values, 
            type='sunburst',  
            text = ~paste0(percentage, "%"),
            branchvalues = "total",
            maxdepth = 5) |> 
      layout(
        margin = list(l = 0, r = 0, b = 0, t = 0),
        hoverlabel = list(bgcolor = "black"),
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "#f5f5f5",
        colorway = c("#f00001", "#fcdc00", "#009946", "#0094de"),
        images = list(
          list(source = "https://upload.wikimedia.org/wikipedia/commons/thumb/2/26/Wappen_Eiken.svg/1200px-Wappen_Eiken.svg.png",
               xref = "paper", yref = "paper",
               x = 0.1, y = 1,
               sizex = 0.2, sizey = 0.2,
               opacity = 1
          ),
          list(source = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/CHE_Sisseln_COA.svg/120px-CHE_Sisseln_COA.svg.png",
               xref = "paper", yref = "paper",
               x = 0.1, y = 0.2,
               sizex = 0.2, sizey = 0.2,
               opacity = 1
          ),
          list(source = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/89/CHE_M%C3%BCnchwilen_COA.svg/1200px-CHE_M%C3%BCnchwilen_COA.svg.png",
               xref = "paper", yref = "paper",
               x = 0.8, y = 0.2,
               sizex = 0.2, sizey = 0.2,
               opacity = 1
          ),
          list(source = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d3/CHE_Stein_COA.svg/120px-CHE_Stein_COA.svg.png",
               xref = "paper", yref = "paper",
               x = 0.8, y = 1,
               sizex = 0.2, sizey = 0.2,
               opacity = 1
          )
        )
      ) |>
      config(displayModeBar = FALSE)
  })
  

  ###################################################
  #Ranking Nachbarschaft
  included_ranked_solarroofs <- reactive({
    if(input$rankingtype == "Street") {
      ranking <- solardach |>
        filter(Street == selected_solardach()$Street, ZIP == selected_solardach()$ZIP) |>
        arrange(desc(Electricity_Yield)) |>
        mutate(Rank = row_number())
    } else if(input$rankingtype == "Municipality") {
      zip <- "4332"
      if(!is.null(input$Municipality)) {
        if(input$Municipality == "Stein") {
          zip <- "4332"
        } else if (input$Municipality == "Eiken") {
          zip <- "5074"
        } else if (input$Municipality == "Sisseln") {
          zip <- "4334"
        } else if (input$Municipality == "Muenchwilen") {
          zip <- "4333"
        }
      }
      
      ranking <- solardach |>
        filter(ZIP == zip) |>
        arrange(desc(Electricity_Yield)) |>
        mutate(Rank = row_number())
    } else if(input$rankingtype == "Visible Extent") {
      bounds <- input$map_bounds
      bbox <- c(bounds$west, bounds$south, bounds$east, bounds$north)
      coords <- matrix(c(bbox[1], bbox[2], bbox[1], bbox[4], bbox[3], bbox[4], bbox[3], bbox[2], bbox[1], bbox[2]), ncol = 2, byrow = TRUE)
      bbox_polygon <- st_sfc(st_polygon(list(coords)), crs = 4326)
      
      solardach$is_in_polygon = st_within(solardach, bbox_polygon) |> sapply(\(x)length(x)>0)
      
      ranking <- solardach |> 
        filter(is_in_polygon) |> 
        arrange(desc(Electricity_Yield)) |>
        mutate(Rank = row_number())
    } else if(input$rankingtype == "Where am I King?") {  #Where I am the champ
      champ_distance <- solardach |>
        filter(Electricity_Yield >= selected_solardach()$Electricity_Yield) |>
        mutate(distance_m = st_distance(geom, selected_solardach()) |> as.data.frame()) |>
        arrange(distance_m) |>
        mutate(Rank = row_number()) |> 
        filter(Rank == 2) |>
        st_drop_geometry()
      
      ranking <- st_buffer(selected_solardach(), dist = as.integer(round(champ_distance$distance_m[[1]], 0)))
    }
    ranking
  })

  #Highlight all solar roofs in bbox or street
  observe({
    if(input$rankingtype != "Where am I King?") {
      leafletProxy("map", data = included_ranked_solarroofs(), session) |>
        clearGroup("ranking") |>
        clearGroup("ranking_top_3") |>
        clearGroup("champ_distance") |>
        clearGroup("range_calc") |>
        addMapPane("ranking", zIndex = 315) |>
        addPolygons(
          layerId = included_ranked_solarroofs()$GWR_EGID,
          color = "darkorange",
          fill = FALSE,
          weight = 7,
          group = "ranking",
          options = pathOptions(pane = "ranking")
        )
      
      #Highlight the top 3 with gold, silver, bronze
      ranking_top_3 <- included_ranked_solarroofs() |> head(3)
      
      leafletProxy("map", data =ranking_top_3, session) |>
        addMapPane("ranking_top_3", zIndex = 316) |>
        addPolygons(
          fillOpacity = 1,
          layerId = ranking_top_3$GWR_EGID,
          color = unname(colorsSchema[names(colorsSchema) %in% ranking_top_3$Rank]),
          opacity = 1,
          weight = 1,
          group = "ranking_top_3",
          options = pathOptions(pane = "ranking_top_3")
        )
      
      #Add icon for top 3
      ranking_top_3_points <- ranking_top_3 |> st_centroid()
      leafletProxy("map", data = ranking_top_3_points, session) |>
        addMapPane("ranking_top_3", zIndex = 316) |>
        addAwesomeMarkers(
          icon = icons[ranking_top_3_points$Rank],
          group = "ranking_top_3",
          options = pathOptions(pane = "ranking_top_3")
        )
      } else {
        leafletProxy("map", data = included_ranked_solarroofs(), session) |>
          clearGroup("ranking") |>
          clearGroup("ranking_top_3") |>
          clearGroup("champ_distance") |>
          clearGroup("range_calc") |>
          addMapPane("champ_distance", zIndex = 300) |>
          addPolygons(
            fillOpacity = 0.5,
            layerId = included_ranked_solarroofs()$GWR_EGID,
            color = "orange",
            opacity = 1,
            weight = 1,
            group = "champ_distance",
            options = pathOptions(pane = "champ_distance")
          ) 
      }
  })
  
  #Show plot of ranking
  output$plot_ranking <- renderPlotly({
    if(input$rankingtype == "Where am I King?") {
      plotly_empty() |>
        config(displayModeBar = FALSE) |>
        layout(plot_bgcolor = "#f5f5f5",
               paper_bgcolor = "#f5f5f5")
    } else {
      ranking_top_3 <- included_ranked_solarroofs() |> head(3)
      
      #Check if the selected roof is already part of the top 3 otherwise add it as well for the plot
      ranking_top_3 <- ranking_top_3 |> mutate(Rank_label = as.character(Rank))
      if(!any(ranking_top_3$GWR_EGID == selected_solardach()$GWR_EGID)) {
        you_ranking <- included_ranked_solarroofs() |> filter(GWR_EGID == selected_solardach()$GWR_EGID)
        you_ranking$Rank_label <- "You"
        ranking_top_3 <- ranking_top_3 |> rbind(you_ranking)
      } else {
        ranking_top_3 <- ranking_top_3 |> 
          mutate(Rank_label = replace(Rank_label, GWR_EGID == selected_solardach()$GWR_EGID, "You"))
      }
      
      ranking_top_3 |>
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
               xaxis = list(title = "Address"),
               yaxis = list(title = "Possible electricity yield (kWh)"),
               plot_bgcolor = "#f5f5f5",
               paper_bgcolor = "#f5f5f5")
    }
  })

  ######################################################################################################################
  #Outputs for car tab
  
  selected_car <- reactive({
    cars |> filter(model == input$car)
  })
  
  #Update slider with max hours of charging time
  observeEvent(input$car, {
    times <- seq(from = 0, 
                 to = 24, #selected_car()$battery_charge_time, 
                 by = 1)
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
      select(max_range_km, battery_capacity_kwh, battery_charge_time, charge_type) |>
      mutate(max_range_km = formatC(max_range_km, format="f", big.mark=",", digits=1),
             battery_capacity_kwh = formatC(battery_capacity_kwh, format="f", big.mark=",", digits=1)) |>
      rename("Max range (km)" = max_range_km, 
            "Battery Capacity (kwh)" = battery_capacity_kwh,
            "Charge time (h)" = battery_charge_time,
            "Charge type" = charge_type)
  })
  output$carimage <- renderUI({
    url <- cars |> filter(model == input$car) |> pull(url)
    tags$img(src = url, width = "50%", height="auto")
  })
  
  ##Weather information
  output$tab_weather <- function() {
    weather_data <-weather_data |>
      select(date, icon)
    colnames(weather_data) <- NULL
    table <- weather_data |> 
      t() |> 
      kable(format = "html", escape = F, align = rep("c", 3)) |>
      kable_styling(bootstrap_options = c("striped"))
  }
  
  output$currentTime <- renderText({
    invalidateLater(as.integer(8.64e+7), session)
    paste("Current time:", format(Sys.time(), "%a, %d %b %Y %H:%M"))
  })
  
  ##Generate isoline for car
  observeEvent(input$traveling_distance_go, {
    selected_solardach <- selected_solardach() |>
      st_centroid() #Get point of selected location
    
    selected_charging_hours <- as.numeric(hm(input$charginghours), "hours")
    max_charging_speed_car_per_hour <- selected_car()$battery_capacity_kwh / selected_car()$battery_charge_time
    max_charging_speed_roof_per_hour <- (selected_solardach$Electricity_Yield_Summer / 183 / 24) * (mean(weather_data$sunhours) / (663 / 92)) # 183 -> days from 1. April to 30. Sept
                                                                                                                                              # 663 -> normed 1991-2020 sun hours in Basel in Summer (June, July, August)
                                                                                                                                              #  92 -> days from 1. June to 31. Aug
    actual_charging_speed_per_hour <- ifelse(max_charging_speed_roof_per_hour >= max_charging_speed_car_per_hour, max_charging_speed_car_per_hour, max_charging_speed_roof_per_hour)

    range_per_day_km <- ceiling(selected_car()$max_range_km * ((actual_charging_speed_per_hour * selected_charging_hours) / (max_charging_speed_car_per_hour * selected_car()$battery_charge_time)))
    range_calc <- ifelse(range_per_day_km >= selected_car()$max_range_km, selected_car()$max_range_km, range_per_day_km)
    range_calc <- range_calc |> append(selected_car()$max_range_km)
    
    transport_mode <- ifelse(selected_car()$vehicle_type %in% c("scooter", "bike"), "pedestrian", "car")
      
    isolines <- hereR::isoline(
      selected_solardach,
      datetime = Sys.time(),
      traffic = FALSE,
      range = range_calc * 1000,
      range_type = "distance",
      routing_mode = "short",
      transport_mode = transport_mode) |>
      cbind(title = c("Charging time range", "Max range"))

    myPalette <- c("#ff8800", "#fff700")
    colorsSchema <- setNames(myPalette, c("Charging time range", "Max range"))
    pal <- colorFactor(colorsSchema, isolines$title)
    bounds <- st_bbox(isolines) |> as.vector()
      
    leafletProxy("map", data = isolines, session) |>
      flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4]) |>
      clearGroup("ranking") |>
      clearGroup("ranking_top_3") |>
      clearGroup("champ_distance") |>
      clearGroup("range_calc") |>
      clearControls() |>
      addMapPane("range_calc", zIndex = 300) |>
      addPolygons(
        fillColor = ~ pal(isolines$title),
        fillOpacity = 0.2,
        stroke = FALSE,
        group = "range_calc",
        options = pathOptions(pane = "range_calc")
      ) |>
      addLegend(
        "bottomright",
        title = NULL,
        pal = pal,
        values = isolines$title,
        opacity = 1,
        group = "range_calc"
      )
  })
}
