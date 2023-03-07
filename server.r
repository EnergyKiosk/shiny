source("libraries.r")


solardach <- sfarrow::st_read_parquet("data-processed/solardach.parquet")
sisslerfeld <- st_read("data-processed/prepared_data.gpkg", "sisslerfeld")




server <- function(input, output) {
  output$map <- renderMapdeck({
    # set "show_view_state" to true to see the map view state in the shiny app
    # zoom is ignored, maybe create an issue?
    mapdeck(location = c(7.97170, 47.54841), style = "mapbox://styles/mapbox/light-v11",zoom = 14, show_view_state = FALSE) |>
      add_sf(data = solardach,layer_id = "solardach", stroke_colour = "#000000")
  })


  # observeEvent({input$map_polygon_click},{
  #       # print(input$map_polygon_click)
  #       js <- input$map_polygon_click
  #       lst <- jsonlite::fromJSON(js)
  #       print(lst)
  #   })

  clicked_list <- reactive({
    clicked_list <- jsonlite::fromJSON(input$map_polygon_click)
    # the index is zero based!!!!
    # https://github.com/SymbolixAU/mapdeck/blob/bbee4da6cca1de1dcfc7fd140ee572bec33ef53f/R/mapdeck_map_utilities.R#L142

    clicked_list[["index_new"]] <- clicked_list$index + 1

    clicked_list
  }) |>
  bindEvent(input$map_polygon_click)

  observe({
    

    idx <- clicked_list()$index_new

    
    clicked_geom <- solardach[idx, ] |>
      select()

    # print(clicked_geom)
    
    mapdeck_update(map_id = "map") |>
      add_sf(clicked_geom,stroke_colour = "#ef1a11",fill_colour = "#ef1a11", layer_id = "clicked",update_view = FALSE)

   })



  output$tab <- renderTable({
    clicked <- FALSE

    # if its not licked, just use 1 as index
    if (is.null(input$map_polygon_click)) {
      idx <- 1

    } else {
      js <- input$map_polygon_click
      lst <- jsonlite::fromJSON(js)
      idx <- lst$index
      idx <- clicked_list()$index_new
      clicked <- TRUE
    }

    solardach_sel <- solardach[idx,] |>
        st_drop_geometry() |>
        mutate(across(.fns = as.character)) |>
        pivot_longer(everything(),names_to = "Attribute", values_to = "Value") |>
        mutate(Attribute = str_to_title(str_replace(Attribute, "_", " ")))

    if(!clicked){
      solardach_sel$Value <- ""
    }
    solardach_sel

  })




}
