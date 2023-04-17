source("libraries.r")
source('mobileUI.r')
source('mainscreenUI.r')
source('utils.r')

library(profvis)

ui <- f7Page(
  titlePanel("Energy Kiosk"),
  f7SingleLayout(
    navbar = f7Navbar(
      title = "Energy Kiosk",
      hairline = FALSE,
      shadow = TRUE
    ),
    mobileDetect('isMobile'),  #Run mobile detection to define content of UI
    uiOutput("screen")  #This is where dynamically the content is added
  )
)

server <- function(input, output, session) {
  
  profvis({
  
  ##Add mobile or mainscreen UI depending if it is a mobile device or not
  output$screen <- renderUI({
    ifelse(input$isMobile, mobileUI, mainscreenUI)
  })
  
  ##Generate QR code so the URL can be scanned via the mobile device
  output$qrcode = renderPlot({
    p <- qr_code(paste0(session$clientData$url_protocol,"//", session$clientData$url_hostname,":", session$clientData$url_port))
    plot(p)
  })

  ##Generate Map with solar roofs
  output$map <- renderMapdeck({
    mapdeck(location = c(7.97170, 47.54841), style = "mapbox://styles/mapbox/light-v11",zoom = 14, show_view_state = FALSE) |>
      add_sf(data = solardach, layer_id = "solardach", stroke_colour = "#000000")
  })
  
  ##Generate table with information on the roof
  #output$tab <- renderTable({
  #  clicked <- FALSE
    
    # if its not licked, just use 1 as index
  #  if (is.null(input$map_polygon_click)) {
  #    idx <- 1
  #  } else {
  #    js <- input$map_polygon_click
  #    lst <- jsonlite::fromJSON(js)
  #    idx <- lst$index
  #    idx <- clicked_list()$index_new
  #   clicked <- TRUE
  #  }
    
  #  solardach_sel <- solardach[idx,] |>
  #    st_drop_geometry() |>
  #    mutate(across(.fns = as.character)) |>
  #    pivot_longer(everything(),names_to = "Attribute", values_to = "Value") |>
  #    mutate(Attribute = str_to_title(str_replace(Attribute, "_", " ")))
    
  #  if(!clicked){
  #    solardach_sel$Value <- ""
  #  }
  #  solardach_sel
  #})
  
  ##Handle the selection of a car
  
  ##Handle the clicking on a roof
  #clicked_list <- reactive({
  #  clicked_list <- jsonlite::fromJSON(input$map_polygon_click)
  #  clicked_list[["index_new"]] <- clicked_list$index + 1
  #  clicked_list
  #}) |> bindEvent(input$map_polygon_click)

  ##Generate isoline data
  isoline_data <- reactive({
    
    location <- input$map_polygon_click #Get point of selected location
    isoline <- hereR::isoline(location, traffic = FALSE, range = c(), range_type = "consumption", transport_mode = "car")

    isoline
  })
  
  #observe({
  #idx <- clicked_list()$index_new
    
  #clicked_geom <- solardach[idx, ] |> select()
    
  #mapdeck_update(map_id = "map") |>
  # add_sf(clicked_geom, stroke_colour = "#ef1a11", fill_colour = "#ef1a11", layer_id = "clicked", update_view = FALSE) |>
  #add_sf(data = isoline_data(), fillOpacity = 0.2)
  #})
  
  })
}

shinyApp(ui, server)
