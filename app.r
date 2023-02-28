library(shiny)
library(deckgl)

backend <- function(input, output) {
  output$rdeck <- renderDeckgl({
    deckgl() %>%
      add_grid_layer(
        data = sf_bike_parking,
        getPosition = ~lng + lat,
        cellSize = 400,
        pickable = TRUE
      ) %>%
    add_basemap()
  })
}

frontend <- fluidPage(
  deckglOutput("rdeck")
)

shinyApp(frontend, backend)