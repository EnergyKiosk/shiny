source("libraries.r")

server <- function(input, output) {

  output$map <- leaflet::renderLeaflet({

    leaflet()
})
}
