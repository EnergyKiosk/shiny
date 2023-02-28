source("libraries.r")


solardach <- sfarrow::st_read_parquet("data-processed/solardach.parquet")
sisslerfeld <- st_read("data-processed/prepared_data.gpkg", "sisslerfeld")


server <- function(input, output) {

  output$map <- leaflet::renderLeaflet({

    leaflet(solardach) |>
      addPolygons() |>
      addProviderTiles("Stamen.Toner")
})
}
