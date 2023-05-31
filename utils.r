source("libraries.r")

#Load data
cars <- read_delim("data-processed/electric_cars.csv", delim = ";")
solardach <- read_sf("data-processed/prepared_data.gpkg", "solardach")
weather_data <- read_delim("data-processed/weather_data.csv", delim = ",")

factpal <- colorFactor(c("#320385", "darkgreen") , solardach$Has_Solar) 

##Basemap with Solar roof and Toolbox
basemap <- leaflet() |>
  setView(7.966669, 47.541800, 15) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(data = solardach, 
              group = "solardach",
              fillOpacity = 0.5,
              layerId = ~ GWR_EGID,
              color = ~ factpal(Has_Solar),
              opacity = 1,
              weight = 1,
              popup = ~ paste0(
                "<b>Street: </b>", Street, " ", Number, 
                "<br><b>ZIP: </b>", ZIP, 
                "<br><b>Municipality: </b>", Municipality,
                "<br><img src='", qrcode, "' alt='QR Code' width='200px' height ='200px'>"),
              highlightOptions = highlightOptions(color = "red",
                                                  weight = 2, 
                                                  fillColor = 'red', 
                                                  bringToFront = TRUE)) |>
  addHomeButton(st_bbox(solardach), group = "Sisslerfeld", position = "topright", add = TRUE) |>
  addHomeButton(st_bbox(solardach |> filter(ZIP == "5074")), group = "Eiken", position = "topright", add = TRUE) |>
  addHomeButton(st_bbox(solardach |> filter(ZIP == "4333")), group = "Münchwilen", position = "topright", add = TRUE) |>
  addHomeButton(st_bbox(solardach |> filter(ZIP == "4334")), group = "Sisseln", position = "topright", add = TRUE) |>
  addHomeButton(st_bbox(solardach |> filter(ZIP == "4332")), group = "Stein", position = "topright", add = TRUE)

big_number_donut_plot <- function(value, municipality_text_label, img_url, font_family, highlight_color) {
  df <- tibble(x = 1, y = value) |> 
    mutate(y_negative = 1 - y) |>  
    pivot_longer(cols = -x) 

  big_number_text_label <- percent(value, accuracy = 1)
  
  p <- ggplot(df, aes(x = x, y = value, fill = name)) +
    geom_col(show.legend = FALSE) +
    coord_polar(theta = "y", direction = -1) +
    xlim(c(-2, 2)) +
    scale_fill_manual(values = c(highlight_color, "grey90")) +
    theme_void() +
    annotate("text", label = big_number_text_label, family = font_family, fontface = "bold", color = highlight_color, size = 5, x = -0.3, y = 0) +
    annotate("text", label = municipality_text_label, family = font_family, fontface = "bold", color = highlight_color, size = 5, x = -1.3, y = 0)

  img <- rasterGrob(image_read(img_url), interpolate=TRUE)
  
  h <- ggdraw(p)
  h + draw_grob(img, 0.4, 0.3, 0.2, 0.2)
}

ranking_palette <- c("#AF9500", "#D7D7D7", "#AD8A56", "red") 
colorsSchema <- setNames(ranking_palette, c("1","2","3","You"))
color_scale <- colorFactor(palette = ranking_palette, domain = c("1", "2", "3", "You"))

icons <- awesomeIconList(
  "1" = makeAwesomeIcon(
    icon = '1',
    iconColor = '#AF9500',
    library = 'fa',
    markerColor = "black"
  ),
  "2" = makeAwesomeIcon(
    icon = '2',
    iconColor = '#D7D7D7',
    library = 'fa',
    markerColor = "black"
  ),
  "3" = makeAwesomeIcon(
    icon = '3',
    iconColor = '#AD8A56',
    library = 'fa',
    markerColor = "black"
  )
)
