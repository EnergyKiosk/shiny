library("sf")
library("tidyverse")
library("sfarrow")
# install.packages("sfarrow")

##Sisslerfeld
sisslerfeld <- read_sf("data-raw/swissboundaries3d_2023-01_2056_5728.shp/swissBOUNDARIES3D_1_4_TLM_HOHEITSGEBIET.shp",
               query = "SELECT NAME as name FROM swissBOUNDARIES3D_1_4_TLM_HOHEITSGEBIET WHERE 
               NAME in ('Sisseln',	'Stein (AG)', 'Münchwilen (AG)', 'Eiken')")

st_bbox(sisslerfeld)
#    xmin    ymin    xmax    ymax 
# 2637380 1263691 2642887 1267625 

st_bbox(sisslerfeld)  |>
 st_as_sfc()  |>
 st_as_text()

###Adress information
gebaeudeadressverzeichnis <- read_delim("data-raw/ch.swisstopo.amtliches-gebaeudeadressverzeichnis/pure_adr.csv") |>
  select(BDG_EGID, STN_LABEL, ADR_NUMBER, ZIP_LABEL, COM_CANTON) |>
  separate(ZIP_LABEL, c("ZIP", "MUNICIPALITY"), sep = "\\s(?=\\S)") |>
  filter(ZIP %in% c("4334", "4332", "4333", "5074")) |>
  group_by(BDG_EGID, STN_LABEL, ZIP, MUNICIPALITY, COM_CANTON) |>
  summarize(ADR_NUMBER = paste(ADR_NUMBER, collapse = ", "))

#Existierende Solaranlagen
existing_solarroofs <- read_csv("data-raw/ch.bfe.elektrizitaetsproduktionsanlagen/ElectricityProductionPlant.csv") |>
  drop_na("_x") |>
  st_as_sf(coords = c("_x", "_y"), crs = 2056, remove = FALSE) |>
  st_transform(4326)

##Solardach data
create_qr_code <- function(featureId) {
  path <- file.path("www/qrcodes", paste0(featureId, ".png"))
  if(!file.exists(path)) {
    qrcode <- qr_code(paste0("https://www.uvek-gis.admin.ch/BFE/sonnendach/index.html?featureId=", featureId, "&lang=de"))
    png(path, width = 200, height = 200, units = "px", bg = "white")
    plot(qrcode)
    dev.off()
  }
  file.path("qrcodes", paste0(featureId, ".png"))
}

solardach <- read_sf("data-raw/solarenergie-eignung-daecher_2056.gpkg/SOLKAT_DACH.gpkg", 
                     query = "SELECT st_intersection(solar.geom, sisslerfeld.geom) as geom_intersect, * FROM (
                     SELECT * FROM SOLKAT_CH_DACH) solar, (
                     SELECT ST_PolygonFromText('POLYGON((2637380 1263691, 2642887 1263691, 2642887 1267625, 2637380 1267625, 2637380 1263691))') as geom) sisslerfeld 
                     WHERE ST_intersects(solar.geom, sisslerfeld.geom)") |>
  select(FLAECHE, STROMERTRAG, GWR_EGID, DF_UID) |>
  na.omit() |>
  group_by(GWR_EGID) |>
  summarise(
    FLAECHE = round(sum(FLAECHE), 0),
    STROMERTRAG = sum(STROMERTRAG),
    DF_UID = first(DF_UID)) |>
  st_cast() |>
  left_join(gebaeudeadressverzeichnis, by = c("GWR_EGID" = "BDG_EGID")) |>
  rename(Street = STN_LABEL, Number = ADR_NUMBER, Area = FLAECHE, Municipality = MUNICIPALITY, Canton = COM_CANTON, Electricity_Yield = STROMERTRAG) |>
  rowwise() |>
  mutate(qrcode = create_qr_code(DF_UID)) |>
  ungroup() |>
  st_transform(4326)

solardach$Has_Solar = st_intersects(solardach, existing_solarroofs) |> sapply(\(x)length(x)>0)

sisslerfeld <- sisslerfeld |>
    st_transform(4326)

write_sf(solardach, "data-processed/prepared_data.gpkg", "solardach", delete_layer = TRUE)
write_sf(solardach, "data-processed/solardach.geojson")
sfarrow::st_write_parquet(solardach,"data-processed/solardach.parquet")
write_sf(sisslerfeld, "data-processed/prepared_data.gpkg", "sisslerfeld", delete_layer = TRUE)

###Weather data
select_weather_icon <- function(input) {
  if(input <= 2.0) {
    "<i class='fa-solid fa-clouds fa-2xl' style='color: #919191;'></i>"
  } else if (input > 2.0 & input <= 6.0) {
    "<i class='fa-duotone fa-cloud-sun fa-2xl' style='--fa-primary-color: #919191; --fa-secondary-color: #d6ba00;'></i>"
  } else {
    "<i class='fa-solid fa-sun fa-2xl' style='color: #d6ba00;'></i>"
  }
}

Sys.setlocale("LC_TIME", "en_US.UTF-8")

forecast_header <- read.delim("https://data.geo.admin.ch/ch.meteoschweiz.prognosen/punktprognosen/COSMO-E-all-stations.csv", skip=24, na = "-999.0", sep = ";", nrows=1) |> select(-X)
forecast_data <- read.delim("https://data.geo.admin.ch/ch.meteoschweiz.prognosen/punktprognosen/COSMO-E-all-stations.csv", skip=27, na = "-999.0", sep = ";", header=F)
colnames(forecast_data) <- colnames(forecast_header)
forecast_moe <- forecast_data[1:ncol(forecast_data)-1] %>% mutate(time = ymd_hm(time, tz="CET")) |> 
  select(starts_with(c("stn", "time", "DURSUN"))) |>
  filter(stn == "MOE") |>
  na.omit()
forecast_final <- mutate(forecast_moe, mean_col = rowMeans(select(forecast_moe, starts_with("DURSUN")), na.rm = TRUE)) |>
  mutate(date = date(time)) |>
  group_by(date) |>
  summarise(sunhours = round(sum(mean_col) / 3600, 2)) |>
  head(3) |>
  mutate(icon = sapply(sunhours, select_weather_icon)) |>
  mutate(date = format(date, format = "%a, %d. %b %Y")) |>
  select(date, icon)

write_csv(forecast_final, "data-processed/weather_data.csv", append = FALSE)

