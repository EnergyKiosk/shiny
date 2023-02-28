library("sf")
library("tidyverse")
library("sfarrow")
# install.packages("sfarrow")

st_layers("data-raw/swissboundaries3d_2023-01_2056_5728.shp/swissBOUNDARIES3D_1_4_TLM_HOHEITSGEBIET.shp")
sisslerfeld <- read_sf("data-raw/swissboundaries3d_2023-01_2056_5728.shp/swissBOUNDARIES3D_1_4_TLM_HOHEITSGEBIET.shp",
               query = "SELECT NAME as name FROM swissBOUNDARIES3D_1_4_TLM_HOHEITSGEBIET WHERE NAME in ('Sisseln',	'Stein (AG)', 'Münchwilen (AG)', 'Eiken')")

st_bbox(sisslerfeld)  |>
 st_as_sfc()  |>
 st_as_text()

st_layers("data-raw/SOLKAT_DACH.gpkg")
read_sf("data-raw/SOLKAT_DACH.gpkg", query = "(SELECT * FROM SOLKAT_CH_DACH LIMIT 1) solar") |>


read_sf("data-raw/SOLKAT_DACH.gpkg", 
query = "(SELECT ST_PolygonFromText('POLYGON((2637380 1263691, 2642887 1263691, 2642887 1267625, 2637380 1267625, 2637380 1263691))') as geom) solar")

solardach <- read_sf("data-raw/SOLKAT_DACH.gpkg", query = "SELECT st_intersection(solar.geom, sisslerfeld.geom) as geom_intersect, * FROM (SELECT * FROM SOLKAT_CH_DACH) solar, (SELECT ST_PolygonFromText('POLYGON((2637380 1263691, 2642887 1263691, 2642887 1267625, 2637380 1267625, 2637380 1263691))') as geom) sisslerfeld WHERE ST_intersects(solar.geom, sisslerfeld.geom)")

solardach <- solardach |>
    select(-geom)

solardach <- solardach |>
    st_transform(4326)

sisslerfeld <- sisslerfeld |>
    st_transform(4326)


write_sf(solardach, "data-processed/prepared_data.gpkg", "solardach", delete_layer = TRUE)

sfarrow::st_write_parquet(solardach,"data-processed/solardach.parquet")

write_sf(sisslerfeld, "data-processed/prepared_data.gpkg", "sisslerfeld", delete_layer = TRUE)





