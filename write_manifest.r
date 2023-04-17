appfiles <- c(
    "server.r",
    "ui.r",
    "data-processed/solardach.parquet",
    "data-processed/electric_cars.csv",
    "libraries.r"
    )
rsconnect::writeManifest(appFiles = appfiles)
