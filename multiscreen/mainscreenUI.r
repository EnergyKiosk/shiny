source("libraries.r")

mainscreenUI <- list(
  ##Map with selected roof and isochrones
  f7Row(
    f7Col(
      f7Card(
          #mapdeckOutput("map", height = "90vh")
      )
    ),
    ##Table with information on the roof
    ##Static QR code to scan with mobile device
    f7Col(
      f7Card(
        f7Badge("Mainscreen", color = "red"),
        plotOutput("qrcode")
      ),
      f7Card(
        #tableOutput("tab")
      )
    )
  )
)
