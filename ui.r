
source("libraries.r")


ui <- fluidPage(

  titlePanel("EnergyKiosk"),

  sidebarLayout(
    sidebarPanel(
      tableOutput("tab"),
      actionButton("rayshade", "Go!",)
    ),

    mainPanel(
       mapdeckOutput("map",height = "90vh")
    )
  )
)