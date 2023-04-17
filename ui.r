source("libraries.r")

ui <- fluidPage(
  
  titlePanel("EnergyKiosk"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("car", "Select a car: ", choices = c(
        "Tesla Model S Long Range",
        "Tesla Model X Long Range",
        "Tesla Model 3 Long Range",
        "Tesla Model Y Long Range",
        "Ford Mustang Mach-E",
        "Chevrolet Bolt EV",       
        "Porsche Taycan 4S",       
        "Audi e-tron",             
        "Volkswagen ID.4",         
        "Nissan Leaf Plus",        
        "BMW i3",                  
        "Hyundai Kona Electric",   
        "Kia Niro EV",             
        "Rivian R1T",              
        "Polestar 2",              
        "Mercedes-Benz EQC",       
        "Mini Cooper SE")),
      tableOutput("tab"),
      actionButton("rayshade", "Go!",)
    ),
    
    mainPanel(
      leaflet::leafletOutput("map", height = "90vh")
    )
  )
)