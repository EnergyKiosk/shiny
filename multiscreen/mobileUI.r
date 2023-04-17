source("libraries.r")

mobileUI <- list(
  f7Row(
    f7Col(
      f7Card(
        f7Badge("Mobile", color = "green"),
        mapdeckOutput("map"), ##Map to select roof
        f7SmartSelect("car", "Select a car: ", choices = c(
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
        f7Stepper("chargetime", "Hours of charge: ", min = 0, max = 24, value = 12, step = 1) ##Stepper to select the charge time for the car
      )
    )
  )
)
