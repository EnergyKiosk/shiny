source("libraries.r")

#options(shiny.host = "160.85.102.45") #overwrite localhost as default to simplify testing in LAN

## mobile detection based on https://g3rv4.com/2017/08/shiny-detect-mobile-browsers
#mobileDetect <- function(inputId, value = 0) {
#  tagList(
#    singleton(tags$head(tags$script(src = "js/mobile.js"))),
#    tags$input(id = inputId,
#               class = "mobile-element",
#               type = "hidden")
#  )
#}
