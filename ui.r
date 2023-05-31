source("libraries.r")

ui <- 
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(
        HTML("
          .overlay {
            height: 100%;
            width: 0;
            position: fixed;
            z-index: 9999;
            top: 0;
            left: 0;
            background-color: rgb(0,0,0);
            overflow-x: hidden;
            transition: 0.5s;
          }
          .overlay .closebtn {
            position: absolute;
            top: 20px;
            right: 45px;
            font-size: 60px;
          }    
          .center {
            margin: auto;
            width: 100%;
            padding: 30px;
          }
          #mySidebarPanel {
            height: 90vh;
        }
        ")
      ),
      tags$script(
        HTML("
          function openNav(id) {
              document.getElementById(id).style.width = '100%';
            }
            
            function closeNav() {
              document.getElementById(id).style.width = '0%';
            }
        ")
      )
    ),
    div(id = "overlay_question", class = "overlay",
        slickROutput("slickr", height="100%"),
        tags$a(
          href = "javascript:void(0)",
          class = "closebtn",
          onclick = "closeNav()",
          "X"
        ),
    ),
    div(id = "overlay_info", class = "overlay",
      fluidRow(
        column(
          width = 6,
          div(class = "center",
            style = "height: 45vh; background-color: white; border: 2px solid black; border-radius: 5px;",
            h1("Sisslerfeld", img(src = "info/sisslerfeld.jpg", height = "100px")),
            a(href = "https://www.sisslerfeld.ch/", "Webseite Sisslerfeld"),
            hr(),
            p("Im Sisslerfeld liegt die grösste Arbeitsplatzzone des Kantons Aargau. Hier sollen gute Arbeitsplätze entstehen. 
              Dabei sollen die schöne Landschaft gestärkt und das Verkehrswachstum begrenzt werden. Damit dies gelingt, braucht es 
              Rahmenbedingungen und Koordination. Die Gemeinden Eiken, Münchwilen, Sisseln und Stein, der Regionalplanungsverband 
              Fricktal Regio und der Kanton Aargau, arbeiten darum gemeinsam dafür, dass das Sisslerfeld eine Erfolgsgeschichte 
              für alle wird. Auch die Stadt Bad Säckingen und der deutsche Regionalverband wirken mit.")
          )
        ), 
        column(
          width = 6,
          div(class = "center",
              style = "height: 45vh; background-color: white; border: 2px solid black; border-radius: 5px;",
              h1("EnergyLab Booster", img(src = "info/energylabBooster.jpg", height = "100px")),
              a(href = "https://www.energylab.site/about/", "Webseite EnergyLab"),
              hr(),
              p("The organization of the Energy Lab follows a holocratic structure for optimal agility and open innovation. It consists of the following bodies: 
              Leading House, Lab Team, Pitch Jury, Consortium Experts, Project Teams and Advisory Board.")
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          div(class = "center",
              style = "height: 45vh; background-color: white; border: 2px solid black; border-radius: 5px;",
              h1("Ben Sawicki - Opportunity Sisslerfeld", img(src = "info/eth.png", height = "100px")),
              a(href = "https://ladeplan.ch/wp-content/uploads/2022/04/opendata.html", "Webseite Opportunity Sisslerfeld"), 
              hr(),
              p("Video Ben")
          )
        ),
        column(
          width = 6,
          div(class = "center",
              style = "height: 45vh; background-color: white; border: 2px solid black; border-radius: 5px;",
              h1("ZHAW Forschungsgruppe Geoinfromatik", img(src = "info/zhaw_lsfm_iunr_4f.png", height = "100px")),
              a(href = "https://www.zhaw.ch/de/lsfm/institute-zentren/iunr/geooekologie/geoinformatik/", "Webseite FG Geoinformatik"),
              hr(),
              p("Die Forschungsgruppe Geoinformatik beschäftigt sich mit der Entwicklung, Anwendung und Validation innovativer Methoden zur Verarbeitung und Analyse 
              raumzeitlicher Information. Den inhaltlichen Rahmen bilden dabei Themen im Bereich Umwelt und Natürliche Ressourcen. Als Kompetenzzentrum für den 
              praxisbezogenen Umgang mit Daten, Software und Infrastruktur bietet die Gruppe lösungs- und kundenorientierte Beratung zur Planung und Umsetzung von 
              Projekten im Bereich Geoinformatik.")
          )
        )
      ),
      tags$a(
        href = "javascript:void(0)",
        class = "closebtn",
        onclick = "closeNav()",
        "X"
      ),
    ),
    titlePanel(
      windowTitle = "EnergyKiosk - Sisslerfeld",
      div(
        h1(icon("bolt"), icon("store"), "EnergyKiosk - Sisslerfeld"),
        actionButton("manualbutton", label = "Manual", icon = icon("circle-question fa-solid"), style = "position:absolute; top:23px; left:530px; cursor: pointer;"),
        actionButton("infobutton", label = "Information", icon = icon("circle-info"), style = "position:absolute; top:23px; left:650px;"),
        img(src = "https://api.i-web.ch/public/guest/getImageString/g361/777466aa81be18304c099c5891446218/0/0/4a5200092ece4//", height  = "50px", style = "position:absolute; top:0px; right:20px; margin-top:10px; margin-right:20px;"),
        img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/89/CHE_M%C3%BCnchwilen_COA.svg/1200px-CHE_M%C3%BCnchwilen_COA.svg.png", height  = "50px", style = "position:absolute; top:0px; right:70px; margin-top:10px; margin-right:20px;"),
        img(src = "https://api.i-web.ch/public/guest/getImageString/g645/2df716745c4d8c7cd4e595c6f112c4f1/0/0/58070581aac92//", height  = "50px", style = "position:absolute; top:0px; right:120px; margin-top:10px; margin-right:20px;"),
        img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/2/26/Wappen_Eiken.svg/1200px-Wappen_Eiken.svg.png", height  = "50px", style = "position:absolute; top:0px; right:170px; margin-top:10px; margin-right:20px;")
      )
    ),
    sidebarLayout(
      sidebarPanel(
        id = "mySidebarPanel",
        tabsetPanel(
          tabPanel(
            icon("solar-panel"),
            tags$h4("Potential solar"),
            plotOutput("plot_municipality", height = "30vh"),
            tags$hr(),
            tags$h4("Neighbourhood ranking"),
            div(
              style = "display: flex; flex-direction: row; margin-bottom: 20px;",
              radioButtons("rankingtype", NULL, c("Street", "Bounding box"), selected = "Street", inline = TRUE), 
              tags$div(style = "margin-right: 80px;"), 
              div(
                style = "margin-top: -5px;",
                actionButton("distance_king", label = "Where am I King?")
              )
              
            ),
            plotlyOutput("plot_ranking", height = "30vh")
          ),
          tabPanel(
            icon("car-side"),
            tags$h4("Vehicle Information"),
            fluidRow(
              column(
                width = 7,
                selectInput(
                  "car",
                  "Select vehicle:",
                  choices = c(
                    "Tesla Model 3",
                    "Renault ZOE",
                    "Volkswagen ID.3",
                    "BMW i3",
                    "Nissan LEAF",
                    "Hyundai Kona EV",
                    "Audi e-tron",
                    "Jaguar I-PACE",
                    "Kia Niro EV",
                    "Mercedes-Benz EQC",
                    "Xiaomi Mi Electric Scooter Pro",
                    "Specialized Turbo Vado 4.0",
                    "Riese & Müller Load 60"
                  )
                )
              ),
              column(
                width = 5,
                sliderTextInput(
                  inputId = "charginghours",
                  label = "Charging hours:",
                  choices = "slider",
                  grid = TRUE,
                  hide_min_max = FALSE
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                tableOutput("vehicle_info")            
              )
            ),
            fluidRow(
              column(
                width = 8,
                uiOutput("carimage")
              )
            ),
            tags$hr(),
            tags$h4("Weather Information"),
            textOutput("currentTime"),
            tableOutput("tab_weather"),
            tags$hr(),
            actionButton("rayshade", "Go!", )
          )
        )
      ),
      mainPanel(
        leaflet::leafletOutput("map", height = "90vh")
      )
    )
  )
