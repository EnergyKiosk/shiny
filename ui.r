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
            top: 30px;
            right: 45px;
            font-size: 60px;
            background-color: white;
          }    
          .center {
            margin: auto;
            width: 100%;
            padding: 30px;
          }
          #mySidebarPanel {
            height: 90vh;
          }
          #invisibleLayer1 {
            position: absolute;
            top: 100px; <!-- need to be changed depending on the screen size, change background color to see the div -->
            left: 80px; <!-- need to be changed depending on the screen size, change background color to see the div -->
            width: 100px;
            height: 100px;
            background-color: tansparent; <!-- yellow; -->
            opacity: 0.8;
            cursor: pointer;
            z-index: 9999;
          } 
          #invisibleLayer2 {
            position: absolute;
            top: 100px; <!-- need to be changed depending on the screen size, change background color to see the div -->
            left: 475px; <!-- need to be changed depending on the screen size, change background color to see the div -->
            width: 100px;
            height: 100px;
            background-color:  tansparent; <!-- red; -->
            opacity: 0.8;
            cursor: pointer;
            z-index: 9999;
          }
          #invisibleLayer3 {
            position: absolute;
            top: 435px; <!-- need to be changed depending on the screen size, change background color to see the div -->
            left: 80px; <!-- need to be changed depending on the screen size, change background color to see the div -->
            width: 100px;
            height: 100px;
            background-color:  tansparent; <!-- green; -->
            opacity: 0.8;
            cursor: pointer;
            z-index: 9999;
          }
          #invisibleLayer4 {
            position: absolute;
            top: 435px; <!-- need to be changed depending on the screen size, change background color to see the div -->
            left: 475px; <!-- need to be changed depending on the screen size, change background color to see the div -->
            width: 100px;
            height: 100px;
            background-color: tansparent;  <!-- blue; -->
            opacity: 0.8;
            cursor: pointer;
            z-index: 9999;
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
            
            function triggerMunicipalityEvent(attribute) {
              Shiny.setInputValue('Municipality', {attribute : attribute});
            }
        ")
      )
    ),
    div(id = "overlay_question", class = "overlay",
        slickROutput("slickr"),
        tags$a(href = "javascript:void(0)",
          class = "closebtn",
          onclick = "closeNav()",
          tags$i(class = "fa-solid fa-circle-xmark fa-xl", style = "color: red;")
        )
    ),
    div(id = "overlay_info", class = "overlay",
      fluidRow(
        column(
          width = 6,
          div(class = "center",
            style = "height: 45vh; background-color: white; border: 2px solid black; border-radius: 5px;",
            h1("Sisslerfeld", a(href = "https://www.sisslerfeld.ch/", img(src = "info/sisslerfeld.jpg", height = "100px"), target = "blank"), style = "font-size:24px;"),
            hr(),
            p("The largest employment zone in the canton of Aargau is located in the Sisslerfeld. Good jobs 
              are to be created here. At the same time, the beautiful landscape should be strengthened and traffic growth limited. 
              For this to succeed, framework conditions and coordination are needed. The municipalities of Eiken, Münchwilen, 
              Sisseln and Stein, the Fricktal Regio Regional Planning Association and the Canton of Aargau are therefore working 
              together to make the Sisslerfeld a success story for everyone. The town of Bad Säckingen and the German regional 
              association are also involved.", style = "font-size:20px;")
          )
        ), 
        column(
          width = 6,
          div(class = "center",
              style = "height: 45vh; background-color: white; border: 2px solid black; border-radius: 5px;",
              div(
                a(href = "https://energylivinglab.com/de/", img(src = "info/energylivinglab.png", height = "100px"), target = "blank"),
                a(href = "https://www.innosuisse.ch/inno/en/home/project-set-up-assistance-and-networking/innovation-booster.html", img(src = "info/logo-Innovation-Booster.png", height = "100px"), target = "blank")
              ),
              hr(),
              p("The Energy Living Lab Association (ELLA) was founded in 2020 as a spin-off to replicate, disseminate and communicate 
                Living Lab approaches in the energy sector. It supports an ecosystem of actors in using the methods and tools and invites 
                people and institutions from the public, private, governmental and academic sectors as well as civil society to co-create 
                solutions for the decarbonisation of energy supply.", style = "font-size:20px;")
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          div(class = "center",
              style = "height: 45vh; background-color: white; border: 2px solid black; border-radius: 5px;",
              a(href = "https://nccr-automation.ch", img(src = "info/nccr_automation.png", height = "100px"), target = "blank"), 
              hr(),
              p("NCCRs are interdisciplinary research networks funded by the Swiss National Science Foundation (SNSF). 
              They support research in areas that are of strategic importance for the future of Swiss science, economy and society. 
              The National Centre of Competence in Research «Dependable, ubiquitous automation», NCCR Automation for short, 
                investigates new approaches to the control of complex automated systems and implements them in concrete applications in practice.
                Through networked research, the development of new technologies and education, the NCCR aims to strengthen Switzerland's leading role 
                in automation and control technology.", style = "font-size:20px;")
          )
        ),
        column(
          width = 6,
          div(class = "center",
              style = "height: 45vh; background-color: white; border: 2px solid black; border-radius: 5px;",
              a(href = "https://www.zhaw.ch/de/lsfm/institute-zentren/iunr/geooekologie/geoinformatik/", img(src = "info/zhaw_lsfm_iunr_4f.png", height = "80px"), target = "blank"),
              hr(),
              p("The Geoinformatics Research Group focuses on the development, application and validation of innovative methods 
                for processing and analysing spatiotemporal data on topics relating to the environment and natural resources. 
                As a centre of excellence for industry-focused approaches to data, software and infrastructure, the Group provides 
                solutions-based and customer-focused advice on the planning and implementation of geoinformatics projects.", style = "font-size:20px;")
          )
        )
      ),
      tags$a(
        href = "javascript:void(0)",
        class = "closebtn",
        onclick = "closeNav()",
        tags$i(class = "fa-solid fa-circle-xmark fa-xl", style = "color: red;")
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
            div(class="fa-solid fa-solar-panel fa-xl"),
            tags$h4("Solar roof potential"),
            #plotOutput("plot_municipality", height = "15vh"),
            plotlyOutput("plot_sunburst", height = "40vh"),
            tags$div(id = "invisibleLayer1", onclick = "triggerMunicipalityEvent('Eiken')"),
            tags$div(id = "invisibleLayer2", onclick = "triggerMunicipalityEvent('Stein')"),
            tags$div(id = "invisibleLayer3", onclick = "triggerMunicipalityEvent('Sisseln')"),
            tags$div(id = "invisibleLayer4", onclick = "triggerMunicipalityEvent('Muenchwilen')"),
            tags$hr(),
            tags$h4("Neighbourhood ranking"),
            div(
              style = "display: flex; flex-direction: row; margin-bottom: 20px;",
              radioButtons("rankingtype", NULL, c("Street", "Municipality", "Visible Extent", "Where am I King?"), selected = "Street", inline = TRUE), 
            ),
            plotlyOutput("plot_ranking", height = "25vh")
          ),
          tabPanel(
            div(class="fa-solid fa-car-side fa-xl"),
            tags$h4("Weather information"),
            textOutput("currentTime"),
            tags$br(),
            tableOutput("tab_weather"),
            tags$hr(),
            tags$h4("Vehicle information"),
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
                    "Riese & Mueller Load 60"
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12, align="center",
                tableOutput("vehicle_info")            
              )
            ),
            fluidRow(
              column(
                width = 12, align="center",
                uiOutput("carimage")
              )
            ),
            tags$br(),
            tags$br(),
            fluidRow(
              column(
                width = 7,
                sliderTextInput(
                  inputId = "charginghours",
                  label = "Charging hours:",
                  choices = "slider",
                  grid = TRUE,
                  hide_min_max = FALSE
                )
              ),
              column(
                width = 5,
                div(
                  style = "margin-top: 30px; margin-left: 70px",
                  actionButton("traveling_distance_go",  HTML("Calculate possible<br/>traveling distance!"))
                )
              )
            )
          )
        )
      ),
      mainPanel(
        leaflet::leafletOutput("map", height = "90vh")
      )
    )
  )
