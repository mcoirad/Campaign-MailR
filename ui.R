library(leaflet)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)


navbarPage("Sarasota", id="nav",

  tabPanel("Map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Sarasota Registered Republican Finder")
        
        #textInput("address", "your address plz", "5800 bay shore rd. sarasota, fl 34243"),
        
        
        #selectInput("color", "Color", vars),
        #selectInput("size", "Size", vars, selected = "adultpop"),
        #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
        #  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        #),

        #plotOutput("histCentile", height = 200),
        #plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite",
        'Created with: ', tags$em('Florida State Department of Elections Data'), ' by Mario. D.'
      )
    )
  ),

  tabPanel("Enter your address",
    fluidRow(
      column(2,
             textInput("address", "your address plz", value="5800 bay shore rd. sarasota, fl 34243")      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),

  conditionalPanel("false", icon("crosshair"))
)
