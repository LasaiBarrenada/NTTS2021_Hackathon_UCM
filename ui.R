
ui <-
  navbarPage(
    h5("International Trade"),
    collapsible = TRUE,
    inverse = TRUE,
    theme = shinytheme("sandstone"),
    tabPanel(h5("Evolution"),
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   selectInput("category.lines", "Select category", labels.categories[1:18]),
                   checkboxGroupInput(
                     "Country.lines",
                     label = "Select countries to plot",
                     choices = list(
                       "Austria" = "AT",
                       "Belgium" = "BE",
                       "Bulgaria" = "BG",
                       "Croatia" = "HR",
                       "Cyprus" = "CY",
                       "Czechia" = "CZ",
                       "Denmark" = "DK",
                       "Estonia" = "EE",
                       "Finland" = "FI",
                       "France" = "FR",
                       "Germany" = "DE",
                       "Greece" = "GR",
                       "Hungary" = "HU",
                       "Ireland" = "IE",
                       "Italy" = "IT",
                       "Latvia" = "LV",
                       "Lithuania" = "LT",
                       "Luxembourg" = "LU",
                       "Malta" = "MT",
                       "Netherlands" = "NL",
                       "Poland" = "PL",
                       "Portugal" = "PT",
                       "Romania" = "RO",
                       "Slovakia" = "SK",
                       "Slovenia" = "SI",
                       "Spain" = "ES",
                       "Sweden" = "SE",
                       "EU-27" = "EU-27"
                     ),
                     selected = "EU-27"
                   ),
                   radioButtons(
                     inputId = "Trade_type.lines",
                     label = "Choose the trade type",
                     choices = c("Import", "Export")
                   )
                   ,
                   width = 2
                 ),
                 mainPanel(plotOutput("lineplot", height = 800), width = 10)
               ),
             ))
    ,
    tabPanel(h5("Treemap"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("country", "Select country", labels.countries),
                 selectInput("period", "Year", c(2019, 2020), selected = 2020),
                 selectInput("category", "Select category", labels.categories),
                 radioButtons(
                   inputId = "Trade_type",
                   label = "Choose the trade type",
                   choices = c("Import", "Export")
                 )
                 ,
                 width = 2
               )
               ,
               mainPanel(
                 plotOutput("threemap_population_country"),
                 plotOutput("threemap_population_category")
                 
                 ,
                 width = 10
               )
             )),
    tabPanel(h5("Trade flow map"),
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   selectInput("category.network", "Select category", labels.categories),
                   selectInput("country.network", "Select country", labels.countries[1:27]),
                   selectInput("period.net", "Select Year", c(2019, 2020), selected = 2020),
                   sliderInput("month.net", "Select month", min = 1, max = 12 , value = 1),
                   radioButtons(
                     inputId = "Trade_type.network",
                     label = "Choose the trade type",
                     choices = c("Import", "Export")
                   )
                   ,
                   width = 2
                 )
                 ,
                 mainPanel(plotOutput("global_map", height = 800), width = 10)
               )
             )),
    tabPanel(h5("Network"), sidebarPanel(
      selectInput("category.red", "Select category", labels.categories),
      radioButtons(
        inputId = "Trade_type.red",
        label = "Choose the trade type",
        choices = c("Import", "Export")
      )
    ,width = 2), mainPanel(plotOutput("red", height = 800), width = 10)),
    tabPanel(h5("About"), mainPanel(
      HTML(
        "<p>&nbsp;</p>
  <h1>THE TEAM</h1>
  <p><h2>We are  students of EMOS UCM:</h2></p>
  <p><h3>LASAI BARREÑADA TALEB lbarrena@ucm.es</h3> </p>
  <p><h3> RODRIGO GONZALEZ SANTAMARIA rodrgo07@ucm.es </h3></p>
  <p><h3>VICTOR PEREZ SEGURA victop01@ucm.es </h3></p>
    <h1>Thank you for your attention! </h1>

"
        
      )))
  
)


