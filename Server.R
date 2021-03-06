
server <- function(input, output) {
  period <- reactive({
    dt.2020.filtered[year == input$period]
  })
  category <- reactive({
    period()[category == input$category]
  })
  
  tradetypeInput.lines <- reactive({
    switch(input$Trade_type.lines,
           "Import" = 1,
           "Export" = 2)
  })
  tradetypeInput <- reactive({
    switch(input$Trade_type,
           "Import" = 1,
           "Export" = 2)
  })
  
  tradetypeInput.network <- reactive({
    switch(input$Trade_type.network,
           "Import" = "import",
           "Export" = "export")
  })
  tradetypeInput.red <- reactive({
    switch(input$Trade_type.red,
           "Import" = "import",
           "Export" = "export")
  })
  
  countryInput <- reactive({
    switch(
      input$country,
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
      "EU-27" = "Total"
    )
  })
  countryInput.network <- reactive({
    switch(
      input$country.network,
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
      "EU-27" = "Total"
    )
  })
  countryInputlines <- reactive({
    switch(
      input$country.lines,
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
    )
  })
  EU27input <- reactive({
    is.element("EU-27",input$country.lines)
  })  
  categoryInput.lines <- reactive({
    switch (
      input$category.lines,
      "COVID-19 Tests (kits and instruments)" = "COVID19_t",
      "FFP2 Facemasks" = "facemasksFFP2",
      "Surgical masks" = "surgical_masks",
      "Protective head caps" = "head_caps",
      "Gloves (excl. plastic gloves under B4)" = "gloves",
      "Protective gargamets excluding head caps and gloves" = "gargamets",
      "Other protective accessories" = "other_acc",
      "Disinfectants products" = "disinfectans",
      "Sterilizers" = "sterilizers",
      "Devices for oxigen therapy" = "oxigen_therapy",
      "Medical devices" = "medical_dev",
      "Thermometers" = "thermometers",
      "Mediccines" = "mediccines",
      "Bandages" = "bandages",
      "Sterile tubes" = "sterile_tubes",
      "Syringes" = "syringes",
      "Other medical" = "other_med",
      "Medical vehicles and furniture" = "vehicles",
      "All Covid products" = "Total"
    )
  })
    categoryInput <- reactive({
    switch (
      input$category,
      "COVID-19 Tests (kits and instruments)" = "COVID19_t",
      "FFP2 Facemasks" = "facemasksFFP2",
      "Surgical masks" = "surgical_masks",
      "Protective head caps" = "head_caps",
      "Gloves (excl. plastic gloves under B4)" = "gloves",
      "Protective gargamets excluding head caps and gloves" = "gargamets",
      "Other protective accessories" = "other_acc",
      "Disinfectants products" = "disinfectans",
      "Sterilizers" = "sterilizers",
      "Devices for oxigen therapy" = "oxigen_therapy",
      "Medical devices" = "medical_dev",
      "Thermometers" = "thermometers",
      "Mediccines" = "mediccines",
      "Bandages" = "bandages",
      "Sterile tubes" = "sterile_tubes",
      "Syringes" = "syringes",
      "Other medical" = "other_med",
      "Medical vehicles and furniture" = "vehicles",
      "All Covid products" = "Total"
    )
  })
    categoryInput.network <- reactive({
      switch (
        input$category.network,
        "COVID-19 Tests (kits and instruments)" = "COVID19_t",
        "FFP2 Facemasks" = "facemasksFFP2",
        "Surgical masks" = "surgical_masks",
        "Protective head caps" = "head_caps",
        "Gloves (excl. plastic gloves under B4)" = "gloves",
        "Protective gargamets excluding head caps and gloves" = "gargamets",
        "Other protective accessories" = "other_acc",
        "Disinfectants products" = "disinfectans",
        "Sterilizers" = "sterilizers",
        "Devices for oxigen therapy" = "oxigen_therapy",
        "Medical devices" = "medical_dev",
        "Thermometers" = "thermometers",
        "Mediccines" = "mediccines",
        "Bandages" = "bandages",
        "Sterile tubes" = "sterile_tubes",
        "Syringes" = "syringes",
        "Other medical" = "other_med",
        "Medical vehicles and furniture" = "vehicles",
        "All Covid products" = "Total"
      )
    })
  categoryInput <- reactive({
    switch (
      input$category,
      "COVID-19 Tests (kits and instruments)" = "COVID19_t",
      "FFP2 Facemasks" = "facemasksFFP2",
      "Surgical masks" = "surgical_masks",
      "Protective head caps" = "head_caps",
      "Gloves (excl. plastic gloves under B4)" = "gloves",
      "Protective gargamets excluding head caps and gloves" = "gargamets",
      "Other protective accessories" = "other_acc",
      "Disinfectants products" = "disinfectans",
      "Sterilizers" = "sterilizers",
      "Devices for oxigen therapy" = "oxigen_therapy",
      "Medical devices" = "medical_dev",
      "Thermometers" = "thermometers",
      "Mediccines" = "mediccines",
      "Bandages" = "bandages",
      "Sterile tubes" = "sterile_tubes",
      "Syringes" = "syringes",
      "Other medical" = "other_med",
      "Medical vehicles and furniture" = "vehicles",
      "All Covid products" = "Total"
    )
  })
  categoryInput.red <- reactive({
    switch (
      input$category.red,
      "COVID-19 Tests (kits and instruments)" = "COVID19_t",
      "FFP2 Facemasks" = "facemasksFFP2",
      "Surgical masks" = "surgical_masks",
      "Protective head caps" = "head_caps",
      "Gloves (excl. plastic gloves under B4)" = "gloves",
      "Protective gargamets excluding head caps and gloves" = "gargamets",
      "Other protective accessories" = "other_acc",
      "Disinfectants products" = "disinfectans",
      "Sterilizers" = "sterilizers",
      "Devices for oxigen therapy" = "oxigen_therapy",
      "Medical devices" = "medical_dev",
      "Thermometers" = "thermometers",
      "Mediccines" = "mediccines",
      "Bandages" = "bandages",
      "Sterile tubes" = "sterile_tubes",
      "Syringes" = "syringes",
      "Other medical" = "other_med",
      "Medical vehicles and furniture" = "vehicles",
      "All Covid products" = "Total"
    )
  })
  # categoryLines <- reactive({
  #   if (categoryInput()=="Total")
  #   {
  #     EU27 %>% group_by(category) %>% summarise(VALUE_IN_EU)
  #   }
  # })
  country <- reactive({
    if (countryInput() == "Total")
    {
      period()
    }
    else
    {
      period()[DECLARANT_ISO == countryInput()]
    }
  })
  category <- reactive({
    if (categoryInput() == "Total")
    {
      country()
    }
    else
    {
      country()[category == categoryInput()]
    }
    
  })
  
  output$lineplot <- renderPlot({

    gg <-
      ggplot(data = grouped.dt.values[FLOW == tradetypeInput.lines()], aes(x = Date, colour = DECLARANT_ISO)) +
      labs(x = "Date", y = paste("Total", input$Trade_type.lines, "in million euros")) +
      ggtitle(paste(
        input$Trade_type.lines, "of", input$category.lines
      ))+
      
      theme(panel.background = element_blank(),
            panel.grid = element_line(colour = "darkgrey", linetype = "dotted"),
            axis.text = element_text( size = 15),
            axis.title = element_text(size = 25),
            plot.title = element_text(size = 40, hjust = 0.5),
            legend.key = element_rect(colour = NA, fill = NA)
      ) +
      scale_x_date(date_labels = "%b/%y", date_breaks = "months")
    
    for (country in input$Country.lines)
    {
      if (country == "EU-27")
      {
        gg <- gg +geom_line(data = EU.27[FLOW == tradetypeInput.lines() & category == categoryInput.lines()], aes(x = Date,y = VALUE_IN_EUROS)) +
          geom_point(data = EU.27[FLOW == tradetypeInput.lines() & category == categoryInput.lines()], aes(x = Date,y = VALUE_IN_EUROS))
      }
      else
      {
        gg <-
          gg + geom_line(data = grouped.dt.values[DECLARANT_ISO == country &
                                                    FLOW == tradetypeInput.lines() & category == categoryInput.lines()], aes(y = VALUE_IN_EUROS)) +
          geom_point(data = grouped.dt.values[DECLARANT_ISO == country &
                                                FLOW == tradetypeInput.lines() & category == categoryInput.lines()], aes(y = VALUE_IN_EUROS))
      } 
    }
    gg
    
    
  })
  output$threemap_population_country <-
    renderPlot({
      treemap(
        dtf = category()[FLOW == tradetypeInput()],
        index = "PARTNER_ISO",
        vSize = "VALUE_IN_EUROS",
        vColor = "VALUE_IN_EUROS",
        type = "value",
        title = paste(
          input$Trade_type,
          "of",
          input$category,
          "by country in",
          input$country
        ),
        title.legend = "Value in million euros",
        palette = "YlOrRd"
      )
    })
  
  output$threemap_population_category <-
    renderPlot({
      treemap(
        dtf = country()[FLOW == tradetypeInput()],
        index = "category",
        vSize = "VALUE_IN_EUROS",
        vColor = "VALUE_IN_EUROS",
        type = "value",
        title =  paste(
          input$Trade_type,
          "of COVID related goods in",
          input$country,
          "by product"
        ),
        title.legend = "Value in million euros ",
        palette = "YlGn"
      )
    })
  output$global_map <-
    renderPlot({
      mapa_flechas(declarant_iso =countryInput.network(),
                   flow= tradetypeInput.network(),
                   continent = "bn",
                   product= categoryInput.network(),
                   year = input$period.net,
                   month = input$month.net)
    })
  output$red <-renderPlot({
    red(tradetypeInput.red(),categoryInput.red(),grouped.dt)
  })
  output$text <- renderText({
    "Viva andalucia libre cojoneh!"
  })
}