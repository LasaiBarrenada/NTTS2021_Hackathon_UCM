
dt.2020.filtered <- read.csv("sample.csv")
dt.2020.filtered <- as.data.table(dt.2020.filtered)
dt.2020.filtered <- dt.2020.filtered[PERIOD != "202012"]
# columns = c("X","X.1","DECLARANT","PARTNER","PRODUCT_SITC","PRODUCT_CPA2002","PRODUCT_CPA2_1","STAT_REGIME","PRODUCT_SECTION","PRODUCTS_BEC","SUPP_UNIT","QUANTITY_IN_KG","SUP_QUANTITY","year","month")
# drop columns
dt.2020.filtered<- dt.2020.filtered[,  c("X","X.1","DECLARANT","PARTNER", "PRODUCT_SITC", "PRODUCT_CPA2002","PRODUCT_CPA2_1","STAT_REGIME","PRODUCT_SECTION","PRODUCT_BEC","SUPP_UNIT","QUANTITY_IN_KG","SUP_QUANTITY"):=NULL]

dt.2020.filtered
dt.2020.filtered$Date <-
  as.Date(paste0(as.character(dt.2020.filtered$PERIOD), '01'), format = '%Y%m%d')


grouped.dt <-
  dt.2020.filtered %>% group_by(DECLARANT_ISO,  FLOW, Date, category)
grouped.dt.values <-
  grouped.dt %>% summarise(VALUE_IN_EUROS = sum(VALUE_IN_EUROS))
grouped.dt.values <- as.data.table(grouped.dt.values)

EU.27 <-
  dt.2020.filtered %>% group_by(FLOW, Date, category) %>% summarise(VALUE_IN_EUROS = sum(VALUE_IN_EUROS))
EU.27$DECLARANT_ISO = "EU-27"
EU.27<- as.data.table(EU.27)

labels.countries <- c(
  "Austria",
  "Belgium",
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "Czechia",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Ireland",
  "Italy",
  "Latvia",
  "Lithuania",
  "Luxembourg",
  "Malta",
  "Netherlands",
  "Poland",
  "Portugal",
  "Romania",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Sweden",
  "EU-27"
)


labels.categories <-
  c(
    "COVID-19 Tests (kits and instruments)",
    "FFP2 Facemasks",
    "Surgical masks",
    "Protective head caps",
    "Gloves (excl. plastic gloves under B4)",
    "Protective gargamets excluding head caps and gloves",
    "Other protective accessories",
    "Disinfectants products",
    "Sterilizers",
    "Devices for oxigen therapy",
    "Medical devices",
    "Thermometers",
    "Mediccines",
    "Bandages",
    "Sterile tubes",
    "Syringes",
    "Other medical",
    "Medical vehicles and furniture",
    "All Covid products"
  )

dt.2020.filtered$VALUE_IN_EUROS <-
  as.numeric(dt.2020.filtered$VALUE_IN_EUROS)

