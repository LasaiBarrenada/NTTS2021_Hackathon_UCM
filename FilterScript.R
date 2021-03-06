library(data.table)
df <- fread("data from BULK DOWNLOAD") #add as much periods as you want

# COVID-19 TESTS
COVID19_T_kits <- c(30021300, 30021400, 30021500, 30029090, 38220000)  #30021098 (prior to 2017)
COVID19_T_instruments <- c(38210000, 902780)
COVID19_t <- c(COVID19_T_kits, COVID19_T_instruments)

# Protective gargaments
facemasksFFP2 <- 630790 # asterisco porque categoria incluye otros articulos
surgical_masks <- c(48189010, 48189090)
head_caps <- c(65050030, 65050090, 65061010, 65061080, 65069100)
gloves <- c(40151100, 40151900, 61161020, 62160000)

protect_plastic <- c(39262000, 39269097) # Protective unisex garments made of plastic sheeting, textile reinforced plastics or textile backed plastics
hygienic_plastic <- c(40149000, 40159000) # Hygienic or pharmaceutical articles, articles of apparel, clothing accessories and protective unisex garments made of rubber or vulcanized rubber
surgical_paper <- c(48185000) # Paper or cellulose garments and clothing accessories for surgical/medical use
ind_clothing <- c(62113210, 62113290, 62113310, 62113390, 62113900, 621142, 621143, 621149) # Industrial and occupational clothing
other_garg <- c(62101010, 62101092, 62101068, 62102000, 62103000, 62104000, 62105000, 611300) # Other protective garments for surgical/medical use
gargamets <- c(protect_plastic, hygienic_plastic, surgical_paper, ind_clothing, other_garg) # Clothing excluding head caps, facemasks and gloves
other_acc <- c(56031110, 902000, 56039490, 6379092, 900490) # Other protective accessories

# Disinfectants and sterilization products
hydoealcoholic <- c(22071000, 22072000, 22089091, 22089099, 28470000, 29051200, 29151100, 29151200, 29182100) # Hydroalcoholic and other solutions
hand_sanitizers <- c(38089410, 38089420, 38089490) # Hand sanitizers and other disinfectant preparations 
sterilizers <- 84192000
soap <- c(3401110, 34011900, 34012010, 34012090, 34013000, 34021200)
disinfectans <- c(hydoealcoholic, hand_sanitizers, soap)

# Oxygen therapy equipment
oxigen_therapy <- c(842139, 901920)

# Medical devices and equipment
humidifiers <- c(841510, 85098000)
monitors <- c(85285291, 85285299, 85285900, 90181910, 90181990) # c(85285920, 85285931, 85285939, 85285970) prior to 2017
pumps <- c(84138100, 90189050, 90189084)
thermometers <- c(902511, 902519)
other_dev <- c(7017, 731100, 73249000, 76130000, 37011000, 37021000, 83437090, 90181100, 90181200, 90189020, 90189060, 90221200, 90221400, 90282000)
medical_dev <- c(humidifiers, monitors, pumps, other_dev)  # medical devices except thermmeters

# Medical consumables
mediccines <- c(300220, 30039000, 30049000)
bandages <- c(30051000, 30059010, 30059031, 30059099) # Wadding, gauze, bandages, tape and similar, impregnated or coated with pharmaceutical substances, for medical use.
sterile_tubes <- c(391721, 391722, 391723, 39172900, 39173100, 39173200, 39173300, 39173900)
syringes <- c(90183110, 90183190, 90183210, 90183290, 90183900)
other_med <- c(28044000, 30067000, 392329) # Other medical consumables

# Medical vehicles and furniture
vehicles <- c(870590, 63062200, 63062900, 84798997, 87139000, 94029000, 84248970) # 84248900 prior to 2017

filters <- c(COVID19_t, facemasksFFP2, surgical_masks,
             head_caps, gloves, gargamets, 
             other_acc, disinfectans, 
             sterilizers, oxigen_therapy,  
             medical_dev, thermometers, 
             mediccines, bandages, sterile_tubes, 
             syringes, other_med, vehicles)
dt <- as.data.table(df)
raw.dt <- data.table()
for (filter in as.character(filters))
{
  largo <- nchar(filter)
  filtered.dt <- df[substr(PRODUCT_NC,1,largo) == filter]
  if (filter %in% COVID19_t)
  {
    filtered.dt$category <- "COVID19_t"
  }
  if (filter %in% facemasksFFP2)
  {
    filtered.dt$category <- "facemasksFFP2"
  }
  if (filter %in% surgical_masks)
  {
    filtered.dt$category <- "surgical_masks"
  }
  if (filter %in% head_caps)
  {
    filtered.dt$category <- "head_caps"
  }
  if (filter %in% gloves)
  {
    filtered.dt$category <- "gloves"
  }
  if (filter %in% gargamets)
  {
    filtered.dt$category <- "gargamets"
  }
  if (filter %in% other_acc)
  {
    filtered.dt$category <- "other_acc"
  }
  if (filter %in% disinfectans)
  {
    filtered.dt$category <- "disinfectans"
  }
  if (filter %in% sterilizers)
  {
    filtered.dt$category <- "sterilizers"
  }
  if (filter %in% oxigen_therapy)
  {
    filtered.dt$category <- "oxigen_therapy"
  }
  if (filter %in% medical_dev)
  {
    filtered.dt$category <- "medical_dev"
  }
  if (filter %in% thermometers)
  {
    filtered.dt$category <- "thermometers"
  }
  if (filter %in% mediccines)
  {
    filtered.dt$category <- "mediccines"
  }
  if (filter %in% bandages)
  {
    filtered.dt$category <- "bandages"
  }
  if (filter %in% sterile_tubes)
  {
    filtered.dt$category <- "sterile_tubes"
  }
  if (filter %in% syringes)
  {
    filtered.dt$category <- "syringes"
  }
  if (filter %in% other_med)
  {
    filtered.dt$category <- "other_med"
  }
  if (filter %in% vehicles)
  {
    filtered.dt$category <- "vehicles"
  }
  raw.dt <- rbind(raw.dt, filtered.dt, fill= TRUE)
}

write.csv(raw.dt, "dt2020_filtered.csv")
