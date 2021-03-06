


dt_<- dt.2020.filtered

dt_$dec_cont<-countrycode(dt_$DECLARANT_ISO, origin = 'iso2c', destination = 'continent')
dt_$par_cont<-countrycode(dt_$PARTNER_ISO, origin = 'iso2c', destination = 'continent')

# # Descargar las coordenadas de los centroides de cada pais (estaría bien guardar el archivo por si acaso)
# url <- 'https://developers.google.com/public-data/docs/canonical/countries_csv'
# webpage <- read_html(url)
# centroids <- url %>% read_html %>% html_nodes('table') %>% html_table() %>% as.data.frame

centroids <- read.csv("centroids.csv")
declarant_iso<-'FR'
flow<-"import"
continent<-'Europe'
product<-'COVID19_t'
year <- 2020
month = 1

# Función Mapa de lineas
mapa_flechas<-function(declarant_iso,flow,continent,product,year,month){
  # Filtro import/export
  if (flow=='import'){
    dt_f<- dt_%>% 
      dplyr::filter(FLOW==1)
  } else {
    dt_f<- dt_%>% 
      dplyr::filter(FLOW==2)
  }
  
  # Filtro año
  if (year==2019){
    dt_f<-dt_f%>% 
      dplyr::filter(year==2019)
  } else {
    dt_f<-dt_f%>% 
      dplyr::filter(year==2020)
  }
  
  # Filtro mes
  if (month==1){
    dt_f<-dt_f %>% dplyr::filter(month==1)
  } else if (month==2){
    dt_f<-dt_f %>% dplyr::filter(month==2)
  } else if (month==3){
    dt_f<-dt_f %>% dplyr::filter(month==3)
  } else if (month==4){
    dt_f<-dt_f %>% dplyr::filter(month==4)
  } else if (month==5){
    dt_f<-dt_f %>% dplyr::filter(month==5)
  } else if (month==6){
    dt_f<-dt_f %>% dplyr::filter(month==6)
  } else if (month==7){
    dt_f<-dt_f %>% dplyr::filter(month==7)
  } else if (month==8){
    dt_f<-dt_f %>% dplyr::filter(month==8)
  } else if (month==9){
    dt_f<-dt_f %>% dplyr::filter(month==9)
  } else if (month==10){
    dt_f<-dt_f %>% dplyr::filter(month==10)
  } else if (month==11){
    dt_f<-dt_f %>% dplyr::filter(month==11)
  } else {
    dt_f<-dt_f %>% dplyr::filter(month==12)
  }
  
  # Filtro "category"
  if (product=='bandages'){
    dt_f<-dt_f %>% dplyr::filter(category=='bandages')
  } else if (product=='COVID19_t'){
    dt_f<-dt_f %>% dplyr::filter(category=='COVID19_t')
  } else if (product=='disinfectans'){
    dt_f<-dt_f %>% dplyr::filter(category=='disinfectans')
  } else if (product=='facemasksFFP2'){
    dt_f<-dt_f %>% dplyr::filter(category=='facemasksFFP2')
  } else if (product=='gargamets'){
    dt_f<-dt_f %>% dplyr::filter(category=='gargamets')
  } else if (product=='gloves'){
    dt_f<-dt_f %>% dplyr::filter(category=='gloves')
  } else if (product=='head_caps'){
    dt_f<-dt_f %>% dplyr::filter(category=='head_caps')
  } else if (product=='medical_dev'){
    dt_f<-dt_f %>% dplyr::filter(category=='medical_dev')
  } else if (product=='mediccines'){
    dt_f<-dt_f %>% dplyr::filter(category=='mediccines')
  } else if (product=='other_acc'){
    dt_f<-dt_f %>% dplyr::filter(category=='other_acc')
  } else  if (product=='other_med'){
    dt_f<-dt_f %>% dplyr::filter(category=='other_med')
  } else if (product=='oxigen_therapy'){
    dt_f<-dt_f %>% dplyr::filter(category=='oxigen_therapy')
  } else if (product=='sterile_tubes'){
    dt_f<-dt_f %>% dplyr::filter(category=='sterile_tubes')
  } else if (product=='sterilizers'){
    dt_f<-dt_f %>% dplyr::filter(category=='sterilizers')
  } else if (product=='syringes'){
    dt_f<-dt_f %>% dplyr::filter(category=='syringes')
  } else if (product=='thermometers'){
    dt_f<-dt_f %>% dplyr::filter(category=='thermometers')
  } else if (product=='surgical_masks'){
    dt_f<-dt_f %>% dplyr::filter(category=='surgical_masks')
  } else {
    dt_f<-dt_f %>% dplyr::filter(category=='vehicles') 
  }
  
  dt_f <- dt_f %>%  
    group_by(DECLARANT_ISO, PARTNER_ISO) %>%
    summarise(value = sum(VALUE_IN_EUROS),
              par_cont=first(par_cont)) %>% 
    ungroup()%>% na.omit()
  
  
  
  # Filtro continente del partner
  if (continent=='Europe'){
    # Filtrado y fusión con las coordenadas de los centroides
    dt_f<- dt_f%>% 
      dplyr::filter(par_cont=='Europe') %>%
      dplyr::filter(DECLARANT_ISO==declarant_iso) %>%
      merge(centroids,by.x='DECLARANT_ISO',by.y = 'country')%>%
      merge(centroids,by.x='PARTNER_ISO',by.y = 'country')
    
    # Mapa del mundo  
    map("world", fill=T, col="grey8", bg="grey15")
    
    # Punto del declarante
    points(dt_f$longitude.x,dt_f$latitude.x, pch=3, cex=0.1, col='yellow')
    
    # Puntos de los partners
    points(dt_f$longitude.y,dt_f$latitude.y, pch=3, cex=0.1, col='yellow')
    
    # Lineas
    for (i in (1:nrow(dt_f)[1])) { 
      inter <- gcIntermediate(c(dt_f$longitude.x[1],dt_f$latitude.x[1]), c(dt_f$longitude.y[i], dt_f$latitude.y[i]), n=200)
      lines(inter, lwd=0.1, col="turquoise2") 
    }
  } else if (continent=='Americas'){
    dt_f<- dt_f%>% 
      dplyr::filter(par_cont=='Americas') %>%
      dplyr::filter(DECLARANT_ISO==declarant_iso) %>%
      merge(centroids,by.x='DECLARANT_ISO',by.y = 'country')%>%
      merge(centroids,by.x='PARTNER_ISO',by.y = 'country')
    
    map("world", fill=T, col='grey8', bg="grey15")
    
    points(dt_f$longitude.x,dt_f$latitude.x, pch=3, cex=0.1, col='yellow')
    
    points(dt_f$longitude.y,dt_f$latitude.y, pch=3, cex=0.1, col='yellow')
    
    for (i in (1:nrow(dt_f)[1])) { 
      inter <- gcIntermediate(c(dt_f$longitude.x[1],dt_f$latitude.x[1]), c(dt_f$longitude.y[i], dt_f$latitude.y[i]), n=200)
      lines(inter, lwd=0.1, col="turquoise2") 
    } 
  } else if  (continent=='Asia'){
    dt_f<- dt_f%>% 
      dplyr::filter(par_cont=='Asia') %>%
      dplyr::filter(DECLARANT_ISO==declarant_iso) %>%
      merge(centroids,by.x='DECLARANT_ISO',by.y = 'country')%>%
      merge(centroids,by.x='PARTNER_ISO',by.y = 'country')
    
    map("world", fill=T, col='grey8', bg="grey15")
    
    points(dt_f$longitude.x,dt_f$latitude.x, pch=3, cex=0.1, col='yellow')
    
    points(dt_f$longitude.y,dt_f$latitude.y, pch=3, cex=0.1, col='yellow')
    
    for (i in (1:nrow(dt_f)[1])) { 
      inter <- gcIntermediate(c(dt_f$longitude.x[1],dt_f$latitude.x[1]), c(dt_f$longitude.y[i], dt_f$latitude.y[i]), n=200)
      lines(inter, lwd=0.1, col="turquoise2")
    } 
  } else if (continent=='Africa') {
    dt_f<- dt_f%>% 
      dplyr::filter(par_cont=='Africa') %>%
      dplyr::filter(DECLARANT_ISO==declarant_iso) %>%
      merge(centroids,by.x='DECLARANT_ISO',by.y = 'country')%>%
      merge(centroids,by.x='PARTNER_ISO',by.y = 'country')
    
    map("world", fill=T, col='grey8', bg="grey15")
    
    points(dt_f$longitude.x,dt_f$latitude.x, pch=3, cex=0.1, col='yellow')
    
    points(dt_f$longitude.y,dt_f$latitude.y, pch=3, cex=0.1, col='yellow')
    
    for (i in (1:nrow(dt_f)[1])) { 
      inter <- gcIntermediate(c(dt_f$longitude.x[1],dt_f$latitude.x[1]), c(dt_f$longitude.y[i], dt_f$latitude.y[i]), n=200)
      lines(inter, lwd=0.1, col="turquoise2")  
    }
  } else if (continent=='Oceania') {
    dt_f<- dt_f%>% 
      dplyr::filter(par_cont=='Oceania') %>%
      dplyr::filter(DECLARANT_ISO==declarant_iso) %>%
      merge(centroids,by.x='DECLARANT_ISO',by.y = 'country')%>%
      merge(centroids,by.x='PARTNER_ISO',by.y = 'country')
    
    map("world", fill=T, col='grey8', bg="grey15")
    
    points(dt_f$longitude.x,dt_f$latitude.x, pch=3, cex=0.1, col='yellow')
    
    points(dt_f$longitude.y,dt_f$latitude.y, pch=3, cex=0.1, col='yellow')
    
    for (i in (1:nrow(dt_f)[1])) { 
      inter <- gcIntermediate(c(dt_f$longitude.x[1],dt_f$latitude.x[1]), c(dt_f$longitude.y[i], dt_f$latitude.y[i]), n=200)
      lines(inter, lwd=0.1, col="turquoise2")  
    }
  } else { 
    # Opción todo el mundo
    dt_f<- dt_f%>% 
      dplyr::filter(DECLARANT_ISO==declarant_iso) %>%
      merge(centroids,by.x='DECLARANT_ISO',by.y = 'country')%>%
      merge(centroids,by.x='PARTNER_ISO',by.y = 'country')
    
    map("world", fill=T, col='grey8', bg="grey15")# Tamaño mapa
    points(dt_f$longitude.x,dt_f$latitude.x, pch=3, cex=0.1, col='yellow')
    points(dt_f$longitude.y,dt_f$latitude.y, pch=3, cex=0.1, col='yellow')
    
    for (i in (1:nrow(dt_f)[1])) { 
      inter <- gcIntermediate(c(dt_f$longitude.x[1],dt_f$latitude.x[1]), c(dt_f$longitude.y[i], dt_f$latitude.y[i]), n=200)
      lines(inter, lwd=0.1, col="turquoise2")  
    }
  }
  p <- recordPlot()
  dt<-dt_f%>%dplyr::select(name.y,value)%>%arrange(desc(value))%>%head(10) # Tabla de las díez transas más caras 
  newlist <- list(p,dt)
  return(newlist)
}


#Ejemplos
# mapa_flechas('FR','import','Europe','COVID19_t')
