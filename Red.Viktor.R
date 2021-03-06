red<- function (flow,product,dt_){
  ######################################################################################
  # Estos dos country code se emplean tanto aquí como en el mapa de flechas estaría bien que 
  # se sacaran de la función porque es algo lenta
  dt = dt_
  dt$par_cont<-countrycode(dt$PARTNER_ISO, origin = 'iso2c', destination = 'continent')
  dt$dec_cont<-countrycode(dt$DECLARANT_ISO, origin = 'iso2c', destination = 'continent')
  ######################################################################################
  dt<- dt %>%
    dplyr::filter(par_cont=='Europe') %>%
    dplyr::filter(dec_cont=='Europe')
  
  # Filtro "FLOW"
  if (flow =='import'){
    dt_f<-dt %>% dplyr::filter(FLOW==1)
  } else if (flow =='export'){
    dt_f<-dt %>% dplyr::filter(FLOW==2)
  } else {
    dt_f<-dt 
  }
  
  # Definir paramtro "mode" para luego calcular un tipo de medida de centralidad
  if (flow =='import'){
    flow='in'
  } else if (flow =='export') {
    flow='out'
  } else {
    flow='total'
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
  
  ## Add these as attributes to the igraph object
  library(igraph)
  dt_ <- dt_f %>%  
    group_by(DECLARANT_ISO, PARTNER_ISO) %>%
    summarise(value = sum(VALUE_IN_EUROS)) %>% 
    ungroup()%>% na.omit()
  
  g_cases <- graph_from_data_frame(dt_[c("DECLARANT_ISO", "PARTNER_ISO")],
                                   directed=T)
  
  V(g_cases)$cent<-degree(g_cases, mode = flow) # Definimos el peso de los nodos en función de la medida de centralidad
  E(g_cases)$value<-dt_$value # Definimos el peso de las aristas en función del valora de la transa
  

  
  ## Convert igraph object to network object
  ITNnet<-asNetwork(g_cases)
  
  ##Convert this to ggnetwork dataframe
  ITN<-ggnetwork(ITNnet)
  ITN$vertex.names<-tolower(ITN$vertex.names)
  
  #First install ggflags
  #devtools::install_github("rensa/ggflags") puede que requiera instalar paquete 'ellipsis'
  
  
  ggplot(ITN, aes(x = x, y = y, xend = xend, yend = yend,
                  country=vertex.names,size=cent)) +
    geom_edges(size = E(g_cases)$value/1000,#the edge weight from the igraph object
               color="grey2")+
    geom_flag() + 
    scale_size(range = c(4, 13))+ #Node size scale
    guides(colour=FALSE,size=FALSE)+ #Remove the legends
    theme_blank()
  
}
