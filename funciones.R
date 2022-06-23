library(readxl)
library(tidyverse)
library(stringr)

# Función para leer datos en formato PRIMER para trabajar con vegan
datos <- function(dat){
  total <- which(str_detect(dat$`SAMPLE CODE`, "total", negate = FALSE))
  col <- which(str_detect(dat$`SAMPLE CODE`, "# col.", negate = FALSE))
  
  if(length(total) == 0){
    temp1 <- dat
  } else {
    temp1 <- dat[-c(total,col),]  
  }
  ec <- colSums(is.na(temp1) | temp1 == "") == nrow(temp1)
  nc <- names(temp1[,ec])[1]
  nr <- which(is.na(temp1[,1]), arr.ind=TRUE)[1]
  sample_code <- colnames(temp1)[-1]
  variables <- temp1[-c(nr:nrow(temp1)),1]
  temp2 <- temp1[-c(nr:nrow(temp1)),] %>% 
    select(!c(1,nc:length(temp1))) %>% 
    mutate(across(where(is.character), as.numeric)) %>%
    replace(is.na(.), 0) %>% 
    t() 
  colnames(temp2) <- as.matrix(temp1[-c(nr:nrow(temp1)),1])
  temp2 <- as.data.frame(temp2)
  return(temp2)
  }
factores <- function(dat){
  temp1 <- dat
  ec <- colSums(is.na(temp1) | temp1 == "") == nrow(temp1)
  nc <- names(temp1[,ec])[1]
  nr <- which(is.na(temp1[,1]), arr.ind=TRUE)[1]
  sample_code <- temp1 %>% 
    select(-c(1,nc:length(temp1))) %>% 
    colnames()
  variables <- temp1[c(nr:nrow(temp1)),1] %>% 
                drop_na()
  temp2 <- temp1[c(nr+1:nrow(temp1)),] %>% 
    select(!c(1,nc:length(temp1))) %>% 
    drop_na() %>% 
    t() 
  colnames(temp2) <- variables$`SAMPLE CODE`
  temp3 <- cbind(temp2,sample_code) %>% 
           as.data.frame() %>% 
           mutate(YEAR_SITE = str_c(YEAR, `SITE NAME`, sep = "_"),
                  YEAR_LOCATION = str_c(YEAR, LOCATION, sep = "_"),
                  YEAR_REGION = str_c(YEAR, REGION, sep = "_"))
  return(temp3)
}
indicadores <- function(dat){
  total <- which(str_detect(dat$`SAMPLE CODE`, "total", negate = FALSE))
  col <- which(str_detect(dat$`SAMPLE CODE`, "# col.", negate = FALSE))
  
  if(length(total) == 0){
    temp1 <- dat
  } else {
    temp1 <- dat[-c(total,col),]  
  }
  ec <- colSums(is.na(temp1) | temp1 == "") == nrow(temp1)
  nc <- names(temp1[,ec])[1]
  nr <- which(is.na(temp1[,1]), arr.ind=TRUE)[1]
  indicators <- temp1 %>% 
    select(c(1,nc:length(temp1),-nc)) %>% 
    colnames()
  #variables <- temp1[-c(nr:nrow(temp1)),1]
  temp2 <- temp1[-c(nr:nrow(temp1)),] %>% 
    select(indicators) %>% 
    return(as.data.frame(temp2))
}
mds <- function(data, factor.esp = c("LOCATION", "REGION", "ISLAND"),
                directorio = "MDS/carpetas...",
                tipo = c("benthic", "fish_inv", "fish_bio"),
                fuente = c("PRCRMP", "SEAMAP", "NCRMP"),
                isla = "PR"){
  
  if(factor.esp == "LOCATION"){
    
    # Extraer datos y factores
    bio <- datos(data)
    fac <- factores(data)
    
    # ID para factor.esp
    localities <- fac %>% 
      group_by(REGION) %>% 
      summarise(muestras = n())
    
    loc <- localities %>% 
      filter(muestras >= 15) %>% 
      select(REGION) 
    
    loc <- levels(factor(loc$LOCATION))
    
    ### Remover muestras vacías
    u_sp <- length(bio)
    
    zero_sample <- bio %>% 
      bind_cols(fac) %>%  
      pivot_longer(cols = 1:u_sp, names_to = "species_name", values_to = "species_count") %>% 
      group_by(YEAR_SITE) %>% 
      summarise(N = sum(species_count)) %>% 
      filter(N == 0) %>% 
      select(YEAR_LOCATION) %>% 
      as.character()
    
    bio$`YEAR_SITE` <- fac$`YEAR_SITE`
    
    fac <- fac %>% 
      filter(YEAR_SITE != zero_sample)
    
    bio <- bio %>% 
      filter(YEAR_SITE != zero_sample) %>% 
      select(-YEAR_SITE)
    
    ### Generación de MDS
    
    mds_plot <- list(NULL)
    
    for (i in 1:length(loc)){
      sel <- which(fac[,3] == loc[i])
      xx <- bio[sel,] 
      xx$dummy <- rep(1, nrow(xx))
      yy <- fac[sel,]
      bray <- vegdist(sqrt(xx))
      disper <- betadisper(bray, group = yy$YEAR_SITE)
      centroides <- disper$centroids
      dis_cen <- vegdist(centroides, method = "euclidean")
      mds <- metaMDS(dis_cen)
      points <- as.data.frame(mds$points)
      stress <- str_c("2D stress = ", round(mds$stress, 3))
      zz <- yy %>% 
        group_by(YEAR,`SITE NAME`) %>% 
        summarise(n = n()) %>% 
        bind_cols(points) 
      mds_plot[[i]] <- ggplot(zz, aes(x= MDS1, y = MDS2))+
        geom_point(aes(colour = `SITE NAME`), size = 5)+
        geom_text(aes(label = `YEAR`), nudge_y = 0.02)+
        geom_path(aes(group = `SITE NAME`), arrow = arrow(),)+
        theme_bw()+
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        annotate(geom = "text", x = Inf,
                 y = Inf, label = stress,
                 hjust = 1, vjust = 1, 
                 size = 3)+
        ylab("")+
        xlab("")+
        ggtitle(loc[i])
      archivo <- paste(directorio,fuente,tipo,loc[i], ".pdf", sep = "_")
      ggsave(archivo, mds_plot[[i]], width = 24, height = 16, units = "cm")
      
      
    }
    
  }
  if(factor.esp == "REGION"){
    # Extraer datos y factores
    bio <- datos(data)
    fac <- factores(data)
    
    ### Remover muestras vacías
    u_sp <- length(bio)
    
    zero_sample <- bio %>% 
      bind_cols(fac) %>%  
      pivot_longer(cols = 1:u_sp, names_to = "species_name", values_to = "species_count") %>% 
      group_by(YEAR_LOCATION) %>% 
      summarise(N = sum(species_count)) %>% 
      filter(N == 0) %>% 
      select(YEAR_LOCATION) %>% 
      as.character()
    
    bio$YEAR_LOCATION <- fac$YEAR_LOCATION
    
    fac <- fac %>% 
      mutate(REGION = str_replace(fac$REGION, pattern = "/", replacement = "-")) %>% 
      filter(YEAR_LOCATION != zero_sample)
    
    bio <- bio %>% 
      filter(YEAR_LOCATION != zero_sample) %>% 
      select(-YEAR_LOCATION)
    
    # ID para factor.esp
    localities <- fac %>% 
      group_by(REGION) %>% 
      summarise(muestras = n())
    
    loc <- localities %>% 
      filter(muestras >= 15) %>% 
      select(REGION) 
    
    loc <- levels(factor(loc$REGION))    
    
    ### Generación de MDS
    
    mds_plot <- list(NULL)
    
    for (i in 1:length(loc)){
      sel <- which(fac[,2] == loc[i])
      xx <- bio[sel,] 
      xx$dummy <- rep(1, nrow(xx))
      yy <- fac[sel,]
      bray <- vegdist(sqrt(xx))
      disper <- betadisper(bray, group = yy$YEAR_LOCATION)
      centroides <- disper$centroids
      dis_cen <- vegdist(centroides, method = "euclidean")
      mds <- metaMDS(dis_cen)
      points <- as.data.frame(mds$points)
      stress <- str_c("2D stress = ", round(mds$stress, 3))
      zz <- yy %>% 
        group_by(YEAR,LOCATION) %>% 
        summarise(n = n()) %>% 
        bind_cols(points) 
      mds_plot[[i]] <- ggplot(zz, aes(x= MDS1, y = MDS2))+
        geom_point(aes(colour = LOCATION), size = 5)+
        geom_text(aes(label = `YEAR`), nudge_y = 0.02)+
        geom_path(aes(group = LOCATION), arrow = arrow(),)+
        theme_bw()+
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        annotate(geom = "text", x = Inf,
                 y = Inf, label = stress,
                 hjust = 1, vjust = 1, 
                 size = 3)+
        ylab("")+
        xlab("")+
        ggtitle(loc[i])
      archivo <- paste(directorio,fuente,tipo,loc[i], ".pdf", sep = "_")
      ggsave(archivo, mds_plot[[i]], width = 24, height = 16, units = "cm")
      
      
    }
  }
  if(factor.esp == "ISLAND"){
    # Extraer datos y factores
    bio <- datos(data)
    fac <- factores(data)
    
    ### Remover muestras vacías
    u_sp <- length(bio)
    
    zero_sample <- bio %>% 
      bind_cols(fac) %>%  
      pivot_longer(cols = 1:u_sp, names_to = "species_name", values_to = "species_count") %>% 
      group_by(YEAR_REGION) %>% 
      summarise(N = sum(species_count)) %>% 
      filter(N == 0) %>% 
      select(YEAR_REGION) %>% 
      as.character()
    
    bio$YEAR_REGION <- fac$YEAR_REGION
    
    fac <- fac %>% 
      mutate(REGION = str_replace(fac$REGION, pattern = "/", replacement = "-"),
             YEAR_REGION = str_replace(fac$YEAR_REGION, pattern = "/", replacement = "-")) %>% 
      filter(YEAR_REGION != zero_sample)
    
    bio <- bio %>% 
      filter(YEAR_REGION != zero_sample) %>% 
      select(-YEAR_REGION)
    
    ### Generación de MDS
      xx <- bio 
      xx$dummy <- rep(1, nrow(xx))
      yy <- fac
      bray <- vegdist(sqrt(xx))
      disper <- betadisper(bray, group = yy$YEAR_REGION)
      centroides <- disper$centroids
      dis_cen <- vegdist(centroides, method = "euclidean")
      mds <- metaMDS(dis_cen)
      points <- as.data.frame(mds$points)
      stress <- str_c("2D stress = ", round(mds$stress, 3))
      zz <- yy %>% 
        group_by(YEAR,REGION) %>% 
        summarise(n = n()) %>% 
        bind_cols(points) 
      mds_plot <- ggplot(zz, aes(x= MDS1, y = MDS2))+
        geom_point(aes(colour = REGION), size = 5)+
        geom_text(aes(label = `YEAR`), nudge_y = 0.02)+
        geom_path(aes(group = REGION), arrow = arrow(),)+
        theme_bw()+
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        annotate(geom = "text", x = Inf,
                 y = Inf, label = stress,
                 hjust = 1, vjust = 1, 
                 size = 3)+
        ylab("")+
        xlab("")+
        ggtitle(isla)
      archivo <- paste(directorio,fuente,tipo,isla, ".pdf", sep = "_")
      ggsave(archivo, mds_plot, width = 24, height = 16, units = "cm")
      
      
    }
  }
