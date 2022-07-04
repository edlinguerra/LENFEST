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
  temp2 <- temp1 %>% #modificado para leer archivo SEAMAP
    filter(`SAMPLE CODE` %in% variables$`SAMPLE CODE`) %>% 
    select(!c(1,nc:length(temp1))) %>% 
    #drop_na() %>% #modificado para leer archivo SEAMAP
    t() 
  colnames(temp2) <- variables$`SAMPLE CODE`
  
  temp3 <- cbind(temp2,sample_code) %>% 
    as.data.frame() %>% 
    mutate(#YEAR_SITE = str_c(YEAR, `SITE NAME`, sep = "_"), #modificado para leer archivo SEAMAP
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


mds <- function(bio, fac, factor.esp = c("LOCATION", "REGION", "REGION-YEAR"),
                directorio = "MDS/nombre_carpetas/",
                tipo = c("benthic", "fish_inv", "fish_bio"),
                fuente = c("PRCRMP", "SEAMAP", "NCRMP")){
  
  if(factor.esp == "LOCATION"){
    
    ### ID para factor.esp
    localities <- fac %>% 
      group_by(LOCATION) %>% 
      summarise(muestras = n())
    
    loc <- localities %>% 
      filter(muestras >= 15) %>% 
      select(LOCATION) 
    
    loc <- levels(factor(loc$LOCATION))
    
    ### Remover muestras vacías
    u_sp <- length(bio)
    
    zero_sample <- bio %>% 
      bind_cols(fac) %>%  
      pivot_longer(cols = 1:u_sp, names_to = "species_name", values_to = "species_count") %>% 
      group_by(YEAR_SITE) %>% 
      summarise(N = sum(species_count)) %>% 
      filter(N == 0) %>% 
      select(YEAR_SITE) %>% 
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
      # slección de datos por localidad
      sel <- which(fac$LOCATION == loc[i])
      xx <- bio[sel,] 
      yy <- fac[sel,]
      
      #Salvar archivo
      write_excel_csv(
        x = 
          xx %>% 
          mutate("-" = NA) %>% 
          bind_cols(yy),
      file = paste(directorio,fuente,tipo,loc[i], ".csv", sep = "_"),
      na = ""
      )

#Pretratamiento y centroides
xx$dummy <- rep(1, nrow(xx))
bray <- vegdist(xx)
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
      # slección de datos por localidad
      sel <- which(fac$REGION == loc[i])
      xx <- bio[sel,] 
      yy <- fac[sel,]
      
      #Salvar archivo
      write_excel_csv(
        x = 
          xx %>% 
          mutate("-" = NA) %>% 
          bind_cols(yy),
      file = paste(directorio,fuente,tipo,loc[i], ".csv", sep = "_"),
      na = ""
      )

    #Pretratamiento y centroides
    xx$dummy <- rep(1, nrow(xx))
    bray <- vegdist(xx)
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
  if(factor.esp == "REGION-YEAR"){
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
    
    # ID para factor.esp
    localities <- fac %>% 
      group_by(YEAR, REGION) %>% 
      summarise(muestras = n())
    
    loc <- localities %>% 
      select(REGION) %>% 
      ungroup(YEAR)
    
    loc <- levels(factor(loc$REGION))    
    
    ### Generación de MDS
    
    mds_plot <- list(NULL)
    
    for (i in 1:length(loc)){
      # slección de datos por localidad
      sel <- which(fac$REGION == loc[i])
      xx <- bio[sel,] 
      yy <- fac[sel,]
      
      #Salvar archivo
      write_excel_csv(
        x = 
          xx %>% 
          mutate("-" = NA) %>% 
          bind_cols(yy),
      file = paste(directorio,fuente,tipo,loc[i], ".csv", sep = "_"),
      na = ""
      )

      #Pretratamiento y centroides
      xx$dummy <- rep(1, nrow(xx))
      bray <- vegdist(xx)
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
      mds_plot[[i]] <- ggplot(zz, aes(x= MDS1, y = MDS2))+
        geom_point(colour = "blue", size = 5)+
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
        ggtitle(loc[i])
      archivo <- paste(directorio,fuente,tipo,loc[i], ".pdf", sep = "_")
      ggsave(archivo, mds_plot[[i]], width = 24, height = 16, units = "cm")

    }
  }
}

pco <- function(bio, fac, factor.esp = c("LOCATION", "REGION", "REGION-YEAR"),
                directorio = "MDS/nombre_carpetas/",
                tipo = c("benthic", "fish_inv", "fish_bio"),
                fuente = c("PRCRMP", "SEAMAP", "NCRMP")){
  
  if(factor.esp == "LOCATION"){
    
    ### ID para factor.esp
    localities <- fac %>% 
      group_by(LOCATION) %>% 
      summarise(muestras = n())
    
    loc <- localities %>% 
      filter(muestras >= 15) %>% 
      select(LOCATION) 
    
    loc <- levels(factor(loc$LOCATION))
    
    ### Remover muestras vacías
    u_sp <- length(bio)
    
    zero_sample <- bio %>% 
      bind_cols(fac) %>%  
      pivot_longer(cols = 1:u_sp, names_to = "species_name", values_to = "species_count") %>% 
      group_by(YEAR_SITE) %>% 
      summarise(N = sum(species_count)) %>% 
      filter(N == 0) %>% 
      select(YEAR_SITE) %>% 
      as.character()
    
    bio$`YEAR_SITE` <- fac$`YEAR_SITE`
    
    fac <- fac %>% 
      filter(YEAR_SITE != zero_sample)
    
    bio <- bio %>% 
      filter(YEAR_SITE != zero_sample) %>% 
      select(-YEAR_SITE)
    
    ### Generación de PCO
    
    pco_plot <- list(NULL)
    
    for (i in 1:length(loc)){
      # slección de datos por localidad
      sel <- which(fac$LOCATION == loc[i])
      xx <- bio[sel,] 
      yy <- fac[sel,]
      
      #Pretratamiento y centroides
      xx$dummy <- rep(1, nrow(xx))
      bray <- vegdist(xx)
      disper <- betadisper(bray, group = yy$YEAR_SITE)
      centroides <- disper$centroids
      dis_cen <- vegdist(centroides, method = "euclidean")
      pco <- pcoa(dis_cen)
      #mds <- metaMDS(dis_cen)
      points <- as.data.frame(pco$vectors[,1:2])
      
      lab.x <- str_c("PCO 1 (", round(pco$values$Relative_eig[1] * 100,0), " %)")
      lab.y <- str_c("PCO 2 (", round(pco$values$Relative_eig[2] * 100,0), " %)")
      
      #stress <- str_c("2D stress = ", round(mds$stress, 3))
      zz <- yy %>% 
        group_by(YEAR,`SITE NAME`) %>% 
        summarise(n = n()) %>% 
        bind_cols(points) 
      pco_plot[[i]] <- ggplot(zz, aes(x= Axis.1, y = Axis.2))+
        geom_point(aes(colour = `SITE NAME`), size = 5)+
        geom_text(aes(label = `YEAR`), nudge_y = 0.01)+
        geom_path(aes(group = `SITE NAME`), arrow = arrow(),)+
        theme_bw()+
        # theme(axis.ticks = element_blank(),
        #       axis.text = element_blank(),
        #       panel.grid.major = element_blank(),
        #       panel.grid.minor = element_blank())+
        # annotate(geom = "text", x = Inf,
        #          y = Inf, label = stress,
        #          hjust = 1, vjust = 1, 
        #          size = 3)+
        ylab(lab.y)+
        xlab(lab.x)+
        ggtitle(loc[i])
      archivo <- paste(directorio,fuente,tipo,loc[i], ".pdf", sep = "_")
      ggsave(archivo, pco_plot[[i]], width = 24, height = 16, units = "cm")
      
      
    }
    
  }
  if(factor.esp == "REGION"){
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
    
    ### Generación de PCO
    
    pco_plot <- list(NULL)
    
    for (i in 1:length(loc)){
      # slección de datos por localidad
      sel <- which(fac$REGION == loc[i])
      xx <- bio[sel,] 
      yy <- fac[sel,]
      
      #Pretratamiento y centroides
      xx$dummy <- rep(1, nrow(xx))
      bray <- vegdist(xx)
      disper <- betadisper(bray, group = yy$YEAR_LOCATION)
      centroides <- disper$centroids
      dis_cen <- vegdist(centroides, method = "euclidean")
      pco <- pcoa(dis_cen)
      #mds <- metaMDS(dis_cen)
      points <- as.data.frame(pco$vectors[,1:2])
      
      lab.x <- str_c("PCO 1 (", round(pco$values$Relative_eig[1] * 100,0), " %)")
      lab.y <- str_c("PCO 2 (", round(pco$values$Relative_eig[2] * 100,0), " %)")
      
      zz <- yy %>% 
        group_by(YEAR,LOCATION) %>% 
        summarise(n = n()) %>% 
        bind_cols(points) 
      
      pco_plot[[i]] <- ggplot(zz, aes(x= Axis.1, y = Axis.2))+
        geom_point(aes(colour = LOCATION), size = 5)+
        geom_text(aes(label = `YEAR`), nudge_y = 0.02)+
        geom_path(aes(group = LOCATION), arrow = arrow(),)+
        theme_bw()+
        ylab(lab.y)+
        xlab(lab.x)+
        ggtitle(loc[i])
      archivo <- paste(directorio,fuente,tipo,loc[i], ".pdf", sep = "_")
      ggsave(archivo, pco_plot[[i]], width = 24, height = 16, units = "cm")
      
      
    }
  }
  if(factor.esp == "REGION-YEAR"){
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
    
    # ID para factor.esp
    localities <- fac %>% 
      group_by(YEAR, REGION) %>% 
      summarise(muestras = n())
    
    loc <- localities %>% 
      select(REGION) %>% 
      ungroup(YEAR)
    
    loc <- levels(factor(loc$REGION))    
    
    ### Generación de PCO
    
    pco_plot <- list(NULL)
    
    for (i in 1:length(loc)){
      # slección de datos por localidad
      sel <- which(fac$REGION == loc[i])
      xx <- bio[sel,] 
      yy <- fac[sel,]
      
      #Pretratamiento y centroides
      xx$dummy <- rep(1, nrow(xx))
      bray <- vegdist(xx)
      disper <- betadisper(bray, group = yy$YEAR_REGION)
      centroides <- disper$centroids
      dis_cen <- vegdist(centroides, method = "euclidean")
      
      pco <- pcoa(dis_cen)
      #mds <- metaMDS(dis_cen)
      points <- as.data.frame(pco$vectors[,1:2])
      
      lab.x <- str_c("PCO 1 (", round(pco$values$Relative_eig[1] * 100,0), " %)")
      lab.y <- str_c("PCO 2 (", round(pco$values$Relative_eig[2] * 100,0), " %)")
      
      zz <- yy %>% 
        group_by(YEAR,REGION) %>% 
        summarise(n = n()) %>% 
        bind_cols(points) 
      
      pco_plot[[i]] <- ggplot(zz, aes(x= Axis.1, y = Axis.2))+
        geom_point(colour = "blue", size = 5)+
        geom_text(aes(label = `YEAR`), nudge_y = 0.0005)+
        geom_path(aes(group = REGION), arrow = arrow(),)+
        theme_bw()+
        ylab(lab.y)+
        xlab(lab.x)+
        ggtitle(loc[i])
      archivo <- paste(directorio,fuente,tipo,loc[i], ".pdf", sep = "_")
      ggsave(archivo, pco_plot[[i]], width = 24, height = 16, units = "cm")
      
    }
  }
}

shadeplot <- function(bio, fac, factor.esp = c("LOCATION", "REGION", "REGIO-YEAR"),
                      directorio = "MDS/carpetas.../",
                      tipo = c("benthic", "fish_inv", "fish_bio"),
                      fuente = c("PRCRMP", "SEAMAP", "NCRMP"),
                      leyenda = c("cobertura", "abundancia", "biomasa"),
                      important.spp){
  if(factor.esp == "LOCATION"){
    s <- as.numeric(important.spp)
    # ID para factor.esp
    localities <- fac %>% 
      group_by(LOCATION) %>% 
      summarise(muestras = n())
    
    loc <- localities %>% 
      filter(muestras >= 15) %>% 
      select(LOCATION) 
    
    loc <- levels(factor(loc$LOCATION))
   
     ### Remover muestras vacías
    u_sp <- length(bio)
    
    zero_sample <- bio %>% 
      bind_cols(fac) %>%  
      pivot_longer(cols = 1:u_sp, names_to = "species_name", values_to = "species_count") %>% 
      group_by(YEAR_SITE) %>% 
      summarise(N = sum(species_count)) %>% 
      filter(N == 0) %>% 
      select(YEAR_SITE) %>% 
      as.character()
    
    bio$`YEAR_SITE` <- fac$`YEAR_SITE`
    bio$YEAR <- fac$YEAR
    bio$`SITE NAME` <- fac$`SITE NAME`
    bio$LOCATION <- fac$LOCATION
    
    fac <- fac %>% 
      filter(YEAR_SITE != zero_sample)
    
    bio <- bio %>% 
      filter(YEAR_SITE != zero_sample) 
    
    ### Generación de shade plot
    
    shade_plot <- list(NULL)
    
    for (i in 1:length(loc)){
     xx <- bio %>% 
              filter(LOCATION == loc[i])
     
     spp <- xx %>% 
              pivot_longer(cols = 1:u_sp, 
                           names_to = "species_name",
                           values_to = "species_count") %>%
              group_by(species_name) %>% 
              filter(species_count != 0) %>% 
              mutate(species_name = factor(species_name))
     
     spp <- levels(spp$species_name)
      
     xx <- xx %>% 
            select(c(YEAR_SITE,YEAR,`SITE NAME`,spp)) %>% 
            #mutate(across(where(is.numeric),.fns = sqrt), dummy = 1) %>% 
            group_by(YEAR_SITE,YEAR,`SITE NAME`) %>%      
            summarise(across(where(is.numeric),.fns = mean)) %>% 
            ungroup()
     
     sel <- xx %>% 
       pivot_longer(cols = 4:length(xx), names_to = "species_name", values_to = "species_count") %>% 
       group_by(species_name) %>% 
       summarise(N = sum(species_count)) %>% 
       arrange(desc(N))
       
     sel <- as.character(sel$species_name[1:s])
     sel <- sel[!is.na(sel)]
     
     xx2 <- xx %>% 
       select(any_of(sel)) %>% 
       as.matrix()

     #Whittaker's index of association
       total <- apply(xx2, MARGIN = 2, FUN = sum, na.rm = TRUE)
       mac2 <- xx2
       for (j in 1:nrow(xx2)){
         mac2[j,] <- 100*(xx2[j,]/total)  
       }
       d <- vegdist(t(mac2[,]), method = "bray", na.rm = TRUE)
       cluster.sp <- hclust(d, method = "average")
       especies <- cluster.sp$labels[cluster.sp$order]
       
      shade_plot[[i]] <-  xx %>% 
         select(c(YEAR_SITE,YEAR,`SITE NAME`, especies)) %>% 
         pivot_longer(cols = especies, 
                      names_to = "species_name",
                      values_to = "species_count") %>% 
         ggplot(aes(x = YEAR_SITE,
                    y = species_name,
                    fill = species_count,
                    group = `SITE NAME`))+
         geom_tile()+
         ylab("species")+
         xlab("YEAR - SITE")+
         ggtitle(loc[i])+
         labs(fill = leyenda)+
         scale_y_discrete(labels = especies, expand = c(0, 0))+
         scale_x_discrete(expand = c(0, 0))+
         scale_fill_gradient(low = "white", high = "black")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
         
     archivo <- paste(directorio,fuente,tipo,loc[i], ".pdf", sep = "_")
     ggsave(filename =  archivo,
            plot = shade_plot[[i]],
            device = "pdf",
            width = 24,
            height = 16, 
            units = "cm")
     
    }
    
  }
  if(factor.esp == "REGION"){
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
    bio$YEAR <- fac$YEAR
    bio$LOCATION <- fac$LOCATION
    bio$REGION <- fac$REGION

    fac <- fac %>%
      mutate(REGION = str_replace(fac$REGION, pattern = "/", replacement = "-")) %>%
      filter(YEAR_LOCATION != zero_sample)

    bio <- bio %>%
      mutate(REGION = str_replace(fac$REGION, pattern = "/", replacement = "-")) %>%
      filter(YEAR_LOCATION != zero_sample)

    # ID para factor.esp
    s <- as.numeric(important.spp)
    
    localities <- fac %>%
      group_by(REGION) %>%
      summarise(muestras = n())

    loc <- localities %>%
      filter(muestras >= 15) %>%
      select(REGION)

    loc <- levels(factor(loc$REGION))

#     ### Generación de Shade Plot
     
     shade_plot <- list(NULL)
     
     for (i in 1:length(loc)){
       xx <- bio %>% 
         filter(REGION == loc[i])
       
       spp <- xx %>% 
         pivot_longer(cols = 1:u_sp, 
                      names_to = "species_name",
                      values_to = "species_count") %>%
         group_by(species_name) %>% 
         filter(species_count != 0) %>% 
         mutate(species_name = factor(species_name))
       
       spp <- levels(spp$species_name)
       
       xx <- xx %>% 
         select(c(YEAR_LOCATION,YEAR,LOCATION,spp)) %>% 
         #mutate(across(where(is.numeric),.fns = sqrt), dummy = 1) %>% 
         group_by(YEAR_LOCATION,YEAR,LOCATION) %>%      
         summarise(across(where(is.numeric),.fns = mean)) %>% 
         ungroup()
       
       sel <- xx %>% 
         pivot_longer(cols = 4:length(xx), names_to = "species_name", values_to = "species_count") %>% 
         group_by(species_name) %>% 
         summarise(N = sum(species_count)) %>% 
         arrange(desc(N))
       
       sel <- as.character(sel$species_name[1:s])
       sel <- sel[!is.na(sel)]
       
       xx2 <- xx %>% 
         select(any_of(sel)) %>% 
         as.matrix()
       
       #Whittaker's index of association
       total <- apply(xx2, MARGIN = 2, FUN = sum, na.rm = TRUE)
       mac2 <- xx2
       for (j in 1:nrow(xx2)){
         mac2[j,] <- 100*(xx2[j,]/total)  
       }
       d <- vegdist(t(mac2[,]), method = "bray", na.rm = TRUE)
       cluster.sp <- hclust(d, method = "average")
       especies <- cluster.sp$labels[cluster.sp$order]
       
       shade_plot[[i]] <-  xx %>% 
         select(c(YEAR_LOCATION,YEAR,LOCATION, especies)) %>% 
         pivot_longer(cols = especies, 
                      names_to = "species_name",
                      values_to = "species_count") %>% 
         ggplot(aes(x = YEAR_LOCATION,
                    y = species_name,
                    fill = species_count,
                    group = LOCATION))+
         geom_tile()+
         ylab("species")+
         xlab("YEAR - LOCATION")+
         ggtitle(loc[i])+
         labs(fill = leyenda)+
         scale_y_discrete(labels = especies, expand = c(0, 0))+
         scale_x_discrete(expand = c(0, 0))+
         scale_fill_gradient(low = "white", high = "black")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
       
       archivo <- paste(directorio,fuente,tipo,loc[i], ".pdf", sep = "_")
       ggsave(filename =  archivo,
              plot = shade_plot[[i]],
              device = "pdf",
              width = 24,
              height = 16, 
              units = "cm")
       
     }
  }
  if(factor.esp == "REGION-YEAR"){

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

      fac <- fac %>%
      mutate(REGION = str_replace(fac$REGION, pattern = "/", replacement = "-"),
             YEAR_REGION = str_replace(fac$YEAR_REGION, pattern = "/", replacement = "-")) %>%
      filter(YEAR_REGION != zero_sample)
    
      bio$YEAR_REGION <- fac$YEAR_REGION
      bio$YEAR <- fac$YEAR
      bio$REGION <- fac$REGION

    bio <- bio %>%
      filter(YEAR_REGION != zero_sample)
    
    # ID para factor.esp
    s <- as.numeric(important.spp)
    
    localities <- fac %>%
      group_by(REGION) %>%
      summarise(muestras = n())
    
    loc <- localities %>%
      filter(muestras >= 15) %>%
      select(REGION)
    
    loc <- levels(factor(loc$REGION))
    
    ### Generación de Shade Plot
    
    shade_plot <- list(NULL)
    
    for (i in 1:length(loc)){
      xx <- bio %>% 
        filter(REGION == loc[i])
      
      spp <- xx %>% 
        pivot_longer(cols = 1:u_sp, 
                     names_to = "species_name",
                     values_to = "species_count") %>%
        group_by(species_name) %>% 
        filter(species_count != 0) %>% 
        mutate(species_name = factor(species_name))
      
      spp <- levels(spp$species_name)
      
      xx <- xx %>% 
        select(c(YEAR_REGION,YEAR,REGION,spp)) %>% 
        #mutate(across(where(is.numeric),.fns = sqrt), dummy = 1) %>% 
        group_by(YEAR_REGION,YEAR,REGION) %>%      
        summarise(across(where(is.numeric),.fns = mean)) %>% 
        ungroup()
      
      sel <- xx %>% 
        pivot_longer(cols = 4:length(xx), names_to = "species_name", values_to = "species_count") %>% 
        group_by(species_name) %>% 
        summarise(N = sum(species_count)) %>% 
        arrange(desc(N))
      
      sel <- as.character(sel$species_name[1:s])
      sel <- sel[!is.na(sel)]
      
      xx2 <- xx %>% 
        select(any_of(sel)) %>% 
        as.matrix()
      
      #Whittaker's index of association
      total <- apply(xx2, MARGIN = 2, FUN = sum, na.rm = TRUE)
      mac2 <- xx2
      for (j in 1:nrow(xx2)){
        mac2[j,] <- 100*(xx2[j,]/total)  
      }
      d <- vegdist(t(mac2[,]), method = "bray", na.rm = TRUE)
      cluster.sp <- hclust(d, method = "average")
      especies <- cluster.sp$labels[cluster.sp$order]
      
      shade_plot[[i]] <-  xx %>% 
        select(c(YEAR_REGION,YEAR,REGION, especies)) %>% 
        pivot_longer(cols = especies, 
                     names_to = "species_name",
                     values_to = "species_count") %>% 
        ggplot(aes(x = YEAR,
                   y = species_name,
                   fill = species_count))+
        geom_tile()+
        ylab("species")+
        xlab("YEAR")+
        ggtitle(loc[i])+
        labs(fill = leyenda)+
        scale_y_discrete(labels = especies, expand = c(0, 0))+
        scale_x_discrete(expand = c(0, 0))+
        scale_fill_gradient(low = "white", high = "black")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
      
      archivo <- paste(directorio,fuente,tipo,loc[i], ".pdf", sep = "_")
      ggsave(filename =  archivo,
             plot = shade_plot[[i]],
             device = "pdf",
             width = 24,
             height = 16, 
             units = "cm")
      
    }
    
    }
  }


pco.env <- function(env,
                    directorio = "PCO/pruebas/",
                    r = 0.5,
                    col = 0.85,
                    vectores = TRUE){
  
  if(vectores == TRUE){
  
  loc <- levels(factor(env$REGION))    
  
  ### Generación de PCO
  
  pco_plot <- list(NULL)
  
  for (i in 1:length(loc)){
    
    xx <- env %>% 
      filter(REGION == loc[i]) %>% 
      select(-c(ID, lat, lon)) %>% 
      group_by(YEAR, REGION) %>% 
      summarise(across(.fns = mean, na.rm = T))
    
    #Salvar archivo
    write_excel_csv(
      x = 
        xx %>% 
        mutate("-" = NA) %>% 
        relocate(YEAR, .after = `-`) %>% 
        relocate(REGION, .after = YEAR) ,
      file = paste(directorio,"env_data",loc[i], ".csv", sep = "_"),
      na = ""
    )
    
    #Pretratamiento y centroides
    xx2 <- scale(as.matrix(xx[,3:length(xx)]))
    euc <- vegdist(xx2, method = "euclidean")
    pco <- pcoa(euc)
    #mds <- metaMDS(dis_cen)
    points <- as.data.frame(pco$vectors[,1:2])
    
    lab.x <- str_c("PCO 1 (", round(pco$values$Relative_eig[1] * 100,0), " %)")
    lab.y <- str_c("PCO 2 (", round(pco$values$Relative_eig[2] * 100,0), " %)")
    
    zz <- xx %>% 
      # group_by(YEAR,REGION) %>% 
      # summarise(n = n()) %>% 
      bind_cols(points) 
    
    # Estimación de vectores para biplot
    correlaciones <- cor(as.matrix(zz[,3:length(zz)]))
    
    # Remoción de variables colineales
    dep <- rep(NA,length(xx)-3)
    
    for(k in 3:(length(xx)-1)){
      if(cor(xx[,k], xx[,k+1]) >= col){
          dep[k] <- colnames(xx[,k+1])        
        }
      }
    
    dep <- na.exclude(dep)
    
    vectores <- as_tibble(correlaciones) %>% 
      mutate(variables = row.names(correlaciones)) %>% 
      select(Axis.1:variables) %>% 
      filter(!variables %in% dep) %>% 
      rename("delta_Axis.1" = "Axis.1",
             "delta_Axis.2" = "Axis.2") %>% 
      mutate("Axis.1" = 0, "Axis.2" = 0) %>% 
      filter(variables != "Axis.1") %>% 
      filter(variables != "Axis.2") %>% 
      filter(delta_Axis.1 >= abs(r) | delta_Axis.2 >= abs(r)) 
      
    
    pco_plot[[i]] <- ggplot(zz, aes(x= Axis.1, y = Axis.2))+
      geom_point(colour = "blue", size = 5)+
      geom_text(aes(label = `YEAR`), nudge_y = 0.3)+
      geom_vector(data = vectores,
                  aes(x = Axis.1,
                      y = Axis.2,
                      dx = 5*delta_Axis.1,
                      dy = 5*delta_Axis.2),
                  skip = 1,
                  pivot = 0,
                  color = "red") +
      geom_text(data = vectores,
                aes(x = 3*delta_Axis.1,
                    y = 3*delta_Axis.2,
                    label = variables))+
      theme_bw()+
      ylab(lab.y)+
      xlab(lab.x)+
    ggtitle(loc[i])
    archivo <- paste(directorio,"vectores",loc[i], ".pdf", sep = "_")
    ggsave(archivo, pco_plot[[i]], width = 24, height = 16, units = "cm")
    
    }
  }
  
  if(vectores == FALSE){
    
    loc <- levels(factor(env$REGION))    
    
    ### Generación de PCO
    
    pco_plot <- list(NULL)
    
    for (i in 1:length(loc)){
      
      xx <- env %>% 
        filter(REGION == loc[i]) %>% 
        select(-c(ID, lat, lon)) %>% 
        group_by(YEAR, REGION) %>% 
        summarise(across(.fns = mean, na.rm = T))
      
      #Salvar archivo
      write_excel_csv(
        x = 
          xx %>% 
          mutate("-" = NA) %>% 
          relocate(YEAR, .after = `-`) %>% 
          relocate(REGION, .after = YEAR) ,
        file = paste(directorio,"env_data",loc[i], ".csv", sep = "_"),
        na = ""
      )
      
      #Pretratamiento y centroides
      xx2 <- scale(as.matrix(xx[,3:length(xx)]))
      euc <- vegdist(xx2, method = "euclidean")
      pco <- pcoa(euc)
      #mds <- metaMDS(dis_cen)
      points <- as.data.frame(pco$vectors[,1:2])
      
      lab.x <- str_c("PCO 1 (", round(pco$values$Relative_eig[1] * 100,0), " %)")
      lab.y <- str_c("PCO 2 (", round(pco$values$Relative_eig[2] * 100,0), " %)")
      
      zz <- xx %>% 
        # group_by(YEAR,REGION) %>% 
        # summarise(n = n()) %>% 
        bind_cols(points) 
      
      pco_plot[[i]] <- ggplot(zz, aes(x= Axis.1, y = Axis.2))+
        geom_point(colour = "blue", size = 5)+
        geom_text(aes(label = `YEAR`), nudge_y = 0.3)+
        geom_path(aes(group = REGION), arrow = arrow(),)+
        theme_bw()+
        ylab(lab.y)+
        xlab(lab.x)+
        ggtitle(loc[i])
      archivo <- paste(directorio,"trayectoria",loc[i], ".pdf", sep = "_")
      ggsave(archivo, pco_plot[[i]], width = 24, height = 16, units = "cm")
      
    }
  }
  
}