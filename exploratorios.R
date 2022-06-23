library(readxl)
library(tidyverse)
library(stringr)
library(vegan)

#SITES
# Benthic (1999-2019) ----------------------------------------------------
d_bentos <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                       sheet = "Benthic (1999-2019)")

## Extraer datos, factores e indicadores
bentos <- datos(d_bentos)
fac_bentos <- factores(d_bentos)
inc_bentos <- indicadores(d_bentos)

### ID para localidades 
localities <- fac_bentos %>% 
  group_by(REGION) %>% 
  summarise(muestras = n())

loc <- localities %>% 
  filter(muestras >= 15) %>% 
  select(REGION) 

loc <- levels(factor(loc$LOCATION))

### Remover muestras vacías
bentos_df <- as.data.frame(bentos)

u_sp <- length(bentos_df)

zero_sample <- bentos_df %>% 
  bind_cols(fac_bentos) %>%  
  pivot_longer(cols = 1:u_sp, names_to = "species_name", values_to = "species_count") %>% 
  group_by(YEAR_SITE) %>% 
  summarise(N = sum(species_count)) %>% 
  filter(N == 0) %>% 
  select(YEAR_LOCATION) %>% 
  as.character()

bentos_df$`YEAR_SITE` <- fac_bentos$`YEAR_SITE`

fac_bentos <- fac_bentos %>% 
  filter(YEAR_SITE != zero_sample)

bentos_df <- bentos_df %>% 
  filter(YEAR_SITE != zero_sample) %>% 
  select(-YEAR_SITE)

### Generación de MDS

mds_plot <- list(NULL)
  
for (i in 1:length(loc)){
  sel <- which(fac_bentos[,3] == loc[i])
  xx <- bentos_df[sel,] 
  xx$dummy <- rep(1, nrow(xx))
  yy <- fac_bentos[sel,]
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
  archivo <- paste("MDS/localities/benthic/","Benthic_" ,loc[i], ".pdf", sep = "")
  ggsave(archivo, mds_plot[[i]], width = 24, height = 16, units = "cm")
}


# Fish-Invert Ab. (1999-2019) ---------------------------------------------
d_f_i <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                    sheet = "Fish-Invert Ab. (1999-2019)")

## Extraer datos, factores e indicadores
fish_inv <- datos(d_f_i)
fac_fish_inv <- factores(d_f_i)
inc_fish_inv <- indicadores(d_f_i)

### ID para localidades
localities <- fac_fish_inv %>% 
  group_by(REGION) %>% 
  summarise(muestras = n())

loc <- localities %>% 
  filter(muestras >= 15) %>% 
  select(REGION) 

loc <- levels(factor(loc$LOCATION))

### Remover muestras vacías
fish_inv_df <- as.data.frame(fish_inv)

u_sp <- length(fish_inv_df)

zero_sample <- fish_inv_df %>% 
  bind_cols(fac_fish_inv) %>%  
  pivot_longer(cols = 1:u_sp, names_to = "species_name", values_to = "species_count") %>% 
  group_by(YEAR_SITE) %>% 
  summarise(N = sum(species_count)) %>% 
  filter(N == 0) %>% 
  select(YEAR_LOCATION) %>% 
  as.character()

fish_inv_df$`YEAR_SITE` <- fac_fish_inv$`YEAR_SITE`

fac_fish_inv <- fac_fish_inv %>% 
  filter(YEAR_SITE != zero_sample)

fish_inv_df <- fish_inv_df %>% 
  filter(YEAR_SITE != zero_sample) %>% 
  select(-YEAR_SITE)

mds_plot <- list(NULL)

for (i in 1:length(loc)){
  sel <- which(fac_fish_inv[,3] == loc[i])
  xx <- as.data.frame(fish_inv[sel,])
  xx$dummy <- rep(1, nrow(xx))
  yy <- fac_fish_inv[sel,]
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
  archivo <- paste("MDS/localities/fish_inv/", "Mobile_fauna_",loc[i], ".pdf", sep = "")
  ggsave(archivo, mds_plot[[i]], width = 24, height = 16, units = "cm")
}

# Fish Biomass (2004-2019) ------------------------------------------------
d_fbio <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                     sheet = "Fish Biomass (2004-2019)")
## Extraer datos, factores e indicadores
fish_biom <- datos(d_fbio)
fac_fish_biom <- factores(d_fbio)
inc_fish_biom <- indicadores(d_fbio)

### ID para localidades
localities <- fac_fish_biom %>% 
  group_by(REGION) %>% 
  summarise(muestras = n())

loc <- localities %>% 
  filter(muestras >= 15) %>% 
  select(REGION) 

loc <- levels(factor(loc$LOCATION))

### Remover muestras vacías
fish_biom_df <- as.data.frame(fish_biom)

u_sp <- length(fish_biom_df)

zero_sample <- fish_biom_df %>% 
  bind_cols(fac_fish_biom) %>%  
  pivot_longer(cols = 1:u_sp, names_to = "species_name", values_to = "species_count") %>% 
  group_by(YEAR_SITE) %>% 
  summarise(N = sum(species_count)) %>% 
  filter(N == 0) %>% 
  select(YEAR_LOCATION) %>% 
  as.character()

fish_biom_df$`YEAR_SITE` <- fac_fish_biom$`YEAR_SITE`

fac_fish_biom <- fac_fish_biom %>% 
  filter(YEAR_SITE != zero_sample)

fish_biom_df <- fish_biom_df %>% 
  filter(YEAR_SITE != zero_sample) %>% 
  select(-YEAR_SITE)

### Generación de MDS

mds_plot <- list(NULL)

for (i in 1:length(loc)){
  sel <- which(fac_fish_biom[,3] == loc[i])
  xx <- as.data.frame(fish_biom[sel,])
  xx$dummy <- rep(1, nrow(xx))
  yy <- fac_fish_biom[sel,]
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
  archivo <- paste("MDS/localities/fish_biomass/", "Fish_biomass_",loc[i], ".pdf", sep = "")
  ggsave(archivo, mds_plot[[i]], width = 24, height = 16, units = "cm")}


# LOCATIONS -----------------------------------------------------------------
# Benthic (1999-2019) -----------------------------------------------------------------
d_bentos <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                       sheet = "Benthic (1999-2019)")

## Extraer datos, factores e indicadores
bentos <- datos(d_bentos)
fac_bentos <- factores(d_bentos)
inc_bentos <- indicadores(d_bentos)

### Remover muestras vacías
bentos_df <- as.data.frame(bentos)

u_sp <- length(bentos_df)

zero_sample <- bentos_df %>% 
  bind_cols(fac_bentos) %>%  
  pivot_longer(cols = 1:u_sp, names_to = "species_name", values_to = "species_count") %>% 
  group_by(YEAR_LOCATION) %>% 
  summarise(N = sum(species_count)) %>% 
  filter(N == 0) %>% 
  select(`YEAR_LOCATION`) %>% 
  as.character()

bentos_df$`YEAR_LOCATION` <- fac_bentos$`YEAR_LOCATION`

fac_bentos <- fac_bentos %>% 
  mutate(REGION = str_replace(fac_bentos$REGION, pattern = "/", replacement = "-")) %>% 
  filter(YEAR_LOCATION != zero_sample)

bentos_df <- bentos_df %>% 
  filter(YEAR_LOCATION != zero_sample) %>% 
  select(-YEAR_LOCATION)

### ID para regiones 
regiones <- fac_bentos %>% 
  group_by(REGION) %>% 
  summarise(muestras = n())

reg <- regiones %>% 
  filter(muestras >= 15) %>% 
  select(REGION) 

reg <- levels(factor(reg$REGION))


### Generación de MDS

mds_plot <- list(NULL)

for (i in 1:length(reg)){
  sel <- which(fac_bentos[,2] == reg[i])
  xx <- bentos_df[sel,] 
  xx$dummy <- rep(1, nrow(xx))
  yy <- fac_bentos[sel,]
  bray <- vegdist(sqrt(xx))
  disper <- betadisper(bray, group = yy$YEAR_LOCATION)
  centroides <- disper$centroids
  dis_cen <- vegdist(centroides, method = "euclidean")
  mds <- metaMDS(dis_cen)
  points <- as.data.frame(mds$points)
  stress <- str_c("2D stress = ", round(mds$stress, 3))
  zz <- yy %>% 
    group_by(YEAR,`LOCATION`) %>% 
    summarise(n = n()) %>% 
    bind_cols(points) 
  mds_plot[[i]] <- ggplot(zz, aes(x= MDS1, y = MDS2))+
    geom_point(aes(colour = `LOCATION`), size = 5)+
    geom_text(aes(label = `YEAR`), nudge_y = 0.02)+
    geom_path(aes(group = `LOCATION`), arrow = arrow(),)+
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
    ggtitle(reg[i])
  archivo <- paste("MDS/regions/benthic/","Benthic_" ,reg[i], ".pdf", sep = "")
  ggsave(archivo, mds_plot[[i]], width = 24, height = 16, units = "cm")
}


# Fish-Invert Ab. (1999-2019) ---------------------------------------------
d_f_i <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                    sheet = "Fish-Invert Ab. (1999-2019)")

## Extraer datos, factores e indicadores
fish_inv <- datos(d_f_i)
fac_fish_inv <- factores(d_f_i)
inc_fish_inv <- indicadores(d_f_i)

### Remover muestras vacías
fish_inv_df <- as.data.frame(fish_inv)

u_sp <- length(fish_inv_df)

zero_sample <- fish_inv_df %>% 
  bind_cols(fac_fish_inv) %>%  
  pivot_longer(cols = 1:u_sp, names_to = "species_name", values_to = "species_count") %>% 
  group_by(YEAR_LOCATION) %>% 
  summarise(N = sum(species_count)) %>% 
  filter(N == 0) %>% 
  select(`YEAR_LOCATION`) %>% 
  as.character()

fish_inv_df$`YEAR_LOCATION` <- fac_fish_inv$`YEAR_LOCATION`

fac_fish_inv <- fac_fish_inv %>% 
  mutate(REGION = str_replace(fac_fish_inv$REGION, pattern = "/", replacement = "-")) %>% 
  filter(YEAR_LOCATION != zero_sample)

fish_inv_df <- fish_inv_df %>% 
  filter(YEAR_LOCATION != zero_sample) %>% 
  select(-YEAR_LOCATION)

### ID para regiones 
regiones <- fac_fish_inv %>% 
  group_by(REGION) %>% 
  summarise(muestras = n())

reg <- regiones %>% 
  filter(muestras >= 15) %>% 
  select(REGION) 

reg <- levels(factor(reg$REGION))

### Generación de MDS

mds_plot <- list(NULL)

for (i in 1:length(reg)){
  sel <- which(fac_fish_inv[,2] == reg[i])
  xx <- fish_inv_df[sel,] 
  xx$dummy <- rep(1, nrow(xx))
  yy <- fac_fish_inv[sel,]
  bray <- vegdist(sqrt(xx))
  disper <- betadisper(bray, group = yy$YEAR_LOCATION)
  centroides <- disper$centroids
  dis_cen <- vegdist(centroides, method = "euclidean")
  mds <- metaMDS(dis_cen)
  points <- as.data.frame(mds$points)
  stress <- str_c("2D stress = ", round(mds$stress, 3))
  zz <- yy %>% 
    group_by(YEAR,`LOCATION`) %>% 
    summarise(n = n()) %>% 
    bind_cols(points) 
  mds_plot[[i]] <- ggplot(zz, aes(x= MDS1, y = MDS2))+
    geom_point(aes(colour = `LOCATION`), size = 5)+
    geom_text(aes(label = `YEAR`), nudge_y = 0.02)+
    geom_path(aes(group = `LOCATION`), arrow = arrow(),)+
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
    ggtitle(reg[i])
  archivo <- paste("MDS/regions/fish_inv/","Mobile_fauna_" ,reg[i], ".pdf", sep = "")
  ggsave(archivo, mds_plot[[i]], width = 24, height = 16, units = "cm")
}


# Fish Biomass (2004-2019) ------------------------------------------------
d_fbio <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                     sheet = "Fish Biomass (2004-2019)")

## Extraer datos, factores e indicadores
fish_biom <- datos(d_fbio)
fac_fish_biom <- factores(d_fbio)
inc_fish_biom <- indicadores(d_fbio)

### Remover muestras vacías
fish_biom_df <- as.data.frame(fish_biom)

u_sp <- length(fish_biom_df)

zero_sample <- fish_biom_df %>% 
  bind_cols(fac_fish_biom) %>%  
  pivot_longer(cols = 1:u_sp, names_to = "species_name", values_to = "species_count") %>% 
  group_by(YEAR_LOCATION) %>% 
  summarise(N = sum(species_count)) %>% 
  filter(N == 0) %>% 
  select(`YEAR_LOCATION`) %>% 
  as.character()

fish_biom_df$`YEAR_LOCATION` <- fac_fish_biom$`YEAR_LOCATION`

fac_fish_biom <- fac_fish_biom %>% 
  mutate(REGION = str_replace(fac_fish_biom$REGION, pattern = "/", replacement = "-")) %>% 
  filter(YEAR_LOCATION != zero_sample)

fish_biom_df <- fish_biom_df %>% 
  filter(YEAR_LOCATION != zero_sample) %>% 
  select(-YEAR_LOCATION)

### ID para regiones 
regiones <- fac_fish_biom %>% 
  group_by(REGION) %>% 
  summarise(muestras = n())

reg <- regiones %>% 
  filter(muestras >= 15) %>% 
  select(REGION) 

reg <- levels(factor(reg$REGION))

### Generación de MDS

mds_plot <- list(NULL)

for (i in 1:length(reg)){
  sel <- which(fac_fish_biom[,2] == reg[i])
  xx <- fish_biom_df[sel,] 
  xx$dummy <- rep(1, nrow(xx))
  yy <- fac_fish_biom[sel,]
  bray <- vegdist(sqrt(xx))
  disper <- betadisper(bray, group = yy$YEAR_LOCATION)
  centroides <- disper$centroids
  dis_cen <- vegdist(centroides, method = "euclidean")
  mds <- metaMDS(dis_cen)
  points <- as.data.frame(mds$points)
  stress <- str_c("2D stress = ", round(mds$stress, 3))
  zz <- yy %>% 
    group_by(YEAR,`LOCATION`) %>% 
    summarise(n = n()) %>% 
    bind_cols(points) 
  mds_plot[[i]] <- ggplot(zz, aes(x= MDS1, y = MDS2))+
    geom_point(aes(colour = `LOCATION`), size = 5)+
    geom_text(aes(label = `YEAR`), nudge_y = 0.02)+
    geom_path(aes(group = `LOCATION`), arrow = arrow(),)+
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
    ggtitle(reg[i])
  archivo <- paste("MDS/regions/fish_biom/","Mobile_fauna_" ,reg[i], ".pdf", sep = "")
  ggsave(archivo, mds_plot[[i]], width = 24, height = 16, units = "cm")
}


# REGIONS -----------------------------------------------------------------
# Benthic -----------------------------------------------------------------
d_bentos <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                       sheet = "Benthic (1999-2019)")

mds(data = d_bentos,
    factor.esp = "ISLAND",
    directorio = "MDS/island/benthic/",
    tipo = "benthic",
    fuente = "PRCRMP",
    isla = "PR")

# Fish-Invert -------------------------------------------------------------
d_f_i <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                    sheet = "Fish-Invert Ab. (1999-2019)")
mds(data = d_f_i,
    factor.esp = "ISLAND",
    directorio = "MDS/island/fish_inv/",
    tipo = "fish_inv",
    fuente = "PRCRMP",
    isla = "PR")

# Fish Biomass (2004-2019) ------------------------------------------------
d_fbio <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                     sheet = "Fish Biomass (2004-2019)")

mds(data = d_fbio,
    factor.esp = "ISLAND",
    directorio = "MDS/island/fish_biom/",
    tipo = "fish_biom",
    fuente = "PRCRMP",
    isla = "PR")
