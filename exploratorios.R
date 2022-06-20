library(readxl)
library(tidyverse)
library(stringr)
library(vegan)

# Benthic (1999-2019) ----------------------------------------------------
d_bentos <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                       sheet = "Benthic (1999-2019)")
bentos <- datos(d_bentos)
fac_bentos <- factores(d_bentos)
inc_bentos <- indicadores(d_bentos)
fac_bentos %>% 
  group_by(YEAR, REGION, LOCATION, `SITE NAME`,YEAR_SITE) %>% 
  summarise(muestras = n())

localities <- fac_bentos %>% 
  group_by(LOCATION) %>% 
  summarise(muestras = n())

loc <- localities %>% 
  filter(muestras >= 15) %>% 
  select(LOCATION) 

loc <- levels(factor(loc$LOCATION))

mds_plot <- list(NULL)
  
for (i in 1:length(loc)){
  sel <- which(fac_bentos[,3] == loc[i])
  xx <- as.data.frame(bentos[sel,])
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
    geom_text(aes(label = YEAR), nudge_y = 0.02)+
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
  archivo <- paste("MDS/bentos/","Benthic_" ,loc[i], ".pdf", sep = "")
    ggsave(archivo, mds_plot[[i]], width = 24, height = 16, units = "cm")
}


# Fish-Invert Ab. (1999-2019) ---------------------------------------------

d_f_i <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                    sheet = "Fish-Invert Ab. (1999-2019)")

fish_inv <- datos(d_f_i)
fac_fish_inv <- factores(d_f_i)
inc_fish_inv <- indicadores(d_f_i)

fac_fish_inv %>% 
  group_by(YEAR, REGION, LOCATION, `SITE NAME`,YEAR_SITE) %>% 
  summarise(muestras = n())

localities <- fac_fish_inv %>% 
  group_by(LOCATION) %>% 
  summarise(muestras = n())

loc <- localities %>% 
  filter(muestras >= 15) %>% 
  select(LOCATION) 

loc <- levels(factor(loc$LOCATION))

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
    geom_text(aes(label = YEAR), nudge_y = 0.02)+
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
  archivo <- paste("MDS/fish_inv/", "Mobile_fauna_",loc[i], ".pdf", sep = "")
  ggsave(archivo, mds_plot[[i]], width = 24, height = 16, units = "cm")
}



# Fish Biomass (2004-2019) ------------------------------------------------
d_fbio <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                     sheet = "Fish Biomass (2004-2019)")

fish_biom <- datos(d_fbio)
fac_fish_biom <- factores(d_fbio)
inc_fish_biom <- indicadores(d_fbio)

fac_fish_biom %>% 
  group_by(YEAR, REGION, LOCATION, `SITE NAME`,YEAR_SITE) %>% 
  summarise(muestras = n())

localities <- fac_fish_biom %>% 
  group_by(LOCATION) %>% 
  summarise(muestras = n())

loc <- localities %>% 
  filter(muestras >= 15) %>% 
  select(LOCATION) 

loc <- levels(factor(loc$LOCATION))

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
    geom_text(aes(label = YEAR), nudge_y = 0.02)+
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
  archivo <- paste("MDS/fish_biomass/", "fish_biomass_",loc[i], ".pdf", sep = "")
  ggsave(archivo, mds_plot[[i]], width = 24, height = 16, units = "cm")
}
