################### PREPARACIÓN DE DATOS AMBIENTALES
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(vegan)
library(ape)
library(readxl)
library(metR)

### Coordenadas por ID

coord <- read_excel("datos_originales/OneDrive_2022-06-21/Ambiental/Lenfest_sites_master_20_08.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric")) 

coord <- coord %>%
          select(lat, lon, ID)

# ### Datos con variación temporal ----------------------------------------


# Wave data y referencia
wave <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_1_wave.csv",
                col_types = cols(date = col_date(format = "%d/%m/%Y")))

wave_xref <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_1_wave_xref.csv",
                      col_types = cols(gridID = col_character()))

anual_wave <- wave %>% 
                pivot_longer(cols = 2:length(wave), names_to = "gridID", values_to = "wave") %>% 
                mutate(YEAR = year(date)) %>% 
                relocate(YEAR, .after = date) %>% 
                group_by(YEAR, gridID) %>% 
                summarise(wave_mean = mean(wave, na.rm = TRUE), wave_sd = sd(wave, na.rm = TRUE)) %>% 
                left_join(wave_xref) %>% 
                relocate(ID, .after = gridID) %>% 
                left_join(coord) %>% 
                relocate(lat:lon, .after = ID)

rm(wave)
rm(wave_xref)

# Rain y referencia

rain <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_3_rain.csv", 
                 col_types = cols(date = col_date(format = "%d/%m/%Y")))

rain_xref <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_3_rain_xref.csv", 
                      col_types = cols(gridID = col_character()))

anual_rain <- rain %>% 
  pivot_longer(cols = 2:length(rain), names_to = "gridID", values_to = "rain") %>% 
  mutate(YEAR = year(date)) %>% 
  relocate(YEAR, .after = date) %>% 
  group_by(YEAR, gridID) %>% 
  summarise(rain_mean = mean(rain, na.rm = TRUE), rain_sd = sd(rain, na.rm = TRUE)) %>% 
  left_join(rain_xref) %>% 
  relocate(ID, .after = gridID) %>% 
  left_join(coord) %>% 
  relocate(lat:lon, .after = ID)

rm(rain)
rm(rain_xref)

# sst y referencia

sst <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_19_sst.csv", 
                 col_types = cols(date = col_date(format = "%d/%m/%Y")))

sst_xref <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_19_sst_xref.csv", 
                      col_types = cols(gridID = col_character()))

anual_sst <- sst %>% 
  pivot_longer(cols = 2:length(sst), names_to = "gridID", values_to = "sst") %>% 
  mutate(YEAR = year(date)) %>% 
  relocate(YEAR, .after = date) %>% 
  group_by(YEAR, gridID) %>% 
  summarise(sst_mean = mean(sst, na.rm = TRUE), sst_sd = sd(sst, na.rm = TRUE)) %>% 
  left_join(sst_xref) %>% 
  relocate(ID, .after = gridID)%>% 
  left_join(coord) %>% 
  relocate(lat:lon, .after = ID)

rm(sst)
rm(sst_xref)

# dhw y referencia

dhw <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_20_dhw.csv")

dhw_xref <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_20_dhw_xref.csv", 
                     col_types = cols(gridID = col_character()))

anual_dhw <- dhw %>% 
  pivot_longer(cols = 2:length(dhw), names_to = "gridID", values_to = "dhw") %>% 
  mutate(YEAR = year(date)) %>% 
  relocate(YEAR, .after = date) %>% 
  group_by(YEAR, gridID) %>% 
  summarise(dhw_mean = mean(dhw, na.rm = TRUE), dhw_sd = sd(dhw, na.rm = TRUE)) %>% 
  left_join(dhw_xref) %>% 
  relocate(ID, .after = gridID)%>% 
  left_join(coord) %>% 
  relocate(lat:lon, .after = ID)

rm(dhw)
rm(dhw_xref)

# prod y referencia

prod <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_21_prod.csv")

prod_xref <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_21_prod_xref.csv", 
                     col_types = cols(gridID = col_character()))

anual_prod <- prod %>% 
  pivot_longer(cols = 2:length(prod), names_to = "gridID", values_to = "prod") %>% 
  mutate(YEAR = year(date)) %>% 
  relocate(YEAR, .after = date) %>% 
  group_by(YEAR, gridID) %>% 
  summarise(prod_mean = mean(prod, na.rm = TRUE), prod_sd = sd(prod, na.rm = TRUE)) %>% 
  left_join(prod_xref) %>% 
  relocate(ID, .after = gridID)%>% 
  left_join(coord) %>% 
  relocate(lat:lon, .after = ID)

rm(prod)
rm(prod_xref)

# k490 y referencia

k490 <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_17_k490.csv")

k490_xref <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_17_k490_xref.csv", 
                      col_types = cols(gridID = col_character()))

anual_k490 <- k490 %>% 
  pivot_longer(cols = 2:length(k490), names_to = "gridID", values_to = "k490") %>% 
  mutate(YEAR = year(date)) %>% 
  relocate(YEAR, .after = date) %>% 
  group_by(YEAR, gridID) %>% 
  summarise(k490_mean = mean(k490, na.rm = TRUE), k490_sd = sd(k490, na.rm = TRUE)) %>% 
  left_join(k490_xref) %>% 
  relocate(ID, .after = gridID)%>% 
  left_join(coord) %>% 
  relocate(lat:lon, .after = ID)

rm(k490)
rm(k490_xref)


# Combinación de variables ------------------------------------------------
### NOTA REMOVER ID para que al combinar solo considere año y coordenadas 

dhw_sst <- anual_dhw %>% 
            left_join(anual_sst) %>% 
            select(-gridID) %>% 
            mutate(lat = round(lat,2),
                   lon = round(lon,2))

anual_k490 <- anual_k490 %>% 
              mutate(lat = round(lat,2),
                    lon = round(lon,2))

anual_prod <- anual_prod %>% 
  mutate(lat = round(lat,2),
         lon = round(lon,2))

anual_rain <- anual_rain %>% 
  mutate(lat = round(lat,2),
         lon = round(lon,2))


anual_wave <- anual_wave %>% 
  mutate(lat = round(lat,2),
         lon = round(lon,2))

anual_env <- dhw_sst %>% 
  full_join(y = anual_k490 %>% select(-gridID)) %>% 
  full_join(y = anual_prod %>% select(-gridID)) %>% 
  full_join(y = anual_rain %>% select(-gridID)) %>% 
  #full_join(y = anual_wave %>% select(-gridID)) %>%
  #ungroup()%>% 
  #drop_na()
  filter(YEAR >=2003) 

rm(anual_dhw, anual_k490, anual_prod, anual_rain, anual_sst, anual_wave)
rm(dhw_sst)
rm(coord)

anual_env <- anual_env %>%
  mutate(ID = as.character(ID))


ggplot(anual_env, aes(x = lon, y = lat))+
    geom_point()+
    scale_x_continuous(breaks = seq(-68.00,-64.00, 0.1))


# Huricanes (mover en lo que se aclare duda)

hurr <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/lenfest_4_hurricanes.csv", 
                 col_types = cols(date = col_date(format = "%d/%m/%Y")))


anual_env_hurr <- hurr %>% 
  pivot_longer(cols = 2:length(hurr), names_to = "ID", values_to = "hurr") %>% 
  mutate(YEAR = year(date)) %>% 
  relocate(YEAR, .after = date) %>%
  filter(YEAR>=2003) %>% 
  group_by(YEAR, ID) %>% 
  summarise(hurr_sum = sum(hurr, na.rm = TRUE)) %>% 
  left_join(anual_env) 

rm(hurr)
rm(anual_env)

# ### Datos sin variación temporal ----------------------------------------

data_not_temporal <- read_csv("datos_originales/OneDrive_2022-06-21/Ambiental/Data not temporal__1 datum per site.csv")

nt_env <- data_not_temporal %>% 
          mutate(lat = round(lat,2),
                   lon = round(lon,2),
                 ID = as.character(ID)) %>% 
          select(-c(file,site, `10_hab_hab`, `10_hab_typ`))

rm(data_not_temporal)

# COMBINACIÓN DE DATOS ANUALES Y ESTÁTICOS --------------------------------

env.data <- anual_env_hurr %>% 
              left_join(nt_env) %>% 
              #Creación de zonas
              mutate(REGION = if_else(lon < -67.15, "PR-West",
                                      if_else(lon < -66 & lat >18.0, "PR-North",
                                              if_else(lon < -66 & lat <18.0, "PR-South",
                                                      if_else(lon < -65.05,  "PR-East",
                                                              if_else(lat > 18, "USVI-St. Thomas",
                                                                          "USVI-St. Croix")))))) %>% 
              relocate(REGION, .after = YEAR) %>% 
              drop_na()


ggplot(env.data, aes(x = lon, y = lat))+
  geom_point(aes(colour = REGION), size = 3)+
  scale_x_continuous(breaks = seq(-68.00,-64.00, 0.2))+
  theme_bw()

## PCO anual por cada region
pco.env(env = env.data, 
        r = 0.7,
        col = 0.85,
        directorio = "PCO/vectores/",
        vectores = TRUE)

pco.env(env = env.data, 
        r = 0.7,
        col = 0.85,
        directorio = "PCO/trayectoria/",
        vectores = FALSE)

# PCO anual todas las regiones
    xx <- env.data %>% 
      select(-c(ID, lat, lon)) %>% 
      group_by(YEAR, REGION) %>% 
      summarise(across(.fns = mean, na.rm = T))
    
    
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
    
    pco_plot <- ggplot(zz, aes(x= Axis.1, y = Axis.2))+
      geom_point(aes(colour = REGION), size = 5)+
      geom_text(aes(label = `YEAR`), nudge_y = 0.3)+
      #geom_path(aes(group = REGION), arrow = arrow(),)+
      theme_bw()+
      ylab(lab.y)+
      xlab(lab.x)+
      ggtitle("PCO Year - Regions")
    
    archivo <- paste("PCO/","env_data","all_regions", ".pdf", sep = "_")
    ggsave(archivo, pco_plot, width = 24, height = 16, units = "cm")
    

    write_excel_csv(
      x = 
        xx %>% 
        mutate("-" = NA) %>% 
        relocate(YEAR, .after = `-`) %>% 
        relocate(REGION, .after = YEAR) ,
      file = paste("PCO/","env_data","all_regions", ".csv", sep = "_"),
      na = ""
    )



