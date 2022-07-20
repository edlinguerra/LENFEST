## VINVULO DATOS LANDINGS CON AMBIENTALES 

library(readr)
library(stringr)
library(vegan)
library(tidyr)
library(dplyr)
library(ggplot2)
library(magrittr)

#Listado de captura por REGION YEAR GEAR
landings <- list.files(path = "LANDINGS/MDS/temporal_region/HL/",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

df_land <- readr::read_csv(landings, id = "file_name")
PR_landing_lvs <- levels(factor(df_land$YEAR_REGION))

#datos ambientales
env_dat <- read_csv("PCO/_env_data_all_regions_.csv")

env_dat %<>% 
  mutate('YEAR_REGION' = str_c(REGION,YEAR, sep = "-"))
 
PR_env_lvs <- levels(factor(env_dat$YEAR_REGION))
  #select(REGION, YEAR, YEAR_REGION)
  #filter(YEAR_REGION %in% PR_lvs) %>% 
  #select(REGION, YEAR, YEAR_REGION)

match_l_e <- df_land %>% 
  select(REGION, YEAR, YEAR_REGION) %>% 
  filter(YEAR_REGION %in% PR_env_lvs)

levels(factor(match_l_e$YEAR_REGION))
