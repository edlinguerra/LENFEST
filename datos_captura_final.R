# ANÁLISIS FINAL DE CAPTURA 

library(readxl)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(ggside)
library(ggdendro)
library(vegan)
library(ape)
library(metR)
library(latex2exp)
library(magrittr)
library(openxlsx)


#Cargar datos de Puerto Rico
PR_raw <- read_excel("datos_originales/Datos Edlin-20230421T133617Z-001/Datos Edlin/Landings/pr_jj_request_final_product_nonconf_20221216 .xlsx", 
                     sheet = "data") |>
          select(-c(SOURCE, POUNDS_LANDED))|>
          rename(POUNDS_LANDED = ADJUSTED_POUNDS)|>
          filter(FISHING_ZONE != "INLAND")

#Cargar datos de USVI 

USVI_raw <- read_excel("datos_originales/Datos Edlin-20230421T133617Z-001/Datos Edlin/Landings/usvi_jj_request_final_product_nonconf_20221206_v1.xlsx", 
                       sheet = "data")

# Combinar datos de PR y USVI
raw <- bind_rows(PR_raw, USVI_raw)

# Sumar datos de captura por año

raw2 <- raw|>
  group_by(TRIP_YEAR, ISLAND, FISHING_ZONE, SPECIES_NAME)|>
  summarise(TOTAL_CATCH = sum(POUNDS_LANDED))

# Convertir raw2 a base de datos formato PRIMER

primer <- raw2|>
  pivot_wider(names_from = SPECIES_NAME, values_from = TOTAL_CATCH, values_fill = 0)|>
  mutate(" " = " ")|>
  relocate(c(TRIP_YEAR, ISLAND, FISHING_ZONE), .after = " ")|>
  mutate(label = str_c(TRIP_YEAR,"-",ISLAND,"-", FISHING_ZONE))|>
  relocate(label, .before = BALLYHOO)

write_csv(primer, file = "landings_final_PRIMER.csv", na = " ")


