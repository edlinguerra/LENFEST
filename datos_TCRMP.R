# USVI - TEMPORAL CHANGES OF REGIONS -----------------------------------------------------------------
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

# Benthic -----------------------------------------------------------------

USVI_2001_2016 <- read_excel('datos_originales/Datos Edlin-20230421T133617Z-001/Datos Edlin/Biological/Benthic cover/TCRMP_Benthic_Master_Feb2020_(USVI) benthos.xlsx',
sheet = 'DATA2001-2016')

USVI_2017 <- read_excel('datos_originales/Datos Edlin-20230421T133617Z-001/Datos Edlin/Biological/Benthic cover/TCRMP_Benthic_Master_Feb2020_(USVI) benthos.xlsx',
sheet = 'DATA2017-PRES')


region <- c(
'Black Point' = 'SOUTH', 
'Botany Bay'  = 'WEST',
'Brewers Bay' = 'SOUTH',
'Buck Island STT' = 'SOUTH',
'Buck Island STX' = 'EAST',
'Cane Bay' = 'NORTH',
'Cane Bay Deep' = 'NORTH',
'Castle' = 'EAST',
'Coculus Rock' =   'SOUTH',
'College Shoal East' = 'SOUTH',
'Coral Bay' = 'EAST',
'Eagle Ray' = 'NORTH',
'Fish Bay' = 'EAST',
'Flat Cay' = 'SOUTH',
'Ginsburg Fringe' = 'SOUTH',
'Grammanik Tiger FSA' = 'SOUTH',
'Great Pond' = 'SOUTH',
'Hind Bank East FSA' = 'EAST',
'Jacks Bay' = 'EAST',
'Kings Corner' = 'WEST',
'Lang Bank EEMP' = 'EAST',
'Lang Bank Red Hind FSA' = 'EAST',
'Magens Bay' = 'NORTH',
'Meri Shoal'  = 'EAST',
'Mutton Snapper FSA', 'SOUTH',
'Salt River Deep' = 'NORTH',
'Salt River West' = 'NORTH',
'Savana' = 'WEST',
'Seahorse Cottage Shoal' = 'EAST',
'South Capella' = 'SOUTH',
'South Water' = 'SOUTH',
'Sprat Hole' = 'WEST',
'St James' = 'EAST')

temp1 <- USVI_2001_2016 |>
  mutate(REGION = str_replace_all(LOCATION, pattern = c(
           'Black Point' = 'SOUTH', 
           'Botany Bay'  = 'WEST',
           'Brewers Bay' = 'SOUTH',
           'Buck Island STT' = 'SOUTH',
           'Buck Island STX' = 'EAST',
           'Cane Bay' = 'NORTH',
           'Cane Bay Deep' = 'NORTH',
           'Castle' = 'EAST',
           'Coculus Rock' =   'SOUTH',
           'College Shoal East' = 'SOUTH',
           'Coral Bay' = 'EAST',
           'Eagle Ray' = 'NORTH',
           'Fish Bay' = 'EAST',
           'Flat Cay' = 'SOUTH',
           'Ginsburg Fringe' = 'SOUTH',
           'Grammanik Tiger FSA' = 'SOUTH',
           'Great Pond' = 'SOUTH',
           'Hind Bank East FSA' = 'EAST',
           'Jacks Bay' = 'EAST',
           'Kings Corner' = 'WEST',
           'Lang Bank EEMP' = 'EAST',
           'Lang Bank Red Hind FSA' = 'EAST',
           'Magens Bay' = 'NORTH',
           'Meri Shoal'  = 'EAST',
           'Mutton Snapper FSA' = 'SOUTH',
           'Salt River Deep' = 'NORTH',
           'Salt River West' = 'NORTH',
           'Savana' = 'WEST',
           'Seahorse Cottage Shoal' = 'EAST',
           'South Capella' = 'SOUTH',
           'South Water' = 'SOUTH',
           'Sprat Hole' = 'WEST',
           'St James' = 'EAST'
         )))|>
  select(1:114)|>
  pivot_longer(cols = 15:114, names_to = 'species', values_to = 'cover')|>
  group_by(YEAR, LOCATION, ISLAND, species)|>
  summarise(cover = mean(cover))|>
  pivot_wider(names_from = species, values_from = cover)|>
  
  relocate(label, .before = YEAR)|>
  relocate(' ', .before = YEAR)|>
  relocate(c(' ', YEAR, REGION, ISLAND), .after = 'Xestospongia muta')

write_csv(prcrmp, file = 'final_prcrmp.csv')
