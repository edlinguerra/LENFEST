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

# Captura de datos --------------------------------------------------------
# Procesamiento datos de captura
capturas_raw <- read_excel("datos_originales/PR_nonconf_LANDINGS_county_2022-05-09.xlsx",
                           sheet = "PR_nonconf_LANDINGS") %>% 
  filter(LANDING_LOCATION_COUNTY != "ISLAND OF PUERTO RICO")

mun_cost <- levels(factor(capturas_raw$LANDING_LOCATION_COUNTY))

# Coordenadas geográficas por LANDING_LOCATION_COUNTY (municipios)
coord_raw <-
  read_excel("datos_originales/Codigos_Censales_y_Abrev_Municipios.xlsx",
             sheet = "coordenadas")

PR_mun <- coord_raw %>%
  select(municipio, abrev,intptlat, intptlon) %>%
  rename(lat = intptlat,
         lon = intptlon,
         LLC_abrev = abrev) %>% 
  mutate("LANDING_LOCATION_COUNTY" = str_to_upper(make_clean_names(municipio)))%>%
  mutate("LANDING_LOCATION_COUNTY" = str_replace_all(LANDING_LOCATION_COUNTY, "_", " ")) %>% 
  relocate(LANDING_LOCATION_COUNTY, .before = LLC_abrev) %>% 
  select(-municipio) %>% 
  filter(LANDING_LOCATION_COUNTY %in% mun_cost) %>% 
  arrange(LANDING_LOCATION_COUNTY) %>% 
  mutate(REGION = if_else(lon < -67.1, "PR-West",
                        if_else(lon < -65.7 & lat >18.35, "PR-North",
                                if_else(lon < -65.9 & lat <18.35, "PR-South", "PR-East")))) %>% 
  rename(LOCATION = LANDING_LOCATION_COUNTY)

#plot para visualizar clasificación de LLC por region
# ggplot(PR_mun, aes(x = lon, y = lat))+
#   geom_point(aes(colour = REGION), size = 4)+
#   scale_x_continuous(breaks = seq(-68.00,-64.00, 0.2))+
#   geom_text(aes(label = LANDING_LOCATION_COUNTY), size = 2)


# Preprocesamiento --------------------------------------------------------
# Validadas las regiones, se procede a estimar tendencias totales anuales por municipio
capturas <- capturas_raw %>% 
              select(-c(MONTH_LANDED, 
                        FISHING_CENTER_ED,
                        FISHING_CENTER_NAME,
                        SPECIES_ITIS,
                        FIN_GEAR_CODE,
                        NMFS_SPECIES_ED,
                        POUNDS_LANDED,
                        confidential)) %>% 
              group_by(YEAR_LANDED, LANDING_LOCATION_COUNTY, FIN_GEAR_NAME, ITIS_COMMON_NAME) %>% 
              summarise(TOTAL = sum(EXPANDED_POUNDS)) %>% 
              pivot_wider(names_from = ITIS_COMMON_NAME, 
                        values_from = TOTAL,
                        values_fill = 0) %>% 
              rename(YEAR = YEAR_LANDED,
                     LOCATION = LANDING_LOCATION_COUNTY,
                     GEAR = FIN_GEAR_NAME) %>% 
              left_join(PR_mun) %>% # incorporar REGION a la base de datos
              relocate(LLC_abrev:lon, .after = LOCATION) %>% 
              relocate(REGION, .before = LOCATION) %>% 
              mutate("YEAR_REGION" = str_c(REGION, YEAR, sep = "-")) %>% #crear factor REGION-YEAR
              relocate('YEAR_REGION', .before = LOCATION)

rm(capturas_raw)
rm(coord_raw)
rm(PR_mun)
rm(mun_cost)

levels(factor(capturas$GEAR))

# Analisis de esfuerzo por GEAR/REGION
t1 <- capturas %>% 
  tabyl(GEAR, REGION) %>% 
  adorn_totals("row") %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>% 
  mutate("analisis" = c(
           "INCLUIR",
         "INCLUIR",
         "EXCLUIR", #remover combines gear del analisis
         "INCLUIR",
         "INCLUIR",
         "INCLUIR",
         "INCLUIR",
         "INCLUIR",
         "INCLUIR",
         "INCLUIR",
         "INCLUIR",
         "INCLUIR",
         "INCLUIR",
         "INCLUIR",
         "INCLUIR",
         "Total"
         )) %>% 
  arrange(desc(analisis)) %>% 
  mutate("GEAR2" = c(
    "Total",
    "BHD",
    "NETS",
    "NETS",
    "HL",
    "NETS",
    "HL",
    "HL",
    "LL",
    "PT",
    "PT",
    "PT",
    "BHD",
    "NETS",
    "TL",
    "CG"
  ))%>% #agrupar GEAR originales en cuatro grupos
  relocate(GEAR2, .after = 'GEAR/REGION') %>% 
  relocate(analisis, .after = GEAR2)

#%>% 
#  knitr::kable()

# procedimiento rápido para asignar grupos
gear <- t1$`GEAR/REGION`
gear2 <- t1$GEAR2

tibble(gear = gear, gear2 = gear2)%>% 
    knitr::kable()

capturas %<>% 
  mutate(GEAR2 = str_replace_all(GEAR, c(
  'BY HAND, DIVING GEAR'          = 'BHD',   
  'CAST NETS'                   =  'NETS',  
  'GILL NETS, OTHER'            =  'NETS' , 
  'HAND LINE'                   =  'HL',    
  'HAUL SEINES'                 =  'NETS',  
  'HOOK AND LINE'               =  'HL',   
  'HOOK AND LINE, BOTTOM'       =  'HL',    
  'LONG LINES, BOTTOM'          =  'LL',    
  'POTS AND TRAPS'              =  'PT',    
  'POTS AND TRAPS, FISH'        =  'PT',    
  'POTS AND TRAPS, SPINY LOBSTER' = 'PT',    
  'SPEARS'                      =  'BHD',   
  'TRAMMEL NETS'                 = 'NETS',  
  'TROLL LINES'                 = 'TL',  
  'COMBINED GEARS'               = 'CG'))) %>% 
  relocate(GEAR2, .after = GEAR)

capturas %<>% 
  mutate(GEAR2 = str_replace_all(GEAR2, c(
        'HL, BOTTOM' = 'HL',
        'PT, FISH' = 'PT',
        'PT, SPINY LOBSTER' = 'PT')))


# Análisis de captura -----------------------------------------------------

PR2022_dat <- as.data.frame(capturas[,c(10:132)])
PR2022_fac <- as.data.frame(capturas[,c(1:9)])

#variables para selecciones
todas_spp <- colnames(PR2022_dat)
todos_factores <- colnames(PR2022_fac)

#Datos por arte de pesca
HL <- PR2022_dat %>% 
  bind_cols(PR2022_fac) %>% 
  filter(GEAR2 == "HL")

LL <- PR2022_dat %>% 
  bind_cols(PR2022_fac) %>% 
  filter(GEAR2 == "LL")

PT <- PR2022_dat %>% 
  bind_cols(PR2022_fac) %>% 
  filter(GEAR2 == "PT")

BHD <- PR2022_dat %>% 
  bind_cols(PR2022_fac) %>% 
  filter(GEAR2 == "BHD")

NETS <- PR2022_dat %>% 
  bind_cols(PR2022_fac) %>% 
  filter(GEAR2 == "NETS")

TL <- PR2022_dat %>% 
  bind_cols(PR2022_fac) %>% 
  filter(GEAR2 == "TL")

PR2022_fac %<>% 
  mutate('YEAR_REGION_GEAR' = str_c(REGION, YEAR, GEAR2, sep = "-"))

PR2022_dat_std <- decostand(PR2022_dat, method = "total") *100

dat_primer(PR2022_dat, PR2022_fac,
           factor.esp = "REGION-YEAR-GEAR",
           directorio = "LANDINGS/temporal_region_gear/",
           tipo = "landings",
           fuente = "UPR")

mds(bio = sqrt(PR2022_dat_std),
    fac = PR2022_fac,
    factor.esp = "REGION-YEAR-GEAR",
    directorio = "LANDINGS/temporal_region_gear/",
    tipo = "MDS_std_sqrt_LANDINGS_PR2022",
    fuente = "UPR")

pco(bio = sqrt(PR2022_dat_std),
    fac = PR2022_fac,
    factor.esp = "REGION-YEAR-GEAR",
    directorio = "LANDINGS/temporal_region_gear/pco/",
    tipo = "PCO_std_sqrt_LANDINGS_PR2022",
    fuente = "UPR")

shadeplot(bio = sqrt(PR2022_dat_std),
          fac = PR2022_fac,
          factor.esp = "REGION-YEAR-GEAR",
          directorio = "LANDINGS/temporal_region_gear/shadeplot/",
          tipo = "shadeplot_std_sqrt_LANDINGS_PR2022",
          fuente = "UPR",
          leyenda = "Square root of Landings (%)",
          important.spp = 30)

# Análisis para HL --------------------------------------------------------
HL_dat <- HL %>% 
  select(all_of(todas_spp))

HL_fac <- HL %>% 
  select(all_of((todos_factores)))

HL_dat_std <- decostand(HL_dat, method = "total") *100

dat_primer(HL_dat, HL_fac,
           factor.esp = "REGION-YEAR",
           directorio = "LANDINGS/temporal_region/HL/",
           tipo = "landings",
           fuente = "UPR")

mds(bio = sqrt(HL_dat_std),
    fac = HL_fac,
    factor.esp = "REGION-YEAR",
    directorio = "LANDINGS/temporal_region/HL/",
    tipo = "MDS_std_sqrt_LANDINGS_HL",
    fuente = "UPR")

pco(bio = sqrt(HL_dat_std),
    fac = HL_fac,
    factor.esp = "REGION-YEAR",
    directorio = "LANDINGS/temporal_region/HL/pco/",
    tipo = "PCO_std_sqrt_LANDINGS_HL",
    fuente = "UPR")

shadeplot(bio = sqrt(HL_dat_std),
          fac = HL_fac,
          factor.esp = "REGION-YEAR",
          directorio = "LANDINGS/temporal_region/HL/shadeplot/",
          tipo = "shadeplot_std_sqrt_LANDINGS_HL",
          fuente = "UPR",
          leyenda = "Square root of Landings (%)",
          important.spp = 30)

# Análisis para LL --------------------------------------------------------
LL_dat <- LL %>% 
  select(all_of(todas_spp))

LL_fac <- LL %>% 
  select(all_of((todos_factores)))

LL_dat_std <- decostand(LL_dat, method = "total") *100

dat_primer(LL_dat, LL_fac,
           factor.esp = "REGION-YEAR",
           directorio = "LANDINGS/temporal_region/LL/",
           tipo = "landings",
           fuente = "UPR")

mds(bio = sqrt(LL_dat_std),
    fac = LL_fac,
    factor.esp = "REGION-YEAR",
    directorio = "LANDINGS/temporal_region/LL/",
    tipo = "MDS_std_sqrt_LANDINGS_LL",
    fuente = "UPR")

pco(bio = sqrt(LL_dat_std),
    fac = LL_fac,
    factor.esp = "REGION-YEAR",
    directorio = "LANDINGS/temporal_region/LL/pco/",
    tipo = "PCO_std_sqrt_LANDINGS_LL",
    fuente = "UPR")

shadeplot(bio = sqrt(LL_dat_std),
          fac = LL_fac,
          factor.esp = "REGION-YEAR",
          directorio = "LANDINGS/temporal_region/LL/shadeplot/",
          tipo = "shadeplot_sqrt_LANDINGS_LL",
          fuente = "UPR",
          leyenda = "Square root of Landings (%)",
          important.spp = 30)

# Análisis para PT --------------------------------------------------------
PT_dat <- PT %>% 
  select(all_of(todas_spp))

PT_fac <- PT %>% 
  select(all_of((todos_factores)))

PT_dat_std <- decostand(PT_dat, method = "total") *100

dat_primer(PT_dat, PT_fac,
           factor.esp = "REGION-YEAR",
           directorio = "LANDINGS/temporal_region/PT/",
           tipo = "landings",
           fuente = "UPR")

mds(bio = sqrt(PT_dat_std),
    fac = PT_fac,
    factor.esp = "REGION-YEAR",
    directorio = "LANDINGS/temporal_region/PT/",
    tipo = "MDS_std_sqrt_LANDINGS_PT",
    fuente = "UPR")

pco(bio = sqrt(PT_dat_std),
    fac = PT_fac,
    factor.esp = "REGION-YEAR",
    directorio = "LANDINGS/temporal_region/PT/pco/",
    tipo = "PCO_std_sqrt_LANDINGS_PT",
    fuente = "UPR")

shadeplot(bio = sqrt(PT_dat_std),
          fac = PT_fac,
          factor.esp = "REGION-YEAR",
          directorio = "LANDINGS/temporal_region/PT/shadeplot/",
          tipo = "shadeplot_sqrt_LANDINGS_PT",
          fuente = "UPR",
          leyenda = "Square root of Landings (%)",
          important.spp = 30)

# Análisis para NETS --------------------------------------------------------
NETS_dat <- NETS %>% 
  select(all_of(todas_spp))

NETS_fac <- NETS %>% 
  select(all_of((todos_factores)))

NETS_dat_std <- decostand(NETS_dat, method = "total") *100

dat_primer(NETS_dat, NETS_fac,
           factor.esp = "REGION-YEAR",
           directorio = "LANDINGS/temporal_region/NETS/",
           tipo = "landings",
           fuente = "UPR")

mds(bio = sqrt(NETS_dat_std),
    fac = NETS_fac,
    factor.esp = "REGION-YEAR",
    directorio = "LANDINGS/temporal_region/NETS/",
    tipo = "MDS_std_sqrt_LANDINGS_NETS",
    fuente = "UPR")

pco(bio = sqrt(NETS_dat_std),
    fac = NETS_fac,
    factor.esp = "REGION-YEAR",
    directorio = "LANDINGS/temporal_region/NETS/pco/",
    tipo = "PCO_std_sqrt_LANDINGS_NETS",
    fuente = "UPR")

shadeplot(bio = sqrt(NETS_dat_std),
          fac = NETS_fac,
          factor.esp = "REGION-YEAR",
          directorio = "LANDINGS/temporal_region/NETS/shadeplot/",
          tipo = "shadeplot_sqrt_LANDINGS_NETS",
          fuente = "UPR",
          leyenda = "Square root of Landings (%)",
          important.spp = 30)

# Análisis para BHD --------------------------------------------------------
BHD_dat <- BHD %>% 
  select(all_of(todas_spp))

BHD_fac <- BHD %>% 
  select(all_of((todos_factores)))

BHD_dat_std <- decostand(BHD_dat, method = "total")*100 

dat_primer(BHD_dat, BHD_fac,
           factor.esp = "REGION-YEAR",
           directorio = "LANDINGS/temporal_region/BHD/",
           tipo = "landings",
           fuente = "UPR")

mds(bio = sqrt(BHD_dat_std),
    fac = BHD_fac,
    factor.esp = "REGION-YEAR",
    directorio = "LANDINGS/temporal_region/BHD/",
    tipo = "MDS_std_sqrt_LANDINGS_BHD",
    fuente = "UPR")

pco(bio = sqrt(BHD_dat_std),
    fac = BHD_fac,
    factor.esp = "REGION-YEAR",
    directorio = "LANDINGS/temporal_region/BHD/pco/",
    tipo = "PCO_std_sqrt_LANDINGS_BHD",
    fuente = "UPR")

shadeplot(bio = sqrt(BHD_dat_std),
          fac = BHD_fac,
          factor.esp = "REGION-YEAR",
          directorio = "LANDINGS/temporal_region/BHD/shadeplot/",
          tipo = "shadeplot_sqrt_LANDINGS_BHD",
          fuente = "UPR",
          leyenda = "Square root of Landings (%)",
          important.spp = 30)

# Análisis para TL --------------------------------------------------------
TL_dat <- TL %>% 
  select(all_of(todas_spp))

TL_fac <- TL %>% 
  select(all_of((todos_factores)))

TL_dat_std <- decostand(TL_dat, method = "total")*100 

dat_primer(TL_dat, TL_fac,
           factor.esp = "REGION-YEAR",
           directorio = "LANDINGS/temporal_region/TL/",
           tipo = "landings",
           fuente = "UPR")

mds(bio = sqrt(TL_dat_std),
    fac = TL_fac,
    factor.esp = "REGION-YEAR",
    directorio = "LANDINGS/temporal_region/TL/",
    tipo = "MDS_std_sqrt_LANDINGS_TL",
    fuente = "UPR")

pco(bio = sqrt(TL_dat_std),
    fac = TL_fac,
    factor.esp = "REGION-YEAR",
    directorio = "LANDINGS/temporal_region/TL/pco/",
    tipo = "PCO_std_sqrt_LANDINGS_TL",
    fuente = "UPR")

shadeplot(bio = sqrt(TL_dat_std),
          fac = TL_fac,
          factor.esp = "REGION-YEAR",
          directorio = "LANDINGS/temporal_region/TL/shadeplot/",
          tipo = "shadeplot_sqrt_LANDINGS_TL",
          fuente = "UPR",
          leyenda = "Square root of Landings (%)",
          important.spp = 30)
