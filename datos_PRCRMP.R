# TEMPORAL CHANGES OF REGIONS -----------------------------------------------------------------
# Benthic -----------------------------------------------------------------
d_bentos <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                       sheet = "Benthic (1999-2019)")

## Extraer datos, factores e indicadores
bentos <- datos(d_bentos)
fac_bentos <- factores(d_bentos)
ind_bentos <- indicadores(d_bentos)

levels(factor(fac_bentos$REGION))

fac_bentos %<>% 
  mutate(REGION = str_replace_all(REGION, c("Mona/Desecheo" = "West",
                                            "Vieques/Culebra" = "East",
                                            "Southeast" = "South",
                                            "Southwest" ="South"
  ))) %>%
  mutate(REGION = ifelse(LOCATION == "Fajardo", "East",
                         ifelse(LOCATION == "Vega Baja", "North",
                                ifelse(LOCATION == "Carolina", "North",
                                       ifelse(LOCATION == "San Juan", "North", REGION))))) %>%  
  mutate(YEAR_REGION = str_c(YEAR_REGION = REGION, YEAR, sep = "-"))

levels(factor(fac_bentos$REGION))
levels(factor(fac_bentos$YEAR_REGION))

dat_primer(bio = bentos,
           fac = fac_bentos,
           directorio = "PRCRMP/temporal_region/benthic/",
           factor.esp = "REGION-YEAR",
           tipo = "benthic",
           fuente = "PRCRMP")

mds(bio = sqrt(bentos[,-c(1:6)]),
    fac = fac_bentos,
    factor.esp = "REGION-YEAR",
    directorio = "PRCRMP/temporal_region/benthic/",
    tipo = "sqrt_benthic",
    fuente = "PRCRMP")

pco(bio = sqrt(bentos[,-c(1:6)]),
    fac = fac_bentos,
    factor.esp = "REGION-YEAR",
    directorio = "PRCRMP/temporal_region/benthic/pco/",
    tipo = "sqrt_PCO_benthic",
    fuente = "PRCRMP")

shadeplot(bio = sqrt(bentos[,-c(1:6)]),
          fac = fac_bentos,
          factor.esp = "REGION-YEAR",
          directorio = "PRCRMP/temporal_region/benthic/shadeplot/",
          tipo = "sqrt_shadeplot_benthic",
          important.spp = 50)

# Fish-Invert -------------------------------------------------------------
d_f_i <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                    sheet = "Fish-Invert Ab. (1999-2019)")


## Extraer datos, factores e indicadores
f_i <- datos(d_f_i)
fac_f_i <- factores(d_f_i)

levels(factor(fac_f_i$REGION))

fac_f_i %<>% 
  mutate(REGION = str_replace_all(REGION, c("Mona/Desecheo" = "West",
                                            "Vieques/Culebra" = "East",
                                            "Southeast" = "South",
                                            "Southwest" ="South"
  ))) %>%
  mutate(REGION = ifelse(LOCATION == "Fajardo", "East",
                         ifelse(LOCATION == "Vega Baja", "North",
                                ifelse(LOCATION == "Carolina", "North",
                                       ifelse(LOCATION == "San Juan", "North", REGION))))) %>%  
  mutate(YEAR_REGION = str_c(YEAR_REGION = REGION, YEAR, sep = "-"))

levels(factor(fac_f_i$REGION))
levels(factor(fac_f_i$YEAR_REGION))

dat_primer(bio = f_i,
           fac = fac_f_i,
           directorio = "PRCRMP/temporal_region/fish_inv/",
           factor.esp = "REGION-YEAR",
           tipo = "fish_inv",
           fuente = "PRCRMP")


mds(bio = sqrt(f_i),
    fac = fac_f_i,
    factor.esp = "REGION-YEAR",
    directorio = "PRCRMP/temporal_region/fish_inv/",
    tipo = "sqrt_fish_inv",
    fuente = "PRCRMP")

pco(bio = sqrt(f_i),
    fac = fac_f_i,
    factor.esp = "REGION-YEAR",
    directorio = "PRCRMP/temporal_region/fish_inv/pco/",
    tipo = "sqrt_fish_inv",
    fuente = "PRCRMP")

shadeplot(bio = sqrt(f_i),
          fac = fac_f_i,
          factor.esp = "REGION-YEAR",
          directorio = "PRCRMP/temporal_region/fish_inv/shadeplot/",
          tipo = "sqrt_fish_inv",
          leyenda = "Species abundance (sqrt)",
          important.spp = 50)

# Fish Biomass (2004-2019) ------------------------------------------------
d_fbio <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                     sheet = "Fish Biomass (2004-2019)")
## Extraer datos, factores e indicadores
fish_biom <- datos(d_fbio)
fac_fish_biom <- factores(d_fbio)

levels(factor(fac_fish_biom$REGION))

fac_fish_biom %<>% 
  mutate(REGION = str_replace_all(REGION, c("Mona/Desecheo" = "West",
                                            "Vieques/Culebra" = "East",
                                            "Southeast" = "South",
                                            "Southwest" ="South"
  ))) %>%
  mutate(REGION = ifelse(LOCATION == "Fajardo", "East",
                         ifelse(LOCATION == "Vega Baja", "North",
                                ifelse(LOCATION == "Carolina", "North",
                                       ifelse(LOCATION == "San Juan", "North", REGION))))) %>%  
  mutate(YEAR_REGION = str_c(YEAR_REGION = REGION, YEAR, sep = "-"))

levels(factor(fac_fish_biom$REGION))
levels(factor(fac_fish_biom$YEAR_REGION))

dat_primer(bio = fish_biom,
           fac = fac_fish_biom,
           directorio = "PRCRMP/temporal_region/fish_biom/",
           factor.esp = "REGION-YEAR",
           tipo = "log_fish_biomass",
           fuente = "PRCRMP")

mds(bio = log(fish_biom+1),
    fac = fac_fish_biom,
    factor.esp = "REGION-YEAR",
    directorio = "PRCRMP/temporal_region/fish_biom/",
    tipo = "log_fish_biomass",
    fuente = "PRCRMP")

pco(bio = log(fish_biom+1),
    fac = fac_fish_biom,
    factor.esp = "REGION-YEAR",
    directorio = "PRCRMP/temporal_region/fish_biom/pco/",
    tipo = "log_fish_biomass_PCO",
    fuente = "PRCRMP")

shadeplot(bio = log(fish_biom+1),
          fac = fac_fish_biom,
          factor.esp = "REGION-YEAR",
          directorio = "PRCRMP/temporal_region/fish_biom/shadeplot/",
          tipo = "log_fish_biomass",
          leyenda = "Fish biomass (log transformed)",
          important.spp = 50)

# Combinar datos

archivos <- list.files("PRCRMP/temporal_region/benthic/data",
                       pattern = "*.xlsx",
                       full.names = TRUE) %>% 
                  lapply(read_xlsx) %>% 
                  bind_rows() %>% 
                  relocate('Rugosity (m)':'Gaps/Holes', .after = YEAR_REGION) %>% 
                  rename(" " = "...242")

write.xlsx(archivos, "PRCRMP/temporal_region/benthic/data/benthic_all_region.xlsx")
write.xlsx(ind_bentos, "PRCRMP/temporal_region/benthic/data/benthic_indicators.xlsx")

## Modelación temporal

ind_b <- ind_bentos %>% 
  select(`SAMPLE CODE`:`Benthic Indicator`) %>% 
  rename("species" = `SAMPLE CODE`,
         "indicator" = `Benthic Indicator`)

fac_b <- fac_bentos %>% 
  select(YEAR, REGION, YEAR_REGION)


dat_agg <- bentos %>% 
  bind_cols(fac_b) %>% 
  pivot_longer(cols = colnames(bentos), names_to = "species", values_to = "cover") %>% 
  left_join(ind_b) %>% 
  group_by(REGION, YEAR, indicator) %>% 
  summarise(cover = sum(cover)) %>%
  ungroup() %>% 
  filter(indicator != 'Abiotic', indicator != 'Rugosity')

  ggplot(dat_agg, aes(x = as.numeric(YEAR), y = cover)) + 
  geom_point(aes(colour = REGION))+
  geom_smooth(method = "loess", aes(colour = REGION), se = FALSE)+
  facet_wrap(~indicator, scales = "free_y")+
  theme_bw()

REGION_l <- gl (n = 4, k = 17, labels = levels(factor(fac_bentos$REGION))) 
YEAR_l <- gl(n = 17, k = 1, labels = c(2003:2019))
BENTHIC_l <- gl(n= 10, k = 68, labels = levels(factor(dat_agg$indicator)))

mod_df <- data.frame(
  YEAR_REGION_B = interaction(REGION_l, YEAR_l, BENTHIC_l, sep = "-"))

mod_df %<>% 
  mutate(YEAR =str_extract(YEAR_REGION_B, "\\d+"),
         REGION = str_to_title(str_extract(str_to_lower(YEAR_REGION_B), "[a-z]+")),
         YEAR_REGION = str_c(REGION, YEAR, sep = "-"),
         indicator = BENTHIC_l) 

mod_df %<>% 
  left_join(dat_agg)

xx <- mod_df %>% 
  nest_by(REGION, indicator) %>%
  mutate(mod = list(loess(cover~YEAR,
                          data = data,
                          na.action = "na.exclude",
                          control = loess.control(surface = "direct"))))

ajuste <- xx %>% 
  summarise(cover2 = predict(mod, data.frame(YEAR = 2003:2019), se = FALSE)) %>% 
  mutate(YEAR = as.character(2003:2019)) %>% 
  right_join(mod_df) %>% 
  relocate(YEAR, .after = REGION) %>% 
  relocate(YEAR_REGION, .after = YEAR) %>% 
  mutate(cover3 = if_else(is.na(cover), cover2, cover)) %>% 
  mutate(cover = if_else(cover3 < 0, 0, cover3)) %>% 
  select(-c(YEAR_REGION_B, cover2, cover3)) %>%
  pivot_wider(names_from = indicator, values_from = cover) %>% 
  mutate(" " = NA) %>% 
  relocate(REGION:YEAR_REGION, .after = " ")

write.xlsx(ajuste, "PRCRMP/temporal_region/benthic/data/benthic_sim.xlsx")  

