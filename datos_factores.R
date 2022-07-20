
#archivos, tipos de datos y factores

PRCRMP_benthic <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                       sheet = "Benthic (1999-2019)")
PRCRMP_benthic_fac <- factores(PRCRMP_benthic)

PRCRMP_fish_inv <- d_f_i <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                                       sheet = "Fish-Invert Ab. (1999-2019)")
PRCRMP_fish_inv_fac <- factores(PRCRMP_fish_inv)

PRCRMP_fish_biomass <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                                  sheet = "Fish Biomass (2004-2019)")
PRCRMP_fish_biomass_fac <- factores(PRCRMP_fish_biomass)

SEAMAP_PR2022 <- read_excel("datos_originales/SEAMAP_PR2022.xlsx")

PR2022_fac <- factores(SEAMAP_PR2022)

rm(d_f_i)
rm(PRCRMP_benthic)
rm(PRCRMP_fish_biomass)
rm(PRCRMP_fish_inv)


