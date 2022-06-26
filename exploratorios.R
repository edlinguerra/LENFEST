library(readxl)
library(tidyverse)
library(stringr)
library(vegan)
library(ape)


# TEMPORAL CHANGES OF SITES BY LOCATIONS ----------------------------------

# Benthic (1999-2019) ----------------------------------------------------
d_bentos <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                       sheet = "Benthic (1999-2019)")

## Extraer datos, factores e indicadores
bentos <- datos(d_bentos)
fac_bentos <- factores(d_bentos)

mds(bio = sqrt(bentos[,-c(1:6)]),
    fac = fac_bentos,
    factor.esp = "LOCATION",
    directorio = "MDS/localities/benthic/",
    tipo = "sqrt_MDS_benthic",
    fuente = "PRCRMP")

pco(bio = sqrt(bentos[,-c(1:6)]),
    fac = fac_bentos,
    factor.esp = "LOCATION",
    directorio = "MDS/localities/benthic/pco/",
    tipo = "sqrt_PCO_benthic",
    fuente = "PRCRMP")

shadeplot(bio = sqrt(bentos[,-c(1:6)]),
    fac = fac_bentos,
    factor.esp = "LOCATION",
    directorio = "MDS/localities/benthic/shadeplot/",
    tipo = "sqrt_shadeplot_benthic",
    important.spp = 50)

# Fish-Invert Ab. (1999-2019) ---------------------------------------------
d_f_i <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                    sheet = "Fish-Invert Ab. (1999-2019)")


## Extraer datos, factores e indicadores
f_i <- datos(d_f_i)
fac_f_i <- factores(d_f_i)

mds(bio = sqrt(f_i),
    fac = fac_f_i,
    factor.esp = "LOCATION",
    directorio = "MDS/localities/fish_inv/",
    tipo = "fish_inv",
    fuente = "PRCRMP")

pco(bio = sqrt(f_i),
    fac = fac_f_i,
    factor.esp = "LOCATION",
    directorio = "MDS/localities/fish_inv/pco/",
    tipo = "sqrt_PCO_fish_inv",
    fuente = "PRCRMP")

shadeplot(bio = sqrt(f_i),
          fac = fac_f_i,
          factor.esp = "LOCATION",
          directorio = "MDS/localities/fish_inv/shadeplot/",
          tipo = "sqrt_shadeplot_fish_inv",
          important.spp = 50)

# Fish Biomass (2004-2019) ------------------------------------------------
d_fbio <- read_excel("datos_originales/PRCRMP Database Compilation (2-1-2021).xlsx",
                     sheet = "Fish Biomass (2004-2019)")
## Extraer datos, factores e indicadores
fish_biom <- datos(d_fbio)
fac_fish_biom <- factores(d_fbio)

mds(bio = log(fish_biom+1),
    fac = fac_fish_biom,
    factor.esp = "LOCATION",
    directorio = "MDS/localities/fish_biomass/",
    tipo = "log_fish_biomass",
    fuente = "PRCRMP")

pco(bio = log(fish_biom+1),
    fac = fac_fish_biom,
    factor.esp = "LOCATION",
    directorio = "MDS/localities/fish_biomass/pco/",
    tipo = "log_PCO_fish_biomass",
    fuente = "PRCRMP")

shadeplot(bio = log(fish_biom+1),
          fac = fac_fish_biom,
          factor.esp = "LOCATION",
          directorio = "MDS/localities/fish_biomass/shadeplot/",
          tipo = "log_shadeplot_fish_biomass",
          leyenda = "Fish biomass (log transformed)",
          important.spp = 25)


# TEMPORAL CHANGES OF LOCATIONS BY REGION---------------------------------------
# Benthic (1999-2019) -----------------------------------------------------------------

mds(bio = sqrt(bentos[,-c(1:6)]),
    fac = fac_bentos,
    factor.esp = "REGION",
    directorio = "MDS/regions/benthic/",
    tipo = "sqrt_benthic",
    fuente = "PRCRMP")

pco(bio = sqrt(bentos[,-c(1:6)]),
    fac = fac_bentos,
    factor.esp = "REGION",
    directorio = "MDS/regions/benthic/pco/",
    tipo = "sqrt_benthic",
    fuente = "PRCRMP")

shadeplot(bio = sqrt(bentos[,-c(1:6)]),
          fac = fac_bentos,
          factor.esp = "REGION",
          directorio = "MDS/regions/benthic/shadeplot/",
          tipo = "sqrt_benthic",
          leyenda = "Species abundance (sqrt)",
          important.spp = 50)
# Fish-Invert Ab. (1999-2019) ---------------------------------------------
mds(bio = sqrt(f_i),
    fac = fac_f_i,
    factor.esp = "REGION",
    directorio = "MDS/regions/fish_inv/",
    tipo = "fish_inv",
    fuente = "PRCRMP")

pco(bio = sqrt(f_i),
    fac = fac_f_i,
    factor.esp = "REGION",
    directorio = "MDS/regions/fish_inv/pco/",
    tipo = "sqrt_fish_inv",
    fuente = "PRCRMP")

shadeplot(bio = sqrt(f_i),
          fac = fac_f_i,
          factor.esp = "REGION",
          directorio = "MDS/regions/fish_inv/shadeplot/",
          tipo = "sqrt_fish_inv",
          leyenda = "Species abundance (sqrt)",
          important.spp = 50)

# Fish Biomass (2004-2019) ------------------------------------------------
mds(bio = log(fish_biom+1),
    fac = fac_fish_biom,
    factor.esp = "REGION",
    directorio = "MDS/regions/fish_biom/",
    tipo = "log_fish_biomass",
    fuente = "PRCRMP")

pco(bio = log(fish_biom+1),
    fac = fac_fish_biom,
    factor.esp = "REGION",
    directorio = "MDS/regions/fish_biom/pco/",
    tipo = "log_PCO_fish_biomass",
    fuente = "PRCRMP")

shadeplot(bio = log(fish_biom+1),
          fac = fac_fish_biom,
          factor.esp = "REGION",
          directorio = "MDS/regions/fish_biom/shadeplot/",
          tipo = "log_fish_biomass",
          leyenda = "Fish biomass (log transformed)",
          important.spp = 25)


# TEMPORAL CHANGES OF REGIONS -----------------------------------------------------------------
# Benthic -----------------------------------------------------------------
mds(bio = sqrt(bentos[,-c(1:6)]),
    fac = fac_bentos,
    factor.esp = "REGION-YEAR",
    directorio = "MDS/temporal_region/benthic/",
    tipo = "sqrt_benthic",
    fuente = "PRCRMP")

pco(bio = sqrt(bentos[,-c(1:6)]),
    fac = fac_bentos,
    factor.esp = "REGION-YEAR",
    directorio = "MDS/temporal_region/benthic/pco/",
    tipo = "sqrt_PCO_benthic",
    fuente = "PRCRMP")

shadeplot(bio = sqrt(bentos[,-c(1:6)]),
          fac = fac_bentos,
          factor.esp = "REGION-YEAR",
          directorio = "MDS/temporal_region/benthic/shadeplot/",
          tipo = "sqrt_shadeplot_benthic",
          important.spp = 50)

# Fish-Invert -------------------------------------------------------------
mds(bio = sqrt(f_i),
    fac = fac_f_i,
    factor.esp = "REGION-YEAR",
    directorio = "MDS/temporal_region/fish_inv/",
    tipo = "sqrt_fish_inv",
    fuente = "PRCRMP")

pco(bio = sqrt(f_i),
    fac = fac_f_i,
    factor.esp = "REGION-YEAR",
    directorio = "MDS/temporal_region/fish_inv/pco/",
    tipo = "sqrt_fish_inv",
    fuente = "PRCRMP")

shadeplot(bio = sqrt(f_i),
          fac = fac_f_i,
          factor.esp = "REGION-YEAR",
          directorio = "MDS/temporal_region/fish_inv/shadeplot/",
          tipo = "sqrt_fish_inv",
          leyenda = "Species abundance (sqrt)",
          important.spp = 50)

# Fish Biomass (2004-2019) ------------------------------------------------
mds(bio = log(fish_biom+1),
    fac = fac_fish_biom,
    factor.esp = "REGION-YEAR",
    directorio = "MDS/temporal_region/fish_biom/",
    tipo = "log_fish_biomass",
    fuente = "PRCRMP")

pco(bio = log(fish_biom+1),
    fac = fac_fish_biom,
    factor.esp = "REGION-YEAR",
    directorio = "MDS/temporal_region/fish_biom/pco/",
    tipo = "log_fish_biomass_PCO",
    fuente = "PRCRMP")

shadeplot(bio = log(fish_biom+1),
          fac = fac_fish_biom,
          factor.esp = "REGION-YEAR",
          directorio = "MDS/temporal_region/fish_biom/shadeplot/",
          tipo = "log_fish_biomass",
          leyenda = "Fish biomass (log transformed)",
          important.spp = 50)

# SEAMAP ------------------------------------------------------------------
SEAMAP_PR2022 <- read_excel("datos_originales/SEAMAP_PR2022.xlsx")

PR2022_dat <- datos(SEAMAP_PR2022)

PR2022_fac <- factores(SEAMAP_PR2022)
