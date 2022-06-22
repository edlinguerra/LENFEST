library(readxl)
library(tidyverse)
library(stringr)

# Función para leer datos en formato PRIMER para trabajar con vegan
datos <- function(dat){
  total <- which(str_detect(dat$`SAMPLE CODE`, "total", negate = FALSE))
  col <- which(str_detect(dat$`SAMPLE CODE`, "# col.", negate = FALSE))
  
  if(length(total) == 0){
    temp1 <- dat
  } else {
    temp1 <- dat[-c(total,col),]  
  }
  ec <- colSums(is.na(temp1) | temp1 == "") == nrow(temp1)
  nc <- names(temp1[,ec])[1]
  nr <- which(is.na(temp1[,1]), arr.ind=TRUE)[1]
  sample_code <- colnames(temp1)[-1]
  variables <- temp1[-c(nr:nrow(temp1)),1]
  temp2 <- temp1[-c(nr:nrow(temp1)),] %>% 
    select(!c(1,nc:length(temp1))) %>% 
    mutate(across(where(is.character), as.numeric)) %>%
    replace(is.na(.), 0) %>% 
    t() 
  colnames(temp2) <- as.matrix(temp1[-c(nr:nrow(temp1)),1])
  return(temp2)
  }
factores <- function(dat){
  temp1 <- dat
  ec <- colSums(is.na(temp1) | temp1 == "") == nrow(temp1)
  nc <- names(temp1[,ec])[1]
  nr <- which(is.na(temp1[,1]), arr.ind=TRUE)[1]
  sample_code <- temp1 %>% 
    select(-c(1,nc:length(temp1))) %>% 
    colnames()
  variables <- temp1[c(nr:nrow(temp1)),1] %>% 
                drop_na()
  temp2 <- temp1[c(nr+1:nrow(temp1)),] %>% 
    select(!c(1,nc:length(temp1))) %>% 
    drop_na() %>% 
    t() 
  colnames(temp2) <- variables$`SAMPLE CODE`
  temp3 <- cbind(temp2,sample_code) %>% 
           as.data.frame() %>% 
           mutate(YEAR_SITE = str_c(YEAR, `SITE NAME`, sep = "_"),
                  YEAR_LOCATION = str_c(YEAR, LOCATION, sep = "_"),
                  YEAR_REGION = str_c(YEAR, REGION, sep = "_"))
  return(temp3)
}
indicadores <- function(dat){
  total <- which(str_detect(dat$`SAMPLE CODE`, "total", negate = FALSE))
  col <- which(str_detect(dat$`SAMPLE CODE`, "# col.", negate = FALSE))
  
  if(length(total) == 0){
    temp1 <- dat
  } else {
    temp1 <- dat[-c(total,col),]  
  }
  ec <- colSums(is.na(temp1) | temp1 == "") == nrow(temp1)
  nc <- names(temp1[,ec])[1]
  nr <- which(is.na(temp1[,1]), arr.ind=TRUE)[1]
  indicators <- temp1 %>% 
    select(c(1,nc:length(temp1),-nc)) %>% 
    colnames()
  #variables <- temp1[-c(nr:nrow(temp1)),1]
  temp2 <- temp1[-c(nr:nrow(temp1)),] %>% 
    select(indicators) %>% 
    return(as.data.frame(temp2))
}
