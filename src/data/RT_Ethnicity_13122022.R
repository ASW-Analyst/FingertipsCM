# Ethnicity Census Data 

#install.packages("devtools")
devtools::install_github("PublicHealthEngland/fingertipsR")
library(fingertipsR)
library(tidyverse)
library(leaflet)
library(readxl)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)
library(openxlsx)

rm(list=ls())

###Filter shapes for North West only
NW_LTLA<-read_excel("ref/NW_LTLA_Mapping.xlsx")

## LSOA Shape file
lsoa<- geojsonio::geojson_read(
  x = "geo/Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.geojson",  # the same file_path as above
  what = "sp"  # returns the read with spatial class
)

## Load Ethnicity Data
ethnicity<-read_excel("Ethnicity_LSOA_CM.xlsx",
                      sheet = 2)
imd<-read.xlsx("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx", sheet = 2)
ethnicity<-ethnicity%>%
left_join(imd, c("Lower Layer Super Output Areas Code" = "LSOA.code.(2011)"))

## Join Shape files with data
lsoa_data<-merge(lsoa, ethnicity, by.x = "LSOA11CD", "Lower Layer Super Output Areas Code")

### Filter for North West
lsoa_data_nw<-subset(lsoa_data, Region == "North West")

## Creating Palettes
pal_fac_eth<-colorFactor(palette = "Blues", 
                         levels = 10:1, domain = lsoa_data_nw$`Proportion`)

lat<-53.3
lng<--2.5

lsoa_data_cm<-subset(lsoa_data_nw, System == "Cheshire & Merseyside")

leaflet(lsoa_data_cm)%>%
  addPolygons(color = "black", weight = 2)%>%
  addPolygons(stroke = TRUE, weight = 0.4, opacity = 1, fillOpacity = 0.6,
              color = "grey", 
              fillColor = ~pal_fac_eth(`Proportion`),
              label = paste0(lsoa_data_cm$LSOA11NM,": Decile - ", 
                             lsoa_data_cm$`Proportion`))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = lsoa_data_cm$'Proportion', pal = pal_fac_eth, 
  title = "Proportion of Ethnic Minorities (2021)",position = "topright")%>%
  setView(lng, lat, zoom = 9)

  
  