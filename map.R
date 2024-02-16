library(mapview)
library(sp)
library(sf)
library(dplyr)
library(arrow)

aed_locations<-read_parquet('Data/aed_locations.parquet.gzip')
belgium.sf<-st_read("4326/postaldistricts.shp")

aedCountByPostalCode<-aed_locations %>% count(postal_code)
aedCountByPostalCode$postal_code<-as.character(aedCountByPostalCode$postal_code)
names(belgium.sf)[names(belgium.sf)=='nouveau_PO']='postal_code'

belgium.sf<-left_join(belgium.sf,aedCountByPostalCode,by='postal_code')
tm_shape(belgium.sf) + tm_polygons(style='fixed',
                                   palette="Greens",
                                   col="n", 
                                   breaks = c(0,1,10,20,100,1000),
                                   border.col="black")
