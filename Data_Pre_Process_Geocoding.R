# 03/28/2020

# Data Pre Process and Geocoding

library(tidyverse)

master_dta <- read_csv("./CH3_Master_Allvar_3.csv")



addrdta<-read.csv('CH3_Address_rlismerge_nospace.csv')




addr_trim <- addrdta[,c(3:8)]

# Keep 3:12,21,40,72:76

master_dta_trim <-master_dta[,c(3:12,21,40,72:76)]

#master_rlis <- master_dta %>% 
#             inner_join(master_dta,addrdta, by = "ResponseID.x")

ch3mast_addr <- addr_trim %>% 
  left_join(master_dta_trim,addr_trim, by = "ID.x")

#Need to add the new RLIS data because addresses aren't complete

rlis <-read_csv("./rlis_add.csv")

rlis_sub <- rlis %>% 
  select(FULLADD, STATE, ZIP,ADDRESS,MAIL_CITY, X_COORD,Y_COORD,siteaddrno)

rlis_sub<-rename(rlis_sub,siteaddrnospac = siteaddrno)

ch3mast_addr_rlis <- ch3mast_addr %>% 
  inner_join(rlis_sub,ch3mast_addr, by = "siteaddrnospac")







# Ok that did most of it.Remove columns 3:5 as they aren't necessary. Now clean it up the ".x" and ".y"
names(ch3mast_addr_rlis) <- gsub(".y", "", names(ch3mast_addr_rlis))
names(ch3mast_addr_rlis) <- gsub(".x", "", names(ch3mast_addr_rlis))


#### NOW TIME FOR GEOCODING  ##################


#1. Need to subset and concatonate variables to get in right format for geocoding

geo_mast<-ch3mast_addr_rlis [,c(2,23,27,24,25)]


geomast_concat<-unite(geo_mast,concat,FULLADD:ZIP, sep = ",",remove = FALSE,na.rm=FALSE)

# good. Concatonating worked.

#2. Geocode

install.packages("ggmap")
library(ggmap)

#register_google(key = "AIzaSyCyDnvvcCL5Cz2kSuBPzhcBI8zeOAV6a-M")

geomast_addr<-geomast_concat$concat

#geocode

#geomast_addr_coord<-geocode(geomast_addr)

## Something wrong with the API above ##

register_google(key = "AIzaSyD2iwJSuieqGApnIjHQYG2TFyTe76ezMJM")

geomast_addr_coord<-geocode(geomast_addr)

# Ok I think that worked...

# Now bind to original dataframe

water_cust_seg_coord <- data.frame(cbind(ch3mast_addr_rlis, geomast_addr_coord))

# Before I do anything...Do another test to get more variables

# Will want to use master_dta_trim and innr join with rlis sub

#rlis_sub <-rlis_sub %>%
 # mutate(x_coord= X_COORD)

#rlis_sub <-rlis_sub %>%
 # mutate(siteaddr= ADDRESS)

# The one below worked very well. May still be some NA's introduced but that's ok

#ch3mast_addr_rlis_test <- ch3mast_addr %>% 
 # left_join(rlis_sub,ch3mast_addr, by = "siteaddr")

## Now clean up some columns

#names(ch3mast_addr_rlis_test) <- gsub(".y", "", names(ch3mast_addr_rlis_test))
#names(ch3mast_addr_rlis_test) <- gsub(".x", "", names(ch3mast_addr_rlis_test))


# Drop redundant columns: 4,5,8,12,14,19,20,30,31

#ch3mast_addr_rlis_test<-ch3mast_addr_rlis_test[,-c(4,5,8,12,14,19,20,30,31)]

# Concatonate

#ch3mast_addr_rlis_concat<-unite(ch3mast_addr_rlis_test,concat,FULLADD:ZIP, sep = ",",remove = FALSE,na.rm=FALSE)

#water_cust_addr<-ch3mast_addr_rlis_concat$concat

# Geocode

#water_cust_addr_coord<-geocode(water_cust_addr)

#water_cust_seg_coord_FINAL <- data.frame(cbind(ch3mast_addr_rlis_concat,water_cust_addr_coord))

####### I duplicated a process somewhere in here.


water_cust_seg_coord_FINAL<-water_cust_seg_coord



#####------> OK Good. That worked <-----####

## Save as an .RDS from here, come back later and trim out a few variables.

save(water_cust_seg_coord_FINAL, file = "water_cust_seg_coord_FINAL.RData")

## Came back after a few hours to clean up dataset

load("./water_cust_seg_coord_FINAL.RData")

#3,5
water_cust_seg_coord_FINAL<-water_cust_seg_coord_FINAL[,-c(3:9)]

save(water_cust_seg_coord_FINAL, file = "water_cust_seg_coord_FINAL_MASTER.RData")

# Now save out without addresses. Wanted to keep original file for analysis.

watercust_masterdata<-water_cust_seg_coord_FINAL[,-c(16:22)]

save(watercust_masterdata, file = "watercust_masterdata.rda")

