library(dplyr)
library(tidycensus)

#Modifications to the data to make it compatible with the map

states_FI <- read.csv("FI_State.csv")
counties_FI <- read.csv("FI_County.csv")

colnames(states_FI)[2] <- "state"
colnames(counties_FI)[3] <- "name"
counties_FI$fip <- 0

for(i in 1:length(counties_FI$name)){
  x <- counties_FI$name[i]
  fip <- 0
  if(length(x) > 0){
    county <- strsplit(x,",")[[1]][1]
    state <- trimws(strsplit(x,",")[[1]][2])
    r <- which(fips_codes$state_name == state & fips_codes$county == county)
    fip <- paste(fips_codes[r,2],fips_codes[r,4], sep = "")
  }
  if(length(fip) > 0){
    counties_FI$fip[i] <- fip 
  }
}

  for(x in 2009:2022){
    colnames(states_FI)[x-2006] <- as.character(x)
    colnames(counties_FI)[x-2005] <- as.character(x)
  }
  #exceptions <- c(1939,3144,3145,3146,3151,3152,3153,3154,3155,3156,3157,3158,3159,3160)
  # combine 1939, 3144, 3160 they are all Dona Ana County fips code: 35013, combine data
  # 3145 is LaSelle Parish, fips code: 22059
  #3146 petersburg bourough doesn't exist anymore, it was assimilated into 
  #Petersburg Census Area,  combine data from the rows into Census Area
  #Connecticut changed their system in 2022, applies to these rows:
  #3151 3152 3153 3154 3155 3156 3157 3158 3159 3160
  #09110 09120 09130...
  #will try adding fips manually but might not load into USMaps
  
  #LaSelle Parish
  counties_FI$fip[3145] <- "22059"
  
  
  drop <- c("X","State", "name")
  da1 <- counties_FI[1939,] %>% select_if(~ !any(is.na(.)))
  da1 <- da1[,!names(da1) %in% drop]
  da2 <- counties_FI[3144,] %>% select_if(~ !any(is.na(.)))
  da2 <- da2[,!names(da2) %in% drop]
  da3 <- counties_FI[3160,] %>% select_if(~ !any(is.na(.)))
  da3 <- da3[,!names(da3) %in% drop]
  da12 <- merge(da1,da2,by = "fip")
  da_merged <- merge(da12,da3,by = "fip")
  da_merged$fip <- "35013"
  da_merged$X <- 3160
  da_merged$State <- "NM"
  da_merged$name <- "Dona Ana County, New Mexico"
  da_merged <- da_merged[,c(16,18,17,2,3,5,4,6,7,8,9,10,11,13,14,15,12,1)]
  counties_FI <- counties_FI[-c(1939,3144,3160),]
  counties_FI <- rbind(counties_FI,da_merged)
  
  pb1 <- counties_FI[20,] %>% select_if(~ !any(is.na(.)))
  pb2 <- counties_FI[3144,] %>% select_if(~ !any(is.na(.)))
  pb_merged <- merge(pb1,pb2,by="State")
  pb_merged <- pb_merged[,-c(9,10,20)]
  colnames(pb_merged)[2] <- "X"
  colnames(pb_merged)[3] <- "name"
  colnames(pb_merged)[8] <- "fip"
  pb_merged$"2009" <- NA
  pb_merged <- pb_merged[,c(2,1,3,18,4:7,9:17,8)]
  counties_FI[20,] <- pb_merged
  counties_FI <- counties_FI[-c(3144),]
  
  for(x in 1:9){
    counties_FI[3147+x,]$fip <- paste("091",as.character(x*10),sep="")
  }
colnames(counties_FI)[18] <- "fips"
states_FI <- states_FI[,-c(1)]
counties_FI <- counties_FI[,-c(1)]
write.csv(states_FI, "states_FI_graph.csv")
write.csv(counties_FI, "counties_FI_graph.csv")