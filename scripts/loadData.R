library(jsonlite)
library(dplyr)
library(magrittr)


# Reading Latest file as JSON
# crimeDataJSON <- fromJSON("https://data.cityofnewyork.us/resource/qiz3-axqb.json")

# Reading data from past CSV files.
# This data is already in the workspace
# crimeDataCSV <- as.data.frame(read.csv("/Users/mdabas/Mavez_Personal/NYC_Motot_Vehicle_Collision/Data/NYPD_Motor_Vehicle_Collisions.csv",header = TRUE,na.strings = c("","NA")))
crimeDataCSV <- crimeDataCSV %>%
  dplyr::filter(CONTRIBUTING.FACTOR.VEHICLE.1 != "Unspecified")
crimeDataCSV$BOROUGH <- as.character(crimeDataCSV$BOROUGH)
crimeDataCSV$ON.STREET.NAME <- as.character(crimeDataCSV$ON.STREET.NAME)

crimeDataCSV <- crimeDataCSV[order(as.Date(crimeDataCSV$DATE, format="%m/%d/%Y")),]
crimeDataCSV$DATE <- as.Date(crimeDataCSV$DATE,format='%m/%d/%Y')
crimeDataCSV$Year <- substring(crimeDataCSV$DATE,1,4)
attach(crimeDataCSV)

# naCount <- sapply(crimeDataCSV, function(y) sum(length(which(is.na(y)))))
# data.frame(naCount)

# Major contribution factors.
# topTenFactor <- as.data.frame(plyr::count(crimeDataCSV,"CONTRIBUTING.FACTOR.VEHICLE.1"))
# colnames(topTenFactor) <- c("Factor","Freq")
# topTenFactor <- head(topTenFactor[order(-topTenFactor$Freq),],n = 10)


# Major Accident prone street
# topTenStreet <- head(arrange(aggregate(NUMBER.OF.PERSONS.KILLED, by=list(Street=ON.STREET.NAME), FUN=sum),-x),n = 10)

# Major Accident Areas
# topTenAreas <- arrange(aggregate(NUMBER.OF.PERSONS.KILLED, by=list(Area=BOROUGH), FUN=sum),-x)

# Majort resons of accidenet.
# topTenReason <- head(arrange(aggregate(NUMBER.OF.PERSONS.KILLED, by = list(Factor = CONTRIBUTING.FACTOR.VEHICLE.1),FUN=sum),-x),n = 10)

# Unsafe roads for pedestrians.
# topTenVehicleType <- arrange(aggregate(NUMBER.OF.PERSONS.KILLED,by = list(Vehicle = VEHICLE.TYPE.CODE.1), FUN = sum),-x)


# Filtering rows for which we have a valid lat long. 
# crimeMANHATTAN <- crimeDataCSV %>% 
#   dplyr::filter(BOROUGH == "MANHATTAN") %>%
#   dplyr::filter(LATITUDE != "NA") %>%
#   dplyr::select(DATE,TIME,BOROUGH,ON.STREET.NAME,ZIP.CODE,LATITUDE,LONGITUDE,LOCATION,NUMBER.OF.PERSONS.INJURED,NUMBER.OF.PERSONS.KILLED,
#                 NUMBER.OF.PEDESTRIANS.INJURED,NUMBER.OF.PEDESTRIANS.KILLED,CONTRIBUTING.FACTOR.VEHICLE.1,
#                 CONTRIBUTING.FACTOR.VEHICLE.2,UNIQUE.KEY,VEHICLE.TYPE.CODE.1,VEHICLE.TYPE.CODE.2)
# 
# 
# manhattanVehicleType <- head(arrange(crimeMANHATTAN %>% 
#   group_by(VEHICLE.TYPE.CODE.1) %>%
#   summarise(number = n()),-number),n =10)
# 
# manhattanTAXI <- head(arrange(crimeMANHATTAN %>%
#   dplyr::filter(VEHICLE.TYPE.CODE.1 == "TAXI") %>%
#   dplyr::group_by(CONTRIBUTING.FACTOR.VEHICLE.1) %>%
#   summarise(number = n()),-number),n = 10)

  























































