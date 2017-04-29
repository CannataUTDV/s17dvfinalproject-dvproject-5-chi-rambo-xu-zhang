require(readr)
require(plyr)
require(dplyr)
require(stringr)
require(formattable)
require(data.world)

MassShooting <- query(
  data.world(propsfile = "../02 Shiny/www/.data.world"),
  dataset="awram/us-mass-shootings", type="sql",
  query="SELECT YEAR as Year, `CASE` as `Case`, GENDER as Gender, RACE as Race,
         LOCATION as City, STATE as State, LOCATIONTYPE as LocationType,
         FATALITIES as Fatalities, WOUNDED as Wounded, TOTALVICTIMS as TotalVictims, NUMWEAPONS as NumWeapons,
         PRIORSIGNSOFMENTALILLNESS as MentalIllness, WEAPONSOBTAINEDLEGALLY as LegalWeapon
         from Sheet1")

MassShooting <- na.omit(MassShooting)

StateAbbrev <- query(
  data.world(propsfile = "../02 Shiny/www/.data.world"),
  dataset="markmarkoh/us-state-table", type="sql",
  query="select name as State, abbreviation as Abbreviation
         from state_table")

LatLong <- query(
  data.world(propsfile = "../02 Shiny/www/.data.world"),
  dataset="bryon/dhs-city-location-example", type="sql",
  query="select NAME as City, STATE as State, LATITUDE as Latitude, LONGITUDE as Longitude
         from towns")

MassShooting$City <- gsub(",.*", "", MassShooting$City)

MSmeasures <- c("Year", "Fatalities", "Wounded", "TotalVictims", "NumWeapons")
MSdimensions <- setdiff(names(MassShooting), MSmeasures)

SAdimensions <- c("State", "Abbreviation")

LLmeasures <- c("Latitude", "Longitude")
LLdimensions <- setdiff(names(LatLong), LLmeasures)

for(n in names(MassShooting)) {
  MassShooting[n] <- data.frame(lapply(MassShooting[n], gsub, pattern="[^ -~]",replacement= ""))
}

for(n in names(StateAbbrev)) {
  StateAbbrev[n] <- data.frame(lapply(StateAbbrev[n], gsub, pattern="[^ -~]",replacement= ""))
}

for(n in names(LatLong)) {
  LatLong[n] <- data.frame(lapply(LatLong[n], gsub, pattern="[^ -~]",replacement= ""))
}

na2emptyString <- function (x) {
  x[is.na(x)] <- ""
  return(x)
}

if(length(MSdimensions) > 0) {
  for(d in MSdimensions) {
    # Change NA to the empty string.
    MassShooting[d] <- data.frame(lapply(MassShooting[d], na2emptyString))
    # Get rid of " and ' in dimensions.
    MassShooting[d] <- data.frame(lapply(MassShooting[d], gsub, pattern="[\"'`]",replacement= ""))
    # Change & to and in dimensions.
    MassShooting[d] <- data.frame(lapply(MassShooting[d], gsub, pattern="&",replacement= " and "))
    # Change - to / in dimensions.
    MassShooting[d] <- data.frame(lapply(MassShooting[d], gsub, pattern="-",replacement= "/"))
  }
}

if(length(SAdimensions) > 0) {
  for(d in SAdimensions) {
    # Change NA to the empty string.
    StateAbbrev[d] <- data.frame(lapply(StateAbbrev[d], na2emptyString))
    # Get rid of " and ' in dimensions.
    StateAbbrev[d] <- data.frame(lapply(StateAbbrev[d], gsub, pattern="[\"'`]",replacement= ""))
    # Change & to and in dimensions.
    StateAbbrev[d] <- data.frame(lapply(StateAbbrev[d], gsub, pattern="&",replacement= " and "))
    # Change - to / in dimensions.
    StateAbbrev[d] <- data.frame(lapply(StateAbbrev[d], gsub, pattern="-",replacement= "/"))
  }
}

if(length(LLdimensions) > 0) {
  for(d in LLdimensions) {
    # Change NA to the empty string.
    LatLong[d] <- data.frame(lapply(LatLong[d], na2emptyString))
    # Get rid of " and ' in dimensions.
    LatLong[d] <- data.frame(lapply(LatLong[d], gsub, pattern="[\"'`]",replacement= ""))
    # Change & to and in dimensions.
    LatLong[d] <- data.frame(lapply(LatLong[d], gsub, pattern="&",replacement= " and "))
    # Change - to / in dimensions.
    LatLong[d] <- data.frame(lapply(LatLong[d], gsub, pattern="-",replacement= "/"))
  }
}

# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions
if( length(MSmeasures) > 1) {
  for(m in MSmeasures) {
    MassShooting[m] <- data.frame(lapply(MassShooting[m], gsub, pattern="[^(--.0-9)]",replacement= ""))
  }
}

if( length(LLmeasures) > 1) {
  for(m in LLmeasures) {
    LatLong[m] <- data.frame(lapply(LatLong[m], gsub, pattern="[^(--.0-9)]",replacement= ""))
  }
}

MassShooting$Case <- as.character(MassShooting$Case)
MassShooting$Gender <- as.character(MassShooting$Gender)
MassShooting$Race <- as.character(MassShooting$Race)
MassShooting$City <- as.character(MassShooting$City)
MassShooting$State <- as.character(MassShooting$State)
MassShooting$LocationType <- as.character(MassShooting$LocationType)
MassShooting$MentalIllness <- as.character(MassShooting$MentalIllness)
MassShooting$LegalWeapon <- as.character(MassShooting$LegalWeapon)
MassShooting$Year <- as.numeric(as.character(MassShooting$Year))
MassShooting$Fatalities <- as.numeric(as.character(MassShooting$Fatalities))
MassShooting$Wounded <- as.numeric(as.character(MassShooting$Wounded))
MassShooting$TotalVictims <- as.numeric(as.character(MassShooting$TotalVictims))
MassShooting$NumWeapons <- as.numeric(as.character(MassShooting$NumWeapons))
StateAbbrev$State <- as.character(StateAbbrev$State)
StateAbbrev$Abbreviation <- as.character(StateAbbrev$Abbreviation)
LatLong$City <- as.character(LatLong$City)
LatLong$State <- as.character(LatLong$State)
LatLong$Latitude <- as.numeric(as.character(LatLong$Latitude))
LatLong$Longitude <-as.numeric(as.character(LatLong$Longitude))

write.csv(MassShooting, "../01 Data/MassShooting.csv", row.names=FALSE, na = "")
write.csv(StateAbbrev, "../01 Data/StateAbbrev.csv", row.names=FALSE, na = "")
write.csv(LatLong, "../01 Data/LatLong.csv", row.names=FALSE, na = "")