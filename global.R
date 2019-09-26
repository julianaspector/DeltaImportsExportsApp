library(lubridate)
library(leaflet)
library(dplyr)
library(shiny)
library(timeDate)
library(baydeltautils)
library(data.table)

setwd("C:/Users/JSpector/Documents/Delta Imports Exports Shiny App")
sacWAM_base <- read.csv("sac_wam_postprocessor_model_1_2.csv", skip=11)
# convert index to machine readable date
sacWAM_base$Index <- ymd(sacWAM_base$Index)

stationLoc <- read.csv("Station_Coordinates.csv")

wyTypes <- read.csv("water_year_types.csv")

# select relevant columns for SacWAM data and merge station location with station data

FPT <- sacWAM_base %>% select(c(Index,Flow=Sacramento.River.209...SWRCB.Sac.AT.Freeport)) 
FPT$CDEC.Station <- "FPT"
FPT <- merge(FPT, stationLoc, by="CDEC.Station")

YBY <- sacWAM_base %>% select(c(Index, Flow=Yolo.Bypass..22...Reach))
YBY$CDEC.Station <- "YBY"
YBY <- merge(YBY, stationLoc, by="CDEC.Station")

PUT <- sacWAM_base %>% select(c(Index,Flow=Putah.Creek..21...SWRCB.Putah.Creek)) 
PUT$CDEC.Station <- "PUT"
PUT <- merge(PUT, stationLoc, by="CDEC.Station")

CSN <- sacWAM_base %>% select(c(Index,Flow=Cosumnes.River...5...FNF.Cosumnes.at.Michigan.Bar..gauge.)) 
CSN$CDEC.Station <- "CSN"
CSN <- merge(CSN, stationLoc, by="CDEC.Station")

DLC <- sacWAM_base %>% select(c(Index, Flow=Delta.Cross.Channel.fr.Sacramento.River.RM.030...0...Headflow)) 
DLC$CDEC.Station <- "DLC"
DLC <- merge(DLC, stationLoc, by="CDEC.Station")

GSS <- sacWAM_base %>% select(c(Index, Flow=Georgiana.Slough.fr.Sacramento.River.RM.029...0...Headflow)) 
GSS$CDEC.Station <- "GSS"
GSS <- merge(GSS, stationLoc, by="CDEC.Station")

WBR <- sacWAM_base %>% select(c(Index,Flow=Mokelumne.River..39...REG.Mokelumne.blw.Woodbridge)) 
WBR$CDEC.Station <- "WBR"
WBR <- merge(WBR, stationLoc, by="CDEC.Station")

QMISC <- sacWAM_base %>% select(c(Index,Flow=Calaveras.River..29...SWRCB.Calaveras.River)) 
QMISC$CDEC.Station <- "QMISC"
QMISC <- merge(QMISC, stationLoc, by="CDEC.Station")

IDB <- sacWAM_base %>% select(c(Index, Flow=Old.River.Pipeline...0...Headflow)) 
IDB$CDEC.Station <- "IDB"
IDB <- merge(IDB, stationLoc, by="CDEC.Station")                                      
                                      
INB <- sacWAM_base %>% select(c(Index,Flow=Rock.Slough.Intake...0...Headflow)) 
INB$CDEC.Station <- "INB"
INB <- merge(INB, stationLoc, by="CDEC.Station") 

WCI <- sacWAM_base %>% select(c(Index, Flow=Clifton.Court.Forebay)) 
WCI$CDEC.Station <- "WCI"
WCI <- merge(WCI, stationLoc, by="CDEC.Station")
WCI <- WCI %>% mutate_at(c("Flow"), as.factor)

SJR <- sacWAM_base %>% select(c(Index, Flow=San.Joaquin.River...1...Inflow.at.Vernalis.Inflow)) 
SJR$CDEC.Station <- "SJR"
SJR <- merge(SJR, stationLoc, by="CDEC.Station")

BKS <- sacWAM_base %>% select(c(Index, Flow=North.Bay.Aqueduct...0...Headflow))
BKS$CDEC.Station <- "BKS"
BKS <- merge(BKS, stationLoc, by="CDEC.Station")
BKS <- BKS %>% mutate_at(c("Flow"), as.factor)

TRP <- sacWAM_base %>% select(c(Index, Flow=Delta.SOD.Export))
TRP$CDEC.Station <- "TRP"
TRP <- merge(TRP, stationLoc, by="CDEC.Station")
TRP <- TRP %>% mutate_at(c("Flow"), as.factor)

# separate data into imports and exports

data <- rbind(FPT, YBY, PUT, CSN, DLC, GSS, WBR, QMISC, SJR, IDB, INB, WCI)
imports <- rbind(FPT, YBY, PUT, CSN, DLC, GSS, WBR, QMISC, SJR)
exports <- rbind(IDB, INB, WCI, BKS, TRP)


imports <- imports %>% mutate_at (c("Flow"), as.numeric)
imports <- imports %>% mutate_at (c("Name"), as.character)

exports <- exports %>% mutate_at (c("Flow"), as.numeric)
exports <- exports %>% mutate_at (c("Name"), as.character)

data <- data %>% mutate_at (c("Flow"), as.numeric)

imports$Units <- "TAF"
exports$Units <- "TAF"

imports$Label <- "Delta Imports"
exports$Label <- "Delta Exports"


imports$WY <- waterYear(imports$Index)
exports$WY <- waterYear(exports$Index)
data$WY <- waterYear(data$Index)

# combine imports and exports for accessing later
imports_exports <- rbind(imports, exports)

data <- merge(data, wyTypes, by="WY")
imports <- merge(imports, wyTypes, by="WY")
exports <- merge(exports, wyTypes, by="WY")
imports_exports <- merge(imports_exports, wyTypes, by="WY")

# determin min and max dates for each slider bar
data_dt <- data.table::data.table(data)

data_maxDate <- data_dt[,max(Index.x), by=WYT]
data_maxDate <- setorder(data_maxDate, WYT)
data_minDate <- data_dt[,min(Index.x), by=WYT]
data_minDate <- setorder(data_minDate, WYT)

# set wyTypes to be integers for indices and numeric for WY
wyTypes <- wyTypes %>% select(c("WY", "WYT"))


# names of sliders

xAxisGroup <- c("Water Year Type 1", "Water Year Type 2", "Water Year Type 3", "Water Year Type 4", "Water Year Type 5")


# create match table
WY_match <- data.table(WYT=c(1, 2, 3, 4, 5), Description=c("Wet", "Above Normal", "Below Normal", "Dry", "Critical"))

wyTypes <- merge(WY_match, wyTypes, by="WYT")

# rearrange columns
wyTypes <- wyTypes %>%
  select(-WYT, -Description, everything())

# sort by year

wyTypes <- wyTypes[order(WY),]

# divide water year types into two groups for tables
wyTypes_one <- wyTypes[1:47,]
wyTypes_two <- wyTypes[48:94,]

