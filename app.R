library(lubridate)
library(leaflet)
library(dplyr)
library(shiny)


setwd("C:/Users/JSpector/Documents/Delta Imports Exports Shiny App")
sacWAM_base <- read.csv("sac_wam_postprocessor_model_1_2.csv", skip=11)
sacWAM_base$Index <- ymd(sacWAM_base$Index)
sacWAM_base <- sacWAM_base %>% mutate_at(c("Sacramento.River.209...SWRCB.Sac.AT.Freeport", 
                                           "Putah.Creek..21...SWRCB.Putah.Creek",
                                           "Cosumnes.River...5...FNF.Cosumnes.at.Michigan.Bar..gauge.",
                                           "Delta.Cross.Channel.fr.Sacramento.River.RM.030...0...Headflow",
                                           "Georgiana.Slough.fr.Sacramento.River.RM.029...0...Headflow",
                                           "Mokelumne.River..39...REG.Mokelumne.blw.Woodbridge",
                                           "Calaveras.River..29...SWRCB.Calaveras.River",
                                           "Old.River.Pipeline...0...Headflow",
                                           "Rock.Slough.Intake...0...Headflow",
                                           "Clifton.Court.Forebay",
                                           "San.Joaquin.River...1...Inflow.at.Vernalis.Inflow"), as.numeric)
stationLoc <- read.csv("Station_Coordinates.csv")

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

SJR <- sacWAM_base %>% select(c(Index, Flow=San.Joaquin.River...1...Inflow.at.Vernalis.Inflow)) 
SJR$CDEC.Station <- "SJR"
SJR <- merge(SJR, stationLoc, by="CDEC.Station")

data <- rbind(FPT, YBY, PUT, CSN, DLC, GSS, WBR, QMISC, IDB, INB, WCI, SJR)


ui <- fluidPage(
    titlePanel("Sacramento-San Joaquin Delta Imports and Exports"),
    sidebarPanel(dateInput("date", 
                           label="Date", 
                           value="1921-10-31", 
                           min="1921-10-31", 
                           max="2015-09-30", 
                           startview="decade")
    ),
    br(),
    br(),
    mainPanel(leafletOutput("map"))
)

server <- function(input, output, session) {
    datasb <- subset(data, Index==input$date)
    output$map <- renderLeaflet({
        leaflet() %>% addTiles() %>% addCircleMarkers(data=datasb)
    })
    
}

shinyApp(ui, server)