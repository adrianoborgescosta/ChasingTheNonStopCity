############################
#### DATA MANIPULATION  ####
############################

setwd("~/Desktop/ChasingTheNonStopCity_DataAndCode/")

### PACKAGES ----
pkgs <- c("rgeos","spdep","Matrix","rgdal","raster","sp","raster","spatialEco","magrittr","sf","broom","pcse","tidyr",
          "rgdal","ggplot2","rgeos","broom","maptools","dplyr","maps","ggthemes","scales","sf","ggsn","spatialEco")
#install.packages(pkgs) #install 
sapply(pkgs, require, character.only = T) #load 
rm(pkgs)

### DEFINITIONS
GridSize<-2000
ConsiderJurubStops<-F
ConsiderRailBuffer<-T

### OPENS SHAPEFILES AND DATA OF PUBLIC TRANSPORT INFRASTRUCTURES ----
## Trolley shp ----
TrolleyLines<-readOGR(dsn = "Data/Trolley/", layer = "TrolleyLinesMSP",stringsAsFactors = F)

## Train Stations shp ----
# stations
# obs. the start operation date of stations T0701 / T0708 and T1012 / T1014 were not found, but they are not located in the city of SP
TrainSt <- readOGR(dsn = "Data/TrainStation/", layer = "TrainStation_SIRGAS",stringsAsFactors = F)
# splited lines
TrainLines <- readOGR(dsn = "Data/TrainLines", layer = "TrainLinesSplited_SIRGAS",stringsAsFactors = F)

## Subway Stations shp ----
# stations
SubwaySt <- readOGR(dsn = "Data/SubwayStation", layer = "SubwayStation_SIRGAS",stringsAsFactors = F)
# splited lines
SubwayLines <- readOGR(dsn = "Data/SubwayLines", layer = "SubwayLineSplited_SIRGAS",stringsAsFactors = F)

## Bus Corridors shp ----
BusCorridors <- readOGR(dsn = "Data/BusCorridors", layer = "BusCorridor_SIRGAS",stringsAsFactors = F)

## Data on Public Transport Infrastructures ----
TransitData<-read.csv("Data/DataTransitSaoPaulo.csv",
                     sep=",",header = T,stringsAsFactors = F)

# merge data and shp
TrolleyLines<-merge(TrolleyLines,TransitData,by="id",all.x=T, all.y=F)
TrainSt<-merge(TrainSt,TransitData,by="id",all.x=T, all.y=F)
TrainLines<-merge(TrainLines,TransitData,by="id",all.x=T, all.y=F)
SubwaySt<-merge(SubwaySt,TransitData,by="id",all.x=T, all.y=F)
SubwayLines<-merge(SubwayLines,TransitData,by="id",all.x=T, all.y=F)
BusCorridors<-merge(BusCorridors,TransitData,by="id",all.x=T, all.y=F)

# Drops off stops on Ramal Jurubatba that were not considered stations given its relevance
if(ConsiderJurubStops==F) {TrainSt<-TrainSt[TrainSt@data$id!="TJB02"&TrainSt@data$id!="TJB04"&
                       TrainSt@data$id!="TJB05"&TrainSt@data$id!="TJB08"&
                       TrainSt@data$id!="TJB10"&TrainSt@data$id!="TJB11"&
                       TrainSt@data$id!="TJB13"&TrainSt@data$id!="TJB17"&
                       TrainSt@data$id!="TJB07",]} else {}
rm(ConsiderJurubStops)

## Considers only the train and subway sections around the station buffer
# Train
BufferTrainSt<-gBuffer(TrainSt, byid=TRUE, width=1000)
TrainLinesBufferSt<-intersect(TrainLines, BufferTrainSt)
TrainLinesBufferSt<-TrainLinesBufferSt[substr(TrainLinesBufferSt$id.2,1,3) == substr(TrainLinesBufferSt$id.1,1,3)|
                                         substr(TrainLinesBufferSt$id.2,1,3) == substr(TrainLinesBufferSt$id.1,7,9),]
for(i in 1:nrow(TrainLinesBufferSt)) {
  TrainLinesBufferSt$Open[i]<-max(TrainLinesBufferSt$Open.1[i], TrainLinesBufferSt$Open.2[i],na.rm = T)
  TrainLinesBufferSt$Close[i]<-min(TrainLinesBufferSt$Close.1[i], TrainLinesBufferSt$Close.2[i], na.rm=T)
}
TrainLinesBufferSt$Open[is.infinite(TrainLinesBufferSt$Open)]<-NA
TrainLinesBufferSt$Close[is.infinite(TrainLinesBufferSt$Close)]<-NA
TrainLinesBufferSt<-TrainLinesBufferSt[,c("id.1","id.2","KindOfTransp.1","Open","Close","Integration.1","Operating.1","Geocoded.1")]
names(TrainLinesBufferSt)<-c("id","TransitInfra","KindOfTransp","Open","Close","Operating","Integration","Geocoded")
TrainLinesBufferSt$TransitInfra<-paste("Line stretch surrounding the station",TrainLinesBufferSt$TransitInfra)
if(ConsiderRailBuffer==T) {TrainLines<-TrainLinesBufferSt} else {}
rm(TrainLinesBufferSt,BufferTrainSt)

#Subway
BufferSubwaySt<-gBuffer(SubwaySt, byid=TRUE, width=1000)
SubwayLinesBufferSt<-intersect(SubwayLines, BufferSubwaySt)
SubwayLinesBufferSt<-SubwayLinesBufferSt[substr(SubwayLinesBufferSt$id.2,1,3) == substr(SubwayLinesBufferSt$id.1,1,3)|
                                         substr(SubwayLinesBufferSt$id.2,1,3) == substr(SubwayLinesBufferSt$id.1,7,9),]
for(i in 1:nrow(SubwayLinesBufferSt)) {
  SubwayLinesBufferSt$Open[i]<-max(SubwayLinesBufferSt$Open.1[i], SubwayLinesBufferSt$Open.2[i],na.rm = T)
  SubwayLinesBufferSt$Close[i]<-min(SubwayLinesBufferSt$Close.1[i], SubwayLinesBufferSt$Close.2[i], na.rm=T)
}
SubwayLinesBufferSt$Open[is.infinite(SubwayLinesBufferSt$Open)]<-NA
SubwayLinesBufferSt$Close[is.infinite(SubwayLinesBufferSt$Close)]<-NA
SubwayLinesBufferSt<-SubwayLinesBufferSt[,c("id.1","id.2","KindOfTransp.1","Open","Close","Integration.1","Operating.1","Geocoded.1")]
names(SubwayLinesBufferSt)<-c("id","TransitInfra","KindOfTransp","Open","Close","Operating","Integration","Geocoded")
SubwayLinesBufferSt$TransitInfra<-paste("Line stretch surrounding the station",SubwayLinesBufferSt$TransitInfra)
if(ConsiderRailBuffer==T) {SubwayLines<-SubwayLinesBufferSt} else {}
rm(SubwayLinesBufferSt,BufferSubwaySt)
rm(ConsiderRailBuffer)

### OPENS SHAPEFILES AND DATA OF BUILDINGS ----
Buildings<-readOGR(dsn = "Data/Buildings/", layer = "BuildingsMSP_Geocoded",stringsAsFactors = F)
Buildings<-spTransform(Buildings, CRS("+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs"))
Buildings<-Buildings[,c("Match_addr","ANO_DA_CON","TIPO_DE_PA","QUANTIDADE","NUMERO_UNI","AREA_DO_TE","AREA_OCUPA","AREA_CONST")]
names(Buildings)<-c("Address","ContructionYear","KindBuild","NFloor","Units","LandArea","OccupArea","BuiltUpArea")
Buildings$KindBuild<-factor(Buildings$KindBuild)

### OPENS SHAPEFILE AND DATA OF ROADS ----
MainSt<-readOGR(dsn = "Data/MainStreets/", layer = "MainStreetsMSP_CenterLines_SIRGAS",stringsAsFactors = F)
MainSt<-MainSt[!duplicated(MainSt$Codlog),]

## Opens data of artherial roads ----
StreetDates<-read.csv("Data/DataArtherialRoadsSaoPaulo.csv",sep=",",header=T,stringsAsFactors = F)
StreetDates$Open<-as.integer(StreetDates$Open)
MainSt<-merge(MainSt,StreetDates,by.x="Codlog", by.y="id",all=F)
MainSt<-MainSt[order(MainSt$Open,decreasing = F),]
names(MainSt)
MainSt<-MainSt[,c("Codlog","KindRoad","Title","Prep","Name","Open")]
MainSt$Length<-gLength(MainSt,byid = T)/10^3

### OPENS SHAPEFILE OF URBAN FOOTPRINT ----
UrbanArea<-readOGR(dsn = "Data/UrbanExpansion", layer = "UrbanArea",stringsAsFactors = T)
names(UrbanArea)<-c("Year","YearAnalysis")
UrbanAreaDissolve<-readOGR(dsn = "Data/UrbanExpansion", layer = "UrbanAreaDissolve",stringsAsFactors = T)
names(UrbanAreaDissolve)<-c("YearAnalysis","PerAnalysis")

### OPENS SHAPEFILE OF THE CITY BORDERS ----
BorderMSP <- readOGR(dsn = "Data/CityBorder", layer = "BorderMSP_SIRGAS",stringsAsFactors = F)
proj4string(BorderMSP) <- CRS("+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs")
BorderMSP<-gBuffer(BorderMSP, byid=TRUE, width=0)

### CREATING THE GRID OF ANALYSIS ----
## Creates the cell grid
Grid <- st_make_grid(st_as_sf(BorderMSP), cellsize = c(GridSize,GridSize),
                     crs = "+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs", 
                     what = 'polygons') %>%
    st_sf('geometry' = ., data.frame('ID' = 1:length(.)))
Grid<-as(Grid, 'Spatial')
names(Grid)<-"CodGrid"
Grid$CodGrid<-as.character(sprintf('%0.4d', 1:length(Grid)))
Grid$Area<-abs(area(Grid))/10^6
Grid<-gBuffer(Grid, byid=TRUE, width=0)
plot(Grid)
plot(BorderMSP,add=T)

## Year of Analysis ----
YearsOfAnalysis<-data.frame(Years=c(1881,1897,1914,1929,1947,1962,1980,1997,2013),
                            Differences=rep(0))
for(i in 2:nrow(YearsOfAnalysis)) {
  YearsOfAnalysis$Differences[i]=YearsOfAnalysis$Years[i]-YearsOfAnalysis$Years[i-1]
}

### BUILDING DATA PER GRID CELL (AGGREGATING) ----
Buildings<-point.in.poly(Buildings,Grid[,"CodGrid"])

## Number of buildings ----
GridNBuildings<-Grid
GridNBuildingsBuilder<-function(Year){
  if (!min(Buildings$ContructionYear,na.rm = T)<=Year){
    GridNBuildingsTemp<-as.data.frame(Grid[,1])
    GridNBuildingsTemp$NBuildings<-0
  } else {
    GridNBuildingsTemp<-aggregate(Buildings$Address[Buildings$ContructionYear<=Year]
                                  ~Buildings$CodGrid[Buildings$ContructionYear<=Year],
                                  FUN="length")
  }
  names(GridNBuildingsTemp)<-c("CodGrid",paste("NBuildings",Year,sep=""))
  GridNBuildingsTemp<-merge(GridNBuildings[,1:2],GridNBuildingsTemp, by="CodGrid", all=T)
  GridNBuildingsTemp[is.na(as.data.frame(GridNBuildingsTemp[,which(names(GridNBuildingsTemp)==paste("NBuildings", Year,sep=""))])),which(names(GridNBuildingsTemp)==paste("NBuildings", Year,sep=""))]<-0
  GridNBuildingsTemp$BuildDensity<-as.data.frame(GridNBuildingsTemp)[,which(names(GridNBuildingsTemp)==paste("NBuildings", Year,sep=""))]/GridNBuildingsTemp$Area
  GridNBuildingsTemp<-GridNBuildingsTemp[,which(names(GridNBuildingsTemp)!=paste("NBuildings",Year,sep=""))]
  GridNBuildingsTemp<-GridNBuildingsTemp[,which(names(GridNBuildingsTemp)!="Area")]
  names(GridNBuildingsTemp)<-c("CodGrid",paste("BuildDensity",Year,sep=""))
  GridNBuildings<<-merge(GridNBuildings,GridNBuildingsTemp, by="CodGrid", all=T)
}

for(i in 1:length(YearsOfAnalysis$Years)) {
  GridNBuildingsBuilder(YearsOfAnalysis$Years[i])
}

for(i in 2:length(YearsOfAnalysis$Years)){
  VarTemp<-data.frame(CodGrid=GridNBuildings$CodGrid,
                      VarTemp=as.data.frame(GridNBuildings[,which(names(GridNBuildings)==paste("BuildDensity",YearsOfAnalysis$Years[i],sep=""))])-as.data.frame(GridNBuildings[,which(names(GridNBuildings)==paste("BuildDensity",YearsOfAnalysis$Years[i-1],sep=""))]))
  names(VarTemp)<-c("CodGrid",paste("VarBuildDensity",YearsOfAnalysis$Years[i],sep=""))
  GridNBuildings<-merge(GridNBuildings,VarTemp,by="CodGrid",all=T)
  rm(VarTemp)
}

### URBAN AREA DATA PER GRID CELL (AGGREGATING) ----
## Urban area coverage per year ----
UrbanArea<-Grid
UrbanAreaBuilder<-function(Year){
  UrbanAreaTemp<-gIntersection(UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=Year,],BorderMSP)
  UrbanAreaTemp<-gBuffer(UrbanAreaTemp, byid=TRUE, width=0)
  UrbanAreaTemp<-gIntersection(UrbanAreaTemp,Grid,byid = T,drop_lower_td = T)
  UrbanAreaTemp<-gBuffer(UrbanAreaTemp, byid=TRUE, width=0)
  UrbanAreaTemp<-intersect(UrbanAreaTemp,Grid[,1])
  UrbanAreaTemp$UrbanAreaTemp<-abs(area(UrbanAreaTemp))/10^6
  UrbanAreaTemp<-data.frame(UrbanAreaTemp)
  names(UrbanAreaTemp)<-c("CodGrid",paste("UrbanArea",Year,sep=""))
  UrbanArea<<-merge(UrbanArea,UrbanAreaTemp, by="CodGrid", all=T)
  UrbanArea[is.na(as.data.frame(UrbanArea[,which(names(UrbanArea)==paste("UrbanArea", Year,sep=""))])),which(names(UrbanArea)==paste("UrbanArea", Year,sep=""))]<<-0
}

for(i in 1:length(YearsOfAnalysis$Years)) {
  print(YearsOfAnalysis$Years[i])
  UrbanAreaBuilder(YearsOfAnalysis$Years[i])
}

## Total Urbanization Rate ----
GridTotalUrbanRate<-UrbanArea
GridTotalUrbanRateBuilder<-function(Year){
  GridTotalUrbanRateTemp<-GridTotalUrbanRate[,1]
  GridTotalUrbanRateTemp$TotalUrbanRate<-as.data.frame(GridTotalUrbanRate)[,which(names(GridTotalUrbanRate)==paste("UrbanArea", Year,sep=""))]/GridTotalUrbanRate$Area
  names(GridTotalUrbanRateTemp)<-c("CodGrid",paste("TotalUrbanRate",Year,sep=""))
  GridTotalUrbanRate<<-merge(GridTotalUrbanRate,GridTotalUrbanRateTemp, by="CodGrid", all=T)
}

for(i in 1:length(YearsOfAnalysis$Years)) {
  print(YearsOfAnalysis$Years[i])
  GridTotalUrbanRateBuilder(YearsOfAnalysis$Years[i])
}

for(i in 2:length(YearsOfAnalysis$Years)){
  VarTemp<-data.frame(CodGrid=GridTotalUrbanRate$CodGrid,
                      VarTemp=as.data.frame(GridTotalUrbanRate[,which(names(GridTotalUrbanRate)==paste("TotalUrbanRate",YearsOfAnalysis$Years[i],sep=""))])-as.data.frame(GridTotalUrbanRate[,which(names(GridTotalUrbanRate)==paste("TotalUrbanRate",YearsOfAnalysis$Years[i-1],sep=""))]))
  names(VarTemp)<-c("CodGrid",paste("VarTotalUrbanRate",YearsOfAnalysis$Years[i],sep=""))
  GridTotalUrbanRate<-merge(GridTotalUrbanRate,VarTemp,by="CodGrid",all=T)
  rm(VarTemp)
}

## ARTHERIAL ROAD DATA PER GRID CELL (AGGREGATING) ----
GridMainStIntersect<-intersect(MainSt,Grid[,1])
GridMainStIntersect$Length<-gLength(GridMainStIntersect,byid = T)/10^3
GridMainSt<-Grid
GridMainStBuilder<-function(Year){
  if (!min(GridMainStIntersect$Open,na.rm = T)<=Year){
    GridMainStTemp<-as.data.frame(Grid[,1])
    GridMainStTemp$MainSt<-0
  } else {
    GridMainStTemp<-aggregate(GridMainStIntersect$Length[GridMainStIntersect$Open<=Year]
                              ~GridMainStIntersect$CodGrid[GridMainStIntersect$Open<=Year],
                              FUN="sum")
  }
  names(GridMainStTemp)<-c("CodGrid",paste("MainSt", Year,sep=""))
  GridMainSt<<-merge(GridMainSt,GridMainStTemp, by="CodGrid", all=T)
  GridMainSt[is.na(as.data.frame(GridMainSt[,which(names(GridMainSt)==paste("MainSt", Year,sep=""))])),which(names(GridMainSt)==paste("MainSt", Year,sep=""))]<<-0
}

for(i in 1:length(YearsOfAnalysis$Years)) {
  GridMainStBuilder(YearsOfAnalysis$Years[i])
}

for(i in 2:length(YearsOfAnalysis$Years)){
  VarTemp<-data.frame(CodGrid=GridMainSt$CodGrid,
                      VarTemp=as.data.frame(GridMainSt[,which(names(GridMainSt)==paste("MainSt",YearsOfAnalysis$Years[i],sep=""))])-as.data.frame(GridMainSt[,which(names(GridMainSt)==paste("MainSt",YearsOfAnalysis$Years[i-1],sep=""))]))
  names(VarTemp)<-c("CodGrid",paste("VarMainSt",YearsOfAnalysis$Years[i],sep=""))
  GridMainSt<-merge(GridMainSt,VarTemp,by="CodGrid",all=T)
  rm(VarTemp)
}

## TRANSIT DATA PER GRID CELL (AGGREGATING) ----
Transit<-rbind(BusCorridors,TrainLines)
Transit<-rbind(Transit,SubwayLines)
Transit<-rbind(Transit,TrolleyLines)
GridTransitIntersect<-intersect(Transit,Grid[,1])
GridTransitIntersect$Length<-gLength(GridTransitIntersect,byid = T)/10^3
GridTransit<-Grid
GridTransitBuilder<-function(Year){
  if (!min(GridTransitIntersect$Open,na.rm = T)<=Year){
    GridTransitTemp<-as.data.frame(Grid[,1])
    GridTransitTemp$Transit<-0
  } else {
    GridTransitTemp<-aggregate(GridTransitIntersect$Length[GridTransitIntersect$Open<=Year&(GridTransitIntersect$Close>=Year|is.na(GridTransitIntersect$Close))]
                               ~GridTransitIntersect$CodGrid[GridTransitIntersect$Open<=Year&(GridTransitIntersect$Close>=Year|is.na(GridTransitIntersect$Close))],
                               FUN="sum")
  }
  names(GridTransitTemp)<-c("CodGrid",paste("Transit", Year,sep=""))
  GridTransit<<-merge(GridTransit,GridTransitTemp, by="CodGrid", all=T)
  GridTransit[is.na(as.data.frame(GridTransit[,which(names(GridTransit)==paste("Transit", Year,sep=""))])),which(names(GridTransit)==paste("Transit", Year,sep=""))]<<-0
}

for(i in 1:length(YearsOfAnalysis$Years)) {
  GridTransitBuilder(YearsOfAnalysis$Years[i])
}

for(i in 2:length(YearsOfAnalysis$Years)){
  VarTemp<-data.frame(CodGrid=GridTransit$CodGrid,
                      VarTemp=as.data.frame(GridTransit[,which(names(GridTransit)==paste("Transit",YearsOfAnalysis$Years[i],sep=""))])-as.data.frame(GridTransit[,which(names(GridTransit)==paste("Transit",YearsOfAnalysis$Years[i-1],sep=""))]))
  names(VarTemp)<-c("CodGrid",paste("VarTransit",YearsOfAnalysis$Years[i],sep=""))
  GridTransit<-merge(GridTransit,VarTemp,by="CodGrid",all=T)
  rm(VarTemp)
}

GridAbsTransit<-Grid
GridAbsTransitBuilder<-function(Year){
  if (!min(GridTransitIntersect$Open,na.rm = T)<=Year){
    GridAbsTransitTemp<-as.data.frame(Grid[,1])
    GridAbsTransitTemp$AbsTransit<-0
  } else {
    GridAbsTransitTemp<-aggregate(GridTransitIntersect$Length[GridTransitIntersect$Open<=Year]
                               ~GridTransitIntersect$CodGrid[GridTransitIntersect$Open<=Year],
                               FUN="sum")
  }
  names(GridAbsTransitTemp)<-c("CodGrid",paste("AbsTransit", Year,sep=""))
  GridAbsTransit<<-merge(GridAbsTransit,GridAbsTransitTemp, by="CodGrid", all=T)
  GridAbsTransit[is.na(as.data.frame(GridAbsTransit[,which(names(GridAbsTransit)==paste("AbsTransit", Year,sep=""))])),which(names(GridAbsTransit)==paste("AbsTransit", Year,sep=""))]<<-0
}

for(i in 1:length(YearsOfAnalysis$Years)) {
  GridAbsTransitBuilder(YearsOfAnalysis$Years[i])
}

for(i in 2:length(YearsOfAnalysis$Years)){
  VarTemp<-data.frame(CodGrid=GridAbsTransit$CodGrid,
                      VarTemp=as.data.frame(GridAbsTransit[,which(names(GridAbsTransit)==paste("AbsTransit",YearsOfAnalysis$Years[i],sep=""))])-as.data.frame(GridAbsTransit[,which(names(GridAbsTransit)==paste("AbsTransit",YearsOfAnalysis$Years[i-1],sep=""))]))
  names(VarTemp)<-c("CodGrid",paste("VarAbsTransit",YearsOfAnalysis$Years[i],sep=""))
  GridAbsTransit<-merge(GridAbsTransit,VarTemp,by="CodGrid",all=T)
  rm(VarTemp)
}

rm(GridMainStBuilder,GridMainStIntersect)
rm(GridTransitBuilder,GridAbsTransitBuilder,GridTransitIntersect)
rm(i)

### SPATIAL WEIGHTS MATRIX ----
## Distance of Each Cell to CBD ----
CBD<-readOGR("Data/CityCenter","CityCenterMSP_SIRGAS",stringsAsFactors = F)
DistCBD <- t(gDistance(gCentroid(Grid,byid = T), CBD[1,],byid = T))
rownames(DistCBD)<-as.character(sprintf('%0.4d', 1:length(Grid)))

## Proximity Matrix to CBD ----
DistMatrix<-matrix(nrow = nrow(DistCBD),ncol = nrow(DistCBD))
for(i in 1:nrow(DistCBD)) {
  for(j in 1:nrow(DistCBD)) {
if(DistCBD[i]>=DistCBD[j]) {DistMatrix[i,j]<-0} else {DistMatrix[i,j]<-1}
  }
}
rm(i,j)

## Spatial Weight Matrix
GridNb<-poly2nb(Grid, row.names = Grid$CodGrid, queen=T)
GridNeigh<-nb2listw(GridNb,style = "B")
GridMatrix <- as(GridNeigh, "symmetricMatrix")
GridMatrix<-matrix(GridMatrix, nrow = nrow(DistCBD), ncol = nrow(DistCBD))

## Directional Spatial Weight Matrix
GridDistMatrix<-GridMatrix*DistMatrix
GridMatrix<-as.data.frame(GridMatrix)
names(GridMatrix)<-substr(names(GridMatrix),2,6)
rm(GridNb, DistMatrix)

### PANEL DATA JOINT ----
## Joins Grids data ----
#selects grid cell covering any area of the city
Grid<-Grid[BorderMSP,]
#Joins Grids data
GridPanel<-merge(Grid,GridNBuildings[,which(names(GridNBuildings)!="Area")],by="CodGrid",all=F)
GridPanel<-merge(GridPanel,GridTotalUrbanRate[,which(names(GridTotalUrbanRate)!="Area")],by="CodGrid",all=F)
GridPanel<-merge(GridPanel,GridMainSt[,which(names(GridMainSt)!="Area")],by="CodGrid",all=F)
GridPanel<-merge(GridPanel,GridTransit[,which(names(GridTransit)!="Area")],by="CodGrid",all=F)
GridPanel<-merge(GridPanel,GridAbsTransit[,which(names(GridTransit)!="Area")],by="CodGrid",all=F)
# creates variable to identify the stationary cells (without any variation in the period)
GridPanel$ToDrop<-apply(as.data.frame(GridPanel[,which(substr(names(GridPanel),1,3)=="Var")]),1,sum)

# creates panel DataFrame
DataPanel<-as.data.frame(GridPanel)
# includes the distance from each cell to the CBD
DataPanel<-merge(DataPanel, DistCBD, by.x="CodGrid", by.y="row.names")
names(DataPanel)[length(DataPanel)]<-"DistCBD"

### CALCULATES NEIGHBOR'S VARIABLE ----
WDataBuilder<- function(Var) {
  for(i in 1:nrow(YearsOfAnalysis)){
    Year<-YearsOfAnalysis$Years[i]
    DataPanel[,paste("W",Var,Year,sep="")]<<-NA
  for(j in 1:nrow(DataPanel)){
    CodGrid<-as.numeric(DataPanel$CodGrid[j])
    W<-as.numeric(names(GridMatrix)[GridMatrix[CodGrid,]==1])
    WVarYear<-mean(DataPanel[as.numeric(DataPanel$CodGrid) %in% W,which(names(DataPanel)==paste(Var,Year,sep=""))],na.rm=T)
    DataPanel[as.numeric(DataPanel$CodGrid)==CodGrid,which(names(DataPanel)==paste("W",Var,Year,sep=""))]<<-WVarYear
  }
  }
}

VarInt<-c("BuildDensity","TotalUrbanRate","MainSt","Transit","AbsTransit")
for(i in 1:length(VarInt)) {
  print(VarInt[i])
  WDataBuilder(VarInt[i])
}

# calculates the variation between periods
WVarDataBuilder<- function(Var) {
  for(i in 2:nrow(YearsOfAnalysis)){
    YearAfter<-YearsOfAnalysis$Years[i]
    YearBefore<-YearsOfAnalysis$Years[i-1]
    DataPanel[,paste("W","Var",Var,YearAfter,sep="")]<<-NA
    DataPanel[,paste("W","Var",Var,YearAfter,sep="")]<<-DataPanel[,which(names(DataPanel)==paste("W",Var,YearAfter,sep=""))]-DataPanel[,which(names(DataPanel)==paste("W",Var,YearBefore,sep=""))]
  }
}

for(i in 1:length(VarInt)) {
  print(VarInt[i])
  WVarDataBuilder(VarInt[i])
}

## Drops off stationary cells
GridPanel<-GridPanel[which(GridPanel$ToDrop>0),]
GridPanel$ToDrop<-NULL
DataPanel<-DataPanel[which(DataPanel$ToDrop>0),]
DataPanel$ToDrop<-NULL

### CROSSECTION DATA JOIN ----
CrossBuilder<-function(Var,Year) {
  DataTemp<-data.frame(matrix(ncol=3,nrow=nrow(DataPanel)))
  DataTemp[1]<-DataPanel$CodGrid
  DataTemp[2]<-Year
  if(!any(names(DataPanel)==paste(Var,Year,sep = ""))) {
    DataTemp[3]<-NA} else {
      DataTemp[3]<-as.data.frame(DataPanel[,which(names(DataPanel)==paste(Var,Year,sep = ""))])
    }
  names(DataTemp)<-c("CodGrid","Year",paste(Var,"t",sep="_"))
  if(!exists("DataCrossTemp")) {
    DataCrossTemp<-data.frame(matrix(ncol=3,nrow=0))
    names(DataCrossTemp)<-c("CodGrid","Year",paste(Var,"t",sep="_"))
  }
  DataCrossTemp<<-rbind(DataCrossTemp,DataTemp)
}

CrossBuilderLag<-function(Var,Year) {
  DataTemp<-data.frame(matrix(ncol=3,nrow=nrow(DataPanel)))
  DataTemp[1]<-DataPanel$CodGrid
  DataTemp[2]<-Year
  LaggedYear<-YearsOfAnalysis$Years[which(YearsOfAnalysis$Years==Year)-1]
  if(!any(names(DataPanel)==paste(Var,LaggedYear,sep = ""))) {
    DataTemp[3]<-NA} else {
      DataTemp[3]<-as.data.frame(DataPanel[,which(names(DataPanel)==paste(Var,LaggedYear,sep = ""))])
    }
  names(DataTemp)<-c("CodGrid","Year",paste(Var,"t-1",sep="_"))
  if(!exists("DataCrossTemp")) {
    DataCrossTemp<-data.frame(matrix(ncol=3,nrow=0))
    names(DataCrossTemp)<-c("CodGrid","Year",paste(Var,"t-1",sep="_"))
  }
  DataCrossTemp<<-rbind(DataCrossTemp,DataTemp)
}

ApplyBuilder<-function(Var){
  for (i in YearsOfAnalysis$Years) {
    CrossBuilder(Var,i)
  }
  if(!exists("DataCross")) {
    DataCross<<-DataCrossTemp
  } else {
    DataCross<<-merge(DataCross,DataCrossTemp,by=c("CodGrid","Year"),all=T)
  }
  rm(DataCrossTemp, pos = ".GlobalEnv")
}

ApplyBuilderLagged<-function(Var){
  for (i in YearsOfAnalysis$Years) {
    CrossBuilderLag(Var,i)
  }
  if(!exists("DataCross")) {
    DataCross<<-DataCrossTemp} else {
      DataCross<<-merge(DataCross,DataCrossTemp,by=c("CodGrid","Year"),all=T)
    }
  rm(DataCrossTemp, pos = ".GlobalEnv")
}

## Applys functions
# for the model variables
for(i in 1:length(VarInt)) {
  print(VarInt[i])
  ApplyBuilder(VarInt[i])
  ApplyBuilderLagged(VarInt[i])
}

# for variable variations
for(i in 1:length(VarInt)) {
  print(VarInt[i])
  ApplyBuilder(paste("Var",VarInt[i],sep=""))
  ApplyBuilderLagged(paste("Var",VarInt[i],sep=""))
}

# for neighbors' variables
for(i in 1:length(VarInt)) {
  print(VarInt[i])
  ApplyBuilder(paste("W",VarInt[i],sep=""))
  ApplyBuilderLagged(paste("W",VarInt[i],sep=""))
}

# for the variation of the neighbors' variables
for(i in 1:length(VarInt)) {
  print(VarInt[i])
  ApplyBuilder(paste("WVar",VarInt[i],sep=""))
  ApplyBuilderLagged(paste("WVar",VarInt[i],sep=""))
}

### CALCULATE ADDITIONAL VARIABLES IN DATACROSS ----
## Distance from each cell to the CBD ----
DataCross<-merge(DataCross, DistCBD, by.x="CodGrid", by.y="row.names")
names(DataCross)[length(DataCross)]<-"DistCBD"
rm(DistCBD)

## Terrain roughness ----
Topology<-raster("Data/Topology/topografia.tif")
Topology$Drop<-Topology$topografia>=40000
Topology$topografia[Topology$Drop==T]<-NA
Topology<-subset(Topology,"topografia")
TopologySD <- raster::extract(Topology, Grid, fun=sd, na.rm=TRUE,df=T)
TopologySD<-cbind(TopologySD,as.data.frame(Grid$CodGrid))
names(TopologySD)<-c("ID","ElevationSD","CodGrid")
TopologySD$ID<-NULL
TopologyMean <- raster::extract(Topology, Grid, fun=mean, na.rm=TRUE,df=T)
TopologyMean<-cbind(TopologyMean,as.data.frame(Grid$CodGrid))
names(TopologyMean)<-c("ID","ElevationMean","CodGrid")
TopologyMean$ID<-NULL
GridTopology<-merge(Grid,TopologySD,by="CodGrid")
GridTopology<-merge(GridTopology,TopologyMean,by="CodGrid")
GridTopology$Area<-NULL
DataCross<-merge(DataCross,GridTopology,by="CodGrid")
rm(TopologySD,TopologyMean,GridTopology)

## Total urbanization rate for the entire city ----
CityTotalUrbanRate<-data.frame(Year=c(1881,1897,1914,1929,1947,1962,1980,1997,2013),
                               CityTotalUrbanRate_t=c(sum(DataPanel$UrbanArea1881)/sum(DataPanel$Area),
                                                    sum(DataPanel$UrbanArea1897)/sum(DataPanel$Area),
                                                    sum(DataPanel$UrbanArea1914)/sum(DataPanel$Area),
                                                    sum(DataPanel$UrbanArea1929)/sum(DataPanel$Area),
                                                    sum(DataPanel$UrbanArea1947)/sum(DataPanel$Area),
                                                    sum(DataPanel$UrbanArea1962)/sum(DataPanel$Area),
                                                    sum(DataPanel$UrbanArea1980)/sum(DataPanel$Area),
                                                    sum(DataPanel$UrbanArea1997)/sum(DataPanel$Area),
                                                    sum(DataPanel$UrbanArea2013)/sum(DataPanel$Area)),
                               VarCityTotalUrbanRate_t=c(rep(NA)))
for(i in 2:nrow(CityTotalUrbanRate)) {
  CityTotalUrbanRate$VarCityTotalUrbanRate_t[i]<-CityTotalUrbanRate$CityTotalUrbanRate_t[i]-CityTotalUrbanRate$CityTotalUrbanRate_t[i-1]
}
DataCross<-merge(DataCross,CityTotalUrbanRate,by="Year")

## Building density for the whole city ----
CityBuildDensity<-data.frame(Year=c(1881,1897,1914,1929,1947,1962,1980,1997,2013),
                                   CityBuildDensity_t=c(mean(DataPanel$BuildDensity1881),
                                                        mean(DataPanel$BuildDensity1897),
                                                        mean(DataPanel$BuildDensity1914),
                                                        mean(DataPanel$BuildDensity1929),
                                                        mean(DataPanel$BuildDensity1947),
                                                        mean(DataPanel$BuildDensity1962),
                                                        mean(DataPanel$BuildDensity1980),
                                                        mean(DataPanel$BuildDensity1997),
                                                        mean(DataPanel$BuildDensity2013)),
                             VarCityBuildDensity_t=c(rep(NA)))
for(i in 2:nrow(CityBuildDensity)) {
  CityBuildDensity$VarCityBuildDensity_t[i]<-CityBuildDensity$CityBuildDensity_t[i]-CityBuildDensity$CityBuildDensity_t[i-1]
}
DataCross<-merge(DataCross,CityBuildDensity,by="Year")

# Length of the artherial road network for the whole city ----
CityMainSt<-data.frame(Year=c(1881,1897,1914,1929,1947,1962,1980,1997,2013),
                                 CityMainSt_t=c(sum(DataPanel$MainSt1881),
                                                sum(DataPanel$MainSt1897),
                                                sum(DataPanel$MainSt1914),
                                                sum(DataPanel$MainSt1929),
                                                sum(DataPanel$MainSt1947),
                                                sum(DataPanel$MainSt1962),
                                                sum(DataPanel$MainSt1980),
                                                sum(DataPanel$MainSt1997),
                                                sum(DataPanel$MainSt2013)),
                       VarCityMainSt_t=c(rep(NA)))
for(i in 2:nrow(CityMainSt)) {
  CityMainSt$VarCityMainSt_t[i]=CityMainSt$CityMainSt_t[i]-CityMainSt$CityMainSt_t[i-1]
}
DataCross<-merge(DataCross,CityMainSt,by="Year")

# Length of the transit network for the whole city ----
CityTransit<-data.frame(Year=c(1881,1897,1914,1929,1947,1962,1980,1997,2013),
                       CityTransit_t=c(sum(DataPanel$Transit1881),
                                      sum(DataPanel$Transit1897),
                                      sum(DataPanel$Transit1914),
                                      sum(DataPanel$Transit1929),
                                      sum(DataPanel$Transit1947),
                                      sum(DataPanel$Transit1962),
                                      sum(DataPanel$Transit1980),
                                      sum(DataPanel$Transit1997),
                                      sum(DataPanel$Transit2013)),
                       VarCityTransit_t=c(rep(NA)))
for(i in 2:nrow(CityTransit)) {
  CityTransit$VarCityTransit_t[i]=CityTransit$CityTransit_t[i]-CityTransit$CityTransit_t[i-1]
}
DataCross<-merge(DataCross,CityTransit,by="Year")

# Saves the Workspace ----
rm(list = (ls(pattern = "Grid")))
rm(i)
rm(list = (ls(pattern = "City")))
rm(ApplyBuilder,ApplyBuilderLagged,CrossBuilder,CrossBuilderLag,WDataBuilder,WVarDataBuilder,VarInt)
rm(Topology)

#######################
#### DATA ANALYSIS ####
#######################

options(scipen=999,digits = 4)

### ELASTICITIES FUNCTION ----
# Var 1% in VarIndependent -> x% in VarDependent)
ElasticityBuilder<-function(VarDependent, VarIndependent) {
  OriginalValue<-mean(DataCross[,which(names(DataCross)==VarDependent)])
  Model<-get(paste("Exp",gsub("_.*$", "", VarDependent),sep=""))
  Impact<-0.01*mean(DataCross[,which(names(DataCross)==VarIndependent)])*Model$coefficients[grep(VarIndependent, names(Model$coefficients))]
  Elasticity<-Impact/OriginalValue
  assign(value = Elasticity,x = paste("Elast_",gsub("_.*$", "", VarIndependent),"_",gsub("_.*$", "", VarDependent),sep=""),envir = globalenv())
  print(Elasticity)
}

### RESULT TABLE FUNCTION ---
ResultTableBuilder<-function(VarDependent) {
  Model<-get(paste("Exp",gsub("_.*$", "", VarDependent),sep=""))
  Result<-capture.output(summary(Model))
  Result<-Result[grep("(Intercept)", Result):(grep("---", Result)-1)]
  RemoveCharacters<-c("\\*","\\`","\\$","<",">","DataCross")
  for(i in 1:length(RemoveCharacters)){
    Result<-gsub(RemoveCharacters[i],"",Result)
  }
  Result<-gsub("([[:blank:]])\\1+",";",Result)
  Result<-gsub(" ",";",Result)
  Result<-Result[Result != ""]
  Result<-data.frame(Result,stringsAsFactors = F)
  Result<-separate(Result,sep = ";",col = Result,c("Term","Estimate","PCSE","t value","Pr(>|t|)"),extra = "drop")
  Result[,c("Estimate","PCSE","t value","Pr(>|t|)")]<-lapply(Result[,c("Estimate","PCSE","t value","Pr(>|t|)")],as.numeric)
  assign(value = Result,x = paste("Result","_",gsub("_.*$", "", VarDependent),sep=""),envir = globalenv())
}

### RESULTS ----
## Table 2 - Predicting Urban Rate ----
ExpTotalUrbanRate<-lm(DataCross$TotalUrbanRate_t~DataCross$`TotalUrbanRate_t-1`+
                        DataCross$`VarMainSt_t-1`+DataCross$`WVarMainSt_t-1`+
                        DataCross$`VarAbsTransit_t-1`+DataCross$`WVarAbsTransit_t-1`+
                        DataCross$`VarBuildDensity_t-1`+DataCross$`WVarBuildDensity_t-1`+
                        DataCross$`WVarTotalUrbanRate_t-1`+
                        DataCross$VarCityTotalUrbanRate_t+
                        DataCross$DistCBD+DataCross$ElevationSD)
ElastExpTotalUrbanRate<-data.frame("Elasticities"=c(ElasticityBuilder(VarIndependent="MainSt_t",VarDependent="TotalUrbanRate_t"),
                                                    ElasticityBuilder(VarIndependent="AbsTransit_t",VarDependent="TotalUrbanRate_t")))
nobs(ExpTotalUrbanRate)
summary(ExpTotalUrbanRate)$adj.r.squared
ExpTotalUrbanRate<-pcse(ExpTotalUrbanRate, groupN=DataCross$CodGrid[complete.cases(DataCross)], groupT= DataCross$Year[complete.cases(DataCross)])
ResultTableBuilder(VarDependent="TotalUrbanRate_t")
View(Result_TotalUrbanRate)

## Table 3 - Predicting Building Density ----
ExpBuildDensity<-lm(DataCross$BuildDensity_t~DataCross$`BuildDensity_t-1`+
                      DataCross$`VarMainSt_t-1`+DataCross$`WVarMainSt_t-1`+
                      DataCross$`VarAbsTransit_t-1`+DataCross$`WVarAbsTransit_t-1`+
                      DataCross$`VarTotalUrbanRate_t-1`+DataCross$`WVarTotalUrbanRate_t-1`+
                      DataCross$`WVarBuildDensity_t-1`+
                      DataCross$VarCityBuildDensity_t+
                      DataCross$DistCBD+DataCross$ElevationSD)
ElastExpBuildDensity<-data.frame("Elasticities"=c(ElasticityBuilder(VarIndependent="MainSt_t",VarDependent="BuildDensity_t"),
                                                  ElasticityBuilder(VarIndependent="AbsTransit_t",VarDependent="BuildDensity_t")))
nobs(ExpBuildDensity)
summary(ExpBuildDensity)$adj.r.squared
ExpBuiltDensity<-pcse(ExpBuildDensity, groupN=DataCross$CodGrid[complete.cases(DataCross)], groupT= DataCross$Year[complete.cases(DataCross)])
ResultTableBuilder(VarDependent="BuildDensity_t")
View(Result_BuildDensity)

## Table 4 - Predicting Road Network Length ----
ExpMainSt<-lm(DataCross$MainSt_t~DataCross$`MainSt_t-1`+
                DataCross$`VarTotalUrbanRate_t-1`+DataCross$`WVarTotalUrbanRate_t-1`+
                DataCross$`VarBuildDensity_t-1`+DataCross$`WVarBuildDensity_t-1`+
                DataCross$`VarAbsTransit_t-1`+DataCross$`WVarAbsTransit_t-1`+
                DataCross$`WVarMainSt_t-1`+
                DataCross$VarCityMainSt_t+
                DataCross$DistCBD+DataCross$ElevationSD)
ElastExpMainSt<-data.frame("Elasticities"=c(ElasticityBuilder(VarIndependent="TotalUrbanRate_t",VarDependent="MainSt_t"),
                                            ElasticityBuilder(VarIndependent="BuildDensity_t",VarDependent="MainSt_t")))
nobs(ExpMainSt)
summary(ExpMainSt)$adj.r.squared
ExpMainSt<-pcse(ExpMainSt, groupN=DataCross$CodGrid[complete.cases(DataCross)], groupT= DataCross$Year[complete.cases(DataCross)])
ResultTableBuilder(VarDependent="MainSt_t")
View(Result_MainSt)

### Table 5 - Predicting Mass Transit Network Length
ExpAbsTransit<-lm(DataCross$AbsTransit_t~DataCross$`AbsTransit_t-1`+
                    DataCross$`VarTotalUrbanRate_t-1`+DataCross$`WVarTotalUrbanRate_t-1`+
                    DataCross$`VarBuildDensity_t-1`+DataCross$`WVarBuildDensity_t-1`+
                    DataCross$`VarMainSt_t-1`+DataCross$`WVarMainSt_t-1`+
                    DataCross$`WVarAbsTransit_t-1`+
                    DataCross$VarCityTransit_t+
                    DataCross$DistCBD+DataCross$ElevationSD)
nobs(ExpAbsTransit)
summary(ExpAbsTransit)$adj.r.squared
ElastExpAbsTransit<-data.frame("Elasticities"=c(ElasticityBuilder(VarIndependent="TotalUrbanRate_t",VarDependent="AbsTransit_t"),
                                                ElasticityBuilder(VarIndependent="BuildDensity_t",VarDependent="AbsTransit_t")))
ExpAbsTransit<-pcse(ExpAbsTransit, groupN=DataCross$CodGrid[complete.cases(DataCross)], groupT= DataCross$Year[complete.cases(DataCross)])
ResultTableBuilder(VarDependent="AbsTransit_t")
View(Result_AbsTransit)

################################
#### MAPS / TABLES / GRAPHS ####
################################

### MAPS ----
## Open context elements shp
Dams<-readOGR(dsn = "Data/ContextElements/Dams/", layer = "Dams_SIRGAS_MaxLevel",stringsAsFactors = F)
Dams<-gSimplify(Dams, tol = 0.01)
WaterBodies<-readOGR(dsn = "Data/ContextElements/WaterBodies", layer = "WaterBodiesMSP_SIRGAS",stringsAsFactors = F)
MainRivers<-c(3741,3788,3787,3634,3757,2779,2566,2842,2837,2834,2831,2817,2796,2788)
MainRivers<-WaterBodies[WaterBodies$OBJECTID %in% MainRivers,]
MainRivers<-gSimplify(MainRivers, tol = 0.01)
rm(WaterBodies)

## Tidy shp used on maps (do not need region parameter)
ShpToTidy<-c("UrbanAreaDissolve","BorderMSP","MainRivers","Dams")
TidyBuilder<-function(shp){
  ShpTidy<-get(shp)
  ShpTidy <- tidy(ShpTidy)
  assign(x = paste(shp,"_tidy",sep=""),value = ShpTidy,envir = .GlobalEnv) 
}
for(i in 1:length(ShpToTidy)){
  TidyBuilder(ShpToTidy[i])
}

## Design General Definitions
ColorWater<-'#2b8cbe'
ColorStreets<-"gray80"
ColorBackGround<-NA
ColorParks<-"springgreen4"
ColorUrbanArea<-"peachpuff3"
ColorTrolleyLines="gray50"
xlim<-c(313300,360700)
ylim<-c(7343700,7416200)

## Map 1 - Urban area and trolley lines in São Paulo, 1929 ----
row.names(TrolleyLines)<-as.character(seq(1,nrow(TrolleyLines)))
TrolleyLines_tidy<-tidy(TrolleyLines)
TrolleyLines_Data <- data.frame(idTrolleyLines=TrolleyLines@data$id,
                                Inicio=as.integer(TrolleyLines@data$Open),
                                Fechamento=as.integer(TrolleyLines@data$Close),
                                Legend="Trolley Lines in 1929")
TrolleyLines_Data$id <- seq(1,nrow(TrolleyLines_Data))
TrolleyLines_tidy<-merge(TrolleyLines_tidy,TrolleyLines_Data,by="id")
UrbanArea1929_tidy<-tidy(UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1929,])
UrbanArea1929_tidy$Legend<-"Urban Footprint in 1929"
MapUrbanAreaTrolley1929<-ggplot()+
  geom_polygon(data = UrbanArea1929_tidy,
               aes(x = long, y = lat, group=group, fill=Legend),
               alpha=0.6)+
  scale_fill_manual(values = ColorUrbanArea,
                    labels="Urban Footprint in 1929",
                    name="Urban Area")+
  geom_polygon(data = Dams_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,alpha=0.8)+
  geom_polygon(data = MainRivers_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,color=ColorWater,size=0.5,alpha=0.6,show.legend=F)+
  geom_path(data = TrolleyLines_tidy[TrolleyLines_tidy$Inicio<=1929&TrolleyLines_tidy$Fechamento>=1930,],
            aes(x = long, y = lat, group=group,colour=Legend),
            na.rm=T,size=0.35,linetype = "solid")+
  scale_color_manual(values = ColorTrolleyLines,
                     labels="Trolley Lines in 1929",
                     name="Tranportation")+
  geom_polygon(data = BorderMSP_tidy,aes(x = long, y = lat,group=group),
               colour="black",fill=ColorBackGround,size=0.6,show.legend = F)+
  coord_fixed(xlim = c(315000,349000),ylim = c(7375000,7416000),ratio = 0.95)+
  theme_map(base_size = 8) +
  theme(legend.position = "right")+
  scalebar(x.min=335000,x.max = 350000,y.min = 7375000,y.max = 7380000,dist = 5,location = "bottomright",st.size = 3,height=0.2,st.dist=0.2)+
  north(x.min=347500,x.max = 350000,y.min = 7413000,y.max = 7415000,location = "topright",symbol = 1,scale = 3)
ggsave(MapUrbanAreaTrolley1929,device = "png", filename = "Map1_UrbanAreaTrolley1929.png",
       path = "Outputs/",
       width = 6, height = 4, units = "in",dpi = 300)

## Map 2 - Selected Transportation Plans ----
# Gonçalves Plan - proposed in 1906
Plan_1906<-readOGR(dsn="Data/TransportationPlans/", layer="Plan_1906")
Plan_1906<-spTransform(Plan_1906, CRS("+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs"))
Plan_1906_tidy<-tidy(Plan_1906)
Plan_1906_tidy$Legend<-"Planned Express Rail and Subway Network Proposed in 1906"
UrbanArea1914_tidy<-tidy(UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1914,])
UrbanArea1914_tidy$Legend<-"Urban Footprint in 1914"
MapPlan_1906<-ggplot()+
  geom_polygon(data = UrbanArea1914_tidy,
               aes(x = long, y = lat, group=group, fill=Legend),
               alpha=0.6)+
  scale_fill_manual(values = ColorUrbanArea,
                    labels="Urban Footprint in 1914",
                    name="Urban Area")+
  geom_polygon(data = Dams_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,alpha=0.8)+
  geom_polygon(data = MainRivers_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,color=ColorWater,size=0.5,alpha=0.6,show.legend=F)+
  geom_path(data = Plan_1906_tidy,
            aes(x = long, y = lat, group=group,colour=Legend),
            na.rm=T,size=0.4,linetype = "solid")+
  scale_color_manual(values = ColorTrolleyLines,
                     labels="Planned Express Rail\nand Subway Network\nProposed in 1906",
                     name="Transit Plan")+
  geom_polygon(data = BorderMSP_tidy,aes(x = long, y = lat,group=group),
               colour="black",fill=ColorBackGround,size=0.6,show.legend = F)+
  ggtitle("1906/1914")+
  coord_fixed(xlim = c(315000,354000),ylim = c(7375000,7416000),ratio = 0.95)+
  theme_map(base_size = 8) +
  theme(legend.position = "bottom",legend.direction="vertical",
        plot.title = element_text(size = 12, face = "bold"))+
  scalebar(x.min=335000,x.max = 353000,y.min = 7375000,y.max = 7380000,dist = 5,location = "bottomright",st.size = 3,height=0.2,st.dist=0.2)+
  north(x.min=350000,x.max = 353000,y.min = 7413000,y.max = 7415000,location = "topright",symbol = 1,scale = 3)
ggsave(MapPlan_1906,device = "png", filename = "Map2_Plan1906.png",
       path = "Outputs/",
       width = 4, height = 4, units = "in",dpi = 300)
#Light's subway Plan - proposed in 1927
Plan_1927<-readOGR(dsn="Data/TransportationPlans/", layer="Plan_1927")
Plan_1927<-spTransform(Plan_1927, CRS("+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs"))
Plan_1927_tidy<-tidy(Plan_1927)
Plan_1927_tidy$Legend<-"Light's Express Rail and Subway Network Proposed in 1927"
UrbanArea1929_tidy<-tidy(UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1929,])
UrbanArea1929_tidy$Legend<-"Urban Footprint in 1929"
MapPlan_1927<-ggplot()+
  geom_polygon(data = UrbanArea1929_tidy,
               aes(x = long, y = lat, group=group, fill=Legend),
               alpha=0.6)+
  scale_fill_manual(values = ColorUrbanArea,
                    labels="Urban Footprint in 1929",
                    name="Urban Area")+
  geom_polygon(data = Dams_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,alpha=0.8)+
  geom_polygon(data = MainRivers_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,color=ColorWater,size=0.5,alpha=0.6,show.legend=F)+
  geom_path(data = Plan_1927_tidy,
            aes(x = long, y = lat, group=group,colour=Legend),
            na.rm=T,size=0.4,linetype = "solid")+
  scale_color_manual(values = ColorTrolleyLines,
                     labels="Light's Express Rail\nand Subway Network\nProposed in 1927",
                     name="Transit Plan")+
  geom_polygon(data = BorderMSP_tidy,aes(x = long, y = lat,group=group),
               colour="black",fill=ColorBackGround,size=0.6,show.legend = F)+
  ggtitle("1927/1929")+
  coord_fixed(xlim = c(315000,354000),ylim = c(7375000,7416000),ratio = 0.95)+
  theme_map(base_size = 8) +
  theme(legend.position = "bottom",legend.direction="vertical",
        plot.title = element_text(size = 12, face = "bold"))+
  scalebar(x.min=335000,x.max = 353000,y.min = 7375000,y.max = 7380000,dist = 5,location = "bottomright",st.size = 3,height=0.2,st.dist=0.2)+
  north(x.min=350000,x.max = 353000,y.min = 7413000,y.max = 7415000,location = "topright",symbol = 1,scale = 3)
ggsave(MapPlan_1927,device = "png", filename = "Map2_Plan1927.png",
       path = "Outputs/",
       width = 4, height = 4, units = "in",dpi = 300)
# Plan proposed by the Paris' Subway workgroup in 1947
Plan_1947<-readOGR(dsn="Data/TransportationPlans/", layer="Plan_1947")
Plan_1947<-spTransform(Plan_1947, CRS("+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs"))
Plan_1947_tidy<-tidy(Plan_1947)
Plan_1947_tidy$Legend<-"Planned Subway Network Proposed in 1947"
UrbanArea1947_tidy<-tidy(UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1947,])
UrbanArea1947_tidy$Legend<-"Urban Footprint in 1947"
MapPlan_1947<-ggplot()+
  geom_polygon(data = UrbanArea1947_tidy,
               aes(x = long, y = lat, group=group, fill=Legend),
               alpha=0.6)+
  scale_fill_manual(values = ColorUrbanArea,
                    labels="Urban Footprint in 1947",
                    name="Urban Area")+
  geom_polygon(data = Dams_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,alpha=0.8)+
  geom_polygon(data = MainRivers_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,color=ColorWater,size=0.5,alpha=0.6,show.legend=F)+
  geom_path(data = Plan_1947_tidy,
            aes(x = long, y = lat, group=group,colour=Legend),
            na.rm=T,size=0.4,linetype = "solid")+
  scale_color_manual(values = ColorTrolleyLines,
                     labels="Planned Subway Network\nProposed in 1947",
                     name="Transit Plan")+
  geom_polygon(data = BorderMSP_tidy,aes(x = long, y = lat,group=group),
               colour="black",fill=ColorBackGround,size=0.6,show.legend = F)+
  ggtitle("1947")+
  coord_fixed(xlim = c(315000,354000),ylim = c(7375000,7416000),ratio = 0.95)+
  theme_map(base_size = 8) +
  theme(legend.position = "bottom",legend.direction="vertical",
        plot.title = element_text(size = 12, face = "bold"))+
  scalebar(x.min=335000,x.max = 353000,y.min = 7375000,y.max = 7380000,dist = 5,location = "bottomright",st.size = 3,height=0.2,st.dist=0.2)+
  north(x.min=350000,x.max = 353000,y.min = 7413000,y.max = 7415000,location = "topright",symbol = 1,scale = 3)
ggsave(MapPlan_1947,device = "png", filename = "Map2_Plan1947.png",
       path = "Outputs/",
       width = 4, height = 4, units = "in",dpi = 300)
# Basic Network Plan - proposed in 1968
Plan_1968<-readOGR(dsn="Data/TransportationPlans/", layer="Plan_1968",stringsAsFactors = F)
Plan_1968_tidy<-tidy(Plan_1968)
Plan_1968_tidy$Legend[Plan_1968_tidy$id %in% c(0:4)]<-"Planned Subway Basic Network Proposed in 1968"
Plan_1968_tidy$Legend[!Plan_1968_tidy$id %in% c(0:4)]<-"Planned Subway Complementary Lines Proposed in 1968"
UrbanArea1974_tidy<-tidy(UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1974,])
UrbanArea1974_tidy$Legend<-"Urban Footprint in 1974"
MapPlan_1968<-ggplot()+
  geom_polygon(data = UrbanArea1974_tidy,
               aes(x = long, y = lat, group=group, fill=Legend),
               alpha=0.6)+
  scale_fill_manual(values = ColorUrbanArea,
                    labels="Urban Footprint in 1974",
                    name="Urban Area")+
  geom_polygon(data = Dams_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,alpha=0.8)+
  geom_polygon(data = MainRivers_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,color=ColorWater,size=0.5,alpha=0.6,show.legend=F)+
  geom_path(data = Plan_1968_tidy,
            aes(x = long, y = lat, group=group,linetype=Legend),
            colour=ColorTrolleyLines,
            na.rm=T,size=0.4)+
  scale_linetype_manual(values=c("solid","dotted"),
                        labels=c("Planned Subway\nBasic Network\nProposed in 1968",
                                 "Planned Subway\nComplementary Lines\nProposed in 1968"),
                        name="Transit Plan")+
  geom_polygon(data = BorderMSP_tidy,aes(x = long, y = lat,group=group),
               colour="black",fill=ColorBackGround,size=0.6,show.legend = F)+
  ggtitle("1968/1974")+
  coord_fixed(xlim = c(315000,354000),ylim = c(7375000,7416000),ratio = 0.95)+
  theme_map(base_size = 8) +
  theme(legend.position = "bottom",legend.direction="vertical",
        plot.title = element_text(size = 12, face = "bold"))+
  scalebar(x.min=335000,x.max = 353000,y.min = 7375000,y.max = 7380000,dist = 5,location = "bottomright",st.size = 3,height=0.2,st.dist=0.2)+
  north(x.min=350000,x.max = 353000,y.min = 7413000,y.max = 7415000,location = "topright",symbol = 1,scale = 3)
ggsave(MapPlan_1968,device = "png", filename = "Map2_Plan1968.png",
       path = "Outputs/",
       width = 4, height = 4, units = "in",dpi = 300)
# Lines in 2013
SubwayLines_2013_tidy<-tidy(SubwayLines[SubwayLines$Open<=2013,])
SubwayLines_2013_tidy$Legend<-"SubwayLines in 2013"
UrbanArea2013_tidy<-tidy(UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=2013,])
UrbanArea2013_tidy$Legend<-"Urban Footprint in 2013"
MapUrbanAreaSubway2013<-ggplot()+
  geom_polygon(data = UrbanArea2013_tidy,
               aes(x = long, y = lat, group=group, fill=Legend),
               alpha=0.6)+
  scale_fill_manual(values = ColorUrbanArea,
                    labels="Urban Footprint in 2013",
                    name="Urban Area")+
  geom_polygon(data = Dams_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,alpha=0.8)+
  geom_polygon(data = MainRivers_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,color=ColorWater,size=0.5,alpha=0.6,show.legend=F)+
  geom_path(data = SubwayLines_2013_tidy,
            aes(x = long, y = lat, group=group,colour=Legend),
            na.rm=T,size=0.35,linetype = "solid")+
  scale_color_manual(values = ColorTrolleyLines,
                     labels="SubwayLines in 2013",
                     name="Transit Infrastructure")+
  geom_polygon(data = BorderMSP_tidy,aes(x = long, y = lat,group=group),
               colour="black",fill=ColorBackGround,size=0.6,show.legend = F)+
  ggtitle("2013")+
  coord_fixed(xlim = c(315000,354000),ylim = c(7375000,7416000),ratio = 0.95)+
  theme_map(base_size = 8) +
  theme(legend.position = "bottom",legend.direction="vertical",
        plot.title = element_text(size = 12, face = "bold"))+
  scalebar(x.min=335000,x.max = 353000,y.min = 7375000,y.max = 7380000,dist = 5,location = "bottomright",st.size = 3,height=0.2,st.dist=0.2)+
  north(x.min=350000,x.max = 353000,y.min = 7413000,y.max = 7415000,location = "topright",symbol = 1,scale = 3)
ggsave(MapUrbanAreaSubway2013,device = "png", filename = "Map2_UrbanAreaSubway2013.png",
       path = "Outputs",
       width = 4, height = 4, units = "in",dpi = 300)

## Map 3 - Square grid for analysis of evolution in São Paulo’s urban area, 1881–2013 ----
Grid <- st_make_grid(st_as_sf(BorderMSP), cellsize = c(GridSize,GridSize),
                     crs = "+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs", 
                     what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('ID' = 1:length(.)))
Grid<-as(Grid, 'Spatial')
Grid<-gIntersection(Grid,BorderMSP,byid = T)
MapGridUrbanArea<-ggplot()+
  geom_polygon(data = UrbanAreaDissolve_tidy,
               aes(x = long, y = lat, group=group, fill=id))+
  scale_fill_brewer(type = "seq",palette = 5, direction = 1,guide = "legend", name="Urban Area by Year")+
  geom_polygon(data = Dams_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,show.legend=NA)+
  geom_polygon(data = MainRivers_tidy,aes(x = long, y = lat,group=group),
               fill=ColorWater,color=ColorWater,size=1,show.legend=NA)+
  geom_polygon(data = tidy(Grid),
               aes(x = long, y = lat, group=group), 
               colour= "black",fill = "transparent", size=0.6)+
  geom_polygon(data = BorderMSP_tidy,aes(x = long, y = lat,group=group),
               colour="black",fill=ColorBackGround,alpha=0,size=0.8)+
  coord_fixed(xlim = c(313300,360700),  ylim = c(7343700,7416200),ratio = 0.95)+
  theme_map(base_size = 8)+
  theme(legend.position = c(.75,.07), legend.direction="vertical")+
  scalebar(data = BorderMSP_tidy,dist = 5,location = "bottomright",st.size = 3)+
  north(data = BorderMSP_tidy,location = "topright",symbol = 1)
ggsave(MapGridUrbanArea,device = "png", filename = "Map3_GridWithUrbanArea.png",
       path = "Outputs/",
       width = 6, height = 8, units = "in",dpi = 300)

### Specific Data ----
## Urban Area Size in 1881 ----
gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis=="1881",])/ 1e6

## Trolley network length in 1929 ----
gLength(TrolleyLines[TrolleyLines$Open<=1929&TrolleyLines$Close>=1930,])/ 1e3

## Urban area increase between 1905 and 1929
((gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1929,]) - gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1897,]))/gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1897,]))*100

## Urban area increase between 1929 and 1980
((gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1980,]) - gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1929,]))/gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1929,]))*100

(gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1980,]) / gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1929,]))

## Compound annual growth rate - Urban Area
for(i in 2:length(YearsOfAnalysis$Years)){
  print(YearsOfAnalysis$Years[i])
  print(((gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=YearsOfAnalysis$Years[i],])/gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=YearsOfAnalysis$Years[i-1],]))^(1/(YearsOfAnalysis$Years[i]-YearsOfAnalysis$Years[i-1]))-1)*100)
}

# from 1929 to 1980
((gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1980,])/gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1929,]))^(1/(1980-1929))-1)*100

# from 1980 to 2002
((gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=2013,])/gArea(spgeom = UrbanAreaDissolve[UrbanAreaDissolve$YearAnalysis<=1980,]))^(1/(2013-1980))-1)*100

# median age of buildings
hist(Buildings$ContructionYear)
median(2017-Buildings$ContructionYear)
