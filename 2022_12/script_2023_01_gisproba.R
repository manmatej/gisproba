
## CZ grids
gridsPath<-("//freenas/LABGIS/y_gis_data/CR/CZECH_GRIDS_095/") 
gridsFiles<-list.files(gridsPath,pattern = "*.tif$",full.names = T)
gridsNames<-list.files(gridsPath,pattern = "*.tif$",full.names = F)
gridsNames<-substr(gridsNames,1,nchar(gridsNames)-4)
gridsNames<-make.names(gridsNames)

library(terra)
grids<-lapply(gridsFiles,rast)
st<-terra::rast(gridsFiles)
names(st)<-gridsNames


## selecting grids
# writeClipboard(paste(shQuote(gridsNames,type="sh"),collapse = ","))
used<-c('elevation_10m094')

st<-st[[used]]
names(st)



## mask
library(sf)
ms<-st_read("")


## Extraction itself
body_32633vec<-terra::vect(coo) # create terra vector object
e.body <-terra::extract(st,body_32633vec)

## post extraction corrections
e.body[is.nan(e.body[,"CanopyHeightDMP_DMR094"]),"CanopyHeightDMP_DMR094"]<-0 # zero heihgt vegetation
e.body[is.nan(e.body[,"Kves10m094"]),"Kves10m094"]<-0 # non Kves category

is.nan.data.frame <- function(x)
        do.call(cbind, lapply(x, is.nan))

e.body[is.nan(e.body)]<-NA
body_extract<-data.frame(e.body[,-1],coo,stringsAsFactors = F)
summary(as.factor(body_extract$Country_code))
ele<-body_extract[!is.na(body_extract$elevation_10m094) & body_extract$Country_code!="CZ",]
ele<-body_extract[is.na(body_extract$elevation_10m094) & body_extract$Country_code=="CZ",]

final<-body_extract[!is.na(body_extract$elevation_10m094),]
czech_grids<-final[,19,F]
czech_grids$version<-"0.95"
czech_grids$Datum<-"28.04.2022"
czech_grids<-cbind(czech_grids,final[,1:18])
names(czech_grids)


## write new table to DB 
library(RODBC)
options(digits=10,scipen = 999999)
## Set up driver info and database path
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- "c:\\Users\\matej.man\\Desktop\\Cidla_databaze.mdb"
#MDBPATH <- "c:\\Users\\matej.man\\Dropbox/LabGIS SKRIPTY/Cidla_databaze/cidla_databaze.mdb" #ostra
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
channel <- odbcDriverConnect(PATH) #Establish connection

sqlSave(channel,czech_grids,"czech_grids",rownames = F)

close(channel)
rm(channel)

## join table
## Coordinates from DB 
library(RODBC)
options(digits=10,scipen = 999999)
## Set up driver info and database path
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- "c:\\Users\\matej.man\\Desktop\\Cidla_databaze.mdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
channel <- odbcDriverConnect(PATH) #Establish connection
sqlTables(channel)
sqlColumns(channel,"Zaznamy")
sqlColumns(channel,"Cidla")
sqlColumns(channel,"Hemifoto")
lokality <- sqlQuery(channel,
                     paste0("SELECT * 
                            FROM (((Lokalita AS l
                            INNER JOIN Zaznamy AS z ON l.ID_lokalita = z.ID_lokalita)
                            INNER JOIN czech_grids AS c ON z.ID_lokalita = c.ID_lokalita)
                            INNER JOIN Cidla AS ci ON z.ID_cidlo = ci.ID_cidlo)
                            INNER JOIN Hemifoto AS he ON c.ID_lokalita = he.ID_lokalita"),
                     stringsAsFactors=FALSE,
                     as.is=T)

close(channel)
rm(channel)

lokality$posledniakce<-as.numeric(as.POSIXct(lokality$Datum)) # date for ordering
lokality$cidlolokalita<-paste(lokality$ID_lokalita,lokality$Typ_cidlo,sep = "_") # identify locality and logger type
lokality$Datum<-as.POSIXct(lokality$Datum)
library(data.table)
fwrite(lokality,"d:/ownCloud/PRUHONICE/Mikroklima/cidla databaze R/cidla_databaze_big.csv",sep = ";",row.names = F)

#start<-as.POSIXct("2020-01-01")
end<-as.POSIXct("2020-12-31")
lokality<-lokality[lokality$Datum<end,]

lo<-lokality[order(lokality$cidlolokalita,-lokality$posledniakce),]
lo<-lo[!duplicated(lo$cidlolokalita),]
lo<-lo[lo$Akce2=="měření",]
fwrite(lo,"d:/ownCloud/PRUHONICE/Mikroklima/cidla databaze R/cidla_databaze_posledniakce_mereni_2020.csv",sep = ";",row.names = F)



## CHMI ------------------------------
## Coordinates from chmi_station_data repo

library(sf)
coo<-readRDS("d:\\Git\\chmi_station_data\\data_metadata\\coords_time.rds")
coo<- st_as_sf(coo, coords = c("Zeměpisná.délka","Zeměpisná.šířka"), crs = 4326)
plot(st_geometry(coo))
names(coo)
coo<-st_transform(coo,32633) 

## CZ grids
gridsPath<-("//freenas/LABGIS/y_gis_data/CR/CZECH_GRIDS_095/") 
gridsFiles<-list.files(gridsPath,pattern = "*.tif$",full.names = T)
gridsNames<-list.files(gridsPath,pattern = "*.tif$",full.names = F)
gridsNames<-substr(gridsNames,1,nchar(gridsNames)-4)
gridsNames<-make.names(gridsNames)

library(terra)
st<-terra::rast(gridsFiles)
names(st)<-gridsNames


## selecting grids
# writeClipboard(paste(shQuote(gridsNames,type="sh"),collapse = ","))
used<-c('BiotopType094',
        'CanopyHeightDMP_DMR094',
        'ConvergenceIndex50m094',
        'DiurnalAnisotropicHeating094',
        'elevation_10m094',
        #'EVI_CR094',
        'ForestTreeClass094',
        'Kves10m094',
        'MassBalanceIndex094',
        'NDVI_CR094',
        #'NDWI_green_CR094',
        #'NDWI_swir_CR094',
        'PotentialTotalSolarRadiat094',
        'SAGAWetInd095',
        'SlopeDegree',
        'TopographicPositionIndex250m094',
        'TopographicPositionIndex50.500m094',
        'TopoWetInd094',
        'VectorRuggednessMeasure094',
        'VertDistToDibavod094',
        'VertDistToPocatky095')



st<-st[[used]]
names(st)

## Extraction itself
body_32633vec<-terra::vect(coo) # create terra vector object
e.body <-terra::extract(st,body_32633vec)

## post extraction corrections
e.body[is.nan(e.body[,"CanopyHeightDMP_DMR094"]),"CanopyHeightDMP_DMR094"]<-0 # zero heihgt vegetation
e.body[is.nan(e.body[,"Kves10m094"]),"Kves10m094"]<-0 # non Kves category

is.nan.data.frame <- function(x)
        do.call(cbind, lapply(x, is.nan))

e.body[is.nan(e.body)]<-NA
body_extract<-data.frame(e.body[,-1],coo,stringsAsFactors = F)


czech_grids<-body_extract[!is.na(body_extract$elevation_10m094),]
names(czech_grids)

saveRDS(czech_grids,"d:\\Git\\chmi_station_data\\data_metadata\\coords_time_CZG_extract.rds")


