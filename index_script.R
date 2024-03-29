
list.of.packages <- c("sf","raster","mapview","whitebox","randomcoloR","leaflet","Rcpp")
install.packages(list.of.packages)

# install.packages("devtools")
# install.packages('raster', repos='https://rspatial.r-universe.dev')
# library(devtools)
# install_github("r-spatial/sf")

library(sf)
library(raster)
library(mapview)
library(randomcoloR)
library(leaflet)

## Načítání vektorových dat shp
# nastavte kde máte u sebe na PC data
# pozor musíte zdvojit nebo otočit lomítka
cesta<-"d:/Git/gisproba/data/"
# zkonstuuje cestu kde leží vrstva Brdy
data.path<-paste0(cesta,"CHKO_Brdy.shp")
# načte .shp do R
brdy<-st_read(data.path,stringsAsFactors = F,quiet = T)

# obdobně načteme třeba hranici ČR
data.path<-paste0(cesta,"hrcr1_wgs.shp")
hrcr<-st_read(data.path,stringsAsFactors = F,quiet = T)

# prostý obrázek, bez interaktivity
plot(st_geometry(hrcr)) 
# parametr add přidá další vrstvu do existujícího obrázku
plot(st_geometry(brdy),col="red",add=T) 

# Nastaví pracovní adresář working dorectory (WD)
# dále nemusíme data z WD volat celou cestou, stačí názvy 
setwd(cesta) 
## Načítání vektorových dat z tabulky
# načíst prostou tabulku
chmu<-read.table("staniceCHMUtablecoma.csv",header = T, sep=",")
# převést tabulky na prostorová data
chmu_sf<-st_as_sf(chmu, coords = c("Xcoo", "Ycoo"),crs = 4326) 

## vizualizovat prostorová data interaktivně
# pokročilejší balíček leaflet

# definujeme paletu o 17 barvách podle typu stanice
distCol<- colorFactor(distinctColorPalette(17), chmu_sf$Station.ty)

# definoujeme okno s mapou 
leaflet(chmu_sf) %>% 
  addTiles() %>%  
  addCircleMarkers(popup=chmu_sf$Name,color = ~distCol(Station.ty),
                   stroke = FALSE, fillOpacity = 0.8,radius=4) %>%
  addLegend(pal = distCol, values = ~Station.ty)

# -----------------------------------------------------------
# st_crs(brdy) # jaký crs má vrstva brdy ?
# st_crs(hrcr) # jaký crs má vrstva hrnice čr ?

## vektory
# transformace podle EPSG
brdy_32633<-st_transform(brdy,32633)
# st_crs(brdy_32633) # jaký crs má vrstva brdy_32633 ? ---------------------

# transformace podle proj4 string
brdy_5514<-st_transform(brdy, "+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=30.28813972222222 +k=0.9999 +x_0=0 +y_0=0 +ellps=bessel +pm=greenwich +units=m +no_defs +towgs84=570.8,85.7,462.8,4.998,1.587,5.261,3.56")
# st_crs(brdy_5514) # jaký crs má vrstva brdy_5514 ? ---------------------

# transformace podle jiné existující vrstvy
brdy_krovak<-st_transform(brdy, st_crs(brdy_5514))
# st_crs(brdy_krovak) # jaký crs má vrstva brdy_5514 ? ---------------------

# kde jsou ty brdy? no nevidím je protože to má jiný CRS
plot(st_geometry(hrcr),main="Kde jsou ty Brdy?") 
plot(st_geometry(brdy_32633),add=T) 

# musíme tedy transformovat jedno nebo druhé
hrcr_32633<-st_transform(hrcr, 32633)
plot(st_geometry(hrcr_32633),main="No jo, už máme správný CRS")
plot(st_geometry(brdy_32633),add=T,col="red")


# načítání jdnoduchou funkcí "raster"
DEM<-raster(paste0(cesta,"DEM_Jested_100m.tif"))
# kontrolí obrázek
# data tu jsou
plot(DEM)



## Past s Křovákem
# http://freegis.fsv.cvut.cz/gwiki/S-JTSK
crs(DEM)
crs(DEM)<-"+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=30.28813972222222 +k=0.9999 +x_0=0 +y_0=0 +ellps=bessel +pm=greenwich +units=m +no_defs +towgs84=570.8,85.7,462.8,4.998,1.587,5.261,3.56"

# i rastr je možné vizualizovat interaktivně
# definujeme barvy
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(DEM),
                    na.color = "transparent")

leaflet(DEM) %>% 
   addTiles() %>%  
   addRasterImage(DEM,colors = pal, opacity = 0.8) %>%
   addLegend(pal = pal, values = values(DEM),
             title = "Elevation [m]")


# buffer 5 km
brdy_5000<-st_buffer(brdy_32633,5000)
plot(st_geometry(brdy_5000), main="buffer 5 km")
plot(st_geometry(brdy_32633),add=T,col="red")

# plocha polygonů
st_area(brdy)

# centroidy
brdy_cen<-st_centroid(brdy_32633) 
plot(st_geometry(brdy_32633), main="centroid",graticule=T,axes=T)
plot(st_geometry(brdy_cen),add=T,col="blue",pch=19)

# průnik
chmu_brdy<-st_intersection(brdy,chmu_sf)
plot(st_geometry(brdy), main="intersection - chmu stanice v brdech",graticule=T,axes=T)
plot(st_geometry(chmu_brdy),add=T,col="red",pch=19)


# načteme všechny cesty k shp vrstvám v daném adresáři
parky.files<-list.files(path=cesta,pattern="*.shp$",full.names = T)
parky.files<-parky.files[-1]

## když chceme všechny soubor do jednoho objektu
# načte první shpfile
parky.init<-st_read(parky.files[1],quiet = TRUE)

# připojí všechny další shp file do jendoho objektu  
for (i in 2:length(parky.files)) {
  parky.next<-st_read(parky.files[i],quiet = TRUE)
  parky.init<-rbind(parky.init,parky.next)
}  

head(parky.init)
plot(st_geometry(hrcr))
plot(st_geometry(parky.init),add=T,col="green")


# připojit metadat GIS join
meta.path<-paste0(cesta,"metadata_parky.csv")
metadata<-read.table(meta.path,sep=";",header=T,stringsAsFactors = T)
metadata$NAZEV<-iconv(metadata$NAZEV,from="windows-1250",to="UTF-8")
parky<-merge(parky.init,metadata)

head(parky)
plot(st_geometry(hrcr))
plot(st_geometry(parky),add=T,col="red")


# interaktivně
distCol<- colorFactor(distinctColorPalette(32), parky.init$OBJECTID)
leaflet(parky) %>% 
  addTiles() %>%  
  addPolygons(color = ~distCol(OBJECTID),
              stroke = FALSE, fillOpacity = 0.8,
              label = paste0(parky$KAT,"_",parky$NAZEV))

## instalace balíčku whitebox pro rastrové analýzy
# install.packages("whitebox", repos="http://R-Forge.R-project.org") # instalace
# whitebox::wbt_init() # inicializace

library(whitebox)

# pozor na diakritiku a mezery v cestě. Pro WBT nesmí být
# cesta k rastrovým datům
dem <-("d:/Git/gisproba/data/DEM_Jested_100m.tif")

## stínovaný reliéf
# cesta k budoucímu výsledku
output<- ("c:/Users/Public/Documents/Hillshade_100m.tif")

# spustí algoitmus pro výpočet stinovaného reliéfu 
wbt_hillshade(dem, output, azimuth = 315, altitude = 30, 
              zfactor = 1,verbose_mode = FALSE)
# načtu výsledek jako rastr
hill<-raster(output)
# načtu původní dem jako rastr
elev<-raster(dem)
# vytisknu oba obrázky pro kontrolu
plot(hill, col=grey(0:100/100), legend=FALSE,main="stínovaný reliéf")
plot(elev, col=rainbow(25, alpha=0.35), add=TRUE)


## Sklon svahu
output<- ("c:/Users/Public/Documents/slope_100m.tif")
wbt_slope(dem,output,zfactor = 1, verbose_mode = FALSE)
slp<-raster(output)
plot(hill, col=grey(0:100/100), legend=FALSE,main="Sklon svahu")
plot(slp, col=heat.colors(25, alpha=0.2), add=TRUE)

## The terrain ruggedness index (TRI)
output<- ("c:/Users/Public/Documents/TRI_100m.tif")
wbt_ruggedness_index(dem,output,zfactor = 1, verbose_mode = FALSE)
tri<-raster(output)
plot(hill, col=grey(0:100/100), legend=FALSE,main="terrain ruggedness index")
plot(tri, col=cm.colors(25, alpha=0.3), add=TRUE)





