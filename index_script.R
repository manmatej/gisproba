# Knihovny

# Seznam potřebných balíčků
# list.of.packages <- c("sf", "terra", "mapview", "randomcoloR", "leaflet", "RColorBrewer")
# install.packages(list.of.packages)

library(RColorBrewer)
library(sf)
library(terra)
library(mapview)
library(randomcoloR)
library(leaflet)

# Načítání vektorových GIS dat do R

## Načítání vektorových dat (shp souborů)
# Nastavte cestu k vašim datům
cesta <- "d:/Git/gisproba/data/"

# Načtení vrstvy Brdy
data.path <- paste0(cesta, "CHKO_Brdy.shp")
brdy <- st_read(data.path, stringsAsFactors = FALSE, quiet = TRUE)

# Načtení vrstvy hranice ČR
data.path <- paste0(cesta, "hrcr1_wgs.shp")
hrcr <- st_read(data.path, stringsAsFactors = FALSE, quiet = TRUE)

# Jednoduchý plot
plot(st_geometry(hrcr))
# Přidání vrstvy Brdy do existujícího plotu
plot(st_geometry(brdy), col = "red", add = TRUE)

# Nastavení pracovního adresáře
setwd(cesta)

## Načítání vektorových dat z tabulky
# Načtení tabulky
chmu <- read.table("staniceCHMUtablecoma.csv", header = TRUE, sep = ",")
# Převod tabulky na prostorová data
chmu_sf <- st_as_sf(chmu, coords = c("Xcoo", "Ycoo"), crs = 4326)

## Interaktivní vizualizace prostorových dat
# Definujeme paletu 17 barev podle typu stanice
distCol <- colorFactor(distinctColorPalette(17), chmu_sf$Station.ty)

# Vytvoření interaktivní mapy pomocí leaflet
leaflet(chmu_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    popup = chmu_sf$Name,
    color = ~distCol(Station.ty),
    stroke = FALSE,
    fillOpacity = 0.8,
    radius = 4
  ) %>%
  addLegend(pal = distCol, values = ~Station.ty)

# Projekce a souřadnicové systémy (CRS)

# Kontrola CRS vrstev
# st_crs(brdy) # Jaký CRS má vrstva brdy?
# st_crs(hrcr) # Jaký CRS má vrstva hranice ČR?

## Transformace vektorových dat
# Transformace podle EPSG kódu
brdy_32633 <- st_transform(brdy, 32633)
# st_crs(brdy_32633) # Jaký CRS má vrstva brdy_32633?

# Transformace pomocí proj4 stringu
brdy_5514 <- st_transform(
  brdy,
  "+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=30.28813972222222
   +k=0.9999 +x_0=0 +y_0=0 +ellps=bessel +pm=greenwich +units=m +no_defs
   +towgs84=570.8,85.7,462.8,4.998,1.587,5.261,3.56"
)
# st_crs(brdy_5514) # Jaký CRS má vrstva brdy_5514?

# Transformace podle CRS jiné vrstvy
brdy_krovak <- st_transform(brdy, st_crs(brdy_5514))
# st_crs(brdy_krovak) # Jaký CRS má vrstva brdy_krovak?

# Plot pro kontrolu umístění
plot(st_geometry(hrcr), main = "Kde jsou ty Brdy?")
plot(st_geometry(brdy_32633), add = TRUE)

# Transformace hranice ČR do stejného CRS
hrcr_32633 <- st_transform(hrcr, 32633)
plot(st_geometry(hrcr_32633), main = "No jo, už máme správný CRS")
plot(st_geometry(brdy_32633), add = TRUE, col = "red")

# Načítání rastrových GIS dat do R

# Načtení rastrových dat pomocí terra
DEM <- rast(paste0(cesta, "DEM_Jested_100m.tif"))
# Kontrolní plot
plot(DEM)

# Nastavení správného CRS pro DEM
crs(DEM) <- "+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333
             +alpha=30.28813972222222 +k=0.9999 +x_0=0 +y_0=0
             +ellps=bessel +pm=greenwich +units=m +no_defs
             +towgs84=570.8,85.7,462.8,4.998,1.587,5.261,3.56"

# Interaktivní vizualizace rastru pomocí leaflet
pal <- colorNumeric(
  c("#0C2C84", "#41B6C4", "#FFFFCC"),
  values(DEM),
  na.color = "transparent"
)

leaflet() %>%
  addTiles() %>%
  addRasterImage(DEM, colors = pal, opacity = 0.8) %>%
  addLegend(
    pal = pal,
    values = values(DEM),
    title = "Elevation [m]"
  )

# Příklad vektorové analýzy

# Výpočet plochy polygonů
st_area(brdy)

# Vytvoření bufferu 5 km kolem Brd
brdy_5000 <- st_buffer(brdy_32633, 5000)
plot(st_geometry(brdy_5000), main = "Buffer 5 km")
plot(st_geometry(brdy_32633), add = TRUE, col = "red")

# Výpočet centroidů
brdy_cen <- st_centroid(brdy_32633)
plot(st_geometry(brdy_32633), main = "Centroid", graticule = TRUE, axes = TRUE)
plot(st_geometry(brdy_cen), add = TRUE, col = "blue", pch = 19)

# Průnik stanic CHMU a Brd
chmu_brdy <- st_intersection(brdy, chmu_sf)
plot(
  st_geometry(brdy),
  main = "Průnik - CHMU stanice v Brdech",
  graticule = TRUE,
  axes = TRUE
)
plot(st_geometry(chmu_brdy), add = TRUE, col = "red", pch = 19)

# Hromadné zpracování dat

# Načtení všech SHP souborů v adresáři
parky.files <- list.files(path = cesta, pattern = "*.shp$", full.names = TRUE)
parky.files <- parky.files[-1]

## Kombinace všech souborů do jednoho objektu
# Načtení prvního SHP souboru
parky.init <- st_read(parky.files[1], quiet = TRUE)

# Postupné připojení dalších souborů
for (i in 2:length(parky.files)) {
  parky.next <- st_read(parky.files[i], quiet = TRUE)
  parky.init <- rbind(parky.init, parky.next)
}

head(parky.init)
plot(st_geometry(hrcr))
plot(st_geometry(parky.init), add = TRUE, col = "green")

# Připojení metadata (GIS join)
meta.path <- paste0(cesta, "metadata_parky.csv")
metadata <- read.table(meta.path, sep = ";", header = TRUE, stringsAsFactors = TRUE)
metadata$NAZEV <- iconv(metadata$NAZEV, from = "windows-1250", to = "UTF-8")
parky <- merge(parky.init, metadata)

head(parky)
plot(st_geometry(hrcr))
plot(st_geometry(parky), add = TRUE, col = "red")

# Interaktivní vizualizace
distCol <- colorFactor(distinctColorPalette(32), parky.init$OBJECTID)
leaflet(parky) %>%
  addTiles() %>%
  addPolygons(
    color = ~distCol(OBJECTID),
    stroke = FALSE,
    fillOpacity = 0.8,
    label = paste0(parky$KAT, "_", parky$NAZEV)
  )

# Rastrové analýzy pomocí balíčku terra

# Načtení DEM
dem <- rast(paste0(cesta, "DEM_Jested_100m.tif"))
crs(dem) <- "+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333
             +alpha=30.28813972222222 +k=0.9999 +x_0=0 +y_0=0
             +ellps=bessel +units=m +no_defs"

## Stínovaný reliéf
# Vypočítáme sklon a orientaci svahu v radiánech
slope_rad <- terrain(dem, 'slope', unit = 'radians')
aspect_rad <- terrain(dem, 'aspect', unit = 'radians')
# Vypočítáme stínovaný reliéf
hill <- shade(slope_rad, aspect_rad, angle = 30, direction = 315)

# Definujeme kartografickou paletu pro výšku
elevation_palette <- terrain.colors(25, alpha = 0.35)

# Vykreslíme výsledky
plot(hill, col = grey(0:100/100), legend = FALSE, main = "Stínovaný reliéf")
plot(dem, col = elevation_palette, add = TRUE)

## Sklon svahu
# Vypočítáme sklon ve stupních
slope_deg <- terrain(dem, 'slope', unit = 'degrees')

# Definujeme barevnou paletu pro sklon
slope_palette <- brewer.pal(9, "YlOrRd")  # Žlutá až červená
slope_colors <- colorRampPalette(slope_palette)(100)

# Vykreslíme sklon svahu
plot(hill, col = grey(0:100/100), legend = FALSE, main = "Sklon svahu")
plot(slope_deg, col = slope_colors, add = TRUE)

## Terrain Ruggedness Index (TRI)
# Vypočítáme TRI
tri_raster <- terrain(dem, 'TRI')

# Definujeme barevnou paletu pro TRI
tri_palette <- brewer.pal(9, "PuBuGn")  # Fialová až modrozelená
tri_colors <- colorRampPalette(tri_palette)(100)

# Vykreslíme TRI
plot(hill, col = grey(0:100/100), legend = FALSE, main = "Terrain Ruggedness Index")
plot(tri_raster, col = tri_colors, add = TRUE)
