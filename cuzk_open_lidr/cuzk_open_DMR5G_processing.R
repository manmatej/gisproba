## ============================= lidR ==================================
# https://r-lidar.github.io/lidRbook/engine.html

# install.packages("lidR")
# install.packages("sf")
# install.packages("terra")
# install.packages("gstat")
# install.packages("future")

library(lidR)
library(sf)
library(terra)
library(gstat)
library(future)


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## how to download DMR data from CUZK
## https://ags.cuzk.cz/geoprohlizec/?atom=dmr5g
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

loc<-r"(d:\Git\gisproba\cuzk_open_lidr\tiles)"
zips<-list.files(loc,pattern = "*.zip$",full.names = T) # list paths to all zip files in current directory

unzips<-tempdir()
setwd(unzips)


for (i in 1:length(zips)){
  unzip(zips[i],exdir=unzips)  # unzip your file 
}

fil<-list.files(unzips,full.names = T,pattern = "*.laz$")
ctg <- readLAScatalog(fil)
st_crs(ctg)<-5514
plot(ctg)
las_check(ctg)
summary(ctg)

opt_output_files(ctg)<- paste0(tempdir(), "/{XCENTER}_{YCENTER}_{ID}")
cg<-classify_ground(ctg, algorithm = pmf(ws = 5, th = 3))
dtm <- rasterize_terrain(cg, res=10, tin())

setwd(loc)
writeRaster(dtm,"dtm_10m.tif")
plot(dtm)
