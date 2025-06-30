#set the working directory
setwd("~/Desktop/USRA/websites study")

#load the library that we need for this project
library(ncdf4)
library(lattice)
library(classInt)
library(RColorBrewer)
library(ggplot2)

# extract data from target point ------------------------------------------

#extract() from terra package or


#import the data
gcdv3 <- read.csv("GCDv3_MapData_Fig1.csv") 
gcdv3_sf <- st_as_sf(gcdv3, coords = c("Long", "Lat"))

ncin<-nc_open("cru10min30_bio.nc")
print(ncin)

#get the longitude and latitudes
lon<-ncvar_get(ncin,"lon")
nlon<-dim(lon)
head(lon)

lat<-ncvar_get(ncin, "lat")
nlat<-dim(lat)
head(lat)

#get the mtco data
mtco<-ncvar_get(ncin,"mtco")
dlname<-ncatt_get(ncin,"mtco","long_name")
dunits<-ncatt_get(ncin,"mtco","units")
fillvalue<-ncatt_get(ncin,"mtco","_FillValue")

mtco[mtco == fillvalue$value]<-NA
nc_close(ncin)

# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(mtco ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=TRUE, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

ggplot()+
  geom_sf(data = gcdv3_sf)


#calculate which grid each of the point belong to
j <- sapply(gcdv3$Lon, function(x) which.min(abs(lon-x)))
k <- sapply(gcdv3$Lat, function(x) which.min(abs(lat-x)))
head(cbind(j,k)); tail(cbind(j,k))

#transform the data into a vector and get their corresponding position
mtco_vec <- as.vector(mtco)
jk <- (k-1)*nlon + j
gcdv3_mtco <- mtco_vec[jk]
head(cbind(j,k,jk,gcdv3_mtco,lon[j],lat[k]))

gcdv3_mtco[is.na(gcdv3_mtco)] <- -99
pts <- data.frame(gcdv3$Lon, gcdv3$Lat, gcdv3_mtco)
names(pts) <- c("Lon", "Lat", "mtco")
head(pts, 20)

# Plot the targeted points w.r.t their temp
plotclr <- rev(brewer.pal(10,"RdBu"))
plotclr <- c("#AAAAAA", plotclr)#missing value as grey
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
color_class <- findInterval(gcdv3_mtco, cutpts)
plot(gcdv3$Long, gcdv3$Lat, col=plotclr[color_class+1], pch=16)

#using ggplot to draw the graph with the grids as well
gcdv3$color_class <- color_class
ggplot(gcdv3) +
  geom_point(aes(x = Long, y = Lat, color = factor(color_class)), size = 2) +
  scale_color_manual(values = plotclr) +
  theme_minimal()


