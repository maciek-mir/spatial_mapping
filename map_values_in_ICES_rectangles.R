library(sf)
library(data.table)
library(ggplot2)

# Load shapefile with water
# Shapefile downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=1902
water<-st_read("shapefiles/iho_Atlantic/iho.shp")

# Load shapefile with ICES rectangles
# Shapefile downloaded from https://gis.ices.dk/sf/index.html?widget=StatRec (Quick downloads)
ICESrect<-st_read("shapefiles/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp")

# Load input data
dummyEffort <- fread("data/dummyData.csv", colClasses = c("integer","character","numeric"))
dummyEffort$year<-factor(dummyEffort$year,levels=unique(sort(dummyEffort$year)))

###########################
# Map of the total effort #
###########################
dataToPlot<-dummyEffort[,.(effort=sum(effort)), by=.(ices_rect)]

# Merge data with ICES rectangles spatial dataset
dataToPlot<-merge(ICESrect,dataToPlot,by.x="ICESNAME", by.y="ices_rect")

longMin<-min(dataToPlot$WEST)-0.1
longMax<-max(dataToPlot$EAST)+0.1
latMin<-min(dataToPlot$SOUTH)-0.2
latMax<-max(dataToPlot$NORTH)+0.2


# Map 1
m <- ggplot()+
  geom_sf(data=water,fill="lightblue1")+
  geom_sf(data=ICESrect, fill=NA)+
  geom_sf(data=dataToPlot, aes(fill=effort),alpha=0.5)+
  coord_sf(xlim = c(longMin,longMax), ylim = c(latMin,latMax))+
  geom_sf_text(data = ICESrect, aes(label=ICESNAME),size=3)+
  theme(panel.background = element_rect(fill="gray95"))+
  scale_fill_continuous(low = "yellow", high = "red")+
  scale_x_continuous(breaks = seq(floor(longMin),ceiling(longMax), by = 1))+
  scale_y_continuous(breaks = seq(floor(latMin),ceiling(latMax), by = 0.5))+
  labs(x = NULL, y = NULL, 
       fill = paste0("Effort"),
       title = paste0("Effort"))
ggsave(filename = "output/Effort.png", plot=m, height = 10, width = 10)


###################################
# Map of the total effort by year #
###################################

dataToPlot<-dummyEffort

# Merge data with ICES rectangles spatial dataset
dataToPlot<-merge(ICESrect,dataToPlot,by.x="ICESNAME", by.y="ices_rect")

longMin<-min(dataToPlot$WEST)-0.1
longMax<-max(dataToPlot$EAST)+0.1
latMin<-min(dataToPlot$SOUTH)-0.2
latMax<-max(dataToPlot$NORTH)+0.2

# Map 2
m <- ggplot()+
  geom_sf(data=water,fill="lightblue1")+
  geom_sf(data=ICESrect, fill=NA)+
  geom_sf(data=dataToPlot, aes(fill=effort),alpha=0.5)+
  coord_sf(xlim = c(longMin,longMax), ylim = c(latMin,latMax))+
  geom_sf_text(data = ICESrect, aes(label=ICESNAME),size=3)+
  theme(panel.background = element_rect(fill="gray95"))+
  scale_fill_continuous(low = "yellow", high = "red")+
  scale_x_continuous(breaks = seq(floor(longMin),ceiling(longMax), by = 1))+
  scale_y_continuous(breaks = seq(floor(latMin),ceiling(latMax), by = 0.5))+
  labs(x = NULL, y = NULL, 
       fill = paste0("Effort"),
       title = paste0("Effort by year"))+
  facet_wrap(~year,ncol=2,drop=F)
ggsave(filename = "output/Effort_by_year.png", plot=m, height = 10, width = 10)
