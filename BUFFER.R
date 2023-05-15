################Title################
'Exploring Predictors of Domestic Cat 
Home Range Size Variability and 
Implications for Wildlife Management'

'PHILIPPE-LESAFFRE Martin'

'05/2023'
################Title################

################Section################
'BUFFER Computation'
################Section################


###Packages
require(readxl)
require(raster)
require(dplyr)
require(rasterVis)
require(sf)
require(sp)
require(rgeos)
require(terra)
require(sjstats)
require(plyr)


Appendix1 <- readxl::read_xlsx("Appendix1_LL.xlsx")
Appendix1 <- as.data.frame(Appendix1)
Appendix1 <- Appendix1 %>% dplyr::select(ID_chat,longitude,latitude) %>%
  distinct(ID_chat,.keep_all = T)
Appendix1_s = st_as_sf(Appendix1,coords=c("longitude","latitude"),crs= "+init=EPSG:4326")
Appendix1_s


MOS <- raster('MOS_raster.tif')
MOS <- as.factor(MOS)
levels(MOS)
tar<-levels(MOS)[[1]]
tar[["landcover"]]<-c("non-artif", "artif")
levels(MOS)<-tar
rasterVis::levelplot(MOS,col.regions=c("red","white"))


Appendix1_s_t <- st_transform(Appendix1_s, crs = st_crs(MOS))
Appendix1_buffer <- st_buffer(Appendix1_s_t, 100)
Appendix1_buffer
plot(Appendix1_buffer)
poly <- terra::extract(MOS,Appendix1_buffer)
prop <- list()
for(i in 1:56){
 data <- as.data.frame(poly[[i]])
 data <- data %>% mutate(occurence=1)
 data <- ddply(data,.(`poly[[i]]`),summarize,prop=sum(occurence)/length(data$`poly[[i]]`))
 data <- data[1,]$prop
 prop[[i]] <- data
}
prop100 <- do.call(rbind,prop)

Appendix1_s_t <- st_transform(Appendix1_s, crs = st_crs(MOS))
Appendix1_buffer <- st_buffer(Appendix1_s_t, 250)
Appendix1_buffer
plot(Appendix1_buffer)
poly <- terra::extract(MOS,Appendix1_buffer)
prop <- list()
for(i in 1:56){
  data <- as.data.frame(poly[[i]])
  data <- data %>% mutate(occurence=1)
  data <- ddply(data,.(`poly[[i]]`),summarize,prop=sum(occurence)/length(data$`poly[[i]]`))
  data <- data[1,]$prop
  prop[[i]] <- data
}
prop250 <- do.call(rbind,prop)

Appendix1_s_t <- st_transform(Appendix1_s, crs = st_crs(MOS))
Appendix1_buffer <- st_buffer(Appendix1_s_t, 500)
Appendix1_buffer
plot(Appendix1_buffer)
poly <- terra::extract(MOS,Appendix1_buffer)
prop <- list()
for(i in 1:56){
  data <- as.data.frame(poly[[i]])
  data <- data %>% mutate(occurence=1)
  data <- ddply(data,.(`poly[[i]]`),summarize,prop=sum(occurence)/length(data$`poly[[i]]`))
  data <- data[1,]$prop
  prop[[i]] <- data
}
prop500 <- do.call(rbind,prop)


Appendix1 <- cbind(Appendix1,prop100,prop250,prop500)


saveRDS(Appendix1,'BUFFER.RDS')
