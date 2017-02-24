library(ggplot2)
library(rgdal)
library(maptools)
library(dplyr)
library(mapproj)
library(rgeos)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(cleangeo)
library(shinythemes)
library(htmltools)

setwd("C:/Users/PYSung/Desktop/Statistic/competition/2017 competition/自來水公司/Data/")
# setwd("C:/Users/PYSung/Desktop/Statistic/competition/2016 competition/DSP fire warning system")
WaterSupply_data = read.csv("供水系統供水資料.csv")
People_data = read.csv("SupplyPopulationAll.csv", header = TRUE, skip = 1, stringsAsFactors = FALSE)
People_data = People_data[-length(People_data[,1]),]


People_data[is.na(People_data)] = "" ##NA replace to ""
#############merge county-town/remove space char and total count of county############
County_people = c()
index_remove = c()
initial_county = People_data[2, 1]
for (i in 1:length(People_data[,1])){
  check_point = People_data[i, 1]
  if (check_point == initial_county) index_remove = c(index_remove, i)
  if (check_point == "") initial_county = People_data[i+1, 1]
  County_people = c(County_people, initial_county)
}

County_town_people = paste(County_people, People_data[,1], sep = "-")
County_town_people = gsub(" ", "", County_town_people, fixed = TRUE)

People_data = data.frame(County_town = County_town_people, People_data, stringsAsFactors = FALSE)
index_remove = c(1, index_remove, which(People_data[,3]==""))
People_data = People_data[-index_remove,]
####################################################################################



# town_data = readOGR(path.expand("TownArea/TOWN_MOI_1051214.shp"),"TOWN_MOI_1051214",encoding="WINDOWS-1252")
town_data = readOGR(dsn = "TownArea", layer = "TOWN_MOI_1051214", use_iconv = TRUE, encoding = "UTF-8")

pathFolder = "SystemArea"
shps = dir(pathFolder, "*.shp")
shps = gsub('.shp', '', shps)

system_1_data = readOGR(path.expand("SystemArea/1_20150105.shp"),"1_20150105",encoding="UTF-8")

# town_data = readOGR(path.expand("TownArea/TOWN_MOI_1051214.shp"),"TOWN_MOI_1051214",encoding="WINDOWS-1252")

proj4string(system_1_data) = "+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=aust_SA +units=m +no_defs"
pro = sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs')
dat.supp = sp::spTransform(system_1_data, pro)
# dat.supp = clgeo_Clean(dat.pre, verbose = TRUE)

# i <- 8
for (i in 2:length(shps)) {
  print(i)
  reg.i <- readOGR(dsn = pathFolder, layer = shps[i], encoding = "UTF-8", 
                   stringsAsFactors = F, verbose = F)
  proj4string(reg.i) <- "+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=aust_SA +units=m +no_defs"
  dat.i <- sp::spTransform(reg.i, pro)
  # dat.supp = clgeo_Clean(dat.i, verbose = TRUE)
  dat.supp <- raster::bind(dat.supp, dat.i)
}

# system_data = clgeo_Clean(dat.supp, verbose = TRUE)


dat.supp@data$sname
town_data@data$TOWNNAME
county_town = paste(town_data$COUNTYNAME, town_data$TOWNNAME, sep = "-")
# pt1i_1 <- iconv(town_data@data$TOWNNAME[1], from="big5", to="UTF-8")
# tmp <- '\xe4\xb8\xad\xe6\x96\x87'

town_center = data.frame(cbind(coordinates(town_data), county_town), stringsAsFactors = F)
colnames(town_center) = c("long", "lat", "County_Town")
town_center$long = as.numeric(town_center$long)
town_center$lat = as.numeric(town_center$lat)

system = SpatialPolygons(dat.supp@polygons)
town_center_sp = SpatialPoints(town_center[, c(1, 2)])
system_index = over(town_center_sp, system)
system_town = dat.supp@data$sname[system_index]
system_town_data = data.frame(System = system_town, County_town = town_center[,3], stringsAsFactors = FALSE)

# saveRDS(dat.supp, "data/rdata/dat_supp.rds")
# dat.supp <- readRDS("data/rdata/dat_supp.rds")

#########merge People Usage data to correponding System############
bigTable_PeopleSupply = left_join(system_town_data, People_data, by = c("County_town"))
colnames(bigTable_PeopleSupply) = c("System","County_town","Town",
                                    "PeopleCount_102","PeopleSupply_102","coverage_102",
                                    "PeopleCount_103","PeopleSupply_103","coverage_103",
                                    "PeopleCount_104","PeopleSupply_104","coverage_104")

######batch change to numeric column#########
sapply(bigTable_PeopleSupply, class)
cols.num = colnames(bigTable_PeopleSupply)[4:12]
bigTable_PeopleSupply[cols.num] = sapply(bigTable_PeopleSupply[cols.num],as.numeric)
sapply(bigTable_PeopleSupply, class)

# write.csv(bigTable_PeopleSupply, "bigTable_PeopleSupply.csv")
# saveRDS(bigTable_PeopleSupply,"bigTable_PeopleSupply.rds")
# bigTable_PeopleSupply_rmNa = bigTable_PeopleSupply[!is.na(bigTable_PeopleSupply)]
System_PeopleSupply_102_104 = bigTable_PeopleSupply[,c(1,4:12)] %>% group_by(System) %>% 
  summarise_each(funs(sum))
System_PeopleSupply_102_104$coverage_102 = System_PeopleSupply_102_104$PeopleSupply_102/System_PeopleSupply_102_104$PeopleCount_102
System_PeopleSupply_102_104$coverage_103 = System_PeopleSupply_102_104$PeopleSupply_103/System_PeopleSupply_102_104$PeopleCount_103
System_PeopleSupply_102_104$coverage_104 = System_PeopleSupply_102_104$PeopleSupply_104/System_PeopleSupply_102_104$PeopleCount_104

colnames(System_PeopleSupply_102_104)[1] = c("sname")
# save(bigTable_PeopleSupply,file = "bigTable_PeopleSupply.rda")
# load("bigTable_PeopleSupply.rda")

dat.supp@data = left_join(dat.supp@data, System_PeopleSupply_102_104, by=c("sname"))

# sapply(dat.supp@data, class)
dat.supp@data$sname = as.factor(dat.supp@data$sname)
dat.supp@data$sname = droplevels(dat.supp@data$sname)

save(dat.supp,file = "data_supp.rda")
# dat.supp@data$coverage_102 = as.numeric(dat.supp@data$coverage_102)
###html label define###################
label_sname = mapply(function(x,y){
  HTML(sprintf("<b>%s</b></br>102 供水系統涵蓋率:%s</br>",
               htmlEscape(x),htmlEscape(y)))
},dat.supp@data$sname,round(dat.supp@data$coverage_102, digits = 3),SIMPLIFY = F)

####highlighter
my_highlighter = highlightOptions(color="black", 
                                   #opacity = 0.6,
                                   fillColor="#d2fa1b",
                                   fillOpacity=0.6)
color_fun = colorNumeric(c("#00FF40","#FF0000"), round(dat.supp@data$coverage_102,digits = 3))

pal <- colorNumeric(
  palette = c("yellow","pink","red"),  #"YlGnBu"
  domain = round(dat.supp@data$coverage_102, digits = 3)
)

m = leaflet() %>% 
  addProviderTiles("Esri.WorldStreetMap") %>% 
  addPolygons(
    data=dat.supp,
    color="black",weight=1,   #weight: color線的寬度
    label= label_sname,
    labelOptions = lapply(1:171, function(x) {
      labelOptions(textsize='15px')
    }),
    highlightOptions = my_highlighter,
    fillColor=~pal(coverage_102) #地圖顏色
  )
library(htmlwidgets)
saveWidget(m, 'SystemCoverageRate.html', selfcontained = FALSE)

  