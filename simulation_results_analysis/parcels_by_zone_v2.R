pacman::p_load(dplyr, readr, sf, ggplot2, tmap, leaflet)

folder = "C:/Users/carlloga/LRZ Sync+Share/RadLast (Pirmin Fontaine)/Demand/data_20200703/"

#read parcels individual data
parcels_muc <- read_csv(paste(folder, "parcels_muc.csv", sep = ""))
parcels_reg <- read_csv(paste(folder, "parcels_reg.csv", sep = ""))

#filter to exclude parcels via parcel shop
parcels_muc_filtered = parcels_muc %>% filter(transaction != "PARCEL_SHOP")
parcels_reg_filtered = parcels_reg %>% filter(transaction != "PARCEL_SHOP")

#read grids (created in qgis)
zones_muc = st_read(paste(folder, "rasters/grid_4km_muc_31468.shp", sep = ""))
zones_reg = st_read(paste(folder, "rasters/grid_4km_reg_31468.shp", sep = ""))

#assign coordinates in the same column for pick ups and deliveries (the info is still in variable toDestination)
parcels_muc_filtered = parcels_muc_filtered %>%
  mutate(x = as.numeric(if_else(toDestination, destX, originX))) %>%
  mutate(y = as.numeric(if_else(toDestination, destY, originY)))

parcels_reg_filtered = parcels_reg_filtered %>%
  mutate(x = as.numeric(if_else(toDestination, destX, originX))) %>%
  mutate(y = as.numeric(if_else(toDestination, destY, originY)))


#a function to test whether a parcel is whithin a zone
get_raster_from_coordinates = function(x, y, zones){
  
  for (i in 1:nrow(zones)){
    if (x < zones$right[i] & x > zones$left[i] & y < zones$top[i] & y > zones$bottom[i]){
      return(zones$id[i])
    }
  }
  return(-1)
  
}

#assign raster zones to parcels
parcels_muc_filtered = parcels_muc_filtered %>% rowwise() %>%
  mutate(raster_zone = get_raster_from_coordinates(x,y,zones_muc))


parcels_reg_filtered = parcels_reg_filtered %>% rowwise() %>%
  mutate(raster_zone = get_raster_from_coordinates(x,y,zones_reg))


#aggregate parcels by zone, direction(to/from), distribution center and market
aggregated_muc_1 = parcels_muc_filtered %>%
  group_by(raster_zone, toDestination, distributionCenter, transaction) %>%
  summarize(parcels = n())
aggregated_reg_1 = parcels_reg_filtered %>%
  group_by(raster_zone, toDestination, distributionCenter, transaction) %>%
  summarize(parcels = n())

#test totals
sum(aggregated_muc_1$parcels, na.rm  = T)
sum(aggregated_reg_1$parcels, na.rm  = T)


#print out this files
write_csv(aggregated_muc_1, paste(folder, "parcels_by_zone_dc_direction_and_segment_muc.csv", sep = ""))
write_csv(aggregated_reg_1, paste(folder, "parcels_by_zone_dc_direction_and_segment_reg.csv", sep = ""))

#alternative aggregate parcels by zone (all)
aggregated_muc_2 = parcels_muc_filtered %>%
  group_by(raster_zone) %>%
  summarize(parcels = n())
aggregated_reg_2 = parcels_reg_filtered %>%
  group_by(raster_zone) %>%
  summarize(parcels = n())

#merge with some shapefile (grid) contains dimensions and border coordinates
zones_muc = zones_muc %>% left_join(aggregated_muc_2, by = c("id" = "raster_zone"))
zones_reg = zones_reg %>% left_join(aggregated_reg_2, by = c("id" = "raster_zone"))

#test 1 - totals
sum(zones_muc$parcels, na.rm  = T)
sum(zones_reg$parcels, na.rm  = T)

#print out this files
write_csv(aggregated_muc_2, paste(folder, "parcels_by_zone_muc.csv", sep = ""))
write_csv(aggregated_reg_2, paste(folder, "parcels_by_zone_reg.csv", sep = ""))

#map of density

muc_map = tm_basemap(leaflet::providers$CartoDB) + 
  tm_shape( zones_muc, "Munich") + 
  tm_polygons(col = "parcels", convert2density = T, style = "quantile", alpha = 0.5)
tmap_leaflet(muc_map)

reg_map = tm_basemap(leaflet::providers$CartoDB) + 
  tm_shape(zones_reg, "Regensburg") + 
  tm_polygons(col = "parcels", convert2density = T, style = "quantile", alpha = 0.5)
tmap_leaflet(reg_map)

#print out the raster cells with geometry information
write_csv(st_drop_geometry(zones_muc), paste(folder, "raster_muc.csv", sep = ""))
write_csv(st_drop_geometry(zones_reg), paste(folder, "raster_reg.csv", sep = ""))


muc_map = tm_basemap(leaflet::providers$CartoDB) + 
  tm_shape( zones_muc, "Munich") + 
  tm_polygons(col = "red", convert2density = T, style = "quantile", alpha = 0.5)
tmap_leaflet(muc_map)

reg_map = tm_basemap(leaflet::providers$CartoDB) + 
  tm_shape(zones_reg, "Regensburg") + 
  tm_polygons(col = "red", convert2density = T, style = "quantile", alpha = 0.5)
tmap_leaflet(reg_map)




parcels_muc_filtered %>% mutate(class = cut(weight_kg, breaks = c(0,4.1,9.3,17,100))) %>% group_by(class) %>% summarize(n()/nrow(parcels_muc_filtered))


