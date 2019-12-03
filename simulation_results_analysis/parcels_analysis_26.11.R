pacman::p_load(data.table, dplyr, tidyr, sf, ggplot2, leaflet, tmap, randomcoloR)


path_parcels = "C:/models/freightFlows/output/"
parcels_filename = "/parcels.csv"

scenarios = c("test_scenario_weight")

parcels = data.frame()

for (scenario in scenarios){
  this_parcels = fread(paste(path_parcels, scenario, parcels_filename, sep  =""))
  this_parcels$scenario = scenario
  
  parcels = parcels %>% bind_rows(this_parcels)
  rm(this_parcels)  
}



parcels_by_dc = parcels %>%
  filter(toDestination, distributionType != "PARCEL_SHOP") %>%
  group_by(distributionCenter, scenario) %>% summarize(parcels = n()) %>% 
  spread(scenario, parcels)

write.table(parcels_by_dc, "clipboard", sep  ="\t", row.names= F)


parcels_by_zone = parcels %>%
  filter(toDestination, distributionType != "PARCEL_SHOP", transaction == "PRIVATE_CUSTOMER") %>%
  group_by(destMicroZone, scenario) %>% summarize(parcels = n()) %>% 
  spread(scenario, parcels)

write.table(parcels_by_zone, "clipboard", sep  ="\t", row.names= F)

path_dc = "C:/models/freightFlows/input/distributionCenters/distributionCenters.csv"

catchmentAreas = distribution_centers %>% filter(object == "catchmentArea", commodityGroup == "PACKET") %>% select(microZoneId, dcId)

write.table(catchmentAreas, "clipboard", sep  ="\t", row.names= F)




path_muc_shp = "C:/models/freightFlows/input/shp/zones_31468_jobs.shp"

zones_muc = st_read(path_muc_shp)
zones_muc = zones_muc %>% select(id, Population, Employment)

# path_reg_shp = "C:/models/freightFlows/input/shp/zones_regensburg_31468_jobs.shp"

# zones_reg = st_read(path_reg_shp)
# zones_reg = zones_reg %>% select(id, Population)


# zones = zones_muc %>% bind_rows(zones_reg)

zones_muc$area =  as.numeric(st_area(zones_muc)/1e6)

df_zones = data.frame(id = zones_muc$id , pp = zones_muc$Population,area = zones_muc$area,  jj = zones_muc$Employment)

df_zones = df_zones%>% filter(id %in% catchmentAreas$microZoneId)

write.table(df_zones, "clipboard-1000k", sep  ="\t", row.names= F)


distributionCenters = unique(catchmentAreas$dcId)

p =  tm_basemap(leaflet::providers$CartoDB) 

for (this_dc in distributionCenters){
  this_ca_zones = (catchmentAreas %>% filter(dcId == this_dc) %>% select(microZoneId))$microZoneId
  this_shp_ca = zones_muc %>% filter(id %in% this_ca_zones)
  
  p = p + tm_shape(shp = this_shp_ca, name = paste("dc_area",this_dc)) +
    tm_polygons(alpha = 0.8, border.alpha = 0, col = randomColor())
}

tmap_leaflet(p)




