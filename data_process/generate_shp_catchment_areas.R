fileZonesShp = "C:/models/freightFlows/input/shp/zones_31468_jobs.shp"

muc_shp = st_read(fileZonesShp)


file_dcs = "C:/models/freightFlows/input/distributionCenters/distributionCenters_1km.csv"

dc_1km = read_csv(file_dcs)


output_folder = "c:/projects/radLast/analysis/scenarios_paper_ists_2020/dcAreas/"


dcs = unique(dc_1km$dcId)


for (dc in dcs){
  
  zones_this_dc = dc_1km %>% filter(dcId == dc, object == "catchmentArea")
  zones_this_dc  = zones_this_dc$microZoneId
  
  shp_this_dc = muc_shp %>% filter(id %in% zones_this_dc)
  st_write(shp_this_dc, paste(output_folder, dc, ".shp", sep = ""))
  
  
}
