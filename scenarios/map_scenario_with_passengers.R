pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, sf, tmap)


folder = "c:/projects/radLast/analysis/scenario_with_passenger/"

shp_network = st_read(paste(folder, "results/road_network_31468.shp", sep =""))

dc_area = st_read(paste(folder, "dc_20_catchment_31468.shp", sep =""))

counts = read_csv(paste(folder, "results/counts_link_mode.csv", sep ="")) %>%
  group_by(link) %>% summarize(freight = sum(freight), passenger = sum(passenger))

counts = counts %>%
 mutate(share = freight/(freight + passenger)) %>%
  mutate(share = if_else(is.na(share),0, share))

shp_network = shp_network %>% right_join(counts, by = c("ID" = "link"))

p =  tm_basemap(leaflet::providers$CartoDB) 

# p = p + tm_shape(dc_area, "dc" ) +
#   tm_polygons(col = "red", alpha = 0.1, border.alpha = 0.0)

p = p + tm_shape(shp_network, "network" ) +
  tm_lines(col = "share", scale = 3, style = "quantile" )

tmap_leaflet(p)
