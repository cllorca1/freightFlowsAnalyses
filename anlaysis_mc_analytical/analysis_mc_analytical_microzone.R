pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf)

folder = "C:/models/freightFlows/output/"

scenario = "mc_cargo_bike_reg"

mode_choice = read.csv(paste(folder, scenario, "/analyticalModeChoice.csv", sep = ""))

mode_choice_solution = read.csv(paste(folder, scenario, "/analyticalModeChoiceSolution.csv", sep = ""))

ggplot(mode_choice %>% filter(mode == "all"), aes(x=distanceToDc, y=costs_all, color = as.factor(combination))) + geom_point()


mode_choice$parcels = mode_choice$density * mode_choice$area

mode_choice %>% filter(combination == 0) %>% group_by(size) %>% summarize(sum(parcels))

mc_summary = mode_choice %>% group_by(analysisZone, dc, combination) %>% summarize(density = sum(parcels)/area, 
                                                                                   parcels = sum(parcels),
                                                                                   x = mean(x),
                                                                                   y = mean(y),
                                                                  costs_lh = sum(costs_lh),
                                                                  costs_service = sum(costs_service), 
                                                                  cost_extra = sum(cost_extra), 
                                                                  costs_routing = sum(costs_routing_bike) + sum(costs_routing_truck))

mc_summary$total_cost = mc_summary$costs_lh + mc_summary$costs_service + mc_summary$cost_extra + mc_summary$costs_routing

ggplot(mc_summary, aes(x = parcels, y = costs_service, color = as.factor(combination))) + geom_point()

ggplot(mc_summary, aes(x = parcels, y = cost_extra, color = as.factor(combination))) + geom_point()

ggplot(mc_summary, aes(x = parcels, y = costs_routing, color = as.factor(combination))) + geom_point()

ggplot(mc_summary, aes(x = parcels, y = costs_lh, color = as.factor(combination))) + geom_point()

ggplot(mc_summary, aes(x = parcels, y = total_cost, color = as.factor(combination))) + geom_point()

ggplot(mc_summary, aes(x = density, y = total_cost, color = as.factor(combination))) + geom_point()


ggplot(mc_summary, aes(x = parcels/4)) + geom_histogram()

mc_summary = mc_summary %>% left_join(mode_choice_solution)

mc_summary =  mc_summary %>% mutate(selected = if_else(combination == solution, 1, 0))

selected_mc = mc_summary %>% filter(selected == 1) 

muc_shp = st_read("c:/models/freightFlows/input/modeChoice/muc_4k_31468.shp")

muc_shp = st_read("c:/models/freightFlows/input/modeChoice/regensburg_4k_31468.shp")

distributionCenters = read_csv("c:/models/freightFlows/input/distributionCenters/distributionCentersPirmin.csv") %>%
  filter(object == "distributionCenter", zone == 9162, commodityGroup == "PACKET")

distributionCenters$dc = distributionCenters$dcId


selected_mc = selected_mc %>% left_join(muc_shp, by = c("analysisZone" = "id"))

ggplot(data = selected_mc, aes( geometry = geometry, fill = as.integer(combination))) + 
  geom_sf() +
  facet_wrap(.~dc) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = selected_mc, aes( geometry = geometry, fill = parcels/area)) + 
  geom_sf() +
  facet_wrap(.~dc) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = selected_mc, aes( geometry = geometry, fill = congestion)) + 
  geom_sf() +
  facet_wrap(.~dc) +
  theme(axis.text.x = element_text(angle = 90))


ggplot(data = selected_mc, aes( geometry = geometry, fill = total_cost/parcels)) + 
  geom_sf() +
  facet_wrap(.~dc) +
  theme(axis.text.x = element_text(angle = 90))

                    