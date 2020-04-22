pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf)

folder = "C:/models/freightFlows/output/"
scenario = "test_mode_choice_0"


mode_choice = read.csv(paste(folder, scenario, "/analyticalModeChoice.csv", sep = ""))



ggplot(mode_choice %>% filter(mode == "all"), aes(x=distanceToDc, y=costs_all, color = as.factor(combination))) + geom_point()


mode_choice$parcels = mode_choice$density * mode_choice$area

mode_choice %>% filter(combination == 0) %>% group_by(size) %>% summarize(sum(parcels))



mc_summary = mode_choice %>% group_by(analysisZone, dc, combination) %>% summarize(density = mean(density), 
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

mc_summary %>% group_by(combination) %>% summarize_all(mean)

mc_summary = mc_summary %>% group_by(analysisZone, dc) %>% mutate(min_cost = min(total_cost))
mc_summary = mc_summary %>% mutate(selected = if_else(total_cost == min_cost, 1, 0))

mc_summary %>% filter(selected == 1) %>% group_by(combination) %>% summarize(n())

muc_shp = st_read("c:/models/freightFlows/input/shp/zones_31468_jobs.shp") %>% filter(AGS == 9162000) %>% summarize()
distributionCenters = read_csv("c:/models/freightFlows/input/distributionCenters/distributionCenters.csv") %>%
  filter(object == "distributionCenter", zone == 9162, commodityGroup == "PACKET")

distributionCenters$dc = distributionCenters$dcId

ggplot(data = mc_summary %>% filter(selected == 1), aes(x = x, y= y, fill = as.factor(combination))) + 
  geom_sf(data = muc_shp, inherit.aes = F) +
  geom_tile(color = "black", alpha = 0.3) +
  facet_wrap(.~dc) + geom_point() + 
  geom_point(data = distributionCenters, aes(x=dcX, y = dcY), color = "red", size = 3, inherit.aes = F)+ 
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = mc_summary %>% filter(selected == 1), aes(x = x, y= y, alpha = parcels/4)) + 
  geom_sf(data = muc_shp, inherit.aes = F) +
  geom_tile(color = "black") +
  facet_wrap(.~dc) + geom_point() + 
  geom_point(data = distributionCenters, aes(x=dcX, y = dcY), color = "red", size = 3, inherit.aes = F)+ 
  theme(axis.text.x = element_text(angle = 90))
