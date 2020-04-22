pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf)

folder = "C:/models/freightFlows/output/"
scenario = "test_mode_choice_4"


mode_choice = read.csv(paste(folder, scenario, "/analyticalModeChoice.csv", sep = ""))
mode_choice$parcels_bike = mode_choice$density * mode_choice$area * if_else(mode_choice$mode == "cargoBike", 1,0)
mode_choice$parcels_truck = mode_choice$density * mode_choice$area * if_else(mode_choice$mode == "truck", 1,0)

names(mode_choice)
mode_choice_summary = mode_choice %>%
  group_by(zone, analysisZone, x, y, dc, distanceToDc, area, combination) %>%
  summarize(costs_lh = sum(costs_lh), costs_service = sum(costs_service),
            costs_extra = sum(cost_extra), costs_routing_bike = sum(costs_routing_bike),
            costs_routing_truck = sum(costs_routing_truck), costs_all = sum(costs_all), parcels_bike = sum(parcels_bike), parcels_truck = sum(parcels_truck))


mode_choice_summary = mode_choice_summary %>% mutate(min_cost = min(costs_all)) %>%
  mutate(selected = if_else(costs_all == min_cost, 1, 0))
 
mode_choice_summary_2 = mode_choice_summary %>%
  filter(selected == 1) %>%
  summarize(parcels_bike = sum(parcels_bike), parcels_truck = sum(parcels_truck)) %>%
  mutate(parcels = parcels_bike + parcels_truck) %>% 
  mutate(modal_share_bike = parcels_bike / parcels)

ggplot(mode_choice_summary_2, aes(x=parcels, y = modal_share_bike, color = as.factor(dc))) + geom_point() 



parcels = read.csv(paste(folder, scenario, "/parcels.csv", sep = ""))

parcels_summary = parcels %>% filter(toDestination == "true", transaction != "PARCEL_SHOP") %>%
  group_by(distributionCenter, distributionType) %>% summarize(count = n(), weight_kg = sum(weight_kg)) %>%
  mutate(share_in_units = count/sum(count)) %>% 
  mutate(share_in_weight = weight_kg / sum(weight_kg))

parcels %>% filter(toDestination == "true", transaction != "PARCEL_SHOP") %>%
  group_by(distributionType) %>% summarize(count = n(), weight_kg = sum(weight_kg)) %>%
  mutate(share_in_units = count/sum(count)) %>% 
  mutate(share_in_weight = weight_kg / sum(weight_kg))
                                                               
