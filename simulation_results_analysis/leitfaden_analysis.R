pacman::p_load(dplyr, ggplot2, readr, tidyr, reshape, shiny, shinydashboard,
               plotly, processx, leaflet, st, tmap, rgdal, data.table)

foca_visualizer_folder = "C:/code/radLastTools/foca_visualizer/model_data/"

vehicles = read_csv(paste(foca_visualizer_folder, "vehicles.csv", sep = ""))

scenario_folders = "C:/models/freightFlows/output/"


vehicles$configuration = factor(vehicles$scenario, levels = c("0-REG: Nur Transporter", "1-REG: Optimiert", "2-MUC: Nur Transporter", "3-MUC: Optimiert"), 
                                labels = c("Reiner Transporter-Zustellung",
                                           "Optimierte Moduszuteilung",
                                           "Reiner Transporter-Zustellung",
                                           "Optimierte Moduszuteilung"))

vehicles$configuration = factor(vehicles$configuration, levels = c("Optimierte Moduszuteilung",
                                                                   "Reiner Transporter-Zustellung"))

vehicles$city = factor(vehicles$scenario, levels = c("0-REG: Nur Transporter", "1-REG: Optimiert", "2-MUC: Nur Transporter", "3-MUC: Optimiert"), 
                                labels = c("reg",
                                           "reg",
                                           "muc",
                                           "muc"))



for (this_city in c("muc", "reg")){
  
  this_vehicles = vehicles %>% filter(city == this_city)
  
  this_vehicles = this_vehicles %>% filter(vehicle != "Zulieferer - Paketshop", vehicle != "LKW - Fernverkehr")
  
  this_vehicles$Fahrzeug = this_vehicles$vehicle
  
  this_vehicles$Fahrzeug = factor(this_vehicles$Fahrzeug, levels = c("Lastenrad", "Zulieferer - Mikrodepot", "Transporter"))
  
  ggplot(this_vehicles, aes(y=n_tours, label = n_tours, x=configuration, fill = Fahrzeug, group = Fahrzeug)) +
    scale_fill_manual(values = c("#00a3b0", "#7fa1b5", "#00446c")) + 
    geom_bar(stat = "identity", position = "stack", size = 2) +
    ylab("Anzahl Touren") +
    xlab("Moduskonfiguration") +
    theme(text=element_text(size=14)) +
    theme_bw() + 
    geom_text(position=position_stack(0.5), color = "white") + 
    theme(legend.position = "bottom", plot.margin=unit(c(0.5,0.5,0,0),"cm"))
  
  
  ggsave(paste("simulation_results_analysis/leitfaden/tours_",this_city,".pdf",sep =""), device = "pdf", scale = 1, width = 150, height = 150, units = "mm")
  ggsave(paste("simulation_results_analysis/leitfaden/tours_",this_city,".png",sep =""), device = "bmp", scale = 1, width = 150, height = 150, units = "mm")
  
  ggplot(this_vehicles , aes(y=distance/1000, label = round(distance/1000, digits = 0), x=configuration, fill = Fahrzeug)) +
    scale_fill_manual(values = c("#00a3b0", "#7fa1b5", "#00446c")) + 
    geom_bar(stat = "identity", position =  "stack") +
    ylab("Distanz (km)")  +
    xlab("Moduskonfiguration") +
    theme_bw() + 
    geom_text(position=position_stack(0.5), color = "white") + 
    theme(legend.position = "bottom", plot.margin=unit(c(0.5,0.5,0,0),"cm"))
  
  ggsave(paste("simulation_results_analysis/leitfaden/dist_",this_city,".pdf",sep =""), device = "pdf", scale = 1, width = 150, height = 150, units = "mm")
  ggsave(paste("simulation_results_analysis/leitfaden/dist_",this_city,".png",sep =""), device = "bmp", scale = 1, width = 150, height = 150, units = "mm")
  
  ggplot(this_vehicles , aes(y=total_time/3600, label = round(total_time/3600, digits = 0), x=configuration, fill = Fahrzeug)) +
    scale_fill_manual(values = c("#00a3b0", "#7fa1b5", "#00446c")) + 
    geom_bar(stat = "identity", position =  "stack") +
    ylab("Gesamtfahrzeit (h)")  +
    xlab("Moduskonfiguration") +
    theme_bw() + 
    geom_text(position=position_stack(0.5), color = "white") + 
    theme(legend.position = "bottom", plot.margin=unit(c(0.5,0.5,0,0),"cm"))
  
  
  ggsave(paste("simulation_results_analysis/leitfaden/time_",this_city,".pdf",sep =""), device = "pdf", scale = 1, width = 150, height = 150, units = "mm")
  ggsave(paste("simulation_results_analysis/leitfaden/time",this_city,".png",sep =""), device = "bmp", scale = 1, width = 150, height = 150, units = "mm")
  
  simplified_emission_factors = data.frame(vehicle = c("Zulieferer - Paketshop", "Transporter", "Zulieferer - Mikrodepot", "Lastenrad"), 
                                           co2_factor_combustion = c(634,634,634,0), co2_factor_electric = c(259,259,259,15.54))
  
  
  
  summary_aux2 = this_vehicles %>% left_join(simplified_emission_factors, by = "vehicle") %>% mutate(effective_share_of_ev = if_else(vehicle == "Lastenrad", 1, 0))
  
  summary_aux2 = summary_aux2 %>%
    mutate(co2_simple = distance / 1000 * effective_share_of_ev * co2_factor_electric + distance / 1000 * (1 - effective_share_of_ev) * co2_factor_combustion)
  
  
  ggplot(summary_aux2, aes(y= co2_simple/1000, label = round(co2_simple/1000, digits = 0), x=configuration, fill = Fahrzeug, group = Fahrzeug)) +
    scale_fill_manual(values = c("#00a3b0", "#7fa1b5", "#00446c")) + 
    geom_bar(stat = "identity", position = "stack") +
    ylab("CO2 Emissionen (kg)") + 
    xlab("Moduskonfiguration") +
    geom_text(position=position_stack(0.5), color = "white") + 
    theme_bw() + theme() 
  
  ggsave(paste("simulation_results_analysis/leitfaden/co2_",this_city,".pdf",sep =""), device = "pdf", scale = 1, width = 150, height = 150, units = "mm")
  ggsave(paste("simulation_results_analysis/leitfaden/co2_",this_city,".png",sep =""), device = "bmp", scale = 1, width = 150, height = 150, units = "mm")
  
}


#ggsave("simulation_results_analysis/co2.pdf", device = "pdf", scale = 0.75, width = 300, height = 150, units = "mm")
#ggsave("simulation_results_analysis/co2.bmp", device = "bmp", scale = 0.75, width = 300, height = 150, units = "mm")








##counts
# counts = read_model_counts2(upper_folder, scenarios, scenario_folders ,scenarios)
# 
# links_in_catchment_area = read_csv("C:/projects/radLast/analysis/scenarios_new_paper_2020/sub_networks/catchment_area.csv")$ID
# links_in_md_buffer = read_csv("C:/projects/radLast/analysis/scenarios_new_paper_2020/sub_networks/500_m_microDepot.csv")$ID
# 
# ##load the old passenger count file
# pax_count = read_csv("C:/projects/radLast/analysis/scenario_with_passenger/results/counts_link_mode.csv")
# pax_count = pax_count %>% select(link, passenger)
# 
# counts = counts %>% left_join(pax_count, by = "link")
# counts$passenger = counts$passenger/24 ##passenger links cannot be disagregated by hour - the file was already aggregated to days
# 
# aux = counts %>% group_by(link, length, type) %>% summarize()
# aux = aux %>% group_by(type) %>% summarize(sum(length))
# write.table(aux, "clipboard", sep  ="\t")
# 
# 
# counts_long = counts %>% pivot_longer(cols = c("passenger", "cargoBike", "van", "lDTruck", "sDTruck", "feeder"))
# 
# counts_long = counts_long %>% 
#   filter(link %in% links_in_catchment_area) %>%
#   mutate(link_in_md = link %in% links_in_md_buffer)
# 
# 
# summary_counts_1 = counts_long %>%
#   group_by(scenario,name) %>%
#   summarize(count = sum(value)) %>%
#   pivot_wider(id_cols = "scenario", values_from = "count")
# 
# summary_counts_2 = counts_long %>%
#   group_by(scenario,type, name) %>%
#   summarize(count = sum(value))
# 
# selected_types = c("motorway", "trunk","primary", "secondary",  "tertiary", "residential")
# 
# summary_counts_3 = summary_counts_2 %>% filter(type %in% selected_types)
# summary_counts_3$type = factor(summary_counts_3$type , levels = selected_types)
# 
# summary_counts_3$name[summary_counts_3$name == "sDTruck"] = "truck"
# summary_counts_3$name[summary_counts_3$name == "lDTruck"] = "truck"
# 
# summary_counts_3$name = factor(summary_counts_3$name , 
#                                levels = c("passenger", "cargoBike", "feeder",  "van", "truck"), 
#                                labels= c("Passenger", "Cargo bike", "Feeder", "Van", "Truck"))
# 
# summary_counts_3$type = as.character(summary_counts_3$type)
# 
# summary_counts_3$type[summary_counts_3$type == "motorway"] = "Major road (28% in length)"
# summary_counts_3$type[summary_counts_3$type == "primary"] = "Major road (28% in length)"
# summary_counts_3$type[summary_counts_3$type == "secondary"] = "Major road (28% in length)"
# summary_counts_3$type[summary_counts_3$type == "tertiary"] = "Major road (28% in length)"
# summary_counts_3$type[summary_counts_3$type == "trunk"] = "Major road (28% in length)"
# summary_counts_3$type[summary_counts_3$type == "residential"] = "Minor road (72% in length)"
# 
# summary_counts_3= summary_counts_3 %>% group_by(scenario, type, name) %>% summarize(count = sum(count))
# 
# summary_counts_3 = summary_counts_3 %>% filter(name != "Passenger")
# 
# ggplot(summary_counts_3, aes(x=as.factor(scenario), y = count, fill  = name)) +
#   scale_fill_manual(values = c("gray80","gray66","gray25" , "black", "gray5")) + 
#   geom_bar(stat = "identity", position = "fill", color = "black") + 
#   scale_y_continuous(expand = c(0,0)) + 
#   facet_wrap(.~type, scales = "free") + 
#   xlab("Share of cargo bikes (%)") + ylab("Share of traffic volume") +
#   theme_bw()
# 
# ggsave("simulation_results_analysis/share_freight.pdf", device = "pdf", scale = 0.75, width = 300, height = 100, units = "mm")
# ggsave("simulation_results_analysis/share_freight.bmp", device = "bmp", scale = 0.75, width = 300, height = 100, units = "mm")
# 
# ggplot(summary_counts_3, aes(x=as.factor(scenario), y = count, fill  = name)) +
#   scale_fill_manual(values = c("#a5c594", "#70928c","#6b719f" , "gray25", "gray25")) + 
#   geom_bar(stat = "identity", position = "stack", color = "black") + 
#   scale_y_continuous(expand = c(0,0)) + 
#   facet_wrap(.~type, scales = "free") + 
#   xlab("Share of cargo bikes (%)") + ylab("Sum of traffic volume (veh/day)") +
#   theme_bw()
# 
# 
# 
# ggsave("simulation_results_analysis/counts_freight.pdf", device = "pdf", scale = 0.75, width = 300, height = 150, units = "mm")
# ggsave("simulation_results_analysis/counts_freight.bmp", device = "bmp", scale = 0.75, width = 300, height = 150, units = "mm")
# 
# 
# #write.table(summary_counts_3, "clipboard", sep  ="\t")
# 
# summary_counts_4 = counts_long %>%
#   filter(link_in_md) %>%
#   group_by(scenario, type, name) %>%
#   summarize(count = sum(value * length))
# 
# selected_types = c("motorway", "trunk","primary", "secondary",  "tertiary", "residential", "unclassified")
# 
# summary_counts_4 = summary_counts_4 %>% filter(type %in% selected_types)
# summary_counts_4$type = factor(summary_counts_4$type , levels = selected_types)
# summary_counts_4$name = factor(summary_counts_4$name , levels = c("car", "cargoBike", "van", "sDTruck", "lDTruck"))
# 
# summary_counts_4 = summary_counts_4 %>% filter(name != "passenger")
# 
# ggplot(summary_counts_4, aes(x=as.factor(scenario), y = count, fill  = name)) +
#   scale_fill_manual(values = c("gray80", "gray66", "gray25", "gray20")) + 
#   geom_bar(stat = "identity", position = "fill", color = "black") + 
#   scale_y_continuous(expand = c(0,0)) + 
#   facet_wrap(.~type, scales = "free") + 
#   xlab("Share of cargo bikes (%)") + ylab("Share of traffic flows") +
#   theme_bw()
# 
# ggplot(summary_counts_4, aes(x=as.factor(scenario), y = count, fill  = name)) +
#   scale_fill_manual(values = c("gray80", "gray66", "gray25", "gray20")) + 
#   geom_bar(stat = "identity", position = "stack", color = "black") + 
#   scale_y_continuous(expand = c(0,0)) + 
#   facet_wrap(.~type) + 
#   xlab("Share of cargo bikes (%)") + ylab("Sum of traffic counts") +
#   theme_bw()
# 
# 
# daily_counts = counts %>% group_by(link, length, type, scenario) %>% summarise_all(sum)
# 
# write_csv(daily_counts, "C:/projects/radLast/analysis/scenarios_new_paper_2020/counts_daily.csv")
