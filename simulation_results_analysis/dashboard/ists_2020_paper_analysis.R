pacman::p_load(dplyr, ggplot2, readr, tidyr, reshape, shiny, shinydashboard,
               plotly, processx, leaflet, st, tmap, rgdal, data.table)

this_folder = "C:/code/freightFlowsR/simulation_results_analysis/dashboard/"

source(paste(this_folder, "read_data_fun.R", sep =""))
source(paste(this_folder, "read_counts_fun.R", sep =""))
source(paste(this_folder, "read_networks.R", sep =""))

upper_folder = "c:/models/freightFlows/"

### scenarios with shares ####


scenario_folders = c("muc_hd_0","muc_hd_20","muc_hd_40","muc_hd_60","muc_hd_80","muc_hd_100")

scenarios = c(0,20,40,60,80,100)

distribution_centers = c(20,20,20,20,20,20)

scenario_table = data.frame(folders = scenario_folders, names = scenarios, dc = distribution_centers)

colors_two = c("#407dd8", "#36a332")
colors_three = c("#407dd8","#255b89", "#36a332")
colors_four = c("red", "pink", "#407dd8","#36a332")

input = list()

input$selected_scenarios = scenarios

summary = read_model_results(upper_folder, scenarios, scenario_folders, input$selected_scenarios, distribution_centers)


ggplot(summary, aes(y=n, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position =  "stack") +
  ylab("Number of tours") +
  xlab("Share of cargo bikes (%)") +
  theme(text=element_text(size=14)) + 
  theme_bw() + 
  theme(legend.position = "none")
 
folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Sa.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)


# ggplot(summary %>% filter(vehicle != "Feeder"), aes(y=parcels, x=scenario, fill = vehicle)) +
#   scale_fill_manual(values = colors_two) + 
#   geom_bar(stat = "identity", position =  "fill") +
#   ylab("Number ofparcels")  + 
#   xlab("Share of parcels by cargo bike (%)") +
#   theme(text=element_text(size=14), axis.text.x = element_text(angle = 90)) + 
#   theme_bw()+ 
#   theme(legend.position = "none")




ggplot(summary, aes(y=distance/1000, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position =  "stack") +
  ylab("Distance travelled (km)") + 
  xlab("Share of cargo bikes (%)") +
  theme(text=element_text(size=14)) + 
  theme_bw()+ 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Sb.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)


p = ggplot(summary, aes(y=operatingTime, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) +
  geom_bar(stat = "identity", position =  "stack") +
  ylab("Time travelling") +
  xlab("Scenario") +
  theme(text=element_text(size=14))
p


ggplot(summary, aes(y=distance/parcels, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position =  "dodge") +
  ylab("Distance by parcel (m)") + 
  xlab("Share of cargo bikes (%)") +
  theme(text=element_text(size=14)) + 
  theme_bw() + 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Sd.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)




ggplot(summary %>% filter(vehicle!="Cargo bike"), aes(y= NOx, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position = "stack") +
  ylab("NOx emissions (g)") + 
  xlab("Share of cargo bikes (%)") +
  theme(text=element_text(size=14)) + 
  theme_bw()+ 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Sf.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)


### Scenarioswith micro depot densities ####

scenario_folders = c("muc_densities_1000", "muc_densities_2000", "muc_densities_3000")

scenarios = c(1000,2000,3000)

distribution_centers = c(20,20,20)

scenario_table = data.frame(folders = scenario_folders, names = scenarios, dc = distribution_centers)

colors_two = c("#407dd8", "#36a332")
colors_three = c("#407dd8","#255b89", "#36a332")
colors_four = c("red", "pink", "#407dd8","#36a332")

input = list()

input$selected_scenarios = scenarios

summary = read_model_results(upper_folder, scenarios, scenario_folders, input$selected_scenarios, distribution_centers)


ggplot(summary, aes(y=n, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position =  "stack") +
  ylab("Number of tours") +
  xlab("Micro depot grid spacing (m)") +
  theme(text=element_text(size=14)) + 
  theme_bw()+ 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Ma.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)


ggplot(summary, aes(y=distance/1000, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position =  "stack") +
  ylab("Distance travelled (km)") + 
  xlab("Micro depot grid spacing (m)") +
  theme(text=element_text(size=14)) + 
  theme_bw()+ 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Mb.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)


ggplot(summary, aes(y=distance/parcels, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position =  "dodge") +
  ylab("Distance by parcel (m)") + 
  xlab("Micro depot grid spacing (m)") +
  theme(text=element_text(size=14)) + 
  theme_bw()+ 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Mc.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)


ggplot(summary %>% filter(vehicle!="Cargo bike"), aes(y= NOx, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position = "stack") +
  ylab("NOx emissions (g)") + 
  xlab("Micro depot grid spacing") +
  theme(text=element_text(size=14)) + 
  theme_bw()+ 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Md.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)


######Scenarios with demand levels ####

scenario_folders = c("muc_demand_1", "muc_demand_1.5", "muc_demand_2", "muc_demand_2.5")

scenarios = c("100%","150%","200%","250%")

distribution_centers = c(20,20,20,20)

scenario_table = data.frame(folders = scenario_folders, names = scenarios, dc = distribution_centers)

colors_two = c("#407dd8", "#36a332")
colors_three = c("#407dd8","#255b89", "#36a332")
colors_four = c("red", "pink", "#407dd8","#36a332")

input = list()

input$selected_scenarios = scenarios

summary = read_model_results(upper_folder, scenarios, scenario_folders, input$selected_scenarios, distribution_centers)


ggplot(summary, aes(y=n/parcels*100, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position =  "dodge") +
  ylab("Number of tours by 100 parcels") +
  xlab("Parcel demand (%)") +
  theme(text=element_text(size=14)) + 
  theme_bw() + 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Da.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)


ggplot(summary, aes(y=distance/parcels, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position =  "dodge") +
  ylab("Distance travelled by parcel (m)") + 
  xlab("Parcel demand (%)") +
  theme(text=element_text(size=14)) + 
  theme_bw()+ 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Db.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)


ggplot(summary, aes(y=distance/1000, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position =  "stack") +
  ylab("Distance travelled (km)") + 
  xlab("Parcel demand (%)") +
  theme(text=element_text(size=14)) + 
  theme_bw()+ 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Dd.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)

ggplot(summary %>% filter(vehicle!="Cargo bike"), aes(y= NOx/parcels, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position = "dodge") +
  ylab("NOx emission by parcel (g/parcel)") + 
  xlab("Parcel demand (%)") +
  theme(text=element_text(size=14)) + 
  theme_bw() + 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "Dc.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)

### Scenarios with various depots ####

scenario_folders = c("muc_dc_20", "muc_dc_19", "muc_dc_16", "muc_dc_24", "muc_dc_22")

scenarios =  c("DC 1", "DC 2", "DC 3", "DC 4", "DC 5")

distribution_centers = c(20,19,16,24,22)

scenario_table = data.frame(folders = scenario_folders, names = scenarios, dc = distribution_centers)

colors_two = c("#407dd8", "#36a332")
colors_three = c("#407dd8","#255b89", "#36a332")
colors_four = c("red", "pink", "#407dd8","#36a332")



input = list()

input$selected_scenarios = scenarios

summary = read_model_results(upper_folder, scenarios, scenario_folders, input$selected_scenarios, distribution_centers)


ggplot(summary, aes(y=n/parcels*100, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position =  "dodge") +
  ylab("Number of tours by 100 parcels") +
  xlab("DC") +
  theme(text=element_text(size=14)) + 
  theme_bw() + 
  xlab("Distribution center") +
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "DCa.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)


ggplot(summary, aes(y=distance/parcels, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_three) + 
  geom_bar(stat = "identity", position =  "dodge") +
  ylab("Distance by parcel (m)") + 
  xlab("Distribution center") +
  theme(text=element_text(size=14)) + 
  theme_bw() + 
  theme(legend.position = "none")

folder = "C:/projects/Papers/2020_ISTS_Delft/figs/"
fileName = paste(folder, "DCb.bmp", sep  ="")

ggsave(fileName, width = 8, height = 10, units = "cm", dpi = 300)





