pacman::p_load(dplyr, ggplot2, readr, tidyr, reshape, shiny, shinydashboard, plotly, processx, leaflet, stread, tmap, rgdal)

this_folder = "C:/code/freightFlowsR/simulation_results_analysis/dashboard/"

source(paste(this_folder, "read_data_fun.R", sep =""))
source(paste(this_folder, "read_counts_fun.R", sep =""))
source(paste(this_folder, "read_networks.R", sep =""))

upper_folder = "c:/models/freightFlows/"

scenario_folders = c("muc_scenario_zero_c",
                     "muc_scenario_3km",
                     "muc_scenario_1km",
                     "muc_scenario_paketbox",
                     "testRegNoCargoBikes",
                     "testReg",
                     "testReg_2",
                     "muc_hd_0",
                     "muc_hd_20",
                     "muc_hd_40",
                     "muc_hd_60",
                     "muc_hd_80",
                     "muc_hd_100")

scenarios = c("muc-base", 
              "muc-low-density",
              "muc-high_density_grid",
              "muc-high_density_shops",
              "reg-base", 
              "reg_low-density",
              "reg-high-density",
              "muc_hd_0",
              "muc_hd_20",
              "muc_hd_40",
              "muc_hd_60",
              "muc_hd_80",
              "muc_hd_100")
distribution_centers = c(20,20,20,20,10,10,10,20,20,20,20,20,20)

scenario_table = data.frame(folders = scenario_folders, names = scenarios, dc = distribution_centers)

colors_two = c("#407dd8","#255b89", "#36a332")
colors_four = c("red", "pink", "#407dd8","#36a332")

input = list()
input$selected_scenarios = c("muc-base", 
                              "muc-low-density",
                              "muc-high_density_grid",
                              "muc-high_density_shops")


input$selected_scenarios = c("reg-base", 
                             "reg_low-density",
                             "reg-high-density")

input$selected_scenarios = c("muc_hd_0",
                             "muc_hd_20",
                             "muc_hd_40",
                             "muc_hd_60",
                             "muc_hd_80",
                             "muc_hd_100")


summary = read_model_results(upper_folder, scenarios, scenario_folders, input$selected_scenarios, distribution_centers)

#counts = read_model_counts(upper_folder, scenarios, scenario_folders, input$selected_scenarios)

p = ggplot(summary, aes(y=n/weight_tn, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_two) + 
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  ylab("Number of tours (normalized by weight in tn)") +
  xlab("Scenario") +
  theme(text=element_text(size=14)) 
p 

p = ggplot(summary, aes(y=weight_tn, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_two) + 
  geom_bar(stat = "identity", position = "fill") +
  ylab("Parcel weight distribution")  + 
  xlab("Scenario") +
  theme(text=element_text(size=14))
p

p = ggplot(summary, aes(y=distance/weight_tn/1e3, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_two) + 
  geom_bar(stat = "identity", position =  "stack") +
  ylab("Distance to deliver 1kg (m/kg)") + 
  xlab("Scenario") +
  theme(text=element_text(size=14))
p


p = ggplot(summary %>% filter(vehicle!="Cargo bike"), aes(y= NOx/weight_tn/1000, x=scenario, fill = vehicle)) +
  scale_fill_manual(values = colors_two) + 
  geom_bar(stat = "identity", position = "stack") +
  ylab("NOx emission by weight (kg/kg)") + 
  xlab("Scenario") +
  theme(text=element_text(size=14))
p
