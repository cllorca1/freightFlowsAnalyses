read_model_results = function(upper_folder, scenario_names, scenario_folders, selected_scenarios, distribution_centers){
  
  numberOfScenarios = length(selected_scenarios)

  summary = data.frame()
  
  scaleFactorTrucks = 1.0
  scaleFactorParcels = 1.0
  
  for (i in 1:numberOfScenarios){
    
    scenario = selected_scenarios[[i]]
    scenario_index = match(x = scenario, table = scenario_names)
    
    selected_DC = distribution_centers[[scenario_index]]
    
    
    folder = paste(upper_folder, "output/", scenario_folders[[scenario_index]], "/", sep = "")
    parcels = fread(paste(folder, "parcels.csv", sep = ""))
    
    #ld_trucks = fread(paste(folder, "ld_trucks.csv", sep = ""))
    
    #ld_trucks = ld_trucks %>% filter(destinationDistributionCenter == selected_DC)
    
    sd_trucks = fread(paste(folder, "sd_trucks.csv", sep = ""))
    
    vehicle_emissions = fread(paste(folder, "vehicleWarmEmissionFile.csv", sep = ""))
    vehicle_emissions$CO = as.numeric( vehicle_emissions$CO)
    vehicle_emissions$CO2 = as.numeric( vehicle_emissions$CO2)
    vehicle_emissions$HC = as.numeric( vehicle_emissions$HC)
    vehicle_emissions$PM = as.numeric( vehicle_emissions$PM)
    vehicle_emissions$NOx = as.numeric( vehicle_emissions$NOx)
    
    vehicle_emissions = vehicle_emissions %>% filter(distance != 0)
    
    #ld_trucks_assigned = ld_trucks %>% filter(assigned == T)
    
    #trucks_with_emissions = left_join(ld_trucks_assigned, vehicle_emissions, by = "id")
    
    total_weight = parcels %>%
      filter(assigned, toDestination) %>%
      summarize(weight_kg = sum(weight_kg), n = n())
    
    delivered_weight = parcels %>%
      filter(assigned, toDestination, transaction != "PARCEL_SHOP") %>%
      summarize(weight_kg = sum(weight_kg), n = n())
    
    ratio_not_to_parcel_shop = sum(delivered_weight$weight_kg) / sum(total_weight$weight_kg)
    
    delivered_weight_cargo_bike = parcels %>%
      filter(assigned, toDestination, transaction != "PARCEL_SHOP") %>% group_by(distributionType) %>%
      summarize(weight_kg = sum(weight_kg), n = n())
    
    #summary_ld_trucks = trucks_with_emissions %>%
      # filter(commodity != "NA") %>%
      # group_by(commodity) %>%
      # summarize(n = n()/scaleFactorTrucks, weight_tn = sum(weight_tn)/scaleFactorTrucks * ratio_not_to_parcel_shop,
      #           distance = sum(distance)/scaleFactorTrucks, CO2 = sum(CO2)/scaleFactorTrucks,
      #           NOx = sum(NOx)/scaleFactorTrucks, operatingTime = sum(operatingTime)/scaleFactorTrucks)
    
    summary_vans = vehicle_emissions %>%
      rowwise() %>%
      filter(grepl("van", id) & !grepl("feeder", id)) %>%
      mutate(id = "all") %>% 
      group_by() %>% summarize(n = n()/scaleFactorParcels, distance = sum(distance)/scaleFactorParcels,
                               CO2 = sum(CO2)/scaleFactorParcels, NOx = sum(NOx)/scaleFactorParcels,
                               operatingTime =  sum(operatingTime)/scaleFactorParcels)
    
    summary_vans$commodity = "POST_PACKET"
    summary_vans$vehicle = "Van"
    
    
    summary_feeder = vehicle_emissions %>%
      rowwise() %>%
      filter(grepl("feeder",id)) %>%
      mutate(id = "all") %>% 
      group_by() %>% summarize(n = n()/scaleFactorParcels, distance = sum(distance)/scaleFactorParcels,
                               CO2 = sum(CO2)/scaleFactorParcels, NOx = sum(NOx)/scaleFactorParcels,
                               operatingTime =  sum(operatingTime)/scaleFactorParcels)
    
    summary_feeder$commodity = "POST_PACKET"
    summary_feeder$vehicle = "Feeder"
    
    #summary_ld_trucks$vehicle = "Truck"
    
    summary_cargo_bike = vehicle_emissions %>%
      rowwise() %>%
      filter(grepl("cargoBike", id)) %>%
      mutate(id = "all") %>% 
      group_by() %>% summarize(n = n()/scaleFactorParcels, distance = sum(distance)/scaleFactorParcels,
                               CO2 = sum(CO2)/scaleFactorParcels, NOx = sum(NOx)/scaleFactorParcels,
                               operatingTime =  sum(operatingTime)/scaleFactorParcels)
    summary_cargo_bike$commodity = "POST_PACKET"
    summary_cargo_bike$vehicle = "Cargo bike"
    
    if (nrow(delivered_weight_cargo_bike %>% filter(distributionType == "CARGO_BIKE")) == 0){
      summary_cargo_bike$weight_tn = 0
      summary_feeder$weight_tn = 0
      summary_vans$weight_tn = delivered_weight$weight_kg[1] / 1000
      summary_cargo_bike$parcels = 0
      summary_feeder$parcels = 0
      summary_vans$parcels = delivered_weight$n[1]

    } else {
      summary_cargo_bike$weight_tn = delivered_weight_cargo_bike$weight_kg[1] / 1000
      summary_feeder$weight_tn = delivered_weight_cargo_bike$weight_kg[1] / 1000
      summary_vans$weight_tn = delivered_weight$weight_kg[1] / 1000 - delivered_weight_cargo_bike$weight_kg[1]/1000
      summary_cargo_bike$parcels = delivered_weight_cargo_bike$n[1]
      summary_feeder$parcels = delivered_weight_cargo_bike$n[1]
      summary_vans$parcels = delivered_weight$n[1] - delivered_weight_cargo_bike$n[1]
     
    }
    
    
    
    #this_summary = rbind(summary_vans, summary_ld_trucks)
    
    this_summary = rbind(summary_vans, summary_cargo_bike)
    
    this_summary = rbind(this_summary, summary_feeder)
    
    this_summary$scenario = scenario
    
    summary = rbind(summary, this_summary)
    
  }
  
  #summary_ld_trucks$scenario = "All (inter-urban)"
  
  #summary = rbind(summary, summary_ld_trucks)
  
  summary = summary %>% filter(commodity == "POST_PACKET")
  
  #summary$parcels = delivered_weight$n
  
  factor_levels = c(scenario_names)
  
  summary$scenario = factor(summary$scenario, levels = factor_levels)
  summary$vehicle = factor(summary$vehicle, levels = c("Van", "Feeder", "Cargo bike"))
  
  return(summary)
  
}
