read_model_results = function(upper_folder, scenario_names, scenario_folders, selected_scenarios){
  
  numberOfScenarios = length(selected_scenarios)
  
  
  selected_DC = 20
  summary = data.frame()
  
  scaleFactorTrucks = 1.0
  scaleFactorParcels = 1.0
  
  for (i in 1:numberOfScenarios){
    scenario = selected_scenarios[[i]]
    scenario_index = match(x = scenario, table = scenario_names)
    
    folder = paste(upper_folder, "output/", scenario_folders[[i]], "/", sep = "")
    parcels = fread(paste(folder, "parcels.csv", sep = ""))
    
    ld_trucks = fread(paste(folder, "ld_trucks.csv", sep = ""))
    
    ld_trucks = ld_trucks %>% filter(destinationDistributionCenter == selected_DC)
    
    sd_trucks = fread(paste(folder, "sd_trucks.csv", sep = ""))
    
    vehicle_emissions = fread(paste(folder, "vehicleWarmEmissionFile.csv", sep = ""))
    vehicle_emissions$CO = as.numeric( vehicle_emissions$CO)
    vehicle_emissions$CO2 = as.numeric( vehicle_emissions$CO2)
    vehicle_emissions$HC = as.numeric( vehicle_emissions$HC)
    vehicle_emissions$PM = as.numeric( vehicle_emissions$PM)
    vehicle_emissions$NOx = as.numeric( vehicle_emissions$NOx)
    
    vehicle_emissions = vehicle_emissions %>% filter(distance != 0)
    
    ld_trucks_assigned = ld_trucks %>% filter(assigned == T)
    
    trucks_with_emissions = left_join(ld_trucks_assigned, vehicle_emissions, by = "id")
    
    length(unique(parcels %>% filter(distributionType == "CARGO_BIKE") %>% select(destMicroZone))$destMicroZone)
    length(unique(parcels$destMicroZone))
    
    delivered_weight = parcels %>%
      filter(assigned, toDestination, transaction != "PARCEL_SHOP") %>%
      summarize(weight_kg = sum(weight_kg), n = n())
    
    delivered_weight_cargo_bike = parcels %>%
      filter(assigned, toDestination, transaction != "PARCEL_SHOP") %>% group_by(distributionType) %>%
      summarize(weight_kg = sum(weight_kg), n = n())
    
    print(
      parcels %>%
        filter(assigned, toDestination, transaction != "PARCEL_SHOP") %>% group_by(distributionType) %>%
        summarize(accessDistance = mean(accessDistance), n = n())
    )
    
    #write.table(x=trucks_with_emissions, file="clipboard-10000", sep ="\t", row.names = F)
    
    summary_ld_trucks = trucks_with_emissions %>%
      filter(commodity != "NA") %>%
      group_by(commodity) %>%
      summarize(n = n()/scaleFactorTrucks, weight_tn = sum(weight_tn)/scaleFactorTrucks,
                distance = sum(distance)/scaleFactorTrucks, CO2 = sum(CO2)/scaleFactorTrucks,
                NOx = sum(NOx)/scaleFactorTrucks, operatingTime = sum(operatingTime)/scaleFactorTrucks)
    
    summary_vans = vehicle_emissions %>%
      rowwise() %>%
      filter(grepl("van", id) | grepl("feeder",id)) %>%
      mutate(id = "all") %>% 
      group_by() %>% summarize(n = n()/scaleFactorParcels, distance = sum(distance)/scaleFactorParcels,
                               CO2 = sum(CO2)/scaleFactorParcels, NOx = sum(NOx)/scaleFactorParcels,
                               operatingTime =  sum(operatingTime)/scaleFactorParcels)
    
    summary_vans$commodity = "POST_PACKET"
    summary_vans$weight_tn = delivered_weight$weight_kg[1] / 1000
    
    summary_vans$vehicle = "Truck"
    summary_ld_trucks$vehicle = "Truck"
    
    summary_cargo_bike = vehicle_emissions %>%
      rowwise() %>%
      filter(grepl("cargoBike", id)) %>%
      mutate(id = "all") %>% 
      group_by() %>% summarize(n = n()/scaleFactorParcels, distance = sum(distance)/scaleFactorParcels,
                               CO2 = sum(CO2)/scaleFactorParcels, NOx = sum(NOx)/scaleFactorParcels,
                               operatingTime =  sum(operatingTime)/scaleFactorParcels)
    summary_cargo_bike$commodity = "POST_PACKET"
    
    if (i == 1){
      summary_cargo_bike$weight_tn = 0
    } else {
      summary_cargo_bike$weight_tn = delivered_weight_cargo_bike$weight_kg[1] / 1000
    }
    
    summary_cargo_bike$vehicle = "Cargo bike"
    
    #this_summary = rbind(summary_vans, summary_ld_trucks)
    
    this_summary = rbind(summary_vans, summary_cargo_bike)
    
    this_summary$scenario = scenario
    
    summary = rbind(summary, this_summary)
  }
  
  summary_ld_trucks$scenario = "All (inter-urban)"
  
  summary = rbind(summary, summary_ld_trucks)
  
  summary = summary %>% filter(commodity == "POST_PACKET")
  
  summary$parcels = delivered_weight$n
  
  summary$scenario = factor(summary$scenario, levels = c("All (inter-urban)","Base (urban)", "a (urban)", "b (urban)", "c (urban)"))
  summary$vehicle = factor(summary$vehicle, levels = c("Truck", "Cargo bike"))
  
  return(summary)
  
}