read_model_counts2 = function(upper_folder, scenario_names, scenario_folders, selected_scenarios){
  
  numberOfScenarios = length(selected_scenarios)
  
  counts = data.frame()
  
  for (i in 1:numberOfScenarios){
    
    scenario = selected_scenarios[[i]]
    scenario_index = match(x = scenario, table = scenario_names)
    
    folder = paste(upper_folder, scenario_folders[[scenario_index]], "/", sep = "")
   
    this_counts = read.csv(paste(folder, "matsim/counts.csv", sep =""))
    this_counts$scenario = scenario
    
    counts = counts %>% bind_rows(this_counts)
    
    
    
  }
  
  return(counts)
  
}