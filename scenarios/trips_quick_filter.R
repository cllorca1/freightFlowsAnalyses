pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr)



folder = "C:/projects/radLast/analysis/scenario_with_passenger/"

trips = read_csv(paste(folder,"trips.csv", sep  =""))


trips = trips %>% filter(mode == "autoDriver")


box = list(top = 5337000, bottom = 5329000, left = 4460000, right = 4470000)

is_in_box = function(x, y, box){
  if (y > box$top){
    return(F)
  } else if (y < box$bottom){
    return(F)
  } else if (x < box$left){
    return(F)
  } else if (x > box$right){
    return(F)
  } else {
    return(T)
  }
}


trips = trips %>% rowwise() %>% mutate(orig_in_box = is_in_box(originX, originY, box))
trips = trips %>% rowwise() %>% mutate(dest_in_box = is_in_box(destinationX, destinationY, box))


subset_trips = trips %>% filter(orig_in_box | dest_in_box)

subset_trips = subset_trips %>% filter(!is.na(originX),!is.na(destinationX))

write_csv(subset_trips, paste(folder, "subset_trips.csv", sep = ""))

