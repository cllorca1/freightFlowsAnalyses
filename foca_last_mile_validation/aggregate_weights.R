pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr)


upper_folder = "C:/models/freightFlows/output/"

scenario = "base"

parcels = read_csv(paste(upper_folder, scenario, "/parcels.csv", sep =""))

parcels_day = parcels %>% group_by(toDestination, transaction) %>% summarize(count = n(), average =  mean(weight_kg), weight_kg = sum(weight_kg))

write.table(parcels_day, "clipboard", sep = "\t", row.names = F)


ggplot(parcels_day, aes(x=toDestination, y = count, fill = transaction, label = count)) +
  geom_bar(stat = "identity", position = "stack") 


ggplot(parcels_day, aes(x=toDestination, y = weight_kg * 365 / 1e9, fill = transaction, label = count)) +
  geom_bar(stat = "identity", position = "stack") 

ggplot(parcels_day, aes(x=toDestination, y = average, fill = transaction, label = count)) +
  geom_bar(stat = "identity", position = "dodge") 


trucks = read_csv(paste(upper_folder, scenario, "/ld_trucks.csv", sep =""))


parcel_trucks = trucks %>%
  filter(commodity == "POST_PACKET", segmentDestination == 9162 | segmentOrigin == 9162) %>% 
  mutate(toDestination = if_else(segmentOrigin==9162 & segmentDestination == 9162, "INTRA", if_else(segmentOrigin == 9162, "FROM", "TO")))


parcel_trucks %>% group_by(toDestination) %>% summarize(weight = sum(weight_tn)*365)


flows = read_csv(paste(upper_folder, scenario, "/flowSegments.csv", sep =""))
