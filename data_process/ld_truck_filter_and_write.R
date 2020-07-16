pacman::p_load(readr, dplyr)


general_file = read_csv("D:/simulations/foca/ld_only/ld_all_2010/ld_trucks.csv")


to_reg = general_file %>% filter(segmentOrigin == 9362 | segmentDestination == 9362)


write_csv(to_reg, "C:/models/freightFlows/input/preProcessedInput/ld_trucks_reg.csv")
