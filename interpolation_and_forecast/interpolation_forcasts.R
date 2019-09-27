pacman::p_load(data.table, ggplot2, dplyr, reshape,tidyr)

folder = "c:/models/freightFlows/"

source("input/input_zones.R")
source("input/input_commodities.R")
source("input/input_matrices.R")



names(matrices)

exogenous_growth = 1.0

beta_exogenous = 1
beta_global = 0
beta_mode = 0
beta_commodity = 1
beta_origin = 0
beta_destination = 0


# global = matrices %>% group_by(year) %>%
#   summarise(count = n(), TonnenHL = sum(as.numeric(TonnenHL)))
# 
# write.table(global, 'clipboard', row.names = F, sep = "\t")
# 
# by_mode = matrices %>%
#   group_by(year, ModeHL) %>% summarise(count = n(), TonnenHL = sum(as.numeric(TonnenHL)))
# write.table(by_mode, 'clipboard', row.names = F, sep = "\t")
# 
# by_commo = matrices %>%
#   group_by(year, GuetergruppeHL) %>%
#   summarise(count = n(), TonnenHL = sum(as.numeric(TonnenHL)))
# write.table(by_commo, 'clipboard', row.names = F, sep = "\t")
# 
# by_orig = matrices %>% group_by(year, Quellzelle) %>% summarise(count = n(), TonnenHL = sum(as.numeric(TonnenHL)))
# write.table(by_orig, 'clipboard', row.names = F, sep = "\t")
# 
# by_dest = matrices %>% group_by(year, Zielzelle) %>% summarise(count = n(), TonnenHL = sum(as.numeric(TonnenHL)))
# write.table(by_dest, 'clipboard', row.names = F, sep = "\t")

##process the growth factors in excel

##(...)

##read the growth factors from a csv file

growth_factors = read.csv("C:/projects/radLast/data/commodities/forecasts_and_interpolations/growth_factors.csv")

growth_factors_global = growth_factors %>% filter(variable == "all") %>%
  select(global = value, ratio_global = ratio)

growth_factors_by_mode = growth_factors %>% filter(variable == "ModeHL")  %>%
  select(ModeHL = value, ratio_mode = ratio)

growth_factors_by_commodity = growth_factors %>% filter(variable == "GuetergruppeHL")  %>%
  select(GuetergruppeHL = value, ratio_commo = ratio)

growth_factors_by_origin = growth_factors %>% filter(variable == "Quellzelle")  %>%
  select(Quellzelle = value, ratio_orig = ratio)
#needs to be converted to integer 
growth_factors_by_origin$Quellzelle = as.integer(as.character(growth_factors_by_origin$Quellzelle))

growth_factors_by_destination = growth_factors %>% filter(variable == "Zielzelle")  %>%
  select(Zielzelle = value, ratio_dest = ratio)
#needs to be converted to integer
growth_factors_by_destination$Zielzelle = as.integer(as.character(growth_factors_by_destination$Zielzelle))

matrices_30 = filter(matrices, year == 2030)

matrices_30 = matrices_30 %>%
  mutate(ratio_global = growth_factors_global$ratio_global) 

matrices_30 = left_join(matrices_30, growth_factors_by_mode, by=c("ModeHL"))
matrices_30 = left_join(matrices_30, growth_factors_by_commodity, by=c("GuetergruppeHL"))
matrices_30 = left_join(matrices_30, growth_factors_by_origin, by=c("Quellzelle"))
matrices_30 = left_join(matrices_30, growth_factors_by_destination, by=c("Zielzelle"))


#calculate growth rate

matrices_30 = matrices_30 %>% 
  mutate(sum_weights = beta_exogenous + beta_global + beta_mode + beta_commodity +
           beta_origin + beta_destination)

matrices_30 = matrices_30 %>% 
  mutate(growth = (exogenous_growth * beta_exogenous +  ratio_global*beta_global + ratio_mode*beta_mode +
                     ratio_commo*beta_commodity + ratio_orig*beta_origin +
                     ratio_dest * beta_destination) / sum_weights)

#For testing
#matrices_30 %>% group_by(GuetergruppeHL) %>% summarize (mean(ratio_commo))

summary(matrices_30$growth)

matrices_30$TonnenHL_30 = matrices_30$TonnenHL
matrices_30$TonnenHL = matrices_30$TonnenHL_30 * matrices_30$growth

matrices_30$TonnenVL_30 = matrices_30$TonnenVL
matrices_30$TonnenVL = matrices_30$TonnenVL_30 * matrices_30$growth

matrices_30$TonnenNL_30 = matrices_30$TonnenNL
matrices_30$TonnenNL = matrices_30$TonnenNL_30 * matrices_30$growth

matrices_50 = matrices_30 %>% select(1:20)
matrices_50$year = 2050

matrices = rbind(matrices, matrices_50)

#Checking the results
global = matrices %>% group_by(year) %>%
  summarise(count = n(), TonnenHL = sum(as.numeric(TonnenHL)))

ggplot(global, aes(x = year, y = TonnenHL)) + geom_line(size = 2) + geom_point(size = 4) + 
  ylim(3e9, 6e9)
#ggplotly()

by_mode = matrices %>%
  group_by(year, ModeHL) %>% summarise(count = n(), TonnenHL = sum(as.numeric(TonnenHL)))

ggplot(by_mode, aes(x = year, group = ModeHL, y = TonnenHL, color = ModeHL)) +
  geom_line(size = 2) + geom_point(size = 4)
#ggplotly()

by_commo = matrices %>%
  group_by(year, GuetergruppeHL) %>%
  summarise(count = n(), TonnenHL = sum(as.numeric(TonnenHL)))

ggplot(by_commo, aes(x = year, y = TonnenHL, color = GuetergruppeHL)) +
  geom_line(size = 1) + scale_y_log10()
#ggplotly()



matrix_50 = matrices %>% filter(year == 2050)

source("c:/code/freightFlowsR/interpolation_and_forecast/writeMatrix.R")

printOutMatrix(matrix_50,"c:/code/freightFlowsR/interpolation_and_forecast/test.csv" )

