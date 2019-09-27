pacman::p_load(data.table, ggplot2, dplyr, reshape,tidyr, bit64)
folder = "c:/models/freightFlows/"

file_name = "ketten-"

path = paste(folder, "input/matrices/", file_name, 2010, ".csv", sep = "")
matrix_10 = fread(path, sep = ";")
matrix_10$TkmHL = as.numeric(matrix_10$TkmHL)


path = paste(folder, "input/matrices/", file_name, 2030, ".csv", sep = "")
matrix_30 = fread(path, sep = ";")
matrix_30$TkmHL = as.numeric(matrix_30$TkmHL)

join_names = names(matrix_10)[1:15]

#need to group the flows that are repeated (by all factors aggregated)

matrix_10_aux1 = matrix_10 %>% group_by(Quellzelle, Zielzelle, QuellzelleHL, ZielzelleHL,
                                        Quellterminal, Zielterminal, ModeVL, ModeHL,
                                        ModeNL, GuetergruppeVL, GuetergruppeHL, GuetergruppeNL,
                                        VerkArtVL, VerkArtHL, VerkArtNL) %>%
  summarize(TonnenVL = sum(TonnenVL),
            TonnenHL = sum(TonnenHL),
            TonnenNL = sum(TonnenNL))


matrix_30_aux1 = matrix_30 %>% group_by(Quellzelle, Zielzelle, QuellzelleHL, ZielzelleHL,
                                        Quellterminal, Zielterminal, ModeVL, ModeHL,
                                        ModeNL, GuetergruppeVL, GuetergruppeHL, GuetergruppeNL,
                                        VerkArtVL, VerkArtHL, VerkArtNL) %>%
  summarize(TonnenVL = sum(TonnenVL),
            TonnenHL = sum(TonnenHL),
            TonnenNL = sum(TonnenNL))

merged = dplyr::full_join(matrix_10_aux1, matrix_30_aux1, by = join_names, suffix = c("10", "30"))

names(merged)

merged$index = 1:nrow(merged)

merged[is.na(merged)] = 0

matrix_10_uniform = merged %>% select(1:15, 
                                      TonnenVL = TonnenVL10,
                                      TonnenHL = TonnenHL10,
                                      TonnenNL = TonnenNL10,
                                      index)

matrix_30_uniform = merged %>% select(1:15, 
                                      TonnenVL = TonnenVL30,
                                      TonnenHL = TonnenHL30,
                                      TonnenNL = TonnenNL30,
                                      index)

#quality check
sum(merged$TonnenHL10)
sum(matrix_10$TonnenHL)
sum(matrix_10_aux1$TonnenHL)
sum(matrix_10_uniform$TonnenHL)

sum(matrix_30$TonnenHL)
sum(matrix_30_uniform$TonnenHL)
sum(matrix_30_aux1$TonnenHL)
sum(merged$TonnenHL30)

file_name = "ketten-uniform-"

path = paste(folder, "input/matrices/", file_name, 2010, ".csv", sep = "")
fwrite(matrix_10_uniform, path, sep = ";")


path = paste(folder, "input/matrices/", file_name, 2030, ".csv", sep = "")
fwrite(matrix_30_uniform, path, sep = ";")


