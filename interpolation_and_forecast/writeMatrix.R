source("c:/code/freightFlowsR/analysis/load_labels.R")


printOutMatrix = function(matrix, fileName){
  
  matrix$ModeHL = factor(matrix$ModeHL, labels = mode_code, levels = mode_lab)
  matrix$ModeVL = factor(matrix$ModeVL, labels = mode_code, levels = mode_lab)
  matrix$ModeNL = factor(matrix$ModeNL, labels = mode_code, levels = mode_lab)
  
  matrix$GuetergruppeHL = factor(matrix$GuetergruppeHL,
                                   labels = commodity_code,
                                   levels = commodity_lab)
  matrix$GuetergruppeVL = factor(matrix$GuetergruppeVL,
                                   labels = commodity_code,
                                   levels = commodity_lab)
  matrix$GuetergruppeNL = factor(matrix$GuetergruppeNL,
                                   labels = commodity_code,
                                   levels = commodity_lab)
  
  
  fwrite(matrix, fileName, row.names = F, sep =";")
  
  
  
}