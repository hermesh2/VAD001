# S Basics ----------------------------------------------------------------
if(! require("rjson")){
  install.packages("rjson")
}
library("rjson")

list <-  list(
  needed_script = c("dplyr", "data.table", "ez", "ggplot2", "nlme", "lme4", "languageR", "lmerTest", "modeest", "psych"),
  dir_script = getwd(),
  
  ncol_experiment_01 = 230, # Numero de columnas que tiene el experimento
  
  blockAtributo_01 = c(1, 3),
  NombreBlock_vector_01 = 1:4,
  NombreBlock_names_01 = c("ActionF-Pos","ActionF-Neg","ActionR-Pos","ActionR-Neg"),
  variables_guardadas_01 = c("Subject", "correct", "block", "sexo", "target", "tipo", "block2", "response_time", "Group"),
  
  seet_seed = 11234, # 112345
  min_RT = 300,
  max_RT = 3000,
  IQR_prod = 1.5,
  correct_only_01 = FALSE,             # Si quremeos solo las repuestas correctas
  Ratio_response_Block_01 = 0.7,
  
  
  balance_num_01 = TRUE,               # Solo sexo o sexo + couple
  
  n_per_group_TDC = 29,
  n_per_group_control = 22,
  
  Subject_2_Study = TRUE,          # Seleccionamos solo los que cumplen los criterios. (80% )
  Out_rt_300_3000 = FALSE,
  recode_rt_300_3000 =TRUE,
  Out_IQR = FALSE,                  # Quitamos  por rango intercuartilico
  interfernce_union = TRUE,         # Tipo 3 todos los tipos de respuesta por separado FALSE,
  # Tipo 2 Los factores juntos TRUE
  Error_plus_Subject_block_600 = TRUE,
  Error_plus_Subject_600 = FALSE,
  Only_attribute = FALSE,           # Nos quedamos solo con los atributos
  Subject_out = c(102),             # Falla de hacer
  measure = "log",                  # D, RT, log o z
  save_plots_06 = TRUE              # Booeleano para guardar los plots
)
writeLines(text = toJSON(list), con = "Data/00_Initial.json")

source("Scripts/02_Prepare_EzAnova.R")
# source("Scripts/03_Prepare_EzAnova.R")
# source("Scripts/04_ezANOVA.R")
