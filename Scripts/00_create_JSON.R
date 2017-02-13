# S Basics ----------------------------------------------------------------
if(! require("rjson")){
  install.packages("rjson")
}
library("rjson")

list <-  list(
  needed_script = c("dplyr", "data.table", "ez", "ggplot2", "nlme", "lme4", "languageR", "lmerTest", 
                    "modeest", "psych", "Rcmdr"),
  dir_script = getwd(),
  
  ncol_experiment_01 = 230, # Numero de columnas que tiene el experimento
  
  blockAtributo_01 = c(1, 3),
  NombreBlock_vector_01 = 1:4,
  NombreBlock_names_01 = c("ActionF-Pos","ActionF-Neg","ActionR-Pos","ActionR-Neg"),
  variables_guardadas_01 = c("Subject", "correct", "block", "trial_order", "sexo", "target", "tipo", "block2", "response_time", "Group"),
  
  seet_seed = 112345, # 112345
  min_RT = 300,
  max_RT = 3000,
  IQR_prod = 1.5,
  correct_only_01 = FALSE,             # Si quremeos solo las repuestas correctas
  Ratio_response_Block_01 = 0.7,

  Subject_out_01 = 
    c(
      # 119,
      203,  210,  212,  316,  318,  100,  101,
      112,  113,  204, 209,
      211,  213,  215,  303,  307,  311,  313,  315,  
      # 117, 119, 203, 210, 212, 316, 318, 100, 101,
      # 112, 113, 204, 209, 211, 213, 215, 303,
      # 307, 311, 313, 315,  # Estos son por ratio de acierto
      
      
      301, 109, 305,# 216, 
      319, 111             # Estos son por ser lentos
      
      ),             # Nos dice si quitmos sujetos
   
  balance_num_01 = FALSE,               # Nos dice si elegimos el numero de sujetos
  n_per_group_TDC_01 = 29,
  n_per_group_control_01 = 22,
  
  Subject_2_Study_03 = FALSE,          # Seleccionamos solo los que cumplen los criterios. (80% )
  Out_rt_300_3000_03 = FALSE,
  recode_rt_300_3000_03 =TRUE,
  Out_IQR_03 = FALSE,                  # Quitamos  por rango intercuartilico
  # interfernce_union = TRUE,         # Tipo 3 todos los tipos de respuesta por separado FALSE,
  
  # Tipo 2 Los factores juntos TRUE
  Error_plus_Subject_block_600_03 = TRUE,
  Error_plus_Subject_600_03 = FALSE,

  measure = "log",                  # D, RT, log o z
  save_plots_06 = TRUE              # Booeleano para guardar los plots
)
writeLines(text = toJSON(list), con = "Data/00_Initial.json")
writeLines(text = as.character(sessionInfo()), con = "Results/00_Informacion_sesion.txt")

source("Scripts/02_Prepare_EzAnova.R", encoding = "UTF-8")
source("Scripts/03_filter.R")
source("Scripts/04_Prepare_matrix.R")

# source("Scripts/04_ezANOVA.R")
