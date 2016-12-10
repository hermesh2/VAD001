rm(list = ls());gc()

# S Basics ----------------------------------------------------------------
if(! require("rjson")){
  install.packages("rjson")
}
library("rjson")

list_script <- fromJSON(readLines(con = "Data/00_Initial.json") ) 

lapply(X = list_script$needed_script, FUN = function(x){
  print(x)
  if( !x %in% installed.packages() ){
    install.packages(x)
  }
  library(x, character.only = TRUE)
})

setwd(dir = list_script$dir_script)
# rm(list = ls());gc()

sessionInfo()
Sys.info()
# E Basics ----------------------------------------------------------------


# S load data -------------------------------------------------------------
load("RData/00_read_prepare_data.RData")
# E load data -------------------------------------------------------------


# S Prepare data ----------------------------------------------------------
data$block2 <-NA
for( i in list_script$NombreBlock_vector_01){
  data$block2[ data$block == i] <- 
    list_script$NombreBlock_names_01[i]
}
data$block2 <-data$block2 %>% factor
# table(data$block, data$block2)

data$Subject <-
  data$Subject %>%  as.character %>%  gsub("[^0-9]", "", .)%>%  factor # Clean the repeatd files. Looks better in this way.

number_trial_per_block <- nrow(data) / length( unique( data$Subject)) / 
  length(list_script$NombreBlock_vector_01)# 4 Antiguamente ponia un numero # Is divided by four because we have four blocks
data$trial_order <- 1: number_trial_per_block  
rm( number_trial_per_block);gc()


# Select the data table without the first trial
dataDT <- 
  data[ data$trial_order != 1, 
                list_script$variables_guardadas_01] %>% data.table
# dataDT %>%  summary
save(dataDT, file = "RData/02_01_check_accuracy_ratio.RData")
######################################
#### OUT data checks
######################################
by( data = dataDT$response_time, INDICES = dataDT$Subject, hist)
dataDT$correct %>% table %>%  prop.table
(dataDT$response_time <= list_script$min_RT ) %>%  table %>%  prop.table # Miramos los que tienen menos 300 msec
(dataDT$response_time >= list_script$max_RT ) %>%  table %>%  prop.table # Miramos los que tienen mas 3000 msec

if(list_script$correct_only_01 == TRUE){
  dataDTcorrect <- dataDT[ dataDT$correct == 1, ] # Only the correct answer  
}else{
  dataDTcorrect <- dataDT
}

# (dataDTcorrect$response_time <= list_script$min_RT ) %>%  table %>%  prop.table # Miramos los que tienen menos 300 msec
# (dataDTcorrect$response_time >= list_script$max_RT ) %>%  table %>%  prop.table # Miramos los que tienen mas 3000 msec

######################################
#### Compute range matrix
######################################
Ranges <- dataDTcorrect$response_time %>%  by(data = ., INDICES = dataDTcorrect$Subject, FUN = function(x){ 
  list( min = median(x) - list_script$IQR_prod * IQR(x) , max = median(x)  + list_script$IQR_prod * IQR(x))
  }) # Compute the ranges

Ranges <- 
  data.frame( 
    Subject = NA,
    min = Ranges %>% lapply(FUN = function(x){
      x$min
      }) %>% unlist ,
    max = Ranges %>% lapply(FUN = function(x){
      x$max
      }) %>%  unlist )
Ranges$Subject <- 
  row.names(Ranges)  %>%  factor 
row.names(Ranges) <- rm()

######################################
#### Out people with less than 80% ratio
######################################
CheckSubjects <-
  # dataDT[, list( Subject = Subject[1], Group = Group[1] , mean_correct = mean(correct)),
  #        by = paste(dataDT$Subject, dataDT$block) ]
  dataDT[, list( Subject = Subject[1], Group = Group[1] , mean_correct = mean(correct)),
         by = paste(dataDT$Subject) ]
CheckSubjects$Subject[ CheckSubjects$mean_correct < list_script$Ratio_response_Block ] %>% table
# CheckSubjects[ CheckSubjects$mean_correct < list_script$Ratio_response_Block ,]
SubjectsFailAccuracyRatio <- 
  CheckSubjects$Subject[ CheckSubjects$mean_correct < list_script$Ratio_response_Block] %>% unique

# CheckSubjects$Subject[ CheckSubjects$mean_correct < 0.6] %>% unique %>%  length()
# CheckSubjects %>% write.table(file = "Results/01_ratios_acierto_sujeto.csv", sep = ";", dec = ",", row.names = FALSE)
# E Prepare data ----------------------------------------------------------



# S filters ---------------------------------------------------------------
######################################
#### Selected Per Study
######################################
set.seed(list_script$seet_seed)
if( list_script$balance_num_01 == TRUE){
  Subject_2_Study <- c( dataDTcorrect$Subject[ dataDTcorrect$Group == "Grupo control" &   # Selecciono mujeres
                                                 !(dataDTcorrect$Subject %in% SubjectsFailAccuracyRatio)] %>%
                          unique %>%  as.character %>% 
                          sample( size = list_script$n_per_group_control, replace = FALSE) ,
                        
                        dataDTcorrect$Subject[ dataDTcorrect$Group == "TDC" & # Selecciono hombres
                                                 !(dataDTcorrect$Subject %in% SubjectsFailAccuracyRatio)] %>%
                          unique %>% as.character  %>% 
                          sample( size = list_script$n_per_group_TDC, replace = FALSE)
  )
  dataDTcorrect$Subject_2_Study <- ifelse( test = dataDTcorrect$Subject %in% Subject_2_Study, 
                                           yes = 1, no = 0)
  
}
######################################
#### 300 msec
######################################
dataDTcorrect$rt_minor_300 <- 
  ifelse(test = dataDTcorrect$response_time < list_script$min_RT, yes = 1, no = 0)

######################################
#### 3000 msec
######################################
dataDTcorrect$rt_bigger_3000 <- 
  ifelse(test = dataDTcorrect$response_time > list_script$max_RT, yes = 1, no = 0)

######################################
#### Inside the range
######################################
dataDTcorrect <- merge( x = dataDTcorrect , y = Ranges, by = "Subject", all.x = TRUE, all.y = FALSE)
dataDTcorrect$Out_IQR <- 
  ifelse(test = dataDTcorrect$response_time < dataDTcorrect$min | dataDTcorrect$response_time > dataDTcorrect$max,
       yes = 1, no =  0 )
# E filters ---------------------------------------------------------------


# S selection -------------------------------------------------------------
# if( list_script$Subject_2_Study == TRUE){
#   nrow(dataDTcorrect) %>% print
#   dataDTcorrect <- dataDTcorrect[ dataDTcorrect$Subject_2_Study == 1, ]
#   nrow(dataDTcorrect) %>% print
# }

if( list_script$Out_rt_300_3000 == TRUE){ 
  (dataDTcorrect$rt_minor_300 != 1 &
    dataDTcorrect$rt_bigger_3000 != 1) %>%  table %>% print
    dataDTcorrect <- dataDTcorrect[ dataDTcorrect$rt_minor_300 != 1 &
                                    dataDTcorrect$rt_bigger_3000 != 1, ]
}

if( list_script$recode_rt_300_3000 == TRUE){ 
  (dataDTcorrect$rt_minor_300 != 1 &
     dataDTcorrect$rt_bigger_3000 != 1) %>%  table %>% print
  dataDTcorrect$response_time[ dataDTcorrect$rt_minor_300 == 1] <- list_script$min_RT
  dataDTcorrect$response_time[ dataDTcorrect$rt_bigger_3000 == 1] <- list_script$max_RT
}


if( list_script$Out_IQR == TRUE){ 
  (dataDTcorrect$Out_IQR != 1 )%>%  table %>% print
  dataDTcorrect <- dataDTcorrect[ dataDTcorrect$Out_IQR != 1, ]
}


if( list_script$Error_plus_Subject_block_600 ==  TRUE){ # Borg 2010
  dataDTonlyCorrectSubject <- # La media de las correctas mas 600
    dataDTcorrect[ correct == 1,  list( meanCorrectblock  =  mean(response_time, na.rm = TRUE )  + 600), 
                   by = c("Subject" , "block")]
  dataDTcorrect <- merge( x = dataDTcorrect , y = dataDTonlyCorrectSubject ,  by = c("Subject" , "block")  )
  # dataDTcorrect[ correct == 0 , list(response_time -  meanCorrectblock)] %>%  summary
  dataDTcorrect[ correct == 0 , response_time :=  meanCorrectblock ]
  dataDTcorrect$correct %>%  table
}


if( list_script$Error_plus_Subject_600 ==  TRUE){# Borg 2010 (Basado en pero no es lo que hace)
  dataDTonlyCorrectSubject <- 
    dataDTcorrect[ correct == 1,  list( meanCorrectblock  =  mean(response_time, na.rm = TRUE )  + 600), 
                   by = c("Subject" )]
  dataDTcorrect <- merge( x = dataDTcorrect , y = dataDTonlyCorrectSubject ,  by = c("Subject" )  )
  # dataDTcorrect[ correct == 0 , list(response_time -  meanCorrectblock)] %>%  summary
  dataDTcorrect[ correct == 0 , response_time :=  meanCorrectblock ]
}

# if( list_script$Only_attribute ==  TRUE){
#   dataDTcorrect <- dataDTcorrect[tipo != "FACEBOOK" ,  ]
# }


# E selection -------------------------------------------------------------


# S Prepare Matrix --------------------------------------------------------


################
# Compute values
################
# dataDT_ez <- dcast( formula = Subject ~  block2 + tipo ,data = dataDTcorrect , fun = function(x){
#   mean(x, na.rm = TRUE)
#   } , value.var="response_time")
# 
# dataDT_ez_merge_log <- dcast( formula = Subject ~  block2 + tipo ,data = dataDTcorrect , fun = function(x){
#   mean( log (x) , na.rm = TRUE)
# } , value.var="response_time")
# setnames(x = dataDT_ez_merge_log , old = dataDT_ez_merge_log %>%  names, new = dataDT_ez_merge_log %>%
#            names %>%  paste(. , "log", sep = "_" ) )
# 
# 
# dataDT_ez_merge_z <- dcast( formula = Subject ~  block2 + tipo ,data = dataDTcorrect , fun = function(x){
#   mean( scale (x) , na.rm = TRUE)
# } , value.var="response_time")
# setnames(x = dataDT_ez_merge_z , old = dataDT_ez_merge_z %>%  names, new = dataDT_ez_merge_z %>%
#            names %>%  paste(. , "z", sep = "_" ) )
# 
# 
# 
# 
# # Computo la D measure
# dataDT_ez_sd <- dcast( formula = Subject ~  tipo ,data = dataDTcorrect[ tipo2 == "Interf",] , fun = function(x){
#   sd(x, na.rm = TRUE)
# } , value.var="response_time")
# 
# dataDT_ez_log_sd <- dcast( formula = Subject ~  tipo ,data = dataDTcorrect[ tipo2 == "Interf",] , fun = function(x){
#   sd(log(x), na.rm = TRUE)
# } , value.var="response_time")
# 
# 
# 
# if( list_script$interfernce_union == TRUE){ # Elijo como hago el analisis
#   dataDT_ez_merge_D <- data.table(
#     Subject =  dataDT_ez$Subject,
#     D_D_D =
#       ( dataDT_ez$`Help-FB_Interf`  - dataDT_ez$`Sex-FB_Interf`) /
#       dataDT_ez_sd$Interf
#   )
#   # dataDT_ez_merge_D2 <- data.table( 
#   #   Subject =  dataDT_ez_merge_log$Subject,
#   #   D_D_D =
#   #     ( dataDT_ez_merge_log$`Help-FB_Interf`  - dataDT_ez_merge_log$`Sex-FB_Interf`) / 
#   #     dataDT_ez_log_sd$Interf
#   # )
# }else{
# #  Por hacer
# }
# ################
# # Merge all
# ################
# dataDT_ez <- merge(x = dataDT_ez, y = dataDT_ez_merge_z, by.x = "Subject", by.y = "Subject_z")
# dataDT_ez <- merge(x = dataDT_ez, y = dataDT_ez_merge_log, by.x = "Subject", by.y = "Subject_log")
# dataDT_ez <- merge(x = dataDT_ez, y = dataDT_ez_merge_D, by.x = "Subject", by.y = "Subject")
# 
# dataDT_ez <- melt(data = dataDT_ez, id.vars="Subject")
# 
# dataDT_ez$Block <-
#   dataDT_ez$variable %>%  as.character %>%  strsplit( "_") %>%
#   lapply(FUN = function(x){x[1]}) %>%  unlist
# dataDT_ez$Type <-
#   dataDT_ez$variable %>%  as.character %>%  strsplit( "_") %>%
#   lapply(FUN = function(x){x[2]}) %>%  unlist
# dataDT_ez$measure <-
#   dataDT_ez$variable %>%  as.character %>%  strsplit( "_") %>%
#   lapply(FUN = function(x){x[3]}) %>%  unlist
# dataDT_ez$measure <- ifelse(test = is.na(dataDT_ez$measure), yes = "RT", no =  dataDT_ez$measure )
# 
# dataSubjectSexo <-
#   dataDT[ , list(Sex = sexo[1], Couple = pareja[1]), by =Subject ]
# dataDT_ez <- merge(x = dataDT_ez, y = dataSubjectSexo, by = "Subject", all.x = TRUE, all.y = FALSE )
# dataDT_ez$Block <- dataDT_ez$Block %>%  factor
# 
# dataDT_ez$variable <- dataDT_ez$variable %>%  as.character %>% gsub("_z" , "", .) %>%
#   gsub("_log" , "", .) %>% gsub("_D" , "", .) %>%  as.factor




# Numero de elemetnos por condicion
# (dcast( formula = Subject ~  block2 + tipo3 ,data = dataDTcorrect , fun = function(x){
#   length(x)
# } , value.var="response_time")[, -1, with = FALSE]< 7 ) %>%  table
# dcast( formula = Subject ~  block2 + tipo3 ,data = dataDTcorrect , fun = function(x){
#   length(x)
# } , value.var="response_time")

######################################
#### Checkeo el suejto 102 falla todas en la condición  data$block2 == "Help-FB" & data$tipo == "FACEBOOK"
######################################
# data[ data$Subject == 102 & data$block2 == "Help-FB" , ]
# Ranges[ Ranges$Subject ==  102,]
# 
# 
# dataDTcorrect[ dataDTcorrect$Subject == 102, c("tipo",  "block2")]
# data[ data$Subject == 102, c("tipo",  "block2")]
# data[ data$Subject == 102, c("tipo")] %>%  paste( data[ data$Subject == 102, c(  "block2")]) %>%  table
# data[ data$Subject == 102 & data$block2 == "Help-FB" & data$tipo == "FACEBOOK",]
# dataDT[ dataDT$Subject == 102 & dataDT$block2 == "Help-FB" & dataDT$tipo == "FACEBOOK",]
# CheckSubjects[ CheckSubjects$Subject == 102, ]

# E Prepare Matrix --------------------------------------------------------




# save(dataDT_ez, dataDTcorrect, file = "RData/03_Prepare_EzAnova.RData")
# CheckSubjects
# SubjectsFailAccuracyRatio
# Subject_2_Study
# write.table(dataDT_ez, file = "Results/Con_tipo.csv")
