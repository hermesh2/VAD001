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
source("Scripts/aux/02_01_check_accuracy_ratio.R")
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
dataDTcorrect[  ,SubjectIn := 1] 
if( !is.null(list_script$Subject_out_01 ) ){
  dataDTcorrect[ as.character(Subject) %in% list_script$Subject_out_01  ,SubjectIn := 0] 
}



set.seed(list_script$seet_seed)
if( list_script$balance_num_01 == TRUE){
  Subject_2_Study <- 
    
    c( dataDTcorrect$Subject[ dataDTcorrect$Group == "Grupo control" &   # Selecciono mujeres
                                !(dataDTcorrect$Subject %in% SubjectsFailAccuracyRatio) &
                                SubjectIn == 1] %>%
         unique %>%  as.character %>% 
         sample( size = list_script$n_per_group_control_01, replace = FALSE) ,
       dataDTcorrect$Subject[ dataDTcorrect$Group == "TDC" & # Selecciono hombres
                                !(dataDTcorrect$Subject %in% SubjectsFailAccuracyRatio) &
                                SubjectIn == 1] %>%
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


# S save ------------------------------------------------------------------
save(dataDTcorrect , quitar_Sujetos, file = "RData/02_Prepare_Ez_Anova.RData")
# E save ------------------------------------------------------------------
