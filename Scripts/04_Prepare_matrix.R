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
load("RData/03_filter.RData")
source("Scripts/Functions/t.test.Comparison.Function.Ch.R")
# E Basics ----------------------------------------------------------------


# S Prepare Matrix --------------------------------------------------------
################
# Compute values
################

dataDTcorrect[ , tipo := gsub(" ", "", tipo) ]
dataDT_ez <- dcast( formula = Subject ~  block2 + tipo ,data = dataDTcorrect ,sep = "_", fun = function(x){
  mean(x, na.rm = TRUE)
  } , value.var="response_time")

dataDT_ez_merge_log <- dcast( formula = Subject ~  block2 + tipo ,data = dataDTcorrect , fun = function(x){
  mean( log (x) , na.rm = TRUE)
} , value.var="response_time")
setnames(x = dataDT_ez_merge_log , old = dataDT_ez_merge_log %>%  names, new = dataDT_ez_merge_log %>%
           names %>%  paste(. , "log", sep = "_" ) )


dataDT_ez_merge_z <- dcast( formula = Subject ~  block2 + tipo ,data = dataDTcorrect , fun = function(x){
  mean( scale (x) , na.rm = TRUE)
} , value.var="response_time")
setnames(x = dataDT_ez_merge_z , old = dataDT_ez_merge_z %>%  names, new = dataDT_ez_merge_z %>%
           names %>%  paste(. , "z", sep = "_" ) )




# Computo la D measure
# ActionF-Pos 
dataDTcorrect[, aux := "sd"]
dataDTcorrect[ block %in% c("1","2") , ]$tipo
dataDT_ez_sd_ActionF_Pos_1_2 <-
  dcast( formula = Subject ~  aux ,data = dataDTcorrect[ block %in% c("1","2") & tipo == "accionF", ] , fun = function(x){
  sd(x, na.rm = TRUE)
} , value.var="response_time")

dataDT_ez_sd_ActionF_Pos_3_4 <- 
  dcast( formula = Subject ~  aux ,data = dataDTcorrect[ block %in% c("3","4") & tipo == "accionR",] , fun = function(x){
    sd(x, na.rm = TRUE)
    } , value.var="response_time")


dataDT_ez_merge_D_ActionF_Pos_1_2 <- data.table(
    Subject =  dataDT_ez$Subject,
    D_D_D_ActionF_Pos_1_2 =
      ( dataDT_ez$`ActionF-Pos_accionF` - dataDT_ez$`ActionF-Neg_accionF` ) /
      dataDT_ez_sd_ActionF_Pos_1_2$sd
  )


dataDT_ez_merge_D_ActionR_Pos_3_4 <- data.table(
  Subject =  dataDT_ez$Subject,
  D_D_D_ActionR_Pos_3_4 =
    ( dataDT_ez$`ActionR-Pos_accionR` - dataDT_ez$`ActionR-Neg_accionR` ) /
    dataDT_ez_sd_ActionF_Pos_1_2$sd
)


dataDTcorrect[, aux := rm()]

################
# Merge all
################
dataDT_ez <- merge(x = dataDT_ez, y = dataDT_ez_merge_z, by.x = "Subject", by.y = "Subject_z")
dataDT_ez <- merge(x = dataDT_ez, y = dataDT_ez_merge_log, by.x = "Subject", by.y = "Subject_log")
dataDT_ez <- merge(x = dataDT_ez, y = dataDT_ez_merge_D_ActionF_Pos_1_2, by.x = "Subject", by.y = "Subject")
dataDT_ez <- merge(x = dataDT_ez, y = dataDT_ez_merge_D_ActionR_Pos_3_4, by.x = "Subject", by.y = "Subject")

dataDT_ez <- melt(data = dataDT_ez, id.vars="Subject")

dataDT_ez$Block <-
  dataDT_ez$variable %>%  as.character %>%  strsplit( "_") %>%
  lapply(FUN = function(x){x[1]}) %>%  unlist
dataDT_ez$Type <-
  dataDT_ez$variable %>%  as.character %>%  strsplit( "_") %>%
  lapply(FUN = function(x){x[2]}) %>%  unlist
dataDT_ez$measure <-
  dataDT_ez$variable %>%  as.character %>%  strsplit( "_") %>%
  lapply(FUN = function(x){x[3]}) %>%  unlist
dataDT_ez$measure <- ifelse(test = is.na(dataDT_ez$measure), yes = "RT", no =  dataDT_ez$measure )

dataSubjectGroup <-
  dataDTcorrect[ , list(Group = Group[1]), by =Subject ]
dataDT_ez <- merge(x = dataDT_ez, y = dataSubjectGroup, by = "Subject", all.x = TRUE, all.y = FALSE )
dataDT_ez$Block <- dataDT_ez$Block %>%  factor

dataDT_ez$variable <- dataDT_ez$variable %>%  as.character %>% gsub("_z" , "", .) %>%
  gsub("_log" , "", .) %>% gsub("_D" , "", .) %>%  as.factor
# E Prepare Matrix --------------------------------------------------------





d <- dataDT_ez[ Type == "D" & grepl(pattern = "ActionF", x = variable), ]
d[ , list( mean = mean(value) ), by = Group]  %>%  print
d[ , list( median = median(value) ), by = Group]
# Group       mean
# 1: Grupo control -0.9464425
# 2:           TDC -0.5427282
d[ , list( mean = hist(value, main = paste0( "ActionF_", Group[1])) ), by = Group]
t.test(value ~ Group, data = d, paired = TRUE) %>% print
t.test.Comparison.Function.Ch(data = data.frame(d), StringResponse = "value", StringFactor = "Group",paired = TRUE ) %>% print

d <- dataDT_ez[ Type == "D" & grepl(pattern = "ActionR", x = variable), ]
d[ , list( mean = mean(value) ), by = Group]  %>%  print
d[ , list( median = median(value) ), by = Group]
# Group      mean
# 1: Grupo control -1.063566
# 2:           TDC -1.297336
d[ , list( mean = hist(value, main = paste0( "ActionR_", Group[1])) ), by = Group]
# t.test(value ~ Group, data = d, paired = TRUE) %>% print
# t.test.Comparison.Function.Ch(data = data.frame(d), StringResponse = "value", StringFactor = "Group",paired = TRUE ) %>% print
