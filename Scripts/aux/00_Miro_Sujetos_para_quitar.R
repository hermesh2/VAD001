# Ojo he borrado los datos porque ocupaban mucho en Dropbox

# S Basics ----------------------------------------------------------------
if(! require("rjson")){
  install.packages("rjson")
}
library("rjson")

list_script <- fromJSON(readLines(con = "Data/00_Initial.json") ) 
needed_script <- list_script$needed_script
lapply(X = needed_script, FUN = function(x){
  print(x)
  if( !x %in% installed.packages() ){
    install.packages(x)
  }
  library(x, character.only = TRUE)
})



load("RData/00_read_prepare_data.RData")
data$Subject %>% unique %>% length
data <- data %>%  data.table
data[ , list(.N), by = Subject]

# lista <-       c( 119, 203, 210, 212, 316, 318, 100, 101,
# 112, 113, 204, 209, 211, 213, 215, 303,
# 307, 311, 313, 315
# ,319,111 ) 
# lista <-  209
d <- data
d[ , block2:= 1]
d[ block %in% c("1", "2"), block2:= 1]
d[ block %in% c("3","4"), block2:= 2]

d <- data[ , .(menaErr = mean(correct) , RT= median(response_time), Group= Group[1]), by = c("Subject", "block2") ]
d[ menaErr < 0.7, ][ order(menaErr, decreasing = TRUE) , ]
# d[ menaErr < 0.8, ]$Subject %>% unique %>% length()
# 
# 
# aaa <- c( 117,   203,   207,   210,   212,   214,   306,   308,   316,   318,   100,   101,   101,   112,   113,   118,   204,   208,   209,   213,   215,   217,   220,   221,   303,   304,   305,   307,   311,   313,   315,   319 )
# lista[ !lista %in% aaa]
# aaa[ !aaa %in% lista]
# aaa <- c( 117, 117, 119, 119, 203, 207, 210, 210, 210, 210, 212, 212, 212, 214, 214, 306, 306, 308, 316, 318, 318, 318, 100, 100, 100, 100, 101, 101, 112, 113, 113, 113, 118, 204, 204, 208, 209, 213, 213, 213, 213, 215, 215, 215, 215, 217, 220, 221, 303, 304, 305, 307, 307, 311, 313, 313, 313, 315, 319)
# aaa %>%  unique
Times[ order( Times$Task_time, decreasing = TRUE), ]
# xx <- Times[ order( Times$Task_time, decreasing = TRUE), ]$Subject
# donde <- grepl(pattern = "119", x = xx) %>%  which()
# xx <- xx[ 1:length(xx) < donde]
# SujetosDentro <- list_script$Subject_out_01
# xx[ !xx %in% SujetosDentro]
d[ Subject == 216, ]





# S estimacion trials quitados  -------------------------------------------
rm(list = ls())
# load("RData/02_Prepare_Ez_Anova.RData")
# dataDTcorrect_all <- dataDTcorrect
load("RData/03_filter.RData")
dd <- dataDTcorrect
dd[ rt_minor_300 == 1, correct := 1]
dd[ rt_bigger_3000 == 1, correct := 1]
dd %>% summary
# 12.69% fuera por incorrectos, un 1.51% eran menores de 300 msec, un 7.33%
# 1- 0.8731
dd_correct <-  dd[, list( mean(correct)) , by = Subject] 


