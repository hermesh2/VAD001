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

# setwd(dir = list_script$dir_script)

sessionInfo() 
Sys.info()
load("RData/02_Prepare_Ez_Anova.RData")
d <- copy( dataDTcorrect)
# E Basics ----------------------------------------------------------------


# S selection -------------------------------------------------------------
if( !is.null(list_script$Subject_out_01) ){
  nrow(dataDTcorrect) %>% print
  dataDTcorrect <- dataDTcorrect[ dataDTcorrect$SubjectIn == 1, ]
  nrow(dataDTcorrect) %>% print
}

if( list_script$Subject_2_Study_03 == TRUE){
  nrow(dataDTcorrect) %>% print
  dataDTcorrect <- dataDTcorrect[ dataDTcorrect$Subject_2_Study == 1, ]
  nrow(dataDTcorrect) %>% print
}

if( list_script$Out_rt_300_3000_03 == TRUE){ # Esto seria quitarlos directamente
  (dataDTcorrect$rt_minor_300 != 1 &
     dataDTcorrect$rt_bigger_3000 != 1) %>%  table %>% print
  dataDTcorrect <- dataDTcorrect[ dataDTcorrect$rt_minor_300 != 1 &
                                    dataDTcorrect$rt_bigger_3000 != 1, ]
}

if( list_script$recode_rt_300_3000_03 == TRUE){ # Esto seria recodificarlos
  (dataDTcorrect$rt_minor_300 != 1 &
     dataDTcorrect$rt_bigger_3000 != 1) %>%  table %>% print
  dataDTcorrect$response_time[ dataDTcorrect$rt_minor_300 == 1] <- list_script$min_RT
  dataDTcorrect$response_time[ dataDTcorrect$rt_bigger_3000 == 1] <- list_script$max_RT
}


if( list_script$Out_IQR_03 == TRUE){ 
  (dataDTcorrect$Out_IQR != 1 )%>%  table %>% print
  dataDTcorrect <- dataDTcorrect[ dataDTcorrect$Out_IQR != 1, ]
}


if( list_script$Error_plus_Subject_block_600_03 ==  TRUE){ # Borg 2010, anyadimos los 600 msec a los errores
  dataDTonlyCorrectSubject <- # La media de las correctas mas 600
    dataDTcorrect[ correct == 1,  list( meanCorrectblock  =  mean(response_time, na.rm = TRUE )  + 600), 
                   by = c("Subject" , "block")]
  dataDTcorrect <- merge( x = dataDTcorrect , y = dataDTonlyCorrectSubject ,  by = c("Subject" , "block")  )
  # dataDTcorrect[ correct == 0 , list(response_time -  meanCorrectblock)] %>%  summary
  dataDTcorrect[ correct == 0 , response_time :=  meanCorrectblock ]
  dataDTcorrect$correct %>%  table
}


if( list_script$Error_plus_Subject_600_03 ==  TRUE){# Borg 2010 (Basado en pero no es lo que hace)
  dataDTonlyCorrectSubject <- 
    dataDTcorrect[ correct == 1,  list( meanCorrectblock  =  mean(response_time, na.rm = TRUE )  + 600), 
                   by = c("Subject" )]
  dataDTcorrect <- merge( x = dataDTcorrect , y = dataDTonlyCorrectSubject ,  by = c("Subject" )  )
  # dataDTcorrect[ correct == 0 , list(response_time -  meanCorrectblock)] %>%  summary
  dataDTcorrect[ correct == 0 , response_time :=  meanCorrectblock ]
}
# E selection -------------------------------------------------------------



save( dataDTcorrect, file = "RData/03_filter.RData")
