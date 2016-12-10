rm(list = ls());gc()

load(file = "RData/02_01_check_accuracy_ratio.RData")


# S miro correctas en general ---------------------------------------------
CheckSubjects <- 
  dataDT[, list( Subject = Subject[1], Group = Group[1] , mean_correct = mean(correct)),
                        by = paste(dataDT$Subject, dataDT$block) ]
x <- dataDT[, list( Subject = Subject[1], Group = Group[1] , mean_correct = round(mean(correct), digits = 2)),
       by = paste(dataDT$Subject) ]

x[ x$mean_correct < 0.7, ]
x[ x$mean_correct >= 0.7, ] 
x[ x$mean_correct >= 0.7, ]$Group %>% table
 6/35
 4/26
 # E miro correctas en general ---------------------------------------------
 
 # S miro por parejas de bloques ------------------------------------------- 
 dataDT2 <- copy(dataDT[ , block3 := ifelse(test = block %in% 1:2, yes = "acction F", no = "acction R")])
 x <- dataDT2[, list( Subject = Subject[1], Group = Group[1] , mean_correct = round(mean(correct), digits = 2)),
             by = paste(dataDT$Subject, dataDT$block3) ]
 x
 x1 <- x[ x$mean_correct < 0.7, ]
 x1
 x1[ , list(min = min(mean_correct) , max = max(mean_correct) , Group = Group[1] ), by = Subject ]
 quitar <- x1$Subject %>% unique 
 # E miro por parejas de bloques -------------------------------------------
 
 


 x <-
   dataDT[! Subject %in% quitar , 
          list( Subject = Subject[1], Group = Group[1] , mean_correct = round(mean(correct), digits = 2)),
          by = Subject ]

 x[, 
        list( tamano_muestra = .N),
        by = Group ] 
 