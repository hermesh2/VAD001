rm(list = ls()) ; gc()
load(file = "RData/02_Prepare_Ez_Anova.RData")
d <- copy( dataDTcorrect)



# S detecto lentos --------------------------------------------------------
d[ , list(response_time = mean(response_time)), by = Subject ]
d[ correct == 1, list(response_time = mean(response_time)), by = Subject ]
d[ correct == 0, list(response_time = mean(response_time)), by = Subject ]
d[ , list(response_time = summary(response_time)), by = Subject ]


d[ , list( count = .N), by = Subject]

d[ response_time > 1.5e4,]$trial_order %>% table
d[ response_time > 1.5e4,]$trial_order %>% table %>%  sum
d[ response_time > 1.5e4,]$Subject %>% table

round(d[ response_time > 3.5e3,]$Subject %>% table /136, digits = 2)
max(d$trial_order)
(d[ response_time > 3.5e3,]$Subject %>% table %>%  sum ) /  nrow(d)
d[ Subject == "212",]


d[ response_time > 3.5e3 & Subject %in% c("111", "109", "101"),]
d[ response_time > 3.5e3 & Subject %in% c("111", "109", "101"),] %>%  summary
d[ response_time > 3.5e3 & Subject %in% c("111", "109", "101"),]$Group %>%  table
d[ response_time > 3.5e3 & Subject %in% c("111", "109", "101"),]$Group %>% 
  table( d[ response_time > 3.5e3 & Subject %in% c("111", "109", "101"),]$Subject %>%  as.character )
# E detecto lentos --------------------------------------------------------


dd <- 
  merge(x = 
          merge( x = 
                   merge(x = 
                           merge(x = d[ response_time > 3e3 & correct == 1, list( count_slow_correct = .N, Group = Group[1]) ,by = Subject] ,
                                 y = d[ correct == 1, list( count_correct = .N) ,by = Subject] ),
                         y = d[ block %in% c("1","2") , list( accuracy_1_2 = sum( correct ) / .N ) , by = Subject] ),
                 y =
                   d[ block %in% c("3","4") , list( accuracy_3_4 = sum( correct ) / .N ) , by = Subject] ),
        y = 
          d[ response_time > 3e3, list( count_slow = .N) ,by = Subject] 
        )


dd[ , ratio_slow_correct := count_slow_correct / count_correct]
dd[ , ratio_slow := count_slow / 136]

dd[ , Fuera_x_ratio_Error := 0]
dd[ accuracy_1_2 < .7 | accuracy_3_4 < 0.7 , Fuera_x_ratio_Error := 1]
dd[ order( ratio_slow_correct , decreasing = TRUE) ,]
dd[ order( count_slow_correct , decreasing = TRUE) ,]
dd[ order( count_slow , decreasing = TRUE) ,]
# dd[ order( count_slow_correct , decreasing = TRUE) ,][ Subject %in% quitar_Sujetos, ]
# dd[ order( count_slow_correct , decreasing = TRUE) ,][ !Subject %in% quitar_Sujetos, ]
