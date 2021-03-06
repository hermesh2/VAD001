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
# rm(list = ls());gc()

sessionInfo()
Sys.info()
load("RData/03_filter.RData")
source("Scripts/Functions/t.test.Comparison.Function.Ch.R")
source("Scripts/Functions/PlotMeanCh.R")

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

# Junto tipos de D
dataDTcorrect[ , tipo2 := gsub("R", "", tipo) %>% gsub("F", "", .) ]
dataDTcorrect[ , block3 := gsub("R", "", block2) %>% gsub("F", "", .) ]


dataDT_ez_all <- dcast( formula = Subject ~  block3 + tipo2 ,data = dataDTcorrect ,sep = "_", fun = function(x){
  mean(x, na.rm = TRUE)
} , value.var="response_time")






# S D mesure --------------------------------------------------------------
# Computo la D measure

####################
# ActionF sd
####################
dataDTcorrect[, aux := "sd"]
dataDTcorrect[ block %in% c("1","2") , ]$tipo
dataDT_ez_sd_ActionF_Pos_1_2 <-
  dcast( formula = Subject ~  aux ,data = dataDTcorrect[ block %in% c("1","2") & tipo == "accionF", ] , fun = function(x){
  sd(x, na.rm = TRUE)
} , value.var="response_time")

####################
# ActionR sd
####################
dataDT_ez_sd_ActionF_Pos_3_4 <- 
  dcast( formula = Subject ~  aux ,data = dataDTcorrect[ block %in% c("3","4") & tipo == "accionR",] , fun = function(x){
    sd(x, na.rm = TRUE)
    } , value.var="response_time")


####################
# Action sd
####################
dataDT_ez_sd_ActionF_Pos_all_sd <-
  dcast( formula = Subject ~  aux ,data = dataDTcorrect[ block %in% c("1","2","3","4") & tipo == "accionF", ] , fun = function(x){
    sd(x, na.rm = TRUE)
  } , value.var="response_time")



####################
# AccionF D
####################
dataDT_ez_merge_D_ActionF_Pos_1_2 <- data.table(
    Subject =  dataDT_ez$Subject,
    D_D_D_ActionF_Pos_1_2 =
      ( dataDT_ez$`ActionF-Pos_accionF` - dataDT_ez$`ActionF-Neg_accionF` ) /
      dataDT_ez_sd_ActionF_Pos_1_2$sd
  )
####################
# AccionR D
####################
dataDT_ez_merge_D_ActionR_Pos_3_4 <- data.table(
  Subject =  dataDT_ez$Subject,
  D_D_D_ActionR_Pos_3_4 =
    ( dataDT_ez$`ActionR-Pos_accionR` - dataDT_ez$`ActionR-Neg_accionR` ) /
    dataDT_ez_sd_ActionF_Pos_3_4$sd
)

####################
# Accion D
####################
dataDT_ez_merge_D_all <- data.table(
  Subject =  dataDT_ez$Subject,
  Dall_D_D_Action_Pos_all =
    ( dataDT_ez_all$`Action-Pos_accion` - dataDT_ez_all$`Action-Neg_accion` ) /
    dataDT_ez_sd_ActionF_Pos_all_sd$sd
)


dataDTcorrect[, aux := rm()]
# E D mesure --------------------------------------------------------------

################
# Merge all
################
dataDT_ez <- merge(x = dataDT_ez, y = dataDT_ez_merge_z                , by.x = "Subject", by.y = "Subject_z"  )
dataDT_ez <- merge(x = dataDT_ez, y = dataDT_ez_merge_log              , by.x = "Subject", by.y = "Subject_log")
dataDT_ez <- merge(x = dataDT_ez, y = dataDT_ez_merge_D_ActionF_Pos_1_2, by.x = "Subject", by.y = "Subject"    )
dataDT_ez <- merge(x = dataDT_ez, y = dataDT_ez_merge_D_ActionR_Pos_3_4, by.x = "Subject", by.y = "Subject"    )
dataDT_ez <- merge(x = dataDT_ez, y = dataDT_ez_merge_D_all, by.x = "Subject", by.y = "Subject"    )




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





# S Accion Fisica ---------------------------------------------------------
cat("\n================================================\n")
cat("Valores para Accion fisica")
cat("\n================================================\n")
d <- dataDT_ez[ Type == "D" & grepl(pattern = "ActionF", x = variable), ]
d[ , list( mean = mean(value), sd = sd(value) ), by = Group]  %>%  print
d[ , list( median = median(value) ), by = Group] %>% print
# Group       mean
# 1: Grupo control -0.9464425
# 2:           TDC -0.5427282
# d[ , list( mean = hist(value, main = paste0( "ActionF_", Group[1])) ), by = Group]
shapiro.test(x = d$value)
shapiro.test(x = d$value[d$Group == "TDC"])
shapiro.test(x = d$value[d$Group == "Grupo control"])
ks.test(d$value[d$Group == "TDC"], "pnorm", mean(d$Group == "TDC"), sd(d$Group == "TDC"))
ks.test(d$value[d$Group == "Grupo control"], "pnorm", mean(d$Group == "Grupo control"), sd(d$Group == "Grupo control"))

t.test(value ~ Group, data = d, paired = F, var.equal = F) %>% print
t.test.Comparison.Function.Ch(data = data.frame(d), StringResponse = "value", StringFactor = "Group"  ) %>% print
wilcox.test(x = d$value[ d$Group == "Grupo control"], y =  d$value[d$Group == "TDC"])

p <- ggplot(d, aes(factor(Group), value))
set.seed(1)
ggplot_distr <-
  p + geom_violin(aes(colour =  factor(Group) ) ) +
    geom_point(aes(colour = factor(Group) ), position= "jitter" ) + 
    # ggtitle ("") +
    # xlab("Month") +  ylab ("Average temperature ( ºC )")
    theme_bw() 
print(ggplot_distr)
d[ , Group2 := "CD"]
d[ Group == "Grupo control", Group2 := "NCD"]
d[ , .N , Group]
plotMeanCh(response = d$value, factor1 = factor(d$Group2), col = 1, error.bars = "se", pch = 15:16 , 
           Xlim = c(0.75, 2.25),
           lty = 0, xlab = "Group", ylab = "D-measuere",
           main = "D-Measure PV" )
grid()
d_F <- d

cat("\nRT fisico\n")
d <- dataDT_ez[ measure == "RT" & grepl(pattern = "ActionF", x = variable), ]
d[ , list( mean = mean(value), sd = sd(value) ), by = Group]  %>%  print

# E Accion Fisica ---------------------------------------------------------


# S Accion relacional -----------------------------------------------------
write.table(x = dataDT_ez[  Block %in% c("D","RT") | measure %in% c("RT")], file = "Results/04_Matriz.csv",sep = ";", dec = ",",row.names = FALSE)

cat("\n================================================\n")
cat("Valores para Accion relacional")
cat("\n================================================\n")

d <- dataDT_ez[ Type == "D" & grepl(pattern = "ActionR", x = variable), ]
d[ , list( mean = mean(value), sd = sd(value) ), by = Group]  %>%  print
d[ , list( median = median(value) ), by = Group] %>% print
# Group      mean
# 1: Grupo control -1.063566
# 2:           TDC -1.297336
d[ , list( mean = hist(value, main = paste0( "ActionR_", Group[1])) ), by = Group]
shapiro.test(x = d$value[d$Group == "TDC"])
shapiro.test(x = d$value[d$Group == "Grupo control"])
ks.test(d$value[d$Group == "TDC"], "pnorm", mean(d$Group == "TDC"), sd(d$Group == "TDC"))
ks.test(d$value[d$Group == "Grupo control"], "pnorm", mean(d$Group == "Grupo control"), sd(d$Group == "Grupo control"))

t.test(value ~ Group, data = d, paired = F, var.equal = F) %>% print
t.test.Comparison.Function.Ch(data = data.frame(d), StringResponse = "value", StringFactor = "Group"  ) %>% print
wilcox.test(x = d$value[ d$Group == "Grupo control"], y =  d$value[d$Group == "TDC"])
# t.test(value ~ Group, data = d, paired = FALSE) %>% print
# t.test.Comparison.Function.Ch(data = data.frame(d), StringResponse = "value", StringFactor = "Group",paired = FALSE ) %>% print

d_R <- d
cat("\nRT relacional\n")
d <- dataDT_ez[ measure == "RT" & grepl(pattern = "ActionR", x = variable), ]
d[ , list( mean = mean(value), sd = sd(value) ), by = Group]  %>%  print
# E Accion relacional -----------------------------------------------------



# S D accion juntos -------------------------------------------------------
cat("\n================================================\n")
cat("Valores para Accion relacional y fisica sin distincion")
cat("\n================================================\n")
d <- dataDT_ez[ Type == "D" & grepl(pattern = "Dall", x = variable), ]
d[ , list( mean = mean(value), sd = sd(value) ), by = Group]  %>%  print
d[ , list( median = median(value) ), by = Group] %>% print
#            Group    median
# 1: Grupo control 1.0423264
# 2:           TDC 0.7067977
d[ , list( mean = hist(value, main = paste0( "Action_", Group[1])) ), by = Group]
shapiro.test(x = d$value[d$Group == "TDC"])
shapiro.test(x = d$value[d$Group == "Grupo control"])
ks.test(d$value[d$Group == "TDC"], "pnorm", mean(d$Group == "TDC"), sd(d$Group == "TDC"))
ks.test(d$value[d$Group == "Grupo control"], "pnorm", mean(d$Group == "Grupo control"), sd(d$Group == "Grupo control"))

t.test(value ~ Group, data = d, paired = F, var.equal = F) %>% print
t.test.Comparison.Function.Ch(data = data.frame(d), StringResponse = "value", StringFactor = "Group"  ) %>% print
wilcox.test(x = d$value[ d$Group == "Grupo control"], y =  d$value[d$Group == "TDC"])
# E D accion juntos -------------------------------------------------------

# S ANOVA -----------------------------------------------------------------
cat("\n================================================\n")
cat("ANOVA")
cat("\n================================================\n")
d <- rbindlist(l =  list(d_F, d_R), use.names = TRUE, fill = TRUE)
d[ , variable2 := variable %>%  as.character %>% substr(start = 1, stop = 9, .) %>% as.factor]
d[ , Group :=  factor( Group)]
d[ , Subject :=  factor( Subject)]
ezANOVA(
  data = d
  , dv = .(value) # Por si las manovas
  , wid = .(Subject)
  , within = .(variable2)
  , between = .(Group)
) %>% print

plotMeanCh(response = d$value, factor1 = factor(d$Group),factor2 = factor(d$variable) ,col = 1:2, error.bars = "se", pch = 15:16 ,
           Xlim = c(0.75, 2.25),
           lty = 1, xlab = "Group", ylab = "D-measuere",
           main = "D-Measure" )
lmer( formula = value ~ variable * Group + ( 1|Subject), data = d)
summary(lme(fixed= value ~ variable * Group, random=   ~ 1|Subject, data=d))
