t.test.Comparison.Function.Ch <- function( data, StringResponse, StringFactor, ...){
  ############################
  # Factor
  ############################
  data[,StringFactor ] <- factor(data[,StringFactor ] )

  ############################
  # Create data.frame
  ############################
  T.Test <- data.frame(Name = NA, LevelOne =NA, LevelTwo= NA,statistic = NA, df = NA, df = NA, p.value = NA )
  T.Test <- T.Test[-1,]
  NumberOfFactors <- length(levels(data[, StringFactor]) ) 
  
  ############################
  # t.test computation
  ############################
  for(i in 1:(  NumberOfFactors - 1 ) ){
    LevelOne <- levels(data[, StringFactor])[i]
    for(j in (i+1):( NumberOfFactors) ){
      LevelTwo <- levels(data[, StringFactor])[j]
      x <-t.test( x =data[data[, StringFactor] == LevelOne, StringResponse] ,
                  y =data[data[, StringFactor] == LevelTwo, StringResponse], ... )  
      
      T.Test <- rbind( T.Test, 
                       data.frame(Name = paste(LevelOne,LevelTwo, sep= "."),
                                  LevelOne = LevelOne, 
                                  LevelTwo= LevelTwo,
                                  statistic = x$statistic, df = x$parameter, p.value = x$p.value )
      )
    }
  }
  
  ############################
  # Significacion
  ############################
  T.Test$Sign <- "n.s."
  if(dim( T.Test[  T.Test$p.value <= 0.1 & T.Test$p.value > 0.05 ,] )[1] > 0 ){
    T.Test[  T.Test$p.value <= 0.1 & T.Test$p.value > 0.05 ,]$Sign <- "·"
  }

  if(dim( T.Test[  T.Test$p.value <= 0.05 & T.Test$p.value > 0.01 ,] )[1] > 0 ){
    T.Test[  T.Test$p.value <= 0.05 & T.Test$p.value > 0.01 ,]$Sign <- "*"
  }

  if(dim( T.Test[  T.Test$p.value <= 0.01 & T.Test$p.value > 0.001 ,] )[1] > 0 ){
    T.Test[  T.Test$p.value <= 0.01 & T.Test$p.value > 0.001 ,]$Sign <- "**"
  }

  if(dim( T.Test[  T.Test$p.value <= 0.001 ,] )[1] > 0 ){
    T.Test[  T.Test$p.value <= 0.001  ,]$Sign <- "***"
  }

  ############################
  # Bonferroni p.value
  ############################
  T.Test$p.value.Adjusted <- p.adjust(T.Test$p.value, method = "bonferroni")
  
  ############################
  # Significacion
  ############################
  T.Test$Sign.Adjusted <- "n.s."
  if(dim( T.Test[  T.Test$p.value.Adjusted <= 0.1 & T.Test$p.value.Adjusted > 0.05 ,] )[1] > 0 ){
    T.Test[  T.Test$p.value.Adjusted <= 0.1 & T.Test$p.value.Adjusted > 0.05 ,]$Sign.Adjusted <- "·"
  }
  
  if(dim( T.Test[  T.Test$p.value.Adjusted <= 0.05 & T.Test$p.value.Adjusted > 0.01 ,] )[1] > 0 ){
    T.Test[  T.Test$p.value.Adjusted <= 0.05 & T.Test$p.value.Adjusted > 0.01 ,]$Sign.Adjusted <- "*"
  }
  
  if(dim( T.Test[  T.Test$p.value.Adjusted <= 0.01 & T.Test$p.value.Adjusted > 0.001 ,] )[1] > 0 ){
    T.Test[  T.Test$p.value.Adjusted <= 0.01 & T.Test$p.value.Adjusted > 0.001 ,]$Sign.Adjusted <- "**"
  }
  
  if(dim( T.Test[  T.Test$p.value.Adjusted <= 0.001 ,] )[1] > 0 ){
    T.Test[  T.Test$p.value.Adjusted <= 0.001  ,]$Sign.Adjusted <- "***"
  }
  
  
  ############################
  # Effect Size d of cohen
  ############################
  t <- T.Test$statistic
  df <-T.Test$df
  T.Test$Effect.Size <- sqrt( t^2/(t^2+df) )
  
  T.Test$statistic <- round( T.Test$statistic, digits = 3)
  T.Test$df <- round( T.Test$df, digits = 3)
  T.Test$p.value <- round( T.Test$p.value, digits = 3)
  T.Test$p.value.Adjusted <- round( T.Test$p.value.Adjusted, digits = 3)
  T.Test$Effect.Size <- round( T.Test$Effect.Size, digits = 3)
  
  
  return(T.Test)
  
  
}

