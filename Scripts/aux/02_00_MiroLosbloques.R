library(data.table)

# S load data -------------------------------------------------------------
load("RData/00_read_prepare_data.RData")
# E load data -------------------------------------------------------------


summary( data[ data$block == 1, ])
data[ data$block == 1, ] %>% head
x <- data[ data$block == 1 & as.character(data$correct_response) == as.character(data$response),]
x %>% head
table( x$tipo , x$correct_response)

summary( data[ data$block == 2, ])
data[ data$block == 2, ] %>% head
x <- data[ data$block == 2 & as.character(data$correct_response) == as.character(data$response),]
x %>% head
table( x$tipo , x$correct_response)


summary( data[ data$block == 3, ])
data[ data$block == 3, ] %>% head
x <- data[ data$block == 3 & as.character(data$correct_response) == as.character(data$response),]
x %>% head
table( x$tipo , x$correct_response)


summary( data[ data$block == 4, ])
data[ data$block == 4, ] %>% head
x <- data[ data$block == 4 & as.character(data$correct_response) == as.character(data$response),]
x %>% head
table( x$tipo , x$correct_response)

