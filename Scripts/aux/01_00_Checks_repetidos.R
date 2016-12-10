library(data.table)


# s 110 -------------------------------------------------------------------
x1 <- fread(input = "Data/TDC/subject-110 - copia.csv")
x2 <- fread(input = "Data/TDC/subject-110.csv")
all( x1 == x2, na.rm = TRUE)
# s 110 -------------------------------------------------------------------


# s 306 -------------------------------------------------------------------
x1 <- fread(input = "Data/Grupo control/subject-306 - copia.csv")
x2 <- fread(input = "Data/Grupo control/subject-306.csv")
x3 <- fread(input = "Data/Grupo control/subject-306astrid - copia.csv")
x4 <- fread(input = "Data/Grupo control/subject-306astrid.csv")
all( x1 == x2, na.rm = TRUE)
all( x1 == x3, na.rm = TRUE)
all( x4 == x3, na.rm = TRUE)
# s 306 -------------------------------------------------------------------

# s 307 -------------------------------------------------------------------
x1 <- fread(input = "Data/TDC/subject-307 - copia.csv")
x2 <- fread(input = "Data/TDC/subject-307.csv")
all( x1 == x2, na.rm = TRUE)
# s 307 -------------------------------------------------------------------

# s 308 -------------------------------------------------------------------
x1 <- fread(input = "Data/Grupo control/subject-308.csv")
x2 <- fread(input = "Data/Grupo control/subject-308 - copia.csv")
all( x1 == x2, na.rm = TRUE)
# s 308 -------------------------------------------------------------------



