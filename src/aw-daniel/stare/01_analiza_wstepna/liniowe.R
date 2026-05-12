install.packages("linearOrdering")
install.packages("devtools")
install.packages("topsis")
devtools::install_github("Yard1/linearOrdering")
library(readxl)
library(linearOrdering)
library(topsis)

dane <- read_xlsx("01_analiza_wstepna/dane.xlsx")

dane <- dane[, -c(1)]

dane <- dane[, -c(6, 8, 11, 12)]
dane

d <- as.matrix(dane)
w <- c(1, 1, 1, 1, 1, 1, 1, 1)
i <- c("+", "+", "+", "-", "-", "+", "-", "-")

hellwig_wyniki <- hellwig(d, w, i)
hellwig_wyniki

topsis_wyniki <- topsis(d, w, i)
topsis_wyniki

copras_wyniki <- copras(d, w, i)
