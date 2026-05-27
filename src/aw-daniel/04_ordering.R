install.packages("linearOrdering")
install.packages("devtools")
install.packages("topsis")
devtools::install_github("Yard1/linearOrdering")
library(readxl)
library(linearOrdering)
library(topsis)

dane <- read_xlsx(
  path = './../../data/dane.xlsx',
  sheet = 'dane'
)

dane
View(dane)

wojewodztwa <- dane_raw[[1]]

dane <- dane[, -c(1)]

library(dplyr)
library(tidyr)

lista_zmiennych <- dane %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100)) %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "cv") %>%
  filter(cv > 15) %>%
  pull(zmienna)

dput(lista_zmiennych)

dput(lista_zmiennych)

dane_wybrane <- dane %>%
  dplyr::select(1, all_of(lista_zmiennych))

dane_wybrane

d <- as.matrix(dane_wybrane)
w <- rep(1, length(lista_zmiennych))
i <- c(
  "+", "+", "-", "+", "+", # x01, x02, x03 (emigracja-), x04, x09
  "+", "+", "+", "+", "+", # x11, x12, x13, x14, x16
  "+", "+", "+", "+", "+", # x17, x18, x19, x20, x21
  "+", "+", "+", "+", "+", # x22, x23, x24 (auta+), x26, x27
  "+", "+", "+", "+", "+", # x31, x33, x34, x35, x36
  "+", "+", "+", "+", "-", # x37, x38, x39, x42, x43 (bezrobocie-)
  "-", "+", "+", "+", "+", # x44 (pomoc społ.-), x45, x46, x48, x49
  "+", "+"                 # x50, x51
)

hellwig_wyniki <- hellwig(d, w, i)
hellwig_wyniki

topsis_wyniki <- topsis(d, w, i)
topsis_wyniki


# ==============================================================================
# TABELE WYNIKÓW
# ==============================================================================

hellwig_tabela <- data.frame(
  Wojewodztwo = wojewodztwa,
  Miernik = as.numeric(hellwig_wyniki[[1]]),
  Ranking = rank(-as.numeric(hellwig_wyniki[[1]]))
) %>%
  arrange(Ranking)

topsis_tabela <- data.frame(
  Wojewodztwo = wojewodztwa,
  Miernik = as.numeric(topsis_wyniki$score),
  Ranking = rank(-as.numeric(topsis_wyniki$score))
) %>%
  arrange(Ranking)

# ==============================================================================
# WYŚWIETLENIE WYNIKÓW
# ==============================================================================

cat("\n--- RANKING METODĄ HELLWIGA ---\n")
print(hellwig_tabela, row.names = FALSE)

cat("\n--- RANKING METODĄ TOPSIS ---\n")
print(topsis_tabela, row.names = FALSE)
