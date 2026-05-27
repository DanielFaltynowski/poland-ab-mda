# ==============================================================================
# 1. PAKIETY I IMPORT DANYCH
# ==============================================================================
# install.packages("linearOrdering")
# install.packages("devtools")
# install.packages("topsis")
# devtools::install_github("Yard1/linearOrdering")

library(readxl)
library(dplyr)
library(tidyr)
library(linearOrdering)
library(topsis)

dane_raw <- read_xlsx(
  path = './../../data/dane.xlsx', 
  sheet = 'dane'
)

# Zachowanie nazw jednostek (województw)
wojewodztwa <- dane_raw[[1]]

# Usunięcie kolumny z nazwami dla operacji numerycznych
dane_num <- dane_raw[, -1]

# ==============================================================================
# 2. SELEKCJA ZMIENNYCH (Współczynnik zmienności CV > 15%)
# ==============================================================================
lista_zmiennych <- dane_num %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100)) %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "cv") %>%
  filter(cv > 15) %>%
  pull(zmienna)

# Przygotowanie macierzy danych do metod wielokryterialnych
d <- as.matrix(dane_num[, lista_zmiennych])

# ==============================================================================
# 3. KONFIGURACJA (Wagi i Kierunki)
# ==============================================================================
# Wagi (równe dla wszystkich zmiennych)
w <- rep(1, length(lista_zmiennych))

# Kierunki: + (stymulanta), - (destymulanta)
# Upewnij się, że liczba elementów w 'i' zgadza się z length(lista_zmiennych)
i <- c(
  "+", "+", "-", "+", "+", # x01, x02, x03, x04, x09
  "+", "+", "+", "+", "+", # x11, x12, x13, x14, x16
  "+", "+", "+", "+", "+", # x17, x18, x19, x20, x21
  "+", "+", "+", "+", "+", # x22, x23, x24, x26, x27
  "+", "+", "+", "+", "+", # x31, x33, x34, x35, x36
  "+", "+", "+", "+", "-", # x37, x38, x39, x42, x43
  "-", "+", "+", "+", "+", # x44, x45, x46, x48, x49
  "+", "+"                 # x50, x51
)

if(length(i) != length(lista_zmiennych)) {
  stop("Liczba zdefiniowanych kierunków (i) nie pasuje do liczby wybranych zmiennych!")
}

hellwig_wyniki <- data.frame(
  Wojewodztwo = wojewodztwa,
  Miernik = as.numeric(hellwig_res$h) 
) %>%
  mutate(Ranking = rank(-Miernik)) %>%
  arrange(Ranking)

topsis_wyniki <- data.frame(
  Wojewodztwo = wojewodztwa,
  Miernik = as.numeric(topsis_res$score)
) %>%
  mutate(Ranking = rank(-Miernik)) %>%
  arrange(Ranking)

print(hellwig_wyniki)
print(topsis_wyniki)

# ==============================================================================
# 6. WYŚWIETLENIE WYNIKÓW
# ==============================================================================
cat("\n--- TABELA WYNIKÓW: HELLWIG ---\n")
print(hellwig_wyniki)

cat("\n--- TABELA WYNIKÓW: TOPSIS ---\n")
print(topsis_wyniki)