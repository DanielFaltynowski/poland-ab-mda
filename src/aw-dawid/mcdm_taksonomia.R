# ==============================================================================
# 1. PAKIETY I IMPORT DANYCH
# ==============================================================================
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Import danych
dane <- read_xlsx(path = 'poland_ab.xlsx', sheet = 'raw_data')
nazwy_wojewodztw <- dane[[1]] # Zapisujemy nazwy województw (np. pierwsza kolumna)
dane <- dane[-c(1)]           # Usuwamy kolumnę tekstową, zostają same numeryczne

# ==============================================================================
# 2. SELEKCJA ZMIENNYCH
# ==============================================================================
# KROK 1: Selekcja na podstawie zmienności (> 25%)
wsp_zmiennosci <- apply(dane, 2, function(x) (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100)
zmienne_wysoka_zmiennosc <- names(wsp_zmiennosci[wsp_zmiennosci > 25])

dane_filtr1 <- dane[zmienne_wysoka_zmiennosc]
cat("Liczba zmiennych po filtrze zmienności (>25%):", ncol(dane_filtr1), "\n")

# KROK 2: Selekcja na podstawie korelacji (eliminacja r > 0.7)
macierz_kor <- abs(cor(dane_filtr1, use = "complete.obs"))

do_usuniecia <- c()
N <- ncol(macierz_kor)

for (i_kor in 1:(N - 1)) {
  for (j_kor in (i_kor + 1):N) {
    if (macierz_kor[i_kor, j_kor] > 0.7) {
      do_usuniecia <- c(do_usuniecia, colnames(macierz_kor)[j_kor])
    }
  }
}

zmienne_do_usuniecia <- unique(do_usuniecia)
dane_final <- dane_filtr1[, !(colnames(dane_filtr1) %in% zmienne_do_usuniecia)]

cat("Liczba zmiennych po eliminacji korelacji (pozostały te z r <= 0.7):", ncol(dane_final), "\n")
cat("Zmienne, które pozostały w analizie:\n", paste(colnames(dane_final), collapse = ", "), "\n")

# ==============================================================================
# 2B. PRZYGOTOWANIE MACIERZY DO METOD MCDM/TAKSONOMII
# ==============================================================================
X_mat <- as.matrix(dane_final)
rownames(X_mat) <- nazwy_wojewodztw

m <- nrow(X_mat)
n_vars <- ncol(X_mat)

# ==============================================================================
# 3. DEFINICJA KIERUNKÓW ZMIENNYCH (STYMULANTY "+" / DESTYMULANTY "-")
# ==============================================================================
# Upewnij się, że poniższy wektor odpowiada dokładnie zmiennym: 
# x01, x03, x09, x16, x23, x26, x36, x37, x45, x49, x51
i <- c(
  "+", # x01 - gęstość zaludnienia (os / km2) - 2021 SP
  "-", # x03 - emigranci - 2021 sp - odsetek emigrantów w % względem ludności województwa
  "+", # x09 - studenccy uczelni na 1,000 ludności - 2024
  "+", # x16 - liczba rowerów publicznych na 100,000 ludności 2024
  "+", # x23 - LINIE REGULARNE KOMUNIKACJI AUTOBUSOWEJ w km na 100km^2 w 2024
  "-", # x26 - wypadki drogowe na 100,000 mieszkańców 2024 [ZMIANA NA DESTYMULANTĘ]
  "+", # x36 - zajęcia prowadzone przez teatry i instytucje muzyczne... na 10,000 ludności w 2024
  "+", # x37 - uczestnicy imprez (wydarzeń kulturalnych) organizowanych przez teatry...
  "+", # x45 - miejsca noclegowe na 1000 ludności w 2024
  "+", # x49 - liczba miejsc w domach studenckich w stosunku do liczby studentów 2024
  "+"  # x51 - zasoby mieszkaniowe gim komunalne 2024 na 10,000 mieszkańców
)

wagi <- rep(1, n_vars) / n_vars 

stymulanty_idx <- which(i == "+")
destymulanty_idx <- which(i == "-")

# ==============================================================================
# 4. METODA COPRAS
# ==============================================================================
X_norm_copras <- prop.table(X_mat, margin = 2)
X_weighted_copras <- sweep(X_norm_copras, 2, wagi, "*")

S_plus <- rowSums(X_weighted_copras[, stymulanty_idx, drop = FALSE])
S_minus <- rowSums(X_weighted_copras[, destymulanty_idx, drop = FALSE])

S_minus_min <- min(S_minus)
Q <- S_plus + (sum(S_minus) / (S_minus * sum(S_minus_min / S_minus)))
U_copras <- (Q / max(Q)) * 100

# ==============================================================================
# 5. WAŻONA METODA HELLWIGA
# ==============================================================================
X_std <- scale(X_mat)

wzorzec <- numeric(n_vars)
names(wzorzec) <- colnames(X_std)

for (j in 1:n_vars) {
  if (i[j] == "+") {
    wzorzec[j] <- max(X_std[, j])
  } else {
    wzorzec[j] <- min(X_std[, j])
  }
}

odleglosci_hellwig <- apply(X_std, 1, function(row) {
  sqrt(sum(wagi * (row - wzorzec)^2))
})

d_0 <- mean(odleglosci_hellwig) + 2 * sd(odleglosci_hellwig)
H_hellwig <- 1 - (odleglosci_hellwig / d_0)

# ==============================================================================
# 6. ŁĄCZENIE WYNIKÓW I KLASYFIKACJA STATYSTYCZNA
# ==============================================================================
wyniki_koncowe <- data.frame(
  Wojewodztwo = nazwy_wojewodztw,
  COPRAS_U = round(U_copras, 2),
  Hellwig_Wazony = round(H_hellwig, 4)
)

sr_H <- mean(wyniki_koncowe$Hellwig_Wazony)
sd_H <- sd(wyniki_koncowe$Hellwig_Wazony)

wyniki_koncowe <- wyniki_koncowe %>%
  mutate(
    Klasa_Rozwoju = case_when(
      Hellwig_Wazony >= (sr_H + sd_H)                  ~ "Grupa I (Bardzo wysoki)",
      Hellwig_Wazony >= sr_H & Hellwig_Wazony < (sr_H + sd_H) ~ "Grupa II (Wysoki)",
      Hellwig_Wazony >= (sr_H - sd_H) & Hellwig_Wazony < sr_H ~ "Grupa III (Przeciętny)",
      TRUE                                             ~ "Grupa IV (Niski)"
    )
  ) %>%
  arrange(desc(Hellwig_Wazony))

print(wyniki_koncowe)

# ==============================================================================
# 7. WIZUALIZACJA (WYKRES RANKINGOWY)
# ==============================================================================
ggplot(wyniki_koncowe, aes(x = reorder(Wojewodztwo, Hellwig_Wazony), y = Hellwig_Wazony, fill = Klasa_Rozwoju)) +
  geom_col(color = "black", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Grupa I (Bardzo wysoki)" = "#1B5E20",
    "Grupa II (Wysoki)"      = "#4CAF50",
    "Grupa III (Przeciętny)" = "#FFC107",
    "Grupa IV (Niski)"       = "#D32F2F"
  )) +
  theme_minimal() +
  labs(
    title = "Klasyfikacja i ranking województw",
    subtitle = "Metoda porządkowania liniowego: Ważony Hellwig",
    x = "Województwo",
    y = "Wartość syntetycznego miernika rozwoju (H)",
    fill = "Klasa poziomu rozwoju"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "bottom"
  )