# ==============================================================================
# PAKIETY I PRZYGOTOWANIE DANYCH
# ==============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggsci)

dane_surowe <- read_xlsx(path = 'poland_ab.xlsx', sheet = 'raw_data')

nazwy_wojewodztw <- dane_surowe$voiodeship
X_mat <- as.matrix(dane_surowe[, -1])
rownames(X_mat) <- nazwy_wojewodztw

# Selekcja zmiennych końcowych (11 zmiennych wybranych taksonomicznie)
zmienne_wybrane <- c("x01", "x03", "x09", "x16", "x23", "x26", "x36", "x37", "x45", "x49", "x51")
X_final <- X_mat[, zmienne_wybrane]

# Charakterystyka zmiennych: + stymulanta, - destymulanta
kierunki <- c("+", "-", "+", "+", "+", "-", "+", "+", "+", "+", "+")

# Standaryzacja danych
X_std <- scale(X_final)
m <- nrow(X_std)
n_vars <- ncol(X_std)

# ==============================================================================
# 1. PORZĄDKOWANIE LINIOWE (METODA HELLWIGA)
# ==============================================================================

# Budowa wzorca rozwoju
wzorzec <- numeric(n_vars)
names(wzorzec) <- colnames(X_std)

for (j in 1:n_vars) {
  if (kierunki[j] == "+") {
    wzorzec[j] <- max(X_std[, j])
  } else {
    wzorzec[j] <- min(X_std[, j])
  }
}

# Obliczanie odległości euklidesowych od wzorca
odleglosci_wzorzec <- apply(X_std, 1, function(row) {
  sqrt(sum((row - wzorzec)^2))
})

# Wyznaczenie wartości krytycznej d_0 i syntetycznego miernika H
d_0 <- mean(odleglosci_wzorzec) + 2 * sd(odleglosci_wzorzec)
H_miernik <- 1 - (odleglosci_wzorzec / d_0)

# Budowa rankingu i klasyfikacja
sr_H <- mean(H_miernik)
sd_H <- sd(H_miernik)

ranking_hellwig <- data.frame(
  Wojewodztwo = nazwy_wojewodztw,
  H_value = round(H_miernik, 4)
) %>%
  mutate(
    Klasa_Rozwoju = case_when(
      H_value >= (sr_H + sd_H)          ~ "Grupa I (Bardzo wysoki)",
      H_value >= sr_H & H_value < (sr_H + sd_H) ~ "Grupa II (Wysoki)",
      H_value >= (sr_H - sd_H) & H_value < sr_H ~ "Grupa III (Przeciętny)",
      TRUE                              ~ "Grupa IV (Niski)"
    )
  ) %>%
  arrange(desc(H_value))

print("--- RANKING LINIOWY HELLWIGA ---")
print(ranking_hellwig)

# Wykres rankingu liniowego
ggplot(ranking_hellwig, aes(x = reorder(Wojewodztwo, H_value), y = H_value, fill = Klasa_Rozwoju)) +
  geom_col(color = "black", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Grupa I (Bardzo wysoki)" = "#1B5E20",
    "Grupa II (Wysoki)"      = "#4CAF50",
    "Grupa III (Przeciętny)" = "#FFC107",
    "Grupa IV (Niski)"       = "#D32F2F"
  )) +
  theme_minimal(base_size = 14) +
  labs(
    title = "RANKING LINIOWY WOJEWÓDZTW (METODA HELLWIGA)",
    subtitle = "Syntetyczny miernik rozwoju (H)",
    x = "WOJEWÓDZTWO", y = "Wartość miernika taksonomicznego H", fill = "Poziom rozwoju"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#1D3557"),
    axis.text = element_text(size = 11, face = "bold", color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# ==============================================================================
# 2. PORZĄDKOWANIE NIELINIOWE (GRAF POWIĄZAŃ / MDS)
# ==============================================================================

# Obliczanie macierzy odległości taksonomicznych między województwami
macierz_odleglosci <- dist(X_std, method = "euclidean")

# Skalowanie wielwymiarowe (MDS) do mapowania nieliniowej sieci podobieństw na 2D
mds_obliczenia <- cmdscale(macierz_odleglosci, k = 2)
mds_dane <- data.frame(
  Wojewodztwo = rownames(mds_obliczenia),
  X = mds_obliczenia[, 1],
  Y = mds_obliczenia[, 2]
)

# Znajdowanie najbliższego sąsiada dla każdego obiektu (krok nieliniowy)
macierz_kwadratowa <- as.matrix(macierz_odleglosci)
diag(macierz_kwadratowa) <- Inf
najblizszy_sasiad_idx <- apply(macierz_kwadratowa, 1, which.min)

polaczenia_grafu <- data.frame(
  Z = mds_dane$Wojewodztwo,
  Z_X = mds_dane$X,
  Z_Y = mds_dane$Y,
  Do = mds_dane$Wojewodztwo[najblizszy_sasiad_idx],
  Do_X = mds_dane$X[najblizszy_sasiad_idx],
  Do_Y = mds_dane$Y[najblizszy_sasiad_idx]
)

# Wykres porządkowania nieliniowego (Struktura podobieństwa terytorialnego)
ggplot(mds_dane, aes(x = X, y = Y)) +
  geom_segment(data = polaczenia_grafu, aes(x = Z_X, y = Z_Y, xend = Do_X, yend = Do_Y),
               color = "#457B9D", size = 1.2, alpha = 0.8, linetype = "solid") +
  geom_point(color = "#1D3557", fill = "#E63946", size = 6, shape = 21, stroke = 2) +
  geom_text_repel(aes(label = Wojewodztwo), size = 4.5, fontface = "bold", color = "black", box.padding = 0.4) +
  theme_minimal(base_size = 14) +
  labs(
    title = "PORZĄDKOWANIE NIELINIOWE (GRAF NAJBLIŻSZEGO SĄSIADA)",
    subtitle = "Połączenia pokazują najsilniejsze powiązania taksonomiczne (MDS)",
    x = "Wymiar taksonomiczny 1", y = "Wymiar taksonomiczny 2"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#1D3557"),
    axis.text = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank()
  )

# ==============================================================================
# 3. GRUPOWANIE PRZY POMOCY DENDROGRAMÓW (METODA WARDA)
# ==============================================================================

# Hierarchiczne grupowanie metodą Warda
grupowanie_ward <- hclust(macierz_odleglosci, method = "ward.D2")

# Wykres profesjonalnego dendrogramu
par(mar = c(5, 4, 4, 10)) # Poszerzenie marginesu prawego na etykiety
dendrogram_obj <- as.dendrogram(grupowanie_ward)

# Zmiana orientacji dendrogramu na poziomą (czytelność dla województw)
plot(dendrogram_obj, horiz = TRUE, 
     main = "STRUKTURA PODZIAŁU HIERARCHICZNEGO (METODA WARDA)",
     sub = "Metryka: euklidesowa | Standaryzacja: Tak",
     xlab = "Odległość wiązania (Przełom wariancji)",
     cex.main = 1.4, font.main = 2, col.main = "#1D3557",
     cex.lab = 1.1, font.lab = 2)

# Nałożenie sugerowanego podziału na 3 główne gałęzie rozwoju
rect.hclust(grupowanie_ward, k = 3, border = c("#E63946", "#457B9D", "#1B5E20"))
par(mfrow = c(1, 1)) # Reset okna graficznego