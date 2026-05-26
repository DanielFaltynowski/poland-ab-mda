# ==============================================================================
# 1. PAKIETY I IMPORT DANYCH
# ==============================================================================
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


dane <- read_xlsx(
  path = 'poland_ab.xlsx',
  sheet = 'raw_data'
)

# ==============================================================================
# 2. SELEKCJA ZMIENNYCH (Współczynnik zmienności > 15%)
# ==============================================================================
lista_zmiennych <- dane %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100)) %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "cv") %>%
  filter(cv > 15) %>%
  pull(zmienna)

X_mat <- dane %>% dplyr::select(all_of(lista_zmiennych)) %>% as.matrix()
rownames(X_mat) <- dane[[1]] 

m <- nrow(X_mat)
n_vars <- ncol(X_mat)

# ==============================================================================
# 3. DEFINICJA WAG I KIERUNKÓW ZMIENNYCH
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


wagi <- rep(1, n_vars) / n_vars 

stymulanty_idx <- which(i == "+")
destymulanty_idx <- which(i == "-")

# ==============================================================================
# 4. METODA COPRAS
# ==============================================================================
# Normalizacja sumacyjna
X_norm_copras <- prop.table(X_mat, margin = 2)
X_weighted_copras <- sweep(X_norm_copras, 2, wagi, "*")

# Sumy dla stymulant (S+) i destymulant (S-)
S_plus <- rowSums(X_weighted_copras[, stymulanty_idx, drop = FALSE])
S_minus <- rowSums(X_weighted_copras[, destymulanty_idx, drop = FALSE])

# Stopień użyteczności Qi
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

# Odległości euklidesowe z uwzględnieniem wag
odleglosci_hellwig <- apply(X_std, 1, function(row) {
  sqrt(sum(wagi * (row - wzorzec)^2))
})

# Miernik rozwoju Hellwiga (H)
d_0 <- mean(odleglosci_hellwig) + 2 * sd(odleglosci_hellwig)
H_hellwig <- 1 - (odleglosci_hellwig / d_0)

# ==============================================================================
# 6. ŁĄCZENIE WYNIKÓW I KLASYFIKACJA STATYSTYCZNA
# ==============================================================================
wyniki_koncowe <- data.frame(
  Wojewodztwo = dane[[1]],
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