pakiety <- c(
  "tidyverse",
  "readxl",
  "e1071",
  "ineq",
  "kableExtra", 
  "gridExtra", 
  "corrplot", 
  "reshape2", 
  "ggrepel",
  "ggpubr", 
  "reactable", 
  "tidytext",
  "heplots",
  "dplyr",
  "CCP",
  "CCA",
  "candisc",
  "arm",
  "semPlot",
  "lavaan",
  "yacca",
)

package.check <- lapply(pakiety, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

data(schooldata)
str(schooldata)
View(schooldata)
dim(schooldata)

dane <- schooldata

# ==============================================================================
# MACIERZ KORELACJI
# ==============================================================================

cor_matrix <- dane %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

cor_matrix_upper <- cor_matrix
cor_matrix_upper[lower.tri(cor_matrix_upper)] <- NA
diag(cor_matrix_upper) <- NA

cor_table <- cor_matrix_upper %>%
  as.data.frame() %>%
  rownames_to_column("Zmienna") %>%
  mutate(across(-Zmienna, ~ map_chr(.x, function(val) {
    if (is.na(val)) return("")
    cell_spec(round(val, 2), "html", 
              bold = TRUE,
              #color = ifelse(abs(val) > 0.7, "white", "black"), 
              color = ifelse(abs(val) > 0.7, "white", ifelse(abs(val) > 0.3, "black", "white")),
              background = case_when(
                abs(val) < 0.3 ~ "#008000",
                abs(val) < 0.7 ~ "#FFFFC5",
                abs(val) <= 1.0 ~ "#FF0000"
              ))
  })))

cor_table %>%
  kbl(escape = FALSE, 
      caption = "<b style='color:black'>Macierz korelacji Pearsona (Górny trójkąt)</b><br>
                 <span style='font-size:12px; font-weight:normal; color:black;'>
                 Interpretacja siły związku (|corr|): 
                 <span style='background:#008000; color:white; padding:2px;'>0.0-0.3 brak/słaba</span>; 
                 <span style='background:#FFFFC5; color:black; padding:2px;'>0.3-0.7 umiarkowana/silna</span>; 
                 <span style='background:#FF0000; color:white; padding:2px;'>0.7-1.0 bardzo silna</span>
                 </span>",
      align = "c") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "blue") %>%
  column_spec(1, bold = TRUE, background = "#f0f0f0") %>%
  scroll_box(width = "100%")


X <- dane %>%
  dplyr::select(1:4)

Y <- dane %>%
  dplyr::select(5:8)

View(X)
View(Y)

cor_matrix <- X %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

cor_matrix_upper <- cor_matrix
cor_matrix_upper[lower.tri(cor_matrix_upper)] <- NA
diag(cor_matrix_upper) <- NA

cor_table <- cor_matrix_upper %>%
  as.data.frame() %>%
  rownames_to_column("Zmienna") %>%
  mutate(across(-Zmienna, ~ map_chr(.x, function(val) {
    if (is.na(val)) return("")
    cell_spec(round(val, 2), "html", 
              bold = TRUE,
              #color = ifelse(abs(val) > 0.7, "white", "black"), 
              color = ifelse(abs(val) > 0.7, "white", ifelse(abs(val) > 0.3, "black", "white")),
              background = case_when(
                abs(val) < 0.3 ~ "#008000",
                abs(val) < 0.7 ~ "#FFFFC5",
                abs(val) <= 1.0 ~ "#FF0000"
              ))
  })))

cor_table %>%
  kbl(escape = FALSE, 
      caption = "<b style='color:black'>Macierz korelacji Pearsona (Górny trójkąt)</b><br>
                 <span style='font-size:12px; font-weight:normal; color:black;'>
                 Interpretacja siły związku (|corr|): 
                 <span style='background:#008000; color:white; padding:2px;'>0.0-0.3 brak/słaba</span>; 
                 <span style='background:#FFFFC5; color:black; padding:2px;'>0.3-0.7 umiarkowana/silna</span>; 
                 <span style='background:#FF0000; color:white; padding:2px;'>0.7-1.0 bardzo silna</span>
                 </span>",
      align = "c") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "blue") %>%
  column_spec(1, bold = TRUE, background = "#f0f0f0") %>%
  scroll_box(width = "100%")

cor_matrix <- Y %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

cor_matrix_upper <- cor_matrix
cor_matrix_upper[lower.tri(cor_matrix_upper)] <- NA
diag(cor_matrix_upper) <- NA

cor_table <- cor_matrix_upper %>%
  as.data.frame() %>%
  rownames_to_column("Zmienna") %>%
  mutate(across(-Zmienna, ~ map_chr(.x, function(val) {
    if (is.na(val)) return("")
    cell_spec(round(val, 2), "html", 
              bold = TRUE,
              #color = ifelse(abs(val) > 0.7, "white", "black"), 
              color = ifelse(abs(val) > 0.7, "white", ifelse(abs(val) > 0.3, "black", "white")),
              background = case_when(
                abs(val) < 0.3 ~ "#008000",
                abs(val) < 0.7 ~ "#FFFFC5",
                abs(val) <= 1.0 ~ "#FF0000"
              ))
  })))

cor_table %>%
  kbl(escape = FALSE, 
      caption = "<b style='color:black'>Macierz korelacji Pearsona (Górny trójkąt)</b><br>
                 <span style='font-size:12px; font-weight:normal; color:black;'>
                 Interpretacja siły związku (|corr|): 
                 <span style='background:#008000; color:white; padding:2px;'>0.0-0.3 brak/słaba</span>; 
                 <span style='background:#FFFFC5; color:black; padding:2px;'>0.3-0.7 umiarkowana/silna</span>; 
                 <span style='background:#FF0000; color:white; padding:2px;'>0.7-1.0 bardzo silna</span>
                 </span>",
      align = "c") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "blue") %>%
  column_spec(1, bold = TRUE, background = "#f0f0f0") %>%
  scroll_box(width = "100%")


correlation_matrix <- cor(X, Y)

cor_table <- correlation_matrix %>%
  as.data.frame() %>%
  rownames_to_column("Zmienna") %>%
  mutate(across(-Zmienna, ~ map_chr(.x, function(val) {
    if (is.na(val)) return("")
    cell_spec(round(val, 2), "html", 
              bold = TRUE,
              #color = ifelse(abs(val) > 0.7, "white", "black"), 
              color = ifelse(abs(val) > 0.7, "white", ifelse(abs(val) > 0.3, "black", "white")),
              background = case_when(
                abs(val) < 0.3 ~ "#008000",
                abs(val) < 0.7 ~ "#FFFFC5",
                abs(val) <= 1.0 ~ "#FF0000"
              ))
  })))

cor_table %>%
  kbl(escape = FALSE, 
      caption = "<b style='color:black'>Macierz korelacji Pearsona (Górny trójkąt)</b><br>
                 <span style='font-size:12px; font-weight:normal; color:black;'>
                 Interpretacja siły związku (|corr|): 
                 <span style='background:#008000; color:white; padding:2px;'>0.0-0.3 brak/słaba</span>; 
                 <span style='background:#FFFFC5; color:black; padding:2px;'>0.3-0.7 umiarkowana/silna</span>; 
                 <span style='background:#FF0000; color:white; padding:2px;'>0.7-1.0 bardzo silna</span>
                 </span>",
      align = "c") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "blue") %>%
  column_spec(1, bold = TRUE, background = "#f0f0f0") %>%
  scroll_box(width = "100%")

correl <- matcor(X, Y)

AK <- cc(X, Y)

View (AK)
AK


options(scipen = 999)
Wilks <- cancor(X, Y)
print(summary(Wilks))

n <- dim(X)[1]
p <- length(X)
q <- length(Y)
rho <- AK$cor


p.asym(rho, n, p, q, tstat = "Wilks")
p.asym(rho, n, p, q, tstat = "Hotelling")
p.asym(rho, n, p, q, tstat = "Pillai")
p.asym(rho, n, p, q, tstat = "Roy")

cc <- cancor(X, Y, set.names=c("RODZINA", "WYNIKI"))
redundancy(cc)

# ==============================================================================
# 1. PRZYGOTOWANIE DANYCH
# ==============================================================================
dane <- schooldata
vars_X <- c("education", "occupation", "visit", "counseling")
vars_Y <- c("teacher", "reading", "mathematics", "selfesteem")
X <- dane[, vars_X]
Y <- dane[, vars_Y]

# Obliczenia
cca_result <- cc(X, Y)
rho <- cca_result$cor[1]
lx <- cor(X, cca_result$scores$xscores)[, 1]
ly <- cor(Y, cca_result$scores$yscores)[, 1]
cx <- lx * rho
cy <- ly * rho

# --- FORMATOWANIE: 3 MIEJSCA PO PRZECINKU ---
fmt <- function(x) sprintf("%.3f", x)

# --- ZMIANA: WSZYSTKIE WARTOŚCI DODATNIE (abs) ---
# Używamy funkcji abs(), aby usunąć minusy
wartosci <- abs(c(lx, ly, rho, cy, cx))
etykiety_gotowe <- fmt(wartosci)

# ==============================================================================
# 2. MODEL LAVAAN (Kolejność linii jest kluczowa dla kolorów!)
# ==============================================================================
model_full <- '
  # GRUPA 1: Ładunki X (4 linie)
  CV_Rodzina =~ education + occupation + visit + counseling
  # GRUPA 2: Ładunki Y (4 linie)
  CV_Wyniki  =~ teacher + reading + mathematics + selfesteem
  # GRUPA 3: Korelacja (1 linia)
  CV_Rodzina ~~ CV_Wyniki
  # GRUPA 4: Krzyżowe Y (4 linie)
  CV_Rodzina =~ teacher + reading + mathematics + selfesteem
  # GRUPA 5: Krzyżowe X (4 linie)
  CV_Wyniki  =~ education + occupation + visit + counseling
'
fit <- cfa(model_full, data = dane)

# ==============================================================================
# 3. DEFINICJA KOLORÓW (TĘCZA POŁĄCZEŃ)
# ==============================================================================
# Definiujemy kolory dla każdej grupy linii z osobna
kolory_linii <- c(
  rep("firebrick", 4),    # GRUPA 1: Ładunki X -> Ciemnoczerwony
  rep("navy", 4),         # GRUPA 2: Ładunki Y -> Ciemnoniebieski
  "black",                # GRUPA 3: Korelacja -> Czarny
  rep("#FFB3B3", 4),      # GRUPA 4: Krzyżowe Y -> Jasnoróżowy
  rep("#99CCFF", 4)       # GRUPA 5: Krzyżowe X -> Jasnoniebieski
)

# Grubość linii (Główne grube, krzyżowe cieńsze)
grubosc <- c(rep(2, 8), 3, rep(2, 8))

# ==============================================================================
# 4. UKŁAD I RYSOWANIE
# ==============================================================================
uklad <- matrix(c(
  -1.2,  1.5,  -1.2,  0.5,  -1.2, -0.5,  -1.2, -1.5, # X
  1.2,  1.5,   1.2,  0.5,   1.2, -0.5,   1.2, -1.5, # Y
  -0.4, 0,      0.4, 0                               # Środek
), ncol = 2, byrow = TRUE)

# Krzywizna (tylko dla krzyżowych)
krzywizna <- c(rep(0, 9), rep(2.5, 4), rep(2.8, 4))

semPaths(fit,
         layout = uklad,
         whatLabels = "hide",
         edgeLabels = etykiety_gotowe,
         # --- CZCIONKA NA STRZAŁKACH ---
         edge.label.cex = 1.2,      # Duża, czytelna czcionka liczb
         edge.label.bg = "white",   # Białe tło
         # --- CZYSTOŚĆ ---
         residuals = FALSE,         # Brak szarych strzałek
         exoVar = FALSE,            # Brak wariancji
         # --- CZCIONKI W PROSTOKĄTACH ---
         sizeMan = 12,              # Szerokość prostokątów
         sizeMan2 = 5,              # Wysokość prostokątów
         sizeLat = 11,              # Wielkość kółek
         label.cex = 0.8,           # Mniejsza czcionka w środku prostokąta
         # --- LINIE I KOLORY ---
         edge.color = kolory_linii, # Twoje kolory (Czerwony, Granatowy, Różowy...)
         edge.width = grubosc,      # Zróżnicowana grubość
         lty = 1, fixedStyle = 1, freeStyle = 1,
         curve = krzywizna,
         # --- KOLORY WĘZŁÓW ---
         groups = list(Rodzina = c("CV_Rodzina", vars_X), Wyniki = c("CV_Wyniki", vars_Y)),
         color = c("#FF9999", "#99CCFF"), # Tło węzłów (Czerwony / Niebieski)
         # --- MARGINESY ---
         mar = c(3, 5, 3, 5),
         rescale = TRUE,
         legend = FALSE,
         border.width = 1.5)
