# ==============================================================================
# INSTALACJA I IMPORT BIBLIOTEK
# ==============================================================================
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
  "CCA",
  "CCP",
  "linearOrdering",
  "lavaan",
  "semPlot",
  "candisc"
)

package.check <- lapply(pakiety, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


# ==============================================================================
# IMPORT DANYCH
# ==============================================================================

dane <- read_xlsx(
  path = './../../data/dane.xlsx',
  sheet = 'dane'
)



dane
View(dane)

# ==============================================================================
# WYBÓR ZMIENNYCH DO ANALIZY
# ==============================================================================

zmienne_zbioru_X <- c("x11", "x12", "x13", "x14", "x15") 
zmienne_zbioru_Y <- c("x27", "x28", "x29", "x30") 

X <- dane %>% dplyr::select(all_of(zmienne_zbioru_X))
Y <- dane %>% dplyr::select(all_of(zmienne_zbioru_Y))


# ==============================================================================
# Macierze korelacji
# ==============================================================================

kor_X <- cor(X)
kor_Y <- cor(Y)
kor_XY <- cor(Y, X) # Korelacja krzyżowa (niesymetryczna macierz X vs Y)

# 2. Definicja palety kolorów
color_palette <- colorRampPalette(c("#E41A1C", "white", "#377EB8"))(200)

# 3. Definicja niestandardowego układu (layout)
# Tworzymy matrycę układu: rząd 1 (wykresy 1 i 2), rząd 2 (wykres 3 rozciągnięty dwukrotnie)
uklad_okien <- matrix(c(1, 2, 
                        3, 3), nrow = 2, byrow = TRUE)

# Uruchomienie układu (ustawiamy wysokości rzędów na równe)
layout(uklad_okien, heights = c(1, 1))

# --- Wykres 1: Korelacja X (Góra, Lewo) ---
corrplot::corrplot(kor_X, 
                   method = "square", 
                   col = color_palette,
                   type = "full", 
                   addCoef.col = "black", 
                   number.cex = 0.8, 
                   tl.col = "black", 
                   tl.srt = 45,
                   title = "Korelacja wewnątrz zbioru X",
                   mar = c(0, 0, 2, 0))

# --- Wykres 2: Korelacja Y (Góra, Prawo) ---
corrplot::corrplot(kor_Y, 
                   method = "square", 
                   col = color_palette,
                   type = "full", 
                   addCoef.col = "black", 
                   number.cex = 0.8, 
                   tl.col = "black", 
                   tl.srt = 45,
                   title = "Korelacja wewnątrz zbioru Y",
                   mar = c(0, 0, 2, 0))

# --- Wykres 3: Korelacja Krzyżowa X vs Y (Dół, Cała szerokość) ---
corrplot::corrplot(kor_XY, 
                   method = "square", 
                   col = color_palette,
                   # Dla macierzy niesymetrycznych parametr 'type' MUSI być "full" (domyślny)
                   addCoef.col = "black", 
                   number.cex = 0.9, # Nieco większe liczby, bo wykres będzie szeroki
                   tl.col = "black", 
                   tl.srt = 45,
                   title = "Korelacja między zmiennymi X i Y (Krzyżowa)",
                   mar = c(0, 0, 2, 0))

# 4. Reset układu graficznego do domyślnego (1x1) po zakończeniu rysowania
layout(1)

# ==============================================================================
# WYKRESY ROZRZUTU (GGPLOT2 / GGALLY) - CZYSTA SIATKA X vs Y
# ==============================================================================
library(ggplot2)
library(GGally)

custom_smooth_cor <- function(data, mapping, ...) {
  # Wyciągamy wartości X i Y do policzenia korelacji
  x_val <- eval_data_col(data, mapping$x)
  y_val <- eval_data_col(data, mapping$y)
  r_value <- round(cor(x_val, y_val, use = "complete.obs"), 2)
  
  ggplot(data = data, mapping = mapping) +
    geom_point(color = "#377EB8") +
    geom_smooth(method = "lm", color = "#E41A1C", se = FALSE, lwd = 0.8) +
    geom_text(
      aes(label = paste0("r = ", r_value)),
      x = max(x_val, na.rm = TRUE), 
      y = min(y_val, na.rm = TRUE), 
      hjust = 1.1, vjust = -0.5, size = 3.5, color = "black",
      inherit.aes = FALSE # Zapobiega konfliktom mapowania
    )
}

ggduo(
  data = dane,
  columnsX = zmienne_zbioru_X,
  columnsY = zmienne_zbioru_Y,
  types = list(continuous = custom_smooth_cor)
) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "#F0F0F0"),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Matryca wykresów rozrzutu")

# ==============================================================================
# ANALIZA KANONICZNA
# ==============================================================================

analiza_kanoniczna<- cc(X, Y)
analiza_kanoniczna

# ==============================================================================
# TESTY ISTOTNOŚCI
# ==============================================================================

n <- dim(X)[1]
p <- length(X)
q <- length(Y)
rho <- analiza_kanoniczna$cor


p.asym(rho, n, p, q, tstat ="Wilks")
p.asym(rho, n, p, q, tstat ="Hotelling")
p.asym(rho, n, p, q, tstat ="Pillai")
p.asym(rho, n, p, q, tstat ="Roy")

# ==============================================================================
# REDUNDANCJA
# ==============================================================================

cc_red <- candisc::cancor(X, Y, set.names = c('X', 'Y'))
redundancy(cc_red)

# ==============================================================================
# WIZUALIZACJA GRAPHICZNA DLA DANYCH (CCA -> SEMPATHS)
# ==============================================================================
library(semPlot)
library(lavaan)

rho_twój <- analiza_kanoniczna$cor[1] # Pierwsza korelacja kanoniczna

lx_twój <- cor(X, analiza_kanoniczna$scores$xscores)[, 1]
ly_twój <- cor(Y, analiza_kanoniczna$scores$yscores)[, 1]

cx_twój <- lx_twój * rho_twój
cy_twój <- ly_twój * rho_twój

wartosci_twóje <- abs(c(lx_twój, ly_twój, rho_twój, cy_twój, cx_twój))
etykiety_gotowe_twóje <- sprintf("%.3f", wartosci_twóje)

model_twój <- '
  # GRUPA 1: Ładunki X (5 linii)
  CV_Zbior_X =~ x11 + x12 + x13 + x14 + x15
  # GRUPA 2: Ładunki Y (4 linie)
  CV_Zbior_Y =~ x27 + x28 + x29 + x30
  # GRUPA 3: Korelacja kanoniczna (1 linia)
  CV_Zbior_X ~~ CV_Zbior_Y
  # GRUPA 4: Krzyżowe Y (4 linie)
  CV_Zbior_X =~ x27 + x28 + x29 + x30
  # GRUPA 5: Krzyżowe X (5 linii)
  CV_Zbior_Y =~ x11 + x12 + x13 + x14 + x15
'
fit_twój <- cfa(model_twój, data = dane)

kolory_linii_twóje <- c(
  rep("firebrick", 5),    # GRUPA 1: Ładunki X -> 5 linii
  rep("navy", 4),         # GRUPA 2: Ładunki Y -> 4 linie
  "black",                # GRUPA 3: Korelacja -> 1 linia
  rep("#FFB3B3", 4),      # GRUPA 4: Krzyżowe Y -> 4 linie
  rep("#99CCFF", 5)       # GRUPA 5: Krzyżowe X -> 5 linii
)

grubosc_twóje <- c(rep(2, 9), 3, rep(2, 9))

uklad_twój <- matrix(c(
  -1.2,  1.6,   
  -1.2,  0.8,   
  -1.2,  0.0,   
  -1.2, -0.8,   
  -1.2, -1.6,   
  1.2,  1.5,   
  1.2,  0.5,   
  1.2, -0.5,   
  1.2, -1.5,   
  -0.4,  0.0,   # CV_Zbior_X
  0.4,  0.0    # CV_Zbior_Y
), ncol = 2, byrow = TRUE)

krzywizna_twóje <- c(rep(0, 10), rep(2.5, 4), rep(2.8, 5))

semPaths(fit_twój,
         layout = uklad_twój,
         whatLabels = "hide",
         edgeLabels = etykiety_gotowe_twóje,
         edge.label.cex = 1.1,      
         edge.label.bg = "white",   
         residuals = FALSE,         
         exoVar = FALSE,            
         sizeMan = 10,              
         sizeMan2 = 5,              
         sizeLat = 12,              
         label.cex = 0.9,           
         edge.color = kolory_linii_twóje, 
         edge.width = grubosc_twóje,      
         lty = 1, fixedStyle = 1, freeStyle = 1,
         curve = krzywizna_twóje,
         groups = list(Zbior_X = c("CV_Zbior_X", zmienne_zbioru_X), Zbior_Y = c("CV_Zbior_Y", zmienne_zbioru_Y)),
         color = c("#FF9999", "#99CCFF"), 
         mar = c(3, 4, 3, 4),
         rescale = TRUE,
         legend = FALSE,
         border.width = 1.5)


# ==============================================================================
# WYKRES KOŁOWY ZMIENNYCH (MAPA ŁADUNKÓW KANONICZNYCH)
# ==============================================================================
library(ggplot2)
library(dplyr)
library(ggrepel)

# 1. WYCIĄGANIE ŁADUNKÓW KANONICZNYCH Z TWOJEJ ANALIZY
# Liczymy korelację surowych zmiennych z wynikami kanonicznymi (scores) dla 1 i 2 wymiaru

loadings_X <- cor(X, analiza_kanoniczna$scores$xscores)[, 1:2]
loadings_Y <- cor(Y, analiza_kanoniczna$scores$yscores)[, 1:2]

# Przygotowanie ramki danych dla zbioru X (Gospodarka)
df_x_circle <- data.frame(
  Dim1 = loadings_X[, 1],
  Dim2 = loadings_X[, 2],
  Zmienna = rownames(loadings_X),
  Grupa = "Zbiór X (Gospodarka)"
)

# Przygotowanie ramki danych dla zbioru Y (Inwestycje)
df_y_circle <- data.frame(
  Dim1 = loadings_Y[, 1],
  Dim2 = loadings_Y[, 2],
  Zmienna = rownames(loadings_Y),
  Grupa = "Zbiór Y (Inwestycje)"
)

# Połączenie obiektów w jedną strukturę
circle_data_twóje <- rbind(df_x_circle, df_y_circle)

# Generowanie punktów do narysowania idealnego okręgu (promień = 1)
angle_twóje <- seq(0, 2 * pi, length.out = 100)
circle_edge_twóje <- data.frame(x = cos(angle_twóje), y = sin(angle_twóje))


# 2. GENEROWANIE WYKRESU GGPLOT2
ggplot() +
  # A. Tło graficzne: okrąg jednostkowy oraz osie współrzędnych
  geom_path(data = circle_edge_twóje, aes(x = x, y = y), color = "gray60", linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray60") +
  
  # B. Rysowanie wektorów (strzałek) od środka (0,0) do punktu ładunków
  geom_segment(data = circle_data_twóje,
               aes(x = 0, y = 0, xend = Dim1, yend = Dim2, color = Grupa),
               arrow = arrow(length = unit(0.3, "cm")), size = 1.5) +
  
  # C. Dodanie nienachodzących na siebie etykiet (nazw zmiennych x11-x15, x27-x30)
  geom_text_repel(data = circle_data_twóje,
                  aes(x = Dim1, y = Dim2, label = Zmienna, color = Grupa),
                  size = 6,                 # Powiększona czcionka etykiet
                  fontface = "bold", 
                  box.padding = 0.5,
                  show.legend = FALSE) +    # Ukrywa literki "a" w legendzie wykresu
  
  # D. Ręczne dopasowanie kolorów do Twoich grup
  scale_color_manual(values = c("Zbiór X (Gospodarka)" = "steelblue", 
                                "Zbiór Y (Inwestycje)" = "firebrick")) +
  
  # E. Formatowanie wyglądu i czcionek (skala 1:1, powiększone opisy)
  coord_fixed() +
  theme_minimal() +
  labs(title = "Wykres kołowy relacji zmiennych z wymiarami kanonicznymi",
       x = "1. Zmienna Kanoniczna (Wymiar 1)",
       y = "2. Zmienna Kanoniczna (Wymiar 2)",
       color = "Podsystem danych:") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    legend.title = element_text(face = "bold", size = 13),
    legend.text = element_text(size = 12),
    legend.key.width = unit(1.5, "cm")
  )
