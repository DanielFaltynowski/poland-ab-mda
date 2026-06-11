##################### ANALIZA KANONICZNA #######################################

# install.packages("heplots")
# install.packages("dplyr")

library(heplots)
library(dplyr)

data(schooldata)
str(schooldata)
View(schooldata)
dim(schooldata)

################################################################################

library(corrplot)
library(ggplot2)

schooldata
kor <- cor(schooldata)
kor <- round(kor, 2); kor


corrplot(kor, method = "square", 
         col = colorRampPalette(c("red", "white", "blue"))(200),
         type = "full",  tl.cex = 1,      # upper, lower jeden trójkąt lub full
         tl.col = "black", tl.srt = 45)           # kolor etykiet na osiach i
# kąt nachylenia etykiet

corrplot(kor, method = "color", 
         type = "full", tl.cex = 0.8,                   # cała macierz korelacji i 
         # rozmiar czcionki etykiet
         title = "Macierz korelacji między zmiennymi", 
         mar = c(0, 0, 2, 0))                   # marginesy (dolny, lewy, górny, prawy)


################################################################################

library(knitr)
library(kableExtra)

kbl(kor, caption = "Macierz korelacji") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F) %>%
  column_spec(1, bold = T) # Pogrubienie nazw wierszy



kbl(kor, 
    caption = "<span style='color: black; font-weight: bold;'>Macierz korelacji</span>",
    align = "c") %>%           # "c" = center (środek) dla WSZYSTKICH kolumn
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>% 
  # Pierwsza kolumna (nazwy): pogrubiona + linia oddzielająca
  column_spec(2:ncol(kor)+1, width = "2cm")      
# Pozostałe kolumny: Stała szerokość 2cm (żeby były równe)



X <- schooldata %>% dplyr::select(1:4)
Y <- schooldata %>% dplyr::select(5:8)

# KORELACJE W ZBIORACH

korX <- cor(X)
korY <- cor(Y)

par(mfrow = c(1,2))
corrplot(korX)
corrplot(korY)

par(mfrow = c(1,1))

correlation_matrix <- cor(X, Y)
correlation_matrix <- round(correlation_matrix, 3)
correlation_matrix # macierz współ. korelacji między 

par(fig = c(0.1, 0.7, 0.2, 1), new = TRUE)
corrplot(correlation_matrix, method = "color",
         type = "full",
         tl.cex = 0.9,   # Zwiększenie rozmiaru czcionki etykiet zmiennych 
         title = "Macierz korelacji między zmiennymi zbioru X i Y",
         mar = c(0, 0, 2, 0))

install.packages("CCP")
library(CCP)

install.packages("CCA")
library(CCA)

correl <- matcor(X, Y)
img.matcor(correl)
img.matcor(correl, type=2)

AK <- cc(X, Y)
View(AK)

AK[1:2]
AK[1:3]
AK[1:4]
AK[1:5]

AK$xcoef  # wagi przeskalowane dla zmiennych zbioru X
AK$ycoef  # wagi przeskalowane dla zmiennych zbioru Y

AK$cor    # korelacje miedzy zmiennymi kanonicznymi
          # liczba korelacji wynika z liczebności zbioru mniejszego

AK$scores$xscores
AK$scores$yscores


# install.packages("candisc")
library(candisc)
options(scipen=999) # wyłączenie zapisu nanukowego

Wilks <- cancor(X, Y)
print(summary(Wilks))

################################################################################

n <- dim(X)[1]; n
p <- length(X); p
q <- length(Y); q

rho <- AK$cor

p.asym(rho, n, p, q, tstat = "Wilks")
p.asym(rho, n, p, q, tstat = "Hotelling")
p.asym(rho, n, p, q, tstat = "Pillai")
p.asym(rho, n, p, q, tstat = "Roy")

################################################################################

plot(AK$cor, type = 'b')

plt.cc(AK, var.label = T)

plt.var(AK, 1, 2, int = 0.5, var.label = TRUE, Xnames = NULL, Ynames = NULL)

X <- as.matrix(schooldata[,c(1:4)]); X # Rodzina
Y <- as.matrix(schooldata[,c(5:8)]); Y # wyniki

cc <- cancor(X, Y, set.names=c("RODZINA", "WYNIKI"))

redundancy(cc)

################################################################################

library(ggplot2)
library(dplyr)
# install.packages("ggrepel")
library(ggrepel) # Do ładnych, nienachodzących napisów

# 1. PRZYGOTOWANIE DANYCH
# Zbiór X (Społeczne)
df_x <- data.frame(
  Dim1 = AK$scores$corr.X.xscores[,1],
  Dim2 = AK$scores$corr.X.xscores[,2],
  Zmienna = rownames(AK$scores$corr.X.xscores),
  Grupa = "Zbiór X (Społeczne)"
)

# Zbiór Y (Wyniki)
df_y <- data.frame(
  Dim1 = AK$scores$corr.Y.yscores[,1],
  Dim2 = AK$scores$corr.Y.yscores[,2],
  Zmienna = rownames(AK$scores$corr.Y.yscores),
  Grupa = "Zbiór Y (Wyniki)"
)

circle_data <- rbind(df_x, df_y)

# Tło (okrąg)
angle <- seq(0, 2 * pi, length.out = 100)
circle_edge <- data.frame(x = cos(angle), y = sin(angle))


# 2. RYSOWANIE WYKRESU Z DUŻYMI CZCIONKAMI
ggplot() +
  # A. Tło (Okrąg i osie)
  geom_path(data = circle_edge, aes(x = x, y = y), color = "gray60", linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray") +
  # B. Strzałki (Tworzą legendę - linie)
  geom_segment(data = circle_data,
               aes(x = 0, y = 0, xend = Dim1, yend = Dim2, color = Grupa),
               arrow = arrow(length = unit(0.3, "cm")), size = 1.5) + # size=1.5 -> Grubsze strzałki
  # C. Napisy (show.legend = FALSE -> usuwa "a" z legendy)
  geom_text_repel(data = circle_data,
                  aes(x = Dim1, y = Dim2, label = Zmienna, color = Grupa),
                  size = 6,              # Duża czcionka napisów na kole
                  fontface = "bold", 
                  box.padding = 0.5,
                  show.legend = FALSE) + # <--- TO USUWA LITERKI "a" Z LEGENDY
  # D. Kolory
  scale_color_manual(values = c("Zbiór X (Społeczne)" = "steelblue", 
                                "Zbiór Y (Wyniki)" = "firebrick")) +
  # E. Wygląd i CZCIONKI (Powiększone)
  coord_fixed() +
  theme_minimal() +
  labs(title = "Wykres kołowy zmiennych",
       x = "1. Zmienna Kanoniczna",
       y = "2. Zmienna Kanoniczna",
       color = "Zbiór danych") +
  theme(
    legend.position = "bottom",
    # 1. DUŻY TYTUŁ WYKRESU
    plot.title = element_text(face = "bold", size = 20),
    # 2. DUŻE TYTUŁY OSI (X i Y)
    axis.title = element_text(face = "bold", size = 16),
    # 3. DUŻE LICZBY NA OSIACH
    axis.text = element_text(size = 14, color = "black"),
    # 4. DUŻA LEGENDA (Tytuł i tekst)
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 14),
    # Dłuższe linie w legendzie (żeby było widać kolory)
    legend.key.width = unit(2, "cm")
  )



################################################################################

# install.packages("arm", dependencies = TRUE)

# ŁADUNKI ZAMIENIONE NA DODATNIE
# install.packages("semPlot", type = "binary")
library(CCA)
library(semPlot)
library(lavaan)
# install.packages("arm")
library(arm)


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






