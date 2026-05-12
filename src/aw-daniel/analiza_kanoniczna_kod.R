# PAKIETY

# install.packages("CCA")
# install.packages("CCP")
# install.packages("ggplot2")
# install.packages("ggrepel")
# install.packages("lavaan")
# install.packages("semPlot")
# install.packages("corrplot")
# install.packages("dplyr")
# install.packages("candisc")

#ctrl shit c - komentarz

library(CCA)
library(CCP)
library(ggplot2)
library(ggrepel)
library(lavaan)
library(semPlot)
library(corrplot)
library(dplyr)
library(candisc)


dane <- read_xlsx(
  path = './../../data/dane.xlsx',
  sheet = 'dane'
)

dane
View(dane)


# I ANALIZA

# ZBIÓR X:

# ZBIÓR Y:


# PRZYGOTOWANIE DANYCH
zmienne_zbioru_X <- c("x06", "x07") 
zmienne_zbioru_Y <- c("x43", "x44") 

X <- dane %>% select(all_of(zmienne_zbioru_X))
Y <- dane %>% select(all_of(zmienne_zbioru_Y))

View(X)
View(Y)


# KORELACJE I ANALIZA KANONICZNA
macierz_korelacji_X <- round(cor(X),3)
macierz_korelacji_Y <- round(cor(Y),3)


# GRAFIKA I
par(mfrow = c(1,2))
corrplot(macierz_korelacji_X)
corrplot(macierz_korelacji_Y)


macierz_korelacji <- round(cor(X,Y), 3)


# GRAFIKA II
corrplot(macierz_korelacji, method = 'color', type = 'full', tl.cex = 1.2,
         title = "Macierz korelacji między zbiorem X i Y", mar = c(0, 0, 2, 0))


# GRAFIKA III
korelacja_wspolna <- matcor(X,Y) # liczy 
img.matcor(korelacja_wspolna, type = 2) # rysuje wszystko osobno, type = 1 - wszystko razem



analiza_kanoniczna<- cc(X, Y)    # korelacje kanoniczne, czyli korelacje między zmiennymi kanonicznymi
print("Korelacje kanoniczne:")
print(analiza_kanoniczna$cor) 


analiza_kanoniczna[1:2] 
analiza_kanoniczna[1:3] 
analiza_kanoniczna[1:4]
analiza_kanoniczna[1:5] # to zawiera wszystko z poprzednich


# 1: cor 
# 2: xcoef – wagi dla zmiennych X 
# 3: ycoef – wagi dla zmiennych Y 
# 4: xcenter – średnie wartości, które R odjął od X, żeby "wyzerować" dane
# 5: ycenter – średnie wartości, które R odjął od Y


print("Testy istotności współczynników")
n <- dim(X)[1] # liczba wierszy w zbiorze X = liczba wierszy w zbiorze Y
p <- length(X) # liczba zmiennych w zbiorze X
q <- length(Y) # liczba zmiennych w zbiorze Y
rho <- analiza_kanoniczna$cor # wartości korelacji kanonicznych


p.asym(rho, n, p, q, tstat ="Wilks")
p.asym(rho, n, p, q, tstat ="Hotelling")
p.asym(rho, n, p, q, tstat ="Pillai")
p.asym(rho, n, p, q, tstat ="Roy")



# REDUNDANCJA

# print("Analiza redundancji")
cc_red <- candisc::cancor(X, Y, set.names = c('X', 'Y'))
redundancy(cc_red)

plot(analiza_kanoniczna$cor, type = 'b', 
     main = "Wykres osunięcia korelacji kanonicznych", 
     xlab = "Numer pierwiastka (Wymiar)", 
     ylab = "Wartość korelacji kanonicznej",
     col = "blue", pch = 16)



# WYKRES KOŁOWY ZMIENNYCH
df_x <- data.frame(
  Dim1 = analiza_kanoniczna$scores$corr.X.xscores[,1],
  Dim2 = analiza_kanoniczna$scores$corr.X.xscores[,2],
  Zmienna = rownames(analiza_kanoniczna$scores$corr.X.xscores),
  Grupa = "Zbiór X"
)

df_y <- data.frame(
  Dim1 = analiza_kanoniczna$scores$corr.Y.yscores[,1],
  Dim2 = analiza_kanoniczna$scores$corr.Y.yscores[,2],
  Zmienna = rownames(analiza_kanoniczna$scores$corr.Y.yscores),
  Grupa = "Zbiór Y"
)

circle_data <- rbind(df_x, df_y)
angle <- seq(0, 2 * pi, length.out = 100)
circle_edge <- data.frame(x = cos(angle), y = sin(angle))

ggplot() +
  geom_path(data = circle_edge, aes(x = x, y = y), color = "gray60", linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray") +
  geom_segment(data = circle_data,
               aes(x = 0, y = 0, xend = Dim1, yend = Dim2, color = Grupa),
               arrow = arrow(length = unit(0.3, "cm")), linewidth = 1.5) +
  geom_text_repel(data = circle_data,
                  aes(x = Dim1, y = Dim2, label = Zmienna, color = Grupa),
                  size = 6, fontface = "bold", box.padding = 0.5, show.legend = FALSE) +
  scale_color_manual(values = c("Zbiór X" = "steelblue", 
                                "Zbiór Y" = "firebrick")) +
  coord_fixed() + theme_minimal() +
  labs(title = "Wykres kołowy zmiennych",
       x = "1. Zmienna Kanoniczna", y = "2. Zmienna Kanoniczna", color = "Zbiór danych") +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12),
        legend.key.width = unit(2, "cm"))


# MODEL ŚCIEŻKOWY (lavaan & semPlot)
# Obliczanie ładunków z naszego obiektu 'analiza_kanoniczna'
lx <- cor(X, analiza_kanoniczna$scores$xscores)[, 1]
ly <- cor(Y, analiza_kanoniczna$scores$yscores)[, 1]
cx <- lx * rho[1]
cy <- ly * rho[1]

fmt <- function(x) sprintf("%.3f", x)
wartosci <- abs(c(lx, ly, rho[1], cy, cx))
etykiety_gotowe <- fmt(wartosci)

model_full <- '
  # GRUPA 1: Ładunki X (3 linie)
  CV_Urbanizacja =~ X04 + X07 + X08

  # GRUPA 2: Ładunki Y (4 linie)
  CV_Skutki  =~ X28 + X30 + X36 + X37

  # GRUPA 3: Korelacja (1 linia)
  CV_Urbanizacja ~~ CV_Skutki

  # GRUPA 4: Krzyżowe Y (4 linie)
  CV_Urbanizacja =~ X28 + X30 + X36 + X37

  # GRUPA 5: Krzyżowe X (3 linie)
  CV_Skutki  =~ X04 + X07 + X08
'
model_full <- '
  # GRUPA 1: Ładunki X (3 linie)
  CV_Urbanizacja =~ x06 + x07
'

fit <- cfa(model_full, data = dane)

kolory_linii <- c(
  rep("firebrick", 3),    # GRUPA 1 (3)
  rep("navy", 4),         # GRUPA 2 (4)
  "black",                # GRUPA 3 (1)
  rep("#FFB3B3", 4),      # GRUPA 4 (4)
  rep("#99CCFF", 3)       # GRUPA 5 (3)
)
grubosc <- c(rep(2, 7), 3, rep(2, 7))
krzywizna <- c(rep(0, 8), rep(2.5, 4), rep(2.8, 3))

uklad <- matrix(c(
  -1.2,  1.0,  -1.2,  0.0,  -1.2, -1.0,               # 3 zmienne X
  1.2,  1.5,   1.2,  0.5,   1.2, -0.5,   1.2, -1.5,  # 4 zmienne Y
  -0.4,  0.0,   0.4,  0.0                             # 2 Zmienne ukryte (Środek)
), ncol = 2, byrow = TRUE)

semPaths(fit, layout = uklad, whatLabels = "hide", edgeLabels = etykiety_gotowe,
         edge.label.cex = 1.2, edge.label.bg = "white",
         residuals = FALSE, exoVar = FALSE,
         sizeMan = 12, sizeMan2 = 5, sizeLat = 11, label.cex = 0.8,
         edge.color = kolory_linii, edge.width = grubosc,
         lty = 1, fixedStyle = 1, freeStyle = 1, curve = krzywizna,
         groups = list(Urbanizacja = c("CV_Urbanizacja", zmienne_zbioru_X), 
                       Skutki = c("CV_Skutki", zmienne_zbioru_Y)),
         color = c("#FF9999", "#99CCFF"),
         mar = c(3, 5, 3, 5), rescale = TRUE, legend = FALSE, border.width = 1.5)



