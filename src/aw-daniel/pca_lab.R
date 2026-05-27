
pakiety <- c(
  "readxl",
  "corrplot",
  "MASS",
  "ppcor",
  "RColorBrewer",
  "ggplot2",
  "factoextra",
  "reshape2",
  "paran",
  "psych",
  "ggfortify",
  "ggrepel"
)

package.check <- lapply(pakiety, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

dane1 <- read_excel("dane1.xlsx")
dane1
str(dane1)

samochody = dane1[, 1]; samochody
stan = dane1[,12]; stan

dane2 <- dane1[,c(1:11)] # cechy ilościowe i opis wierszy
dane2
str(dane2)

dane <- dane1[,c(2:11)] # tylko cechy ilościowe
dane
View(dane)

dane <- as.data.frame(dane1[,2:11])
rownames(dane) <- dane1$samochody
dane

# Korelacja
kor <- cor(dane); kor
pkor <- pcor(dane)$estimate; pkor
colnames(pkor) <- colnames(dane)
rownames(pkor) <- colnames(dane)

corrplot(kor, method="square")
corrplot(pkor, method = "square")
par(mfrow = c(1,2))
corrplot(kor, method = "square", tl.col = "black")
corrplot(pkor, method = "square", tl.col = "black")

par(mfrow = c(1,1))


dane_pca <- prcomp(dane, scale=TRUE)
dane_pca                             # ładunki składowe

# w PCA nie pokazywać odchylenia standardowego, ewentualnie można wariancję


# wartości własne - to w pracy MUSI być. nie używać Dim., zamiast tego pc1, pc2
eig.val <- get_eigenvalue(dane_pca)
eig.val

# ciekawa ciekawostka -> suma wszystkich eigenvalue jest równa liczbie cech

View(dane_pca)
dane_pca$x           # współrzędne przypadków (podobno przydatne do rysowania wykresów)
dane_pca$x[,1]
dane_pca$x[,2]

cor(dane, dane_pca$x) # korelacje (czasem więcej wnoszą niż ładunki, być może warto dodać)



# Liczba kolumn ma odpowiadać wybranej liczbie składowych
# Przygotowanie macierzy korelacji (wybieramy np. pierwsze 5 składowych)
kor_pca <- cor(dane, dane_pca$x)[, 1:10]

# Zamiana na format "długi" dla ggplot
kor_melted <- melt(kor_pca)

ggplot(kor_melted, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  geom_text(aes(label = round(value, 2)), size = 3) + 
  # Wartości wewnątrz kwadratów
  theme_minimal() +
  labs(title = "Heatmapa korelacji: Cechy i główne składowe",
       x = "Składowe główne", y = "Cechy samochodu", fill = "Korelacja")


# korelacje zmiennych ze składową

pc.dane <- princomp(dane, cor = TRUE)
cor(dane, pc.dane$scores)
cor(dane, pc.dane$scores)[,1:3]
View(pc.dane)


pc.dane$loadings       # ładunki składowe
pc.dane$sdev           # odchylenie standardowe składowych głównych
pc.dane$center         # średnie zmiennych
pc.dane$scale          # odchylenie standardowe zmiennych
pc.dane$scores         # współrzędne przypadków
View(pc.dane$scores)


# wykres osypiska - liniowy + odcięcie na poziomie wartości własnej równej 1
screeplot(dane_pca, type = "lines", main = "Wykres osypiska", ylim = c(0,6))
abline(1,0, col="red", lty=2)

# wykres osypiska - słupkowy + odcięcie na poziomie wartości własnej równej 1
screeplot(dane_pca, type = "barplot", col = "lightblue", main="", ylim = c(0, 6))
abline(1,0, col = "red", lty = 2)



# Wskazanie składowych które mają "potencjał"

eigenvalues <- dane_pca$sdev^2
# Tworzymy wektor kolorów: "red" dla wartości > 1, "lightblue" dla pozostałych
colors <- ifelse(eigenvalues > 1, "red", "lightblue")
bar_positions <- barplot(eigenvalues, col = colors, ylim = c(0, 6), 
                         main = "", xlab = "Składowe główne", ylab = "Wartości własne")
abline(h = 1, col = 'darkblue', lty = 2)
# Dodajemy etykiety osi X dokładnie pod słupkami
axis(1, at = bar_positions, labels = paste0("PC", 1:length(eigenvalues)), 
     tick = FALSE, line = -0.5)


# Tworzymy wektor kolorów: "red" dla wartości > 1, "lightblue" dla pozostałych
colors <- ifelse(eigenvalues > 1, "red", "lightblue")
bar_positions <- barplot(eigenvalues, col = colors, ylim = c(0, 6), 
                         main = "", xlab = "Składowe główne", ylab = "Wartości własne")
abline(h = 1, col = 'darkblue', lty = 2)
axis(1, at = bar_positions, labels = paste0("PC", 1:length(eigenvalues)), 
     tick = TRUE, line = 0)




# Tworzenie wykresu osypiska
plot(eigenvalues, 
     type = "b",                  # Typ wykresu: linia z punktami
     main = "Wykres osypiska",   # Tytuł wykresu
     xlab = "Numer składowej PCA", # Etykieta osi X
     ylab = "Wartości własne",    # Etykieta osi Y
     ylim = c(0, max(eigenvalues) + 1),  # Ustawienie zakresu osi Y
     cex.main = 1.5,              # Zwiększenie rozmiaru czcionki tytułu
     font.main = 2,               # Pogrubienie czcionki tytułu
     cex.lab = 1.2,               # Zwiększenie rozmiaru czcionki etykiet osi
     pch = 19,                    # Typ punktu na wykresie
     col = "blue",                # Kolor punktów
     xaxt = "n")                  # Wyłączenie domyślnej osi X
# Dodanie linii poziomej na wysokości 1
abline(h = 1, col = 'red', lty = 2)  
# Ustawienie własnych etykiet na osi X z co drugą etykietą
axis(1, at = seq(1, length(eigenvalues), by = 1), 
     labels = seq(1, length(eigenvalues), by = 1), 
     cex.axis = 1.2)


######################################################################33


# --- Analiza równoległa Horna ---
# centile = 95 oznacza 95. percentyl (standardowe kryterium)
# iterations – liczba symulacji
# graph = TRUE rysuje wykres
horn <- paran(dane, 
              iterations = 5000, 
              centile = 95, 
              quietly = FALSE, 
              graph = TRUE,
              color = TRUE,
              col = c("black", "red", "blue"))




# Analiza równoległa
horn2 <- fa.parallel(dane, 
                     fa = "pc",         # tylko PCA
                     n.iter = 1000,
                     show.legend = TRUE,
                     main = "Analiza równoległa Horna")



# Wynik – ile składowych zatrzymać
cat("Liczba składowych wg Horna:", horn2$ncomp, "\n")



eig_real <- dane_pca$sdev^2
n <- nrow(dane)
p <- ncol(dane)
set.seed(123)
n_sim <- 1000
eig_sim <- matrix(NA, nrow = n_sim, ncol = p)
for (i in 1:n_sim) {
  dane_los <- as.data.frame(apply(dane, 2, function(x) rnorm(n)))
  pca_los <- prcomp(dane_los, scale. = TRUE)
  eig_sim[i, ] <- pca_los$sdev^2
}



eig_horn_95 <- apply(eig_sim, 2, quantile, probs = 0.95)
eig_horn_mean <- colMeans(eig_sim)
df_horn <- data.frame(
  Składowa = 1:p,
  Rzeczywiste = eig_real,
  Horn_95 = eig_horn_95,
  Horn_mean = eig_horn_mean
)
ggplot(df_horn, aes(x = Składowa)) +
  geom_line(aes(y = Rzeczywiste, color = "Wartości własne (dane)"), linewidth = 1.2) +
  geom_point(aes(y = Rzeczywiste, color = "Wartości własne (dane)"), size = 3) +
  geom_line(aes(y = Horn_95, color = "Horn 95. percentyl"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = Horn_95, color = "Horn 95. percentyl"), size = 2) +
  geom_line(aes(y = Horn_mean, color = "Horn średnia"), linewidth = 1, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotdash", color = "gray40") +
  scale_x_continuous(breaks = 1:p) +
  scale_color_manual(values = c("Wartości własne (dane)" = "black",
                                "Horn 95. percentyl" = "red",
                                "Horn średnia" = "blue")) +
  labs(title = "Kryterium Horna – analiza równoległa",
       x = "Numer składowej głównej",
       y = "Wartość własna",
       color = "Legenda") +
  theme_minimal(base_size = 13) +
  annotate("text", x = p - 1, y = 1.1, label = "Kryterium Kaisera (λ = 1)", 
           color = "gray40", size = 3.5)


# Podobno warto go dołączyć do pracy
fviz_eig(dane_pca, addlabels = TRUE, ylim = c(0, 60), main = "Scree Plot")



# 1. Poprawka pierwszego wykresu (ggplot z ręcznym mapowaniem)
# Tworzymy ramkę danych z wyników PCA
df_pca_plot <- as.data.frame(dane_pca$x)
df_pca_plot$samochody <- dane1[,1] # dodajemy nazwy aut

# label = TRUE w autoplot automatycznie dodaje nazwy wierszy z danych
autoplot(dane_pca, data = dane1, 
         loadings = TRUE, 
         loadings.colour = "blue",
         loadings.label = TRUE, 
         loadings.label.size = 4,
         label = TRUE,          # To włącza etykiety punktów
         label.colour = "black",
         label.vjust = -0.5) + 
  ggtitle("Biplot") +
  theme_bw()

# autoplot (z kolorem "stan")
autoplot(dane_pca, data = dane1, 
         colour = "stan", 
         loadings = TRUE, 
         loadings.colour = "blue",
         loadings.label = TRUE, 
         loadings.label.size = 4,
         label = TRUE, 
         label.repel = TRUE) + 
  ggtitle("Biplot z podziałem na stan") +
  theme_light() +
  # KLUCZOWY FRAGMENT:
  guides(colour = guide_legend(
    override.aes = list(
      shape = 15,          # Zmienia kropkę/literę na kwadrat
      size = 5,            # Powiększa kwadrat w legendzie
      label = ""           # Usuwa literę "a"
    )
  ))

#################################################################3

fviz_pca_biplot(dane_pca, 
                repel = TRUE,
                col.var = "deepskyblue",
                title = "Biplot", geom = "point")

fviz_pca_biplot(dane_pca, 
                geom.ind = c("point", "text"), # pokazuje punkty i napisy dla aut
                geom.var = c("arrow", "text"), # pokazuje strzałki i napisy dla cech
                repel = TRUE, 
                col.var = "blue", 
                col.ind = "black")


fviz_pca_var(dane_pca, repel = TRUE,       # repel = TRUE etykiety 
             col.var = "blue")          # mają się nie nakładać


fviz_pca_var(dane_pca, col.var = "cos2", 
             gradient.cols = c("blue", "black", "red"), repel = TRUE)


fviz_pca_ind(dane_pca, 
             col.ind = "cos2", 
             gradient.cols = c("blue", "black", "red"), 
             repel = TRUE,
             title = "Mapa obserwacji – kolor wg cos2")


