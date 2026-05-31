library(factoextra)
library(cluster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(ggrepel)
library(gt)
library(tibble)
library(scales)
library(readxl)


# Import danych
dane <- read_xlsx(path = './../../data/dane.xlsx', sheet = 'dane')
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
dane_finalne <- dane_filtr1[, !(colnames(dane_filtr1) %in% zmienne_do_usuniecia)]

rownames(dane_finalne) <- nazwy_wojewodztw

wyniki_optymalizacji <- fviz_nbclust(
  dane_finalne,
  kmeans,
  method = 'silhouette',
  k.max = 8
)

dane_plot <- wyniki_optymalizacji$data
dane_plot$clusters <- as.numeric(as.character(dane_plot$clusters))

punkt_max <- dane_plot[which.max(dane_plot$y), ]

# BUDOWA WYKRESU
ggplot(dane_plot, aes(x = clusters, y = y)) +
  theme_minimal(base_size = 15) +
  geom_vline(xintercept = punkt_max$clusters, 
             linetype = "dashed", color = "#E64B35", size = 1.2) +
  geom_line(color = "#1d3557", size = 2, group = 1) +
  geom_point(color = "#1d3557", fill = "white", size = 6, shape = 21, stroke = 2.5) +
  geom_point(data = punkt_max, aes(x = clusters, y = y), 
             color = "#E64B35", fill = "#E64B35", size = 8, shape = 19) +
  geom_text(aes(label = round(y, 3)), vjust = -2, fontface = "bold", size = 5, color = "#1d3557") +
  labs(
    title = "ANALIZA OPTYMALNEJ LICZBY KLAS",
    subtitle = paste("Rekomendacja: k =", punkt_max$clusters, "(najwyższa spójność klas)"),
    x = "WYBRANA LICZBA KLAS (k)",
    y = "Średni wskaźnik Silhouette"
  ) +
  scale_x_continuous(breaks = 1:8) +
  expand_limits(y = max(dane_plot$y) + 0.08) +
  theme(
    plot.title = element_text(face = "bold", size = 22, color = "#1d3557"),
    plot.subtitle = element_text(size = 16, color = "#E64B35", face = "bold.italic"),
    axis.text.x = element_text(size = 20, face = "bold", color = "black", vjust = -0.5),
    # POPRAWKA: dodano ggplot2:: przed margin
    axis.title.x = element_text(size = 16, face = "bold", margin = ggplot2::margin(t = 15)),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1.5, color = "black")
  )


wyniki_elbow <- fviz_nbclust(
  dane_finalne,
  kmeans,
  method = "wss",
  k.max = 8
)

dane_elbow <- wyniki_elbow$data
dane_elbow$clusters <- as.numeric(as.character(dane_elbow$clusters))

ggplot(dane_elbow, aes(x = clusters, y = y)) +
  theme_minimal(base_size = 15) +
  geom_line(color = "#457B9D", size = 2) +
  geom_point(color = "#1D3557", fill = "white", size = 6, shape = 21, stroke = 2.5) +
  annotate("point", x = 3, y = dane_elbow$y[3], color = "#E63946", size = 10, shape = 19) +
  annotate("text", x = 3.3, y = dane_elbow$y[3] + max(dane_elbow$y)*0.05, 
           label = "POTENCJALNY\nŁOKIEĆ", color = "#E63946", fontface = "bold", hjust=0) +
  labs(title = "METODA ŁOKCIA (ELBOW METHOD)",
       subtitle = "Szukamy momentu załamania krzywej (punktu przegięcia)",
       x = "LICZBA KLAS (k)", y = "Całkowita suma kwadratów (WSS)") +
  scale_x_continuous(breaks = 1:8) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#1D3557"),
    plot.subtitle = element_text(size = 14, color = "#457B9D"),
    axis.text.x = element_text(size = 18, face = "bold", color = "black"),
    axis.title.x = element_text(size = 16, face = "bold", margin = ggplot2::margin(t = 15)),
    axis.line = element_line(size = 1.5, color = "black")
  )

library(R2HTML)
library(clusterSim)

# Analiza optymalnej liczby klas - Davies-Bouldin index

# zakres testowanych k
k_values <- 2:8
# Wektor na wyniki DBI
dbi_values <- numeric(length(k_values))

set.seed(123)
# Liczenie DBI dla różnych k
for (i in seq_along(k_values)) {
  k <- k_values[i]
  model_kmeans <- kmeans(dane_finalne, centers = k, nstart = 25)
  dbi <- index.DB(dane_finalne, model_kmeans$cluster, centrotypes = "centroids")
  dbi_values[i] <- dbi$DB
}
dane_dbi <- data.frame(clusters = k_values, DBI = dbi_values)

# ==============================================================================
# WYZNACZENIE PIERWSZEGO WYRAŹNEGO SPADKU
# ==============================================================================
# Różnice między kolejnymi DBI
spadki <- diff(dane_dbi$DBI)
# Największy spadek
idx_best <- which.max(abs(spadki))
# Punkt po największym spadku
punkt_best <- dane_dbi[idx_best + 1, ]
# ==============================================================================
# WYKRES
# ==============================================================================
ggplot(dane_dbi, aes(x = clusters, y = DBI)) +
  theme_minimal(base_size = 15) +
  # Pionowa linia rekomendacji
  geom_vline(xintercept = punkt_best$clusters, linetype = "dashed",
             color = "#E64B35", size = 1.2) +
  # Linia trendu
  geom_line(color = "#1d3557", size = 2, group = 1) +
  # Punkty
  geom_point(color = "#1d3557", fill = "white", size = 6,
             shape = 21, stroke = 2.5) +
  # Wyróżnienie najlepszego punktu
  geom_point(data = punkt_best, aes(x = clusters, y = DBI), color = "#E64B35",
             fill = "#E64B35", size = 8, shape = 19) +
  # Opisy wartości
  geom_text(aes(label = round(DBI, 3)), vjust = -2, fontface = "bold", size = 5,
            color = "#1d3557") +
  labs(title = "ANALIZA OPTYMALNEJ LICZBY KLAS",
       subtitle = paste("Rekomendacja: k =", punkt_best$clusters,
                        "(pierwszy wyraźny spadek DBI)"), x = "WYBRANA LICZBA KLAS (k)",
       y = "Davies-Bouldin Index") +
  scale_x_continuous(breaks = 2:8) +
  expand_limits(y = max(dane_dbi$DBI) + 0.1
  ) +
  theme(plot.title = element_text(face = "bold", size = 22, color = "#1d3557"),
        plot.subtitle = element_text(size = 16, color = "#E64B35", face = "bold.italic"),
        axis.text.x = element_text(size = 20, face = "bold", color = "black",
                                   vjust = -0.5),
        axis.title.x = element_text(size = 16, face = "bold",
                                    margin = ggplot2::margin(t = 15)),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(), axis.line = element_line(size = 1.5,
                                                                     color = "black"))
# ==============================================================================
# USTAWIAMY WYBRANE K I URUCHAMIAMY MODEL
# ==============================================================================

k_final <- 3
set.seed(123)
model_k <- kmeans(
  dane_finalne,
  centers = k_final,
  nstart = 50
)

paleta_dynamiczna <- pal_npg()(k_final)
names(paleta_dynamiczna) <- paste("Klasa", 1:k_final)

wyniki <- dane_finalne %>%
  mutate(Klasa = factor(paste("Klasa", model_k$cluster)), Wojewodztwo = nazwy_wojewodztw)

wyniki

podsumowanie_wielkosci <- wyniki %>%
  count(Klasa) %>%
  rename(Liczba_Wojewodztw = n) %>%
  arrange(Klasa)

tabela_podsumowanie <- podsumowanie_wielkosci %>%
  gt() %>%
  tab_header(title = md("**PODSUMOWANIE PODZIAŁU NA KLASY**"),
             subtitle = paste("Liczba województw w grupach (k =", k_final, ")")) %>%
  tab_style(style = list(cell_text(size = px(18), weight = "bold", color = "#E64B35")),
            locations = cells_title(groups = "subtitle")) %>%
  cols_label(Klasa = md("**Nazwa Klasy**"), 
             Liczba_Wojewodztw = md("**Liczba Województw**")) %>%
  tab_style(style = list(cell_text(weight = "bold", size = px(18))), 
            locations = cells_body(columns = Liczba_Wojewodztw)) %>%
  tab_style(style = list(cell_text(weight = "bold", size = px(16), 
                                   color = "white")), locations = cells_body(columns = Klasa)) %>%
  tab_options(table.width = px(550), table.border.top.color = "black", 
              table.border.top.width = px(3))
for (i in 1:k_final) {
  tabela_podsumowanie <- tabela_podsumowanie %>%
    tab_style(style = cell_fill(color = paleta_dynamiczna[i]), 
              locations = cells_body(rows = i, columns = Klasa))
}
tabela_podsumowanie

fviz_cluster(model_k, data = dane_finalne,
             palette = "npg",              # Kolory z palety Nature
             geom = "point",               
             pointsize = 4,                
             ellipse.type = "convex", 
             ellipse.alpha = 0.1,          
             show.clust.cent = TRUE,       
             ggtheme = theme_minimal(base_size = 16)) +
  # Inteligentne etykiety województw
  geom_text_repel(aes(label = rownames(dane_finalne)), 
                  size = 4, fontface = "bold", color = "black",
                  max.overlaps = 20,        
                  box.padding = 0.5) +      
  # Zmiana etykiet w legendzie (1, 2, 3 -> Klasa 1, Klasa 2, Klasa 3)
  scale_color_discrete(labels = paste("Klasa", 1:k_final)) +
  scale_fill_discrete(labels = paste("Klasa", 1:k_final)) +
  scale_shape_discrete(labels = paste("Klasa", 1:k_final)) +
  labs(title = "Struktura podobieństwa województw", 
       subtitle = "Podział na klasy w przestrzeni głównych składowych (PCA)",
       x = "PCA 1", y = "PCA 2",
       color = "Klasa",   
       fill = "Klasa",   
       shape = "Klasa") + 
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 14), 
    legend.text = element_text(face = "bold", size = 12), 
    plot.title = element_text(face = "bold", size = 20, color = "#1d3557"),
    panel.grid.minor = element_blank()
  )

sil <- silhouette(model_k$cluster, dist(dane_finalne))
sil_df <- as.data.frame(sil) %>%
  mutate(Wojewodztwo = factor(nazwy_wojewodztw, levels = nazwy_wojewodztw[order(cluster, sil[, 3])]),
         Klasa_Label = factor(paste("Klasa", cluster)))

ggplot(sil_df, aes(x = Wojewodztwo, y = sil_width, fill = Klasa_Label)) +
  # Słupki
  geom_col(width = 0.8) +
  # Wartości na końcu słupków
  geom_text(aes(label = round(sil_width, 2)), hjust = -0.3, size = 4,
            fontface = "bold", color = "#1d3557") +
  # Kolory z palety
  scale_fill_manual(values = paleta_dynamiczna) +
  # Odwrócenie wykresu
  coord_flip() +
  # Rozszerzenie osi
  expand_limits(y = max(sil_df$sil_width) + 0.08) + 
  labs(title = "JAKOŚĆ WYODRĘBNIONYCH KLAS",
       subtitle = paste("Średnia jakość podziału =", 
                        round(mean(sil_df$sil_width), 3)),
       x = "WOJEWÓDZTWA", y = "Wskaźnik Silhouette") +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 22, color = "#1d3557"),
    plot.subtitle = element_text(size = 16, color = "#E64B35", face = "bold.italic"),
    axis.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)), 
    axis.text.y = element_text(size = 14, face = "bold", color = "black"), 
    axis.text.x = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1.5, color = "black")
  )


# Przygotowanie danych
s_ogolna <- dane_finalne %>%
  as.data.frame() %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(everything(), names_to = "Zmienna", values_to = "Srednia_Ogolna")
s_klas <- wyniki %>%
  group_by(Klasa) %>%
  summarise(across(where(is.numeric), mean)) %>%
  pivot_longer(-Klasa, names_to = "Zmienna", values_to = "Wartosc") %>%
  # POPRAWKA: usunięto names_prefix = "Klasa_"
  pivot_wider(names_from = Klasa, values_from = Wartosc, names_prefix = "")

tabela_kompletna <- left_join(s_klas, s_ogolna, by = "Zmienna")

# Budowa tabeli
tabela_finalna <- tabela_kompletna %>%
  gt() %>%
  tab_header(
    title = md("**Profile Klas: Analiza Odchyleń**"),
    subtitle = "Kolory określają dystans i kierunek względem średniej ogólnej"
  ) %>%
  fmt_number(columns = everything(), decimals = 2) %>%
  cols_label(
    Zmienna = md("**Cechy**"), 
    Srednia_Ogolna = md("**Średnia Ogólna**")
  ) %>%
  # Kolorujemy TYLKO kolumny Klas
  data_color(
    columns = starts_with("Klasa"),
    method = "numeric",
    palette = c("#E64B35", "white", "#1d3557"),
    direction = "row"                           
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "#333333"),
      cell_borders(sides = c("left", "right"), color = "#A6A6A6", weight = px(2))
    ),
    locations = cells_body(columns = Srednia_Ogolna)
  ) %>%
  # Estetyka ogólna
  tab_options(
    table.font.size = px(15),
    column_labels.font.weight = "bold",
    table.width = px(800),
    data_row.padding = px(10),
    table.border.top.style = "none",
    column_labels.background.color = "#F9F9F9"
  ) %>%
  cols_align(align = "center", columns = -Zmienna) %>%
  cols_align(align = "left", columns = Zmienna)

tabela_finalna

# Przygotowanie danych do układu kolumnowego
tabela_wojew_dane <- wyniki %>%
  dplyr::select(Wojewodztwo, Klasa) %>%
  arrange(Klasa, Wojewodztwo) %>%
  group_by(Klasa) %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = Klasa, values_from = Wojewodztwo, 
              names_prefix = "") %>%
  dplyr::select(-id) %>%
  # Zamiana NA na pusty tekst
  mutate(across(everything(), ~replace_na(.x, "")))

# Tworzenie podstawowej struktury tabeli gt
tabela_wojew_final <- tabela_wojew_dane %>%
  gt() %>%
  tab_header(
    title = md("**KLASYFIKACJA WOJEWÓDZTW**"),
    subtitle = "Przynależność terytorialna do wyodrębnionych klas"
  ) %>%
  # Estetyka komórek i obramowań
  tab_options(
    table.width = px(650),
    table.font.size = px(14),
    data_row.padding = px(10),
    column_labels.font.size = px(16),
    table.border.top.color = "black",
    table.border.top.width = px(3),
    heading.title.font.size = px(22)
  ) %>%
  # Delikatne linie pionowe oddzielające kolumny
  tab_style(
    style = cell_borders(sides = "right", color = "#D3D3D3", weight = px(1)),
    locations = cells_body(columns = everything())
  ) %>%
  cols_align(align = "center", columns = everything())

# ============================================================
# DYNAMICZNE KOLOROWANIE NAGŁÓWKÓW (Pętla dla dowolnego k_final)
# ============================================================
for (i in 1:k_final) {
  tabela_wojew_final <- tabela_wojew_final %>%
    tab_style(
      style = list(
        cell_fill(color = paleta_dynamiczna[i]), 
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_column_labels(columns = i) 
    )
}

# Wyświetlenie gotowej tabeli
tabela_wojew_final




# ==============================================================================
# FUZZY C-MEANS
# ==============================================================================

library(e1071)

rozmyta_sylwetka <- function(model_fuzzy, macierz_odleglosci, alpha = 1) {
  standardowa_sylwetka <- silhouette(model_fuzzy$cluster, macierz_odleglosci)
  sylwetka_i <- standardowa_sylwetka[, 3]
  U <- model_fuzzy$membership
  posortowane_U <- t(apply(U, 1, function(wiersz) sort(wiersz, decreasing = TRUE)))
  u_pierwsze <- posortowane_U[, 1]
  u_drugie <- posortowane_U[, 2]
  wagi <- (u_pierwsze - u_drugie)^alpha
  FS <- sum(wagi * sylwetka_i) / sum(wagi)
  return(FS)
}

m <- 2
dystans <- dist(dane_finalne)
wyniki_silhouette <- data.frame(k = 2:8, y = NA)

for (i in 1:nrow(wyniki_silhouette)) {
  set.seed(123)
  model_petla <- cmeans(dane_finalne, centers = wyniki_silhouette$k[i], iter.max = 100, m = m)
  wyniki_silhouette$y[i] <- rozmyta_sylwetka(model_petla, dystans, alpha = 1)
}

# Wyznaczenie punktu maksymalnego
punkt_max <- wyniki_silhouette[which.max(wyniki_silhouette$y), ]

# WYKRES ANALIZY OPTYMALIZACJI (Twój ulubiony styl BOLD)
ggplot(wyniki_silhouette, aes(x = k, y = y)) +
  theme_minimal(base_size = 15) +
  geom_vline(xintercept = punkt_max$k, linetype = "dashed", color = "#E53E3E", size = 1.2) +
  geom_line(color = "#1A365D", size = 2, group = 1) +
  geom_point(color = "#1A365D", fill = "white", size = 6, shape = 21, stroke = 2.5) +
  geom_point(data = punkt_max, aes(x = k, y = y), color = "#E53E3E", fill = "#E53E3E", size = 8, shape = 19) +
  geom_text(aes(label = round(y, 3)), vjust = -2, fontface = "bold", size = 5, color = "#1A365D") +
  labs(
    title = "ANALIZA OPTYMALNEJ LICZBY KLAS (FUZZY SILHOUETTE)",
    subtitle = paste("Rekomendacja: k =", punkt_max$k, "(najwyższa spójność rozmyta)"),
    x = "WYBRANA LICZBA KLAS (k)",
    y = "Wskaźnik Fuzzy Silhouette (FS)"
  ) +
  scale_x_continuous(breaks = 2:8) +
  expand_limits(y = c(min(wyniki_silhouette$y) - 0.05, max(wyniki_silhouette$y) + 0.08)) +
  theme(
    plot.title = element_text(face = "bold", size = 22, color = "#1A365D"),
    plot.subtitle = element_text(size = 16, color = "#E53E3E", face = "bold.italic"),
    axis.text.x = element_text(size = 20, face = "bold", color = "black", vjust = -0.5),
    axis.title.x = element_text(size = 16, face = "bold", margin = ggplot2::margin(t = 15)),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1.5, color = "black")
  )

k_final <- 3
set.seed(123)
model_fuzzy <- cmeans(dane_finalne, centers = k_final, iter.max = 100, m = m)

paleta_dynamiczna <- scales::hue_pal()(k_final)
nazwy_klas <-paste("Klasa", 1:k_final)

membership_mat <- as.data.frame(model_fuzzy$membership)
colnames(membership_mat) <- nazwy_klas
rownames(membership_mat) <- rownames(dane_finalne)
membership_mat

membership_long <- membership_mat %>%
  rownames_to_column(var = "Wojewodztwo") %>%
  pivot_longer(cols = starts_with("Klasa"), names_to = "Klasa", values_to = "Stopień")

ggplot(membership_long, aes(x = reorder(Wojewodztwo, Stopień), y = Stopień, fill = Klasa)) +
  geom_col(width = 0.8, color = "white") +
  scale_fill_manual(values = paleta_dynamiczna) +
  coord_flip() +
  labs(
    title = "STRUKTURA PRZYNALEŻNOŚCI WOJEWÓDZTW",
    subtitle = paste("Model rozmyty Fuzzy C-Means (k =", k_final, ")"),
    x = "WOJEWÓDZTWA", 
    y = "Stopień przynależności (Suma = 1.0)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 22, color = "#1A365D"),
    plot.subtitle = element_text(size = 16, color = "#E53E3E", face = "bold.italic"),
    axis.text.y = element_text(size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold", margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1.5, color = "black")
  )

fviz_cluster(
  list(data = dane_finalne, cluster = model_fuzzy$cluster),
  palette = paleta_dynamiczna,
  ellipse.type = "convex",
  pointsize = 4,
  repel = TRUE,
  ggtheme = theme_minimal(base_size = 15)) +
  labs(
    title = "STRUKTURA PODOBIEŃSTWA WOJEWÓDZTW",
    subtitle = paste("Klasyfikacja Fuzzy C-Means (k =", k_final, ")"),
    x = "PCA 1", y = "PCA 2") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 22, color = "#1A365D"),
        plot.subtitle = element_text(size = 16,color = "#E53E3E",face = "bold.italic"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 1.5, color = "black"),
        axis.text = element_text(face = "bold", size = 12))

set.seed(123)
model_fuzzy <- cmeans(dane_finalne, centers = k_final, iter.max = 100, m = m)

sil <- silhouette(model_fuzzy$cluster, dist(dane_finalne))
sylwetka_i <- sil[, 3]
U <- model_fuzzy$membership
posortowane_U <- t(apply(U, 1, function(wiersz) sort(wiersz, decreasing = TRUE)))
u_pierwsze <- posortowane_U[, 1]
u_drugie <- posortowane_U[, 2]
wagi <- (u_pierwsze - u_drugie)^1

# 2. Obliczamy globalny wskaźnik
FS_mean <- sum(wagi * sylwetka_i) / sum(wagi)

# 3. ROzmyte przynależności
s_i_fuzzy <- (sylwetka_i * wagi) / mean(wagi)

sil_df <- data.frame( 
  Wojewodztwo = rownames(dane_finalne), 
  cluster = model_fuzzy$cluster,
  fuzzy_val = s_i_fuzzy
) %>%
  mutate(Wojewodztwo = factor(Wojewodztwo, levels = Wojewodztwo[order(cluster, fuzzy_val)]),
         Klasa_Label = factor(paste("Klasa", cluster)))

# 4. Wykres
ggplot(sil_df, aes(x = Wojewodztwo, y = fuzzy_val, fill = Klasa_Label)) +
  geom_col(width = 0.8) +
  
  geom_text(aes(label = round(fuzzy_val, 2)), hjust = -0.3, size = 4,
            fontface = "bold", color = "#1A365D") +
  scale_fill_manual(values = paleta_dynamiczna) +
  coord_flip() +
  expand_limits(y = max(sil_df$fuzzy_val) + 0.1) +
  labs(title = "JAKOŚĆ WYODRĘBNIONYCH KLAS",
       subtitle = paste("Globalna rozmyta sylwetka FS =", round(FS_mean, 3)),
       x = "WOJEWÓDZTWA",  y = "Wskaźnik Fuzzy Silhouette") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 22, color = "#1A365D"),
        plot.subtitle = element_text(size = 16, color = "#E53E3E", face = "bold.italic"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", color = "black"),
        axis.title.x = element_text(size = 14, face = "bold", color = "black", 
                                    margin = ggplot2::margin(t = 10)),
        axis.line = element_line(size = 1.5, color = "black"))


tabela_wynikowa <- membership_mat %>%
  mutate(Dominująca_Klasa = paste("Klasa", model_fuzzy$cluster)) %>%
  rownames_to_column(var = "Wojewodztwo") %>%
  arrange(Dominująca_Klasa)

tabela_wynikowa %>%
  gt() %>%
  tab_header(
    title = md("**KLASYFIKACJA ROZMYTA WOJEWÓDZTW**"),
    subtitle = md(paste("Stopień przynależności do wyodrębnionych grup (k =", k_final, ") - Dane surowe"))
  ) %>%
  tab_style(
    style = list(cell_text(size = px(22), weight = "bold", color = "#1A365D")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(cell_text(size = px(16), weight = "bold", color = "#E53E3E", style = "italic")),
    locations = cells_title(groups = "subtitle")
  ) %>%
  fmt_number(columns = starts_with("Klasa"), decimals = 3) %>%
  # DYNAMICZNA SKALA KOLORÓW DLA GT
  data_color(
    columns = starts_with("Klasa"),
    colors = scales::col_numeric(palette = c("white", paleta_dynamiczna[1]), domain = c(0, 1))
  ) %>%
  cols_label(
    Wojewodztwo = md("**Województwo**"),
    Dominująca_Klasa = md("**Klasa Dominująca**")
  ) %>%
  # Wyśrodkowanie danych we wszystkich kolumnach
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.width = px(750),
    column_labels.font.weight = "bold",
    table.border.top.color = "black",
    table.border.top.width = px(3)
  )

# 1. Przygotowanie danych (przypisanie do klas dominujących na podstawie modelu rozmytego)
wyniki_fuzzy <- data.frame(
  Wojewodztwo = rownames(dane_finalne),
  Klasa = factor(paste("Klasa", model_fuzzy$cluster), levels = paste("Klasa", 1:k_final))
)

tabela_wojew_dane <- wyniki_fuzzy %>%
  dplyr::select(Wojewodztwo, Klasa) %>%
  arrange(Klasa, Wojewodztwo) %>%
  group_by(Klasa) %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = Klasa, values_from = Wojewodztwo) %>%
  dplyr::select(-id)

# Zabezpieczenie: Jeśli któraś klasa jest pusta, tworzymy dla niej pustą kolumnę
for(klasa in paste("Klasa", 1:k_final)) {
  if(!klasa %in% colnames(tabela_wojew_dane)) {
    tabela_wojew_dane[[klasa]] <- NA
  }
}

# Porządkujemy kolumny od Klasy 1 do Klasy K i zamieniamy NA na pusty tekst
tabela_wojew_dane <- tabela_wojew_dane %>%
  dplyr::select(all_of(paste("Klasa", 1:k_final))) %>%
  mutate(across(everything(), ~replace_na(.x, "")))

# 2. Tworzenie podstawowej struktury tabeli gt
tabela_wojew_final <- tabela_wojew_dane %>%
  gt() %>%
  tab_header(
    title = md("**KLASYFIKACJA WOJEWÓDZTW (FUZZY C-MEANS)**"),
    subtitle = "Przynależność terytorialna do grup na podstawie dominującego stopnia przynależności"
  ) %>%
  tab_style(
    style = list(cell_text(size = px(22), weight = "bold", color = "#1A365D")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(cell_text(size = px(16), weight = "bold", color = "#E53E3E", style = "italic")),
    locations = cells_title(groups = "subtitle")
  ) %>%
  # Estetyka komórek i obramowań
  tab_options(
    table.width = px(750),
    table.font.size = px(14),
    data_row.padding = px(10),
    column_labels.font.size = px(16),
    table.border.top.color = "black",
    table.border.top.width = px(3)
  ) %>%
  # Delikatne linie pionowe oddzielające kolumny
  tab_style(
    style = cell_borders(sides = "right", color = "#D3D3D3", weight = px(1)),
    locations = cells_body(columns = everything())
  ) %>%
  cols_align(align = "center", columns = everything())

# ============================================================
# DYNAMICZNE KOLOROWANIE NAGŁÓWKÓW (Używa Twojej nowej palety)
# ============================================================
for (i in 1:k_final) {
  tabela_wojew_final <- tabela_wojew_final %>%
    tab_style(
      style = list(
        cell_fill(color = paleta_dynamiczna[i]), 
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_column_labels(columns = i) 
    )
}

# Wyświetlenie gotowej tabeli
tabela_wojew_final
