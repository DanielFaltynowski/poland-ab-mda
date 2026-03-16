# ========================== PAKIETY ===========================================
if (!require("readxl")) install.packages("readxl")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("e1071")) install.packages("e1071")
if (!require("ineq")) install.packages("ineq")
if (!require("kableExtra")) install.packages("kableExtra")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("corrplot")) install.packages("corrplot")

library(readxl)
library(tidyverse)
library(e1071)
library(ineq)
library(kableExtra)
library(gridExtra)
library(corrplot)

# =========================== DANE =============================================
dane <- read_excel("mda.xlsx", sheet = "normalized_data")
head(dane)
str(dane)
View(dane)

# ====================== OBLICZENIE STATYSTYK ==================================

statystyki <- dane %>%
  # Wybieramy tylko zmienne numeryczne, pomijamy kraj
  select(where(is.numeric)) %>%
  # Przekształcamy do długiego formatu
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  # Grupujemy po zmiennej
  group_by(Zmienna) %>%
  # Obliczamy wszystkie statystyki
  summarise(
    N                   = n(),
    Srednia             = mean(Wartosc),
    Mediana             = median(Wartosc),
    Min                 = min(Wartosc),
    Max                 = max(Wartosc),
    Odch_Std            = sd(Wartosc),
    Wsp_Zm_Proc         = (Odch_Std / Srednia) * 100,
    Skosnosc            = skewness(Wartosc, type = 2), 
    Gini                = ineq(Wartosc, type = "Gini")
  ) %>%
  # Dodajemy klasyfikacje
  mutate(
    Klasa_Asymetrii = case_when(
      Skosnosc < -1.2 ~ "silna lewostronna",     # Poniżej -1.2
      Skosnosc > 1.2  ~ "silna prawostronna",    # Powyżej 1.2 (Twoja zmiana)
      TRUE            ~ "słaba/umiarkowana"      # Wszystko pomiędzy (-1.2 a 1.2)
    ),
    Klasa_Zmiennosci = case_when(
      Wsp_Zm_Proc < 10 ~ "bardzo mała zmienność",
      Wsp_Zm_Proc < 25 ~ "mała zmienność",
      Wsp_Zm_Proc < 50 ~ "umiarkowana zmienność",
      Wsp_Zm_Proc < 75 ~ "duża zmienność",
      TRUE             ~ "bardzo duża zmienność"
    )
  ) %>%
  # Zaokrąglamy wszystko do 2 miejsc po przecinku
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  # Sortujemy malejąco po zmienności, tak jak chciałeś
  arrange(desc(Wsp_Zm_Proc))

View(statystyki)


# ====================== HISTOGRAMY NA SZYBKO ==================================

# Szybki przegląd graficzny zmiennych w formie hisgoramu 
dane %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(title = "Rozkłady zmiennych x1-x57")


# ====================== ŁADNA TABLICA W VIEWERZE ==============================

statystyki %>%
  mutate(
    # Kolorowanie ASYMETRII
    Klasa_Asymetrii = cell_spec(
      Klasa_Asymetrii,
      "html", 
      color = case_when(
        Klasa_Asymetrii == "silna lewostronna"     ~ "#B71C1C", # Ciemny Czerwony
        Klasa_Asymetrii == "słaba/umiarkowana"     ~ "#1B5E20", # Ciemny Zielony
        Klasa_Asymetrii == "silna prawostronna"    ~ "#0D47A1"  # Ciemny Niebieski
      ),
      background = case_when(
        Klasa_Asymetrii == "silna lewostronna"     ~ "#FFCDD2", # Jasny Czerwony
        Klasa_Asymetrii == "słaba/umiarkowana"     ~ "#C8E6C9", # Jasny Zielony
        Klasa_Asymetrii == "silna prawostronna"    ~ "#BBDEFB"  # Jasny Niebieski
      ),
      bold = TRUE
    ),
    
    # Kolorowanie ZMIENNOŚCI
    Klasa_Zmiennosci = cell_spec(
      Klasa_Zmiennosci,
      "html",
      color = case_when(
        Klasa_Zmiennosci == "bardzo mała zmienność"  ~ "#1B5E20", # Ciemny Zielony
        Klasa_Zmiennosci == "mała zmienność"         ~ "#388E3C", # Zielony
        Klasa_Zmiennosci == "umiarkowana zmienność"  ~ "#F57C00", # Pomarańczowy
        Klasa_Zmiennosci == "duża zmienność"         ~ "#E64A19", # Ciemny Pomarańczowy
        Klasa_Zmiennosci == "bardzo duża zmienność"  ~ "#B71C1C"  # Ciemny Czerwony
      ),
      background = case_when(
        Klasa_Zmiennosci == "bardzo mała zmienność"  ~ "#C8E6C9", # Jasny Zielony
        Klasa_Zmiennosci == "mała zmienność"         ~ "#A5D6A7", # Jaśniejszy Zielony
        Klasa_Zmiennosci == "umiarkowana zmienność"  ~ "#FFE0B2", # Jasny Pomarańczowy
        Klasa_Zmiennosci == "duża zmienność"         ~ "#FFCCBC", # Jaśniejszy Pomarańczowy
        Klasa_Zmiennosci == "bardzo duża zmienność"  ~ "#FFCDD2"  # Jasny Czerwony
      ),
      bold = TRUE
    )
  ) %>%
  kbl(escape = FALSE, 
      caption = "<b style='font-size:18px; color:black;'>Statystyki opisowe zmiennych</b>",
      align = "c") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "blue") %>%
  column_spec(1, bold = TRUE, background = "#f0f0f0") %>%
  scroll_box(width = "100%", height = "850px")


# ====================== PRZYGOTOWANIE DANYCH DO WYKRESÓW ======================

# Łączymy oryginalne dane z OBIEMA klasyfikacjami
dane_wykresy <- dane %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  left_join(
    statystyki %>% select(Zmienna, Klasa_Asymetrii, Klasa_Zmiennosci, Wsp_Zm_Proc, Skosnosc), 
    by = "Zmienna"
  )

# Sprawdzenie
head(dane_wykresy, 20)

# ====================== HISTOGRAMY WEDŁUG ASYMETRII ===========================

# --- GRUPA 1: SILNA ASYMETRIA LEWOSTRONNA ---
dane_wykresy %>%
  filter(Klasa_Asymetrii == "silna lewostronna") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#ef5350", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Grupa 1: Silna asymetria lewostronna (Skośność < -1.2)",
    subtitle = "Większość wartości wysokich, ogon w stronę niskich",
    x = "Wartość", y = "Liczba krajów"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))


# --- GRUPA 2: ASYMETRIA SŁABA / UMIARKOWANA ---
dane_wykresy %>%
  filter(Klasa_Asymetrii == "słaba/umiarkowana") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#66bb6a", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Grupa 2: Asymetria słaba/umiarkowana (-1.2 do 1.2)",
    subtitle = "Rozkłady zbliżone do symetrycznych",
    x = "Wartość", y = "Liczba krajów"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))

# --- GRUPA 3: SILNA ASYMETRIA PRAWOSTRONNA ---
dane_wykresy %>%
  filter(Klasa_Asymetrii == "silna prawostronna") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#42a5f5", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Grupa 3: Silna asymetria prawostronna (Skośność > 1.2)",
    subtitle = "Większość wartości niskich, pojedyncze bardzo wysokie (np. PKB)",
    x = "Wartość", y = "Liczba krajów"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))

# ====================== HISTOGRAMY WEDŁUG ZMIENNOŚCI ==========================

# --- ZMIENNOŚĆ: BARDZO MAŁA I MAŁA (<25%) ---
dane_wykresy %>%
  filter(Klasa_Zmiennosci %in% c("bardzo mała zmienność", "mała zmienność")) %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#66bb6a", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Zmienne o małej zmienności (Wsp. Zm. < 25%)",
    subtitle = "Stabilne, jednolite wartości między krajami",
    x = "Wartość", y = "Liczba krajów"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))


# --- ZMIENNOŚĆ: UMIARKOWANA (25-50%) ---
dane_wykresy %>%
  filter(Klasa_Zmiennosci == "umiarkowana zmienność") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#FFA726", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Zmienne o umiarkowanej zmienności (Wsp. Zm. 25-50%)",
    subtitle = "Średnie zróżnicowanie między krajami",
    x = "Wartość", y = "Liczba krajów"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))


# --- ZMIENNOŚĆ: DUŻA I BARDZO DUŻA (>50%) ---
dane_wykresy %>%
  filter(Klasa_Zmiennosci %in% c("duża zmienność", "bardzo duża zmienność")) %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#ef5350", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Zmienne o dużej zmienności (Wsp. Zm. > 50%)",
    subtitle = "Silne zróżnicowanie - duże różnice między krajami",
    x = "Wartość", y = "Liczba krajów"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))


# ====================== WYKRES SYNTETYCZNY: ASYMETRIA vs. ZMIENNOŚĆ ===========

statystyki %>%
  ggplot(aes(x = Skosnosc, y = Wsp_Zm_Proc, color = Klasa_Asymetrii, shape = Klasa_Zmiennosci)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text(aes(label = Zmienna), vjust = -0.8, size = 3, color = "black") +
  geom_vline(xintercept = c(-1.2, 1.2), linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = c(25, 50), linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "silna lewostronna" = "#ef5350",
      "słaba/umiarkowana" = "#66bb6a",
      "silna prawostronna" = "#42a5f5"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Mapa zmiennych: Asymetria vs. Zmienność",
    subtitle = "Linie przerywane = granice klasyfikacji",
    x = "Skośność (asymetria)",
    y = "Współczynnik zmienności (%)",
    color = "Klasa asymetrii",
    shape = "Klasa zmienności"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"  # Legendy wertykalnie (jedna pod drugą)
  )


# ====================== RANKING ZMIENNYCH =====================================

# Ranking po zmienności
statystyki %>%
  ggplot(aes(x = reorder(Zmienna, Wsp_Zm_Proc), y = Wsp_Zm_Proc, fill = Klasa_Zmiennosci)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "bardzo mała zmienność" = "#66bb6a",
      "mała zmienność" = "#9ccc65",
      "umiarkowana zmienność" = "#FFA726",
      "duża zmienność" = "#FF7043",
      "bardzo duża zmienność" = "#ef5350"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Ranking zmiennych według zmienności",
    x = "Zmienna",
    y = "Współczynnik zmienności (%)",
    fill = "Klasa zmienności"
  ) +
  theme(legend.position = "bottom")


# Ranking po asymetrii (wartość bezwzględna)
statystyki %>%
  mutate(Abs_Skosnosc = abs(Skosnosc)) %>%
  ggplot(aes(x = reorder(Zmienna, Abs_Skosnosc), y = Skosnosc, fill = Klasa_Asymetrii)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = c(-1.2, 1.2), linetype = "dashed", color = "gray30") +
  scale_fill_manual(
    values = c(
      "silna lewostronna" = "#ef5350",
      "słaba/umiarkowana" = "#66bb6a",
      "silna prawostronna" = "#42a5f5"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Ranking zmiennych według asymetrii",
    x = "Zmienna",
    y = "Skośność",
    fill = "Klasa asymetrii"
  ) +
  theme(legend.position = "bottom")


################################################################################
################################################################################

# ====================== MACIERZ KORELACJI - HEATMAPA ==========================
dane %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs") %>%
  corrplot(
    method = "color", 
    type = "upper",
    tl.col = "black", 
    tl.srt = 45,
    addCoef.col = "black", 
    number.cex = 0.6,
    col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
    # CZERWONY = ujemne, NIEBIESKI = dodatnie
    title = "Macierz korelacji zmiennych",
    mar = c(0,0,2,0)
  )

# ==============================================================================
# INNE MODYFIKACJE

# Najpierw wylicz macierz korelacji raz
cor_matrix <- dane %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

corrplot(
  cor_matrix,
  method = "circle",         # Okręgi (wielkość = siła korelacji)
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
  title = "Wariant: Okręgi (wielkość = siła korelacji)",
  mar = c(0,0,2,0)
)


corrplot(
  cor_matrix,
  method = "shade",          # Zacienione kwadraty
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
  title = "Wariant: Zacienione",
  mar = c(0,0,2,0)
)

corrplot(
  cor_matrix,
  method = "square",         # Kwadraty (jak color, ale bardziej geometryczne)
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.6,
  col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
  title = "Wariant: Kwadraty",
  mar = c(0,0,2,0)
)

corrplot(
  cor_matrix,
  method = "color",
  type = "full",             # Pełna macierz (nie tylko górny trójkąt)
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.5,
  col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
  title = "Wariant: Pełna macierz",
  mar = c(0,0,2,0)
)

# ====================== KORELACJE - WERSJA GGPLOT =============================

install.packages("reshape2")
library(reshape2)

cor_matrix <- dane %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

# Przekształcamy do długiego formatu
cor_matrix %>%
  melt() %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 2.5, color = "black") +
  scale_fill_gradient2(
    low = "#ef5350",      # CZERWONY = ujemne ✅
    mid = "white", 
    high = "#42a5f5",     # NIEBIESKI = dodatnie ✅
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank()
  ) +
  labs(
    title = "Macierz korelacji Pearsona",
    fill = "Korelacja"
  )

# ====================== TOP KORELACJE (bez powtórzeń) =========================

cor_matrix_upper <- cor_matrix
cor_matrix_upper[lower.tri(cor_matrix_upper, diag = TRUE)] <- NA

cor_matrix_upper %>%
  melt(na.rm = TRUE) %>%
  mutate(abs_value = abs(value)) %>%
  arrange(desc(abs_value)) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(paste(Var1, "-", Var2), abs_value), 
             y = value, 
             fill = value > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("TRUE" = "#42a5f5", "FALSE" = "#ef5350"),  # ✅ POPRAWIONE!
    #          NIEBIESKI = dodatnie    CZERWONY = ujemne
    labels = c("Dodatnia", "Ujemna")
  ) +
  theme_minimal() +
  labs(
    title = "Top 20 najsilniejszych korelacji",
    x = "Para zmiennych",
    y = "Współczynnik korelacji",
    fill = "Typ"
  ) +
  theme(legend.position = "bottom")

# ====================== ŁADNA TABELA KORELACJI ================================

cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("Zmienna") %>%
  kbl(digits = 2, 
      caption = "<b>Macierz korelacji Pearsona</b>",
      align = "c") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "blue") %>%
  column_spec(1, bold = TRUE, background = "#f0f0f0")

# ==============================================================================
# ====================== WYKRES ROZRZUTU - PODSTAWOWY ==========================

install.packages("ggrepel")
install.packages("ggpubr")
install.packages("viridis")
library(ggrepel)
library(ggpubr)
library(viridis)

# RÓŻNE PROPOZCYJE DO WYBRANIA

# Wyróżnij top 5 krajów (np. wg x2)
dane_highlight <- dane %>%
  mutate(
    Top = ifelse(x2 >= quantile(x2, 0.75), "Top 25%", "Pozostałe")
  )
dane_highlight %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = Top, size = Top), alpha = 0.8) +
  scale_color_manual(values = c("Top 25%" = "#FF6B6B", "Pozostałe" = "#42a5f5")) +
  scale_size_manual(values = c("Top 25%" = 6, "Pozostałe" = 4)) +
  geom_text_repel(
    aes(label = kraj), 
    size = 3.5,
    max.overlaps = 30,
    box.padding = 0.6,
    point.padding = 0.5,
    segment.color = "grey60",
    segment.size = 0.3,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "x1 vs x2",
    x = "x1",
    y = "x2",
    color = NULL,
    size = NULL
  )

#########################

dane %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = x2), size = 5, alpha = 0.8) +
  scale_color_gradient2(
    low = "#ef5350",
    mid = "#FFA726", 
    high = "#42a5f5",
    midpoint = median(dane$x2, na.rm = TRUE)
  ) +
  geom_text_repel(
    aes(label = kraj), 
    size = 3.5,
    max.overlaps = 30,
    box.padding = 0.6,
    point.padding = 0.4,
    segment.color = "grey60",
    segment.size = 0.3,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "x1 vs x2",
    x = "x1",
    y = "x2",
    color = "Wartość x2"
  )

#########################

dane %>%
  ggplot(aes(x = x1, y = x2, size = x3)) +
  geom_point(alpha = 0.7, color = "#42a5f5") +
  scale_size_continuous(range = c(3, 12)) +
  geom_text_repel(
    aes(label = kraj), 
    size = 3.5,
    max.overlaps = 30,
    box.padding = 0.7,
    point.padding = 0.6,
    segment.color = "grey50",
    segment.size = 0.3,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "x1 vs x2",
    x = "x1",
    y = "x2",
    size = "x3"
  )

#########################

dane %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = x2), size = 6, alpha = 0.85) +
  scale_color_viridis(option = "plasma") +
  geom_text_repel(
    aes(label = kraj), 
    size = 3.5,
    max.overlaps = 30,
    box.padding = 0.6,
    point.padding = 0.5,
    segment.color = "grey60",
    segment.size = 0.3,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "x1 vs x2",
    x = "x1",
    y = "x2",
    color = "Wartość x2"
  )

#########################

dane %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(size = 5.5, color = "#42a5f5", alpha = 0.85) +
  geom_point(size = 2.5, color = "white", alpha = 0.9) +  # Biały środek
  geom_text_repel(
    aes(label = kraj), 
    size = 3.5,
    max.overlaps = 30,
    box.padding = 0.6,
    point.padding = 0.5,
    segment.color = "grey50",
    segment.size = 0.3,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "x1 vs x2",
    x = "x1",
    y = "x2"
  )

#########################

dane %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(size = 3, alpha = 0.8, shape = 21, fill = "#42a5f5", color = "white", stroke = 1) +
  geom_text_repel(
    aes(label = kraj), 
    size = 3, 
    max.overlaps = 20,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = "gray70",
    segment.size = 0.2,
    min.segment.length = 0
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "x1 vs x2",
    x = "x1",
    y = "x2"
  )

# ==============================================================================

# Wybierz 4 zmienne i oblicz korelacje
cor_selected <- dane %>%
  dplyr::select(x1, x2, x3, x5) %>%
  cor(use = "complete.obs")

# Wykres corrplot
corrplot::corrplot(
  cor_selected,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 0,
  tl.cex = 1.2,
  addCoef.col = "black",
  number.cex = 1.2,
  col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
  title = "Macierz korelacji: x1, x2, x3, x5",
  mar = c(0, 0, 2, 0),
  cl.cex = 1
)

#####################

# install.packages("reshape2")
# library(reshape2)

cor_selected <- dane %>%
  dplyr::select(x1, x2, x3, x5) %>%
  cor(use = "complete.obs")

cor_selected %>%
  melt() %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(value, 2)), size = 6, fontface = "bold") +
  scale_fill_gradient2(
    low = "#ef5350",
    mid = "white",
    high = "#42a5f5",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Macierz korelacji: x1, x2, x3, x5",
    fill = "Korelacja"
  ) +
  coord_fixed()

########################

# x1 vs x2
p1 <- dane %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(size = 4, alpha = 0.7, color = "#42a5f5") +
  geom_text_repel(aes(label = kraj), size = 2.5, max.overlaps = 15) +
  theme_minimal() +
  labs(title = "x1 vs x2", x = "x1", y = "x2") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# x1 vs x3
p2 <- dane %>%
  ggplot(aes(x = x1, y = x3)) +
  geom_point(size = 4, alpha = 0.7, color = "#66bb6a") +
  geom_text_repel(aes(label = kraj), size = 2.5, max.overlaps = 15) +
  theme_minimal() +
  labs(title = "x1 vs x3", x = "x1", y = "x3") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# x2 vs x3
p3 <- dane %>%
  ggplot(aes(x = x2, y = x3)) +
  geom_point(size = 4, alpha = 0.7, color = "#FFA726") +
  geom_text_repel(aes(label = kraj), size = 2.5, max.overlaps = 15) +
  theme_minimal() +
  labs(title = "x2 vs x3", x = "x2", y = "x3") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# x2 vs x5
p4 <- dane %>%
  ggplot(aes(x = x2, y = x5)) +
  geom_point(size = 4, alpha = 0.7, color = "#ef5350") +
  geom_text_repel(aes(label = kraj), size = 2.5, max.overlaps = 15) +
  theme_minimal() +
  labs(title = "x2 vs x5", x = "x2", y = "x5") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Połącz w siatkę 2x2
grid.arrange(p1, p2, p3, p4, ncol = 2,
             top = "Wykresy rozrzutu dla wybranych zmiennych")



###################

# x1 vs x2
p1 <- dane %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(size = 4.5, alpha = 0.75, color = "#42a5f5") +
  theme_minimal() +
  labs(title = "x1 vs x2", 
       subtitle = paste("r =", round(cor(dane$x1, dane$x2), 3)),
       x = "x1", y = "x2") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# x1 vs x3
p2 <- dane %>%
  ggplot(aes(x = x1, y = x3)) +
  geom_point(size = 4.5, alpha = 0.75, color = "#66bb6a") +
  theme_minimal() +
  labs(title = "x1 vs x3",
       subtitle = paste("r =", round(cor(dane$x1, dane$x3), 3)),
       x = "x1", y = "x3") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# x2 vs x3
p3 <- dane %>%
  ggplot(aes(x = x2, y = x3)) +
  geom_point(size = 4.5, alpha = 0.75, color = "#FFA726") +
  theme_minimal() +
  labs(title = "x2 vs x3",
       subtitle = paste("r =", round(cor(dane$x2, dane$x3), 3)),
       x = "x2", y = "x3") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# x2 vs x5
p4 <- dane %>%
  ggplot(aes(x = x2, y = x5)) +
  geom_point(size = 4.5, alpha = 0.75, color = "#ef5350") +
  theme_minimal() +
  labs(title = "x2 vs x5",
       subtitle = paste("r =", round(cor(dane$x2, dane$x5), 3)),
       x = "x2", y = "x5") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# Połącz
library(gridExtra)

grid.arrange(p1, p2, p3, p4, ncol = 2,
             top = grid::textGrob("Wykresy rozrzutu dla wybranych zmiennych", 
                                  gp = grid::gpar(fontsize = 16, fontface = "bold")))


# ==============================================================================

# OWOCNEJ ANALIZY I ZABAWY
