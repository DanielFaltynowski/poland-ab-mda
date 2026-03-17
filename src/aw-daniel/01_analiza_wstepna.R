# ========================== PAKIETY ===========================================
if (!require("readxl")) install.packages("readxl")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("e1071")) install.packages("e1071")
if (!require("ineq")) install.packages("ineq")
if (!require("kableExtra")) install.packages("kableExtra")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("corrplot")) install.packages("corrplot")
if (!require("reshape2")) install.packages("reshape2")
if (!require("tibble")) install.packages("tibble")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("viridis")) install.packages("viridis")
if (!require("car")) install.packages("car")
if (!require("purrr")) install.packages("purrr")

library(readxl)
library(tidyverse)
library(e1071)
library(ineq)
library(kableExtra)
library(gridExtra)
library(corrplot)
library(reshape2)
library(tibble)
library(ggrepel)
library(ggpubr)
library(viridis)
library(car)
library(purrr)

# =========================== DANE =============================================
dane <- read_excel("./../../data/cleaned_data/poland_ab.xlsx", sheet = "normalized_data")
View(dane)

# ====================== OBLICZENIE STATYSTYK ==================================
statystyki <- dane %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  group_by(Zmienna) %>%
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
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  arrange(desc(Wsp_Zm_Proc))

# ====================== ŁADNA TABLICA W VIEWERZE ==============================
statystyki %>%
  arrange(Zmienna) %>%
  mutate(
    Klasa_Asymetrii = cell_spec(
      Klasa_Asymetrii,
      "html", 
      color = case_when(
        Klasa_Asymetrii == "silna lewostronna"     ~ "#B71C1C",
        Klasa_Asymetrii == "słaba/umiarkowana"     ~ "#1B5E20",
        Klasa_Asymetrii == "silna prawostronna"    ~ "#0D47A1"
      ),
      background = case_when(
        Klasa_Asymetrii == "silna lewostronna"     ~ "#FFCDD2",
        Klasa_Asymetrii == "słaba/umiarkowana"     ~ "#C8E6C9",
        Klasa_Asymetrii == "silna prawostronna"    ~ "#BBDEFB"
      ),
      bold = TRUE
    ),
    
    Klasa_Zmiennosci = cell_spec(
      Klasa_Zmiennosci,
      "html",
      color = case_when(
        Klasa_Zmiennosci == "bardzo mała zmienność"  ~ "#1B5E20",
        Klasa_Zmiennosci == "mała zmienność"          ~ "#388E3C",
        Klasa_Zmiennosci == "umiarkowana zmienność"  ~ "#F57C00",
        Klasa_Zmiennosci == "duża zmienność"          ~ "#E64A19",
        Klasa_Zmiennosci == "bardzo duża zmienność"  ~ "#B71C1C"
      ),
      background = case_when(
        Klasa_Zmiennosci == "bardzo mała zmienność"  ~ "#C8E6C9",
        Klasa_Zmiennosci == "mała zmienność"          ~ "#A5D6A7",
        Klasa_Zmiennosci == "umiarkowana zmienność"  ~ "#FFE0B2",
        Klasa_Zmiennosci == "duża zmienność"          ~ "#FFCCBC",
        Klasa_Zmiennosci == "bardzo duża zmienność"  ~ "#FFCDD2"
      ),
      bold = TRUE
    )
  ) %>%
  kbl(escape = FALSE, 
      caption = "<b>Statystyki opisowe zmiennych</b><br>
                 <span style='font-size:13px; font-weight:normal; color:gray;'>
                   <b>Asymetria:</b> 
                   <span style='background:#FFCDD2; color:#B71C1C; padding:2px;'>silna lewostronna</span> | 
                   <span style='background:#C8E6C9; color:#1B5E20; padding:2px;'>słaba/umiarkowana</span> | 
                   <span style='background:#BBDEFB; color:#0D47A1; padding:2px;'>silna prawostronna</span>
                 </span><br>
                 <span style='font-size:13px; font-weight:normal; color:gray;'>
                   <b>Zmienność:</b> 
                   <span style='background:#C8E6C9; padding:2px;'>bardzo mała (<10%)</span> | 
                   <span style='background:#A5D6A7; padding:2px;'>mała (10-25%)</span> | 
                   <span style='background:#FFE0B2; padding:2px;'>umiarkowana (25-50%)</span> | 
                   <span style='background:#FFCCBC; padding:2px;'>duża (50-75%)</span> | 
                   <span style='background:#FFCDD2; padding:2px;'>bardzo duża (&ge;75%)</span>
                 </span>",
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
dane_wykresy <- dane %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  left_join(
    statystyki %>% select(Zmienna, Klasa_Asymetrii, Klasa_Zmiennosci, Wsp_Zm_Proc, Skosnosc), 
    by = "Zmienna"
  )

# ====================== HISTOGRAMY WEDŁUG ASYMETRII ===========================

# --- GRUPA 1: SILNA ASYMETRIA LEWOSTRONNA ---
# Brak zmiennych silnie asymetrycznych lewostronnie

# --- GRUPA 2: ASYMETRIA SŁABA / UMIARKOWANA ---
dane_wykresy %>%
  filter(Klasa_Asymetrii == "słaba/umiarkowana") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#66bb6a", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Grupa 2: Asymetria słaba/umiarkowana (-1.2 do 1.2)",
    x = "Wartość",
    y = "Liczba wystąpień"
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
    x = "Wartość", y = "Liczba wystąpień"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))

# ====================== HISTOGRAMY WEDŁUG ZMIENNOŚCI ==========================

# --- ZMIENNOŚĆ: BARDZO MAŁA I MAŁA (<25%) ---
# Brak zmiennych dla których zmienność <25%

# --- ZMIENNOŚĆ: UMIARKOWANA (25-50%) ---
dane_wykresy %>%
  filter(Klasa_Zmiennosci == "umiarkowana zmienność") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#FFA726", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Zmienne o umiarkowanej zmienności (Wsp. Zm. 25-50%)",
    x = "Wartość", y = "Liczba wystąpień"
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
    x = "Wartość", y = "Liczba wystąpień"
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

# ====================== MACIERZ KORELACJI - HEATMAPA ==========================
cor_matrix <- dane %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

corrplot(
  cor_matrix,
  method = "circle",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
  title = "Wariant: Okręgi (wielkość = siła korelacji)",
  mar = c(0,0,2,0)
)

# ====================== ŁADNA TABELA KORELACJI ================================

# --- PEŁNA MACIERZ KORELACJI ---
cor_table <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("Zmienna") %>%
  mutate(across(-Zmienna, ~ cell_spec(round(.x, 2), "html", 
                                      bold = TRUE,
                                      color = ifelse(abs(.x) > 0.7, "white", "black"), 
                                      background = case_when(
                                        abs(.x) < 0.3 ~ "#FFCDD2", # brak/słaba
                                        abs(.x) < 0.7 ~ "#FFF9C4", # umiarkowana/silna
                                        abs(.x) <= 1.0 ~ "#2E7D32"  # bardzo silna
                                      )
  )))

cor_table %>%
  kbl(escape = FALSE, 
      caption = "<b>Macierz korelacji Pearsona</b><br>
                 <span style='font-size:12px; font-weight:normal; color:gray;'>
                 Interpretacja siły związku (|r|):</span>
                 <span style='background:#FFCDD2; padding:2px;'>0.0-0.3 brak/słaba</span>; 
                 <span style='background:#FFF9C4; padding:2px;'>0.3-0.7 umiarkowana/silna</span>; 
                 <span style='background:#2E7D32; color:white; padding:2px;'>0.7-1.0 bardzo silna</span>
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

# --- POŁOWA MACIERZY KORELACJI ---
cor_matrix_upper <- cor_matrix
cor_matrix_upper[lower.tri(cor_matrix_upper)] <- NA

cor_table <- cor_matrix_upper %>%
  as.data.frame() %>%
  rownames_to_column("Zmienna") %>%
  mutate(across(-Zmienna, ~ map_chr(.x, function(val) {
    if (is.na(val)) return("")
    cell_spec(round(val, 2), "html", 
              bold = TRUE,
              color = ifelse(abs(val) > 0.7, "white", "black"), 
              background = case_when(
                abs(val) < 0.3 ~ "#FFCDD2",
                abs(val) < 0.7 ~ "#FFF9C4",
                abs(val) <= 1.0 ~ "#2E7D32"
              ))
  })))

cor_table %>%
  kbl(escape = FALSE, 
      caption = "<b>Macierz korelacji Pearsona (Górny trójkąt)</b><br>
                 <span style='font-size:12px; font-weight:normal; color:gray;'>
                 Interpretacja siły związku (|r|): 
                 <span style='background:#FFCDD2; padding:2px;'>0.0-0.3 brak/słaba</span>; 
                 <span style='background:#FFF9C4; padding:2px;'>0.3-0.7 umiarkowana/silna</span>; 
                 <span style='background:#2E7D32; color:white; padding:2px;'>0.7-1.0 bardzo silna</span>
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

# ====================== TOP 30 KORELACJI (bez powtórzeń) =========================
cor_matrix_upper <- cor_matrix
cor_matrix_upper[lower.tri(cor_matrix_upper, diag = TRUE)] <- NA

cor_matrix_upper %>%
  melt(na.rm = TRUE) %>%
  mutate(abs_value = abs(value)) %>%
  arrange(desc(abs_value)) %>%
  slice_head(n = 30) %>%
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
    title = "Top 30 najsilniejszych korelacji",
    x = "Para zmiennych",
    y = "Współczynnik korelacji",
    fill = "Typ"
  ) +
  theme(legend.position = "bottom")

# ====================== KORELACJE >= 0.7 (bez powtórzeń) =========================
cor_matrix_upper <- cor_matrix
cor_matrix_upper[lower.tri(cor_matrix_upper, diag = TRUE)] <- NA

cor_matrix_upper %>%
  melt(na.rm = TRUE) %>%
  mutate(abs_value = abs(value)) %>%
  filter(abs_value >= 0.7) %>%
  arrange(desc(abs_value)) %>%
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
    title = "Wszystkie korelacje >= 0.7",
    x = "Para zmiennych",
    y = "Współczynnik korelacji",
    fill = "Typ"
  ) +
  theme(legend.position = "bottom")
