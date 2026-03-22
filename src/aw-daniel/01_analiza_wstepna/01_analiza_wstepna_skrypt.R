# ==============================================================================
# INSTALACJA I IMPORT BIBLIOTEK
# ==============================================================================

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
if (!require("tidytext")) install.packages("tidytext")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("reactable")) install.packages("reactable")

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
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reactable)


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
  "tidytext"
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
  path = './01_analiza_wstepna/dualizm_polski.xlsx',
  sheet = 'dane'
)

dane
# View(dane)


# ==============================================================================
# STATYSTYKI OPISOWE
# ==============================================================================

statystyki <- dane %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  group_by(Zmienna) %>%
  summarise(
    N = n(),
    Srednia = mean(Wartosc),
    Mediana = median(Wartosc),
    Min = min(Wartosc),
    Max = max(Wartosc),
    Odch_Std = sd(Wartosc),
    Skosnosc = skewness(Wartosc, type = 2),
    Kurtoza = kurtosis(Wartosc, type = 2),
    Wsp_Zm_Proc = (Odch_Std / Srednia) * 100,
    Gini = ineq(Wartosc, type = "Gini")
  ) %>%
  mutate(
    Klasa_Asymetrii = case_when(
      Skosnosc < -1.2 ~ "silna lewostronna",
      Skosnosc > 1.2  ~ "silna prawostronna",
      TRUE            ~ "słaba/umiarkowana"
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
  arrange(desc(Zmienna))


# ==============================================================================
# TABELARYCZNE BADANIE ASYMETRII I WSPÓŁCZYNNIKA ZMIENNOŚCI
# ==============================================================================

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


# ==============================================================================
# BARPLOTY DLA WSZYSTKICH ZMIENNYCH POSORTOWANE
# ==============================================================================

dane_long <- dane %>%
  pivot_longer(cols = where(is.numeric), 
               names_to = "zmienna", 
               values_to = "wartosc")

ggplot(dane_long, aes(x = reorder_within(woj, wartosc, zmienna), y = wartosc, fill = woj)) +
  geom_col() +
  facet_wrap(~zmienna, scales = "free", ncol = 6) +
  scale_x_reordered() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Porównanie województw w podziale na zmienne",
       subtitle = "Posortowane od największej do najmniejszej wartości",
       x = "Województwo",
       y = "Wartość zmiennej") +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# ==============================================================================
# HISTOGRAMY WEDŁUG ASYMETRII
# ==============================================================================

dane_wykresy <- dane %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  left_join(
    statystyki %>% select(Zmienna, Klasa_Asymetrii, Klasa_Zmiennosci, Wsp_Zm_Proc, Skosnosc), 
    by = "Zmienna"
  )

# --- GRUPA 1: SILNA ASYMETRIA LEWOSTRONNA ---
dane_wykresy %>%
  filter(Klasa_Asymetrii == "silna lewostronna") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#ef5350", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Grupa 3: Silna asymetria lewostronna (Skośność < 1.2)",
    x = "Wartość", y = "Liczba wystąpień"
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


# ==============================================================================
# HISTOGRAMY WEDŁUG ZMIENNOŚCI
# ==============================================================================

dane_wykresy <- dane %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  left_join(
    statystyki %>% select(Zmienna, Klasa_Asymetrii, Klasa_Zmiennosci, Wsp_Zm_Proc, Skosnosc), 
    by = "Zmienna"
  )

# --- ZMIENNOŚĆ: BARDZO MAŁA I MAŁA (<25%) ---
dane_wykresy %>%
  filter(Klasa_Zmiennosci %in% c("mała zmienność", "bardzo małą zmienność")) %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#66bb6a", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Zmienne o umiarkowanej zmienności (Wsp. Zm. < 25%)",
    x = "Wartość", y = "Liczba wystąpień"
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


# ==============================================================================
# WYKRES SYNTETYCZNY
# ==============================================================================

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
    legend.box = "vertical"
  )


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


# ==============================================================================
# WSZYSTKIE PARY ZMIENNYCH SKORELOWANYCH > 0.7
# ==============================================================================

cor_matrix <- dane %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

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
  geom_col(show.legend = FALSE) + 
  geom_hline(yintercept = c(-0.7, 0.7), 
             linetype = "dashed", 
             color = "darkgrey", 
             linewidth = 0.6) +
  geom_text(aes(label = round(value, 2)), 
            hjust = -0.2, 
            size = 3.5, 
            fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#FF0000", "FALSE" = "#FF0000")) +
  theme_minimal() +
  labs(
    title = "Wszystkie korelacje >= 0.7",
    x = "Para zmiennych",
    y = "Współczynnik korelacji"
  ) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

