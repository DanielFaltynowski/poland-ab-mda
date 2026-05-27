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

macierz_korelacji_X <- round(cor(X),3)
macierz_korelacji_Y <- round(cor(Y),3)

par(mfrow = c(1, 2))

color_palette <- colorRampPalette(c("#E41A1C", "white", "#377EB8"))(200)

corrplot(macierz_korelacji_X, 
         method = "color",               
         col = color_palette, 
         type = "full",                  
         addCoef.col = "black",          
         number.cex = 0.7,               
         number.digits = 2,              
         tl.col = "black",               
         tl.srt = 45,                    
         tl.cex = 0.8,                   
         title = "Pełna macierz korelacji X",
         mar = c(0,0,2,0))               

corrplot(macierz_korelacji_Y, 
         method = "color", 
         col = color_palette, 
         type = "full",                  
         addCoef.col = "black", 
         number.cex = 0.7, 
         number.digits = 2,
         tl.col = "black", 
         tl.srt = 45,
         tl.cex = 0.8,                   
         title = "Pełna macierz korelacji Y",
         mar = c(0,0,2,0))

par(mfrow = c(1, 1))

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

