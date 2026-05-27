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

