# analytix

Outils d’analyse descriptive pour la génération de tableaux professionnels avec `flextable`.

## Installation

```r
# Installer devtools si nécessaire
install.packages("devtools")

# Installer analytix depuis GitHub
devtools::install_github("elidpremier/analytix")
# Exemple d'usage
library(analytix)
freq_table(iris, Species)
