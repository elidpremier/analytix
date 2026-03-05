# Getting Started with analytix

## Installation

```r
# Install from GitHub
devtools::install_github("elidpremier/analytix")

# Load the package
library(analytix)
```

## Quick Start (5 minutes)

### 1. Analyze a Single Variable

```r
# Categorical variable
freq_table <- descr_categorial(iris, Species)
freq_table$flextable  # View the formatted table

# Numeric variable
num_table <- descr_numeric(iris, Sepal.Length, digits = 2)
num_table$flextable
```

### 2. Create a Cross-Tabulation

```r
# Compare two categorical variables
cross <- cross_table_uniq_mod(
  mtcars, 
  am,           # outcome variable (transmission)
  cyl,          # predictor variable (cylinders)
  var1_name = "Transmission",
  var2_name = "Cylinders",
  pct = "row"   # row percentages
)
cross
```

### 3. Export to Word

```r
# Create multiple analyses
tab1 <- descr_categorial(iris, Species, var_name = "Species")
tab2 <- descr_numeric(iris, Sepal.Length, var_name = "Sepal Length")
tab3 <- cross_table_uniq_mod(mtcars, am, cyl)

# Export to a professional Word document
export_to_word(
  tab1, tab2, tab3,
  path = "my_report.docx"
)
```

## Common Tasks

### Prepare Data: Quick Recoding

```r
# Add variable with simple recoding
data <- data.frame(
  sex = c("M", "F", "M", NA),
  age = c(25, 30, 45, 50)
)

# Recode quickly
recoded <- quick_code(
  data, 
  sex,
  "M" = "Male",
  "F" = "Female",
  .na = "Unknown"
)
```

### Prepare Data: Collapse Categories

```r
# Simplify a variable with many categories
simplified <- collapse_categories(
  iris,
  Species,
  groups = list(
    "Setosa" = "setosa",
    "Other" = c("versicolor", "virginica")
  )
)
```

### Check Data Quality

```r
# Get missing data summary
missing <- missing_report(airquality)
missing$flextable
```

### Convert Numeric to Categorical

```r
# Create age groups from numeric age
categorized <- categorize_numeric(
  data = mtcars,
  var = wt,
  breaks = c(0, 2.5, 3.5, 5.5),
  labels = c("Light", "Medium", "Heavy")
)
```

### Visualize Results

```r
# Create plots from analysis results
result <- descr_categorial(iris, Species)
plot_distribution(result)

# Works with numeric too
num_result <- descr_numeric(iris, Sepal.Length)
plot_distribution(num_result)
```

## Complete Workflow Example

```r
library(analytix)
library(dplyr)

# Load and prepare data
data <- mtcars %>%
  mutate(
    transmission = case_when(
      am == 0 ~ "Automatic",
      am == 1 ~ "Manual"
    ),
    carb_group = categorize_numeric(
      data = .,
      var = carb,
      breaks = c(0, 2, 4, 8),
      labels = c("Few", "Some", "Many")
    )
  )

# Univariate analyses
freq_trans <- descr_categorial(
  data, transmission, 
  var_name = "Transmission Type"
)

stats_mpg <- descr_numeric(
  data, mpg,
  var_name = "Fuel Consumption (MPG)",
  show_skewness = TRUE
)

# Bivariate analysis
cross_trans_cyl <- cross_table_uniq_mod(
  data, transmission, cyl,
  var1_name = "Transmission",
  var2_name = "Cylinders",
  pct = "col",
  test = "chisq"
)

# Export all results
export_to_word(
  freq_trans, stats_mpg, cross_trans_cyl,
  path = "mtcars_analysis.docx"
)

# Visualize
plot_distribution(freq_trans)
plot_distribution(stats_mpg)
```

## Tips & Tricks

### Customize Variable Names
All functions accept a `var_name` parameter:
```r
descr_numeric(iris, Sepal.Length, var_name = "Sepal Length (cm)")
```

### Control Decimal Places
Use the `digits` parameter:
```r
descr_numeric(iris, Sepal.Length, digits = 1)  # 1 decimal place
```

### Handle Missing Values
Most functions support missing data handling:
```r
descr_categorial(
  data, var,
  include_na = TRUE,      # Include NAs in counts
  na_label = "Not Stated"  # Custom NA label
)
```

### Batch Analysis
Analyze multiple variables at once:
```r
analyse_descriptive_multiple(
  iris,
  vars = c("Sepal.Length", "Sepal.Width", "Species"),
  var_labels = c(
    "Sepal.Length" = "Sepal Length (cm)",
    "Sepal.Width" = "Sepal Width (cm)",
    "Species" = "Flower Species"
  )
)
```

## Getting Help

- **Function documentation:** `?descr_categorial`, `?export_to_word`, etc.
- **Package documentation:** See README.md
- **Examples:** Check function roxygen2 examples
- **Issues:** Open an issue on GitHub for bugs or feature requests

## Next Steps

- Read the full [README](README.md) for detailed function descriptions
- Check out [CONTRIBUTING.md](CONTRIBUTING.md) if interested in development
- Review specific functions: `?descr_categorial`, `?cross_table_uniq_mod`, `?export_to_word`
