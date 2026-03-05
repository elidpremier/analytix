# analytix News

## Version 0.0.0.9000 (Development)

### New Features
- Initial release of `analytix` package
- Comprehensive suite of descriptive analysis functions
- Professional table generation with `flextable`
- Word document export functionality
- Full French language support

### Core Functions
- **Univariate Analysis**
  - `descr_categorial()` - Frequency analysis for categorical variables
  - `descr_numeric()` - Descriptive statistics for numeric variables
  - `analyse_descriptive_multiple()` - Automated multi-variable analysis
  
- **Bivariate Analysis**
  - `cross_table_uniq_mod()` - Cross-tabulation with statistical tests
  - `cross_multi()` - Multiple variable cross-tabulation
  - `descr_by_group()` - Grouped descriptive statistics
  
- **Data Preparation & Utilities**
  - `quick_code()` - Quick variable recoding
  - `categorize_numeric()` - Convert numeric to categorical
  - `collapse_categories()` - Collapse multiple categories
  - `missing_report()` - Missing data summary
  
- **Visualization & Export**
  - `plot_distribution()` - Automatic distribution plots
  - `theme_analytique()` - Professional table styling
  - `export_to_word()` - Export results to Word document

### Bug Fixes
- Fixed documentation inconsistencies between function names and examples
- Corrected Rd file parsing errors

### Documentation
- Comprehensive README with feature overview
- Complete function examples in documentation
- French-language support throughout

### Known Limitations
- Non-ASCII characters in R code (accepted in comments)
- Some functions rely on `library()` calls instead of `::`
- Package not yet on CRAN

### Next Steps
- Expand test coverage
- Add more visualization options
- Performance optimization for large datasets
- Vignette creation with advanced workflows
