# Contributing to analytix

Thank you for your interest in contributing to `analytix`! This document provides guidelines and instructions for contributing.

## Code of Conduct

- Be respectful and constructive in all interactions
- Provide detailed, helpful feedback
- Focus on the code, not the person
- Help create a welcoming environment for all contributors

## Getting Started

### Prerequisites
- R >= 4.0
- RStudio or another R IDE (optional but recommended)
- Git

### Setup
1. Fork the repository
2. Clone your fork: `git clone https://github.com/YOUR_USERNAME/analytix.git`
3. Create a new branch: `git checkout -b feature/your-feature-name`
4. Install development dependencies: `devtools::install_deps(dependencies = TRUE)`

## Development Workflow

### Before You Start
- Check existing issues to avoid duplicate work
- Discuss significant changes by opening an issue first
- Ensure your feature aligns with the package's goals (French-language analytical tools)

### Code Standards
- Follow the existing code style (use `styler` package)
- Write functions that are clear and well-documented
- Use roxygen2 comments for documentation
- Add examples to function documentation
- Test your code thoroughly

### Writing Tests
- Add tests for new functions in `tests/testthat/`
- Use test file naming: `test_function_name.R`
- Aim for meaningful test coverage
- Run tests: `devtools::test()`

### Documentation
- Use roxygen2 for function documentation
- Include `@param`, `@return`, `@examples` tags
- Write examples that are runnable and demonstrate the function's purpose
- Keep documentation in French for consistency

### Code Review
- Explain changes clearly in your PR description
- Be open to feedback and suggestions
- Make requested changes in follow-up commits
- Ensure CI/CD checks pass

## Submitting Changes

### Pull Requests
1. Push your branch to your fork
2. Create a Pull Request with a clear title and description
3. Reference any related issues (e.g., "Fixes #123")
4. Ensure all tests pass
5. Respond to review comments

### Commit Messages
- Use clear, descriptive commit messages
- Start with a verb: "Add", "Fix", "Improve", etc.
- Example: "Add support for numeric categorization in descr_categorial"

## Reporting Bugs

### How to Report
1. Check if the bug is already reported
2. Create a new issue with:
   - Clear title describing the bug
   - Description of expected vs. actual behavior
   - Reproducible example (minimal code)
   - Your R version and operating system
   - Any error messages (complete stack trace)

### Example Bug Report
```
**Title:** descr_numeric fails with NA values

**Describe the bug:**
When analyzing a numeric variable with NA values, descr_numeric throws an error.

**To Reproduce:**
```r
data <- data.frame(x = c(1, 2, NA, 4, 5))
descr_numeric(data, x)
```

**Expected behavior:**
Should return statistics ignoring NAs.

**Actual behavior:**
Error: ...
```

## Proposing Features

### How to Propose
1. Open an issue with the title "Feature Request: [description]"
2. Describe the feature and its use case
3. Provide examples of how it would be used
4. Discuss implementation approach (optional)

### Example Feature Request
```
**Title:** Feature Request: Support for weighted frequencies

**Description:**
Allow descr_categorial to compute weighted frequencies using a weight variable.

**Use Case:**
Survey data with sampling weights requires weighted frequency analysis.

**Example Usage:**
descr_categorial(data, var, weights = "weight_var")
```

## Project Structure

```
analytix/
├── R/                          # Source code
│   ├── descr_categorial.R
│   ├── descr_numeric.R
│   └── ...
├── man/                        # Roxygen documentation
│   ├── descr_categorial.Rd
│   ├── descr_numeric.Rd
│   └── ...
├── tests/
│   ├── testthat.R
│   └── testthat/              # Unit tests
│       ├── test_descr_numeric.R
│       └── ...
├── DESCRIPTION                # Package metadata
├── NAMESPACE                  # Exported functions
├── README.md                  # Package documentation
└── NEWS.md                    # Changelog
```

## Build and Test Commands

```r
# Install development version
devtools::install()

# Run tests
devtools::test()

# Check package
devtools::check()

# Build documentation
devtools::document()

# Check for code style
styler::style_dir("R")
```

## Questions?

- Ask in the issue discussions
- Create a new issue for questions
- Check existing documentation and issues first

## License

By contributing to analytix, you agree that your contributions will be licensed under the MIT License.

---

Thank you for contributing to making `analytix` better! 🙏
