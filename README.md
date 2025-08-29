# cmAnalysis

![R-CMD-check](https://github.com/joshageman/cmAnalysis/actions/workflows/R-CMD-check.yaml/badge.svg)
![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)
![CRAN status](https://www.r-pkg.org/badges/version/cmAnalysis)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/cmAnalysis)

Analysis and visualization of concept mapping data in R.

---

## Overview

`cmAnalysis` provides a complete workflow for the analysis of **concept mapping data**. This package supports:

- **Hierarchical clustering** of participant-sort data
- **Multidimensional scaling (MDS)** to map statements in 2D space
- **Visualization tools** such as cluster maps, dendrograms, and cross-cluster diagrams
- **Tools for managing and summarizing statements and clusters**

Concept mapping is a participatory research method used in fields such as education, public health, and policy evaluation.

---

## Installation

Install the latest stable version from CRAN:

```r
install.packages("cmAnalysis")
```

Or install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("JosHageman/cmAnalysis")
```

---

## Example

```r
library(cmAnalysis)

# Assume you have participant sorting data
data <- read.csv("sortMatrix.csv")

# Run the concept mapping analysis
result <- conceptMapping(data)

# Visualize the MDS and cluster solution
plot(result)

# Generate a statement overview
overview <- createStatementOverview(result)
```

More examples are provided in the help files and vignettes.

---

## Key Functions

| Function                   | Description                                         |
|----------------------------|-----------------------------------------------------|
| `conceptMapping()`         | Performs the full concept mapping analysis          |
| `plot.conceptMap()`        | Visualizes MDS configuration and cluster boundaries |
| `crossClusterMap()`        | Displays overlap or proximity between clusters      |
| `createStatementOverview()`| Creates a tidy overview of statements               |
| `abbreviateStatements()`   | Abbreviates long labels for better plotting         |
| `numberOfSorters()`        | Reports the number of sorters in the dataset        |

---

## Documentation

- 📦 CRAN: <https://cran.r-project.org/package=cmAnalysis>
- 🔍 In R: `?conceptMapping`, `?plot.conceptMap`, or browse `help(package = "cmAnalysis")`
- 📖 Vignettes: Run `vignette("cmAnalysis")` (if available)

---

## Authors

- **Jos Hageman** – Author and maintainer
- **Jarl Kampen** – Co-author

License: **GPL-3**

---

## Citation

If you use this package, please cite it as:

```
Hageman, J. & Kampen, J. (2025). cmAnalysis: Analysis of Concept Mapping Data in R. R package version 1.0.0. https://CRAN.R-project.org/package=cmAnalysis
```

---

## Contributing

Pull requests and issue reports are welcome. Please use the GitHub issues page to report bugs or suggest improvements.
