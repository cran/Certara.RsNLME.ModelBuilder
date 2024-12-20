# RsNLME Model Builder <img src='vignettes/img/ModelBuilder.png' align="right" style = "float:right; height: 150px;" alt="ModelBuilder package logo."/>


## Overview

`Certara.RsNLME.ModelBuilder` is an R package and Shiny application used to build an RsNLME model.

Use the GUI to select from various model building options and observe the PML update in real time. Additionally, users may generate the corresponding RsNLME code to learn reproduce the model object from R.

## Installation

### Windows

```r
install.packages("Certara.RsNLME.ModelBuilder",
                 repos = c("https://certara.jfrog.io/artifactory/certara-cran-release-public/",
                           "https://cloud.r-project.org"),
                 method = "libcurl")

```

### Linux

```r
install.packages("Certara.RsNLME.ModelBuilder",
                 repos = c("https://certara.jfrog.io/artifactory/certara-cran-release-public/",
                           "https://cloud.r-project.org"))

```

## Usage

Use the built in data object from the `Certara.RsNLME` package to explore functionality inside `Certara.RsNLME.ModelBuilder`.

```r
library(Certara.RsNLME.ModelBuilder)
library(Certara.RsNLME)

model <- modelBuilderUI(data = pkData)

```
