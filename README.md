# Sequence Alignment Visualization

## Description
This package enhances the sequence alignment workflow of 
Biostrings package by providing alignment visualization using ggplot.

## Dependency

- Biostrings
- ggplot2


## Installation

```R
library(devtools)
install_github("hannzhang/align")
```

## Example
```{R}
library(Biostrings)
library(ggplot2)
library(align)
align("ACTCGCAATATGVTAGGVVAG", "ACTTTATGCTATGCGC")
```

This will produce the following plot.

![](plot.png)

