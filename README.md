# MStatI: Statistic for mass spectrometry imaging data

**Authors:** [@AudreyBeaufils](https://github.com/AudreyBeaufils) (GeNeHetX & IMAP)  
**Date:** February 2024  
**Contact:** [audrey1.beaufils@inserm.fr](mailto:audrey1.beaufils@inserm.fr),

## Overview
MStatI is a tool for analyzing MSI data, focusing on identifying differentially expressed peptides and annotating them with protein.<br><br>

  - **Principal Component Analysis (PCA)**<br>
  PCA is a statistical method used to explore relationships between variables in a dataset and reduce its dimensionality.<br><br>

  - **Differential Expression Analysis by DESeq2**<br>
  Linear model regression is a method used for identifying peptides with significantly different expression levels between sample groups.<br><br>

  - **Gene Set Enrichment Analysis (GSEA)**<br>
  GSEA is a bioinformatics method used here to annotate differential expressed peptides with protein <br><br>



## Prerequisites : 
MStatI requires the R language (at least version 4.0).<br>
If R is installed, you can launch the application directly via a command terminal or work on Rstudio.

- install R: [download](https://cran.r-project.org/)

- You can find Rstudio here : [download](https://posit.co/download/rstudio-desktop/)
<br>


## Installation 

With internet : 

1 - First-time use MStatI, run this command in a R terminal
```R
    install.packages(c("shiny", "DT","shinydashboard","shinycssloaders","BiocManager", "ggplot2", "plotly", "reshape2", "factoextra", "FactoMineR", "devtools", "ggupset", "Cardinal","ggpubr","uwot"))
```

2- Then, run this command :
```R
   shiny::runGitHub('StatMSI', 'GeNeHetX', subdir='MStatI' ,ref='main')
```
ps : you can precise the version thanks to ref='', for example : ref='v.1.0.0'
___________________________________________________

Without internet, (use just to download and run the following commands without a connection) : 

1- If you are a git user, clone the MStatI folder, otherwise download the MStatI code zip via the green "<>Code" button.

```bash
  git clone https://github.com/GeNeHetX/StatMSI.git
```

2- Open an R terminal or Rstudio where the MStatI codes are stored
     
- First-time use MStatI
```R
   install.packages(c("shiny", "DT","shinydashboard","shinycssloaders","BiocManager", "ggplot2", "plotly", "reshape2", "factoextra", "FactoMineR", "devtools", "ggupset", "Cardinal","ggpubr","uwot"))
  shiny::runApp()
```
or

- Use MStatI 
```R
  shiny::runApp()
```
<br>


