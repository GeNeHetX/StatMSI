# MStatI: Statistic for mass spectrometry imaging data

**Authors:** [@AudreyBeaufils](https://github.com/AudreyBeaufils) (GeNeHetX & IMAP)  
**Date:** February 2024  
**Contact:** [audrey.beaufils@inserm.fr](mailto:audrey.beaufils@inserm.fr),

## Overview
MStatI is a tool for analyzing MSI data, focusing on identifying differentially expressed peptides and annotating them with protein.<br><br>

  - **Principal Component Analysis (PCA)**<br>
  PCA is a statistical method used to explore relationships between variables in a dataset and reduce its dimensionality.<br><br>

  - **Differential Expression Analysis**<br>
  Linear model regression is a method used for identifying peptides with significantly different expression levels between sample groups.<br><br>

  - **Gene Set Enrichment Analysis (GSEA)**<br>
  GSEA is a bioinformatics method used here to annotate differential expressed peptides with protein <br><br>



## Prerequisites : 
MStatI requires the R language (at least version 4.0).<br>
If R is installed, you can launch the application directly via a command terminal or work on Rstudio.

- install R: [download](https://cran.r-project.org/)

- You can find Rstudio here : [download](https://posit.co/download/rstudio-desktop/)
<br>


## Installation :

1- If you are a git user, clone the MStatI folder, otherwise download the MStatI code zip via the green "<>Code" button.

```bash
  git clone https://github.com/GeNeHetX/StatMSI.git
```

2- Open an R terminal or Rstudio where the MStatI codes are stored
     
- First-time use MStatI
```R
   install.packages(c("shiny", "DT","shinydashboard","shinycssloaders","BiocManager", "ggplot2", "plotly", "reshape2", "factoextra", "FactoMineR", "devtools", "ggupset", "Cardinal","ggpubr","uwot","nortest"))
    if (!require("BiocManager", quietly = TRUE))
      install.packages("BiocManager")

    BiocManager::install(c("Cardinal","ComplexHeatmap","UpSetR"))
  shiny::runApp()
```
or

- Use MStatI 
```R
  shiny::runApp()
```
<br>

## Upload data:

### IMZML & IBD : 
You have to first copy your file in the same folder than the shiny application, where are ui.R and server.R 
You have to upload the IMZML and IBD file already processed ( normalization + baseline removal + peak picking). The name of IMZML and IBD files must be the same for a chosen sample. 
Additionnaly the name must follow the following pattern, id_group - id_replicat - ... .imzml/ibd
<br>

### Example :
<ul>
  <li>Control group with 2 replicats :</li> 
  <ul>
    <li>Replicat n°1 :ctrl-1-total ion count.ibd  ; ctrl-1-total ion count.imzML </li>
    <li> Replicat n°2 : ctrl-2-total ion count.ibd ;ctrl-2-total ion count.imzML </li>
  </ul>
<br/> <br/>
  <li>Condition 1 with 3 replicat : </li> 
  <ul>
    <li> group1-1-total ion count.ibd ; group1-1-total ion count.imzML </li>
    <li> group1-2-total ion count.ibd ;group1-2-total ion count.imzML </li>
    <li> group1-3-total ion count.ibd ; group1-3-total ion count.imzML </li>
  </ul>





