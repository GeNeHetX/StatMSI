#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinycssloaders))

library(plotly)

# Define UI for application that draws a histogram

  dashboardPage(skin = "purple",
  dashboardHeader(
            
    title  = span("MStatI",img(src = "imap_logo.png", height = 40))

    

  
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home",icon=shiny::icon('home')),
      menuItem("Overview", tabName = "tab1",icon=icon('arrows-rotate')),
      menuItem("Boxplot", tabName = "tab2",icon=icon('chart-simple')),
      menuItem("PCA", tabName = "tab4",icon=icon('slack')),
      menuItem("Classification", tabName = "tab5",icon=icon('circle-nodes')),
      menuItem("Group Comparison", tabName = "tab3",icon=icon('chart-simple')),
      menuItem("Protein Identification", tabName = "tab6",icon=icon('dna'))
    )
  ),
  dashboardBody(
    tags$style(HTML("
       
     
      .main-header .logo {
        font-family: 'Georgia', Times, 'Times New Roman', serif;
        font-weight: bold;
        font-size: 24px;
      }

      .box.box-solid.box-info>.box-header {
        color:#fff;
        background:#8ACAAD
      }
      .box.box-solid.box-info{
        border-bottom-color:#8ACAAD;
        border-left-color:#8ACAAD;
        border-right-color:#8ACAAD;
        border-top-color:#8ACAAD;
      }

      .box.box-solid.box-success>.box-header {
        color:#fff;
        background:#A5C2E3;
      }
      .box.box-solid.box-primary>.box-header {
        color:#fff;
        background:#F8DA79;
      }
      .box.box-solid.box-primary{
        border-bottom-color:#F8DA79;
        border-left-color:#F8DA79;
        border-right-color:#F8DA79;
        border-top-color:#F8DA79;
      }

      .box.box-solid.box-success{
        border-bottom-color:#A5C2E3;
        border-left-color:#A5C2E3;
        border-right-color:#A5C2E3;
        border-top-color:#A5C2E3;
      }

      .nav-tabs-custom>.nav-tabs {
      margin: 0;
      border-bottom-color: #f4f4f4;
      border-top-right-radius: 3px;
      border-top-left-radius: 3px;
      background: #ddd;
      }
      
      .progress-bar {
        background-color: #A5C2E3;
      }
              
      .btn-default {
        background-color: #A5C2E3;
        color: #fff;
        border-color: #ddd;
      }

      .nav-tabs-custom>.nav-tabs>li.active {
        border-top-color: #1D2C4C;
      }
      .skin-purple .main-header .navbar {
        background-color: #1D2C4C;
      }

      .skin-purple .main-header .logo {
      background-color: #1D2C4C;
      color: #fff;
      border-bottom: 0 solid transparent;
      }
      skin-purple .sidebar-menu>li.active>a, .skin-purple .sidebar-menu>li:hover>a {
    color: #fff;
    background: #1e282c;
    border-left-color: #1D2C4C;
}

              
    ")),     

    tabItems(
      tabItem(tabName ="home",
        fluidRow(         
          box(width = 12, title = h2('Presentation', icon('display')), status = 'success', solidHeader = TRUE, collapsible = TRUE,
            fluidRow(
              column(width = 12,
               strong("Author :"), "Audrey Beaufils(GeNeHetX & IMAP )", br(),
               strong("Date :"), "February 2024", br(),
               strong("Contact :"), "audrey1.beaufils@inserm.fr", br(),
               strong("GitHub :"), a("GeNeHetX", href = "https://github.com/GeNeHetX/"), br(), br()
              )
            )
          ),
          box( width = 12, title =h2("File input",icon("upload")),status = "success",solidHeader=TRUE,
            fluidRow(
                column(width = 12,             
                        strong("IMZML & IBD : "),
                       "You have to upload the IMZML and IBD file already processed, the name of IMZML and IBD file must be the same for a chosen sample. 
                       Additionnaly the name must follow the following pattern, id_group - id_replicat - ... .imzml/ibd", br(),
                       strong("Example : "),
                       HTML("<ul><li>Control group with 2 replicats :</li> <ul>
                         <li>Replicat n°1 :ctrl-1-total ion count.ibd  ; ctrl-1-total ion count.imzML </li>
                          <li> Replicat n°2 : ctrl-2-total ion count.ibd ;ctrl-2-total ion count.imzML </li>
                          </ul>
                          <br/> <br/>


                        <li>Condition 1 with 3 replicat : </li> <ul>
                        <li> group1-1-total ion count.ibd ; group1-1-total ion count.imzML </li>
                        <li> group1-2-total ion count.ibd ;group1-2-total ion count.imzML </li>
                        <li> group1-3-total ion count.ibd ; group1-3-total ion count.imzML </li>
                        </ul>
                      ")), 


)
            ),
          box( width = 12, title =h2("Boxplot",icon('chart-simple')),status = "success",solidHeader=TRUE,
            fluidRow(
                column(width = 12,    
                "This page is used to be sure, here was no problem with your data. Check your mean intensity by condition/replicat",br(),
                strong("Upset Plot:"),
                "Interpretation: at the left you see a barplot, this barplot represent the number of peak/peptide present in each condition,
                at the top you see the number of peaks present in several condition"         
                       
                      )), 



            ),
          box(width = 12, title = h2('Principal Component Analysis (PCA)', icon('slack')), status = 'success', solidHeader = TRUE, collapsible = TRUE,
              fluidRow(
                column(width = 12,                 
                       "Principal Component Analysis (PCA) is a statistical method used to explore and summarize the relationships between variables in a multivariate dataset. 
                       Its main objective is to transform a set of correlated variables into a set of uncorrelated variables, called principal components. 
                       These components are ordered based on the importance of their contribution to the total variability of the data. 
                       PCA is often used to reduce the dimensionality of data by selecting the most significant principal components. 
                       This allows for visualizing the data in a lower-dimensional space, making interpretation and analysis easier.", br(),br(),
                            
                      strong("Tips:"),"PCA reduces the dimensions of your data. These compact dimensions will group your samples by similarity. The idea here is to see if your samples 
                      separate according to a new dimension. If so, you'll have the contribution value of each sample and gene in the new dimension and know which ones contribute most to your dimension.", align="justify"),br(),br(),
                        column(width = 12, align = "center",
                        imageOutput("pca_Image"))), 

                        tags$details(
                            tags$summary(strong("Click for more information")),

                      column(width = 12, 
                            "PCA is based on fundamental mathematical concepts such as the eigenvalue and eigenvector decomposition of a covariance matrix. 
                            Firstly, the covariance matrix is calculated from the input data, representing the correlation relationships between the different variables. 
                            Next, the eigenvalue decomposition of this matrix yields the principal components, which are the directions in which the data vary most. 
                            The eigenvectors associated with these eigenvalues determine the linear combinations of the original variables that form the principal components. 
                            Finally, the data are projected into the space defined by these principal components, thus reducing dimensionality while preserving maximum variance.",br(), 
                            HTML("<p>The decomposition of the covariance matrix Σ is done as follows:</p>
                         <p>Σ = (1/(n-1)) * (X - X̄)^T * (X - X̄)</p>
                         <p>Where X is the centered data matrix, X̄ is the vector of means of each variable, and n is the number of observations. The eigenvalues λ_i and eigenvectors v_i are obtained by solving the characteristic equation:</p>
                         <p>Σv_i = λ_i * v_i</p>
                         <p>The principal components are then calculated as linear combinations of the original variables:</p>
                         <p>Y = X * V</p>
                         <p>Where Y is the matrix of principal components and V is the matrix whose columns are the corresponding eigenvectors v_i.</p>
                         "), align="justify")) 
                                            
                        ),
          box( width = 12, title =h2("Classification",icon('circle-nodes')),status = "success",solidHeader=TRUE,
            fluidRow(column(width=12,"For more information please refers to : ",br(),
              "Bemis, K. D., Harry, A., Eberlin, L. S., Ferreira, C. R., van de Ven, S. M., Mallick, P., et al. (2016).",br(),
              "Probabilistic Segmentation of Mass Spectrometry (MS) Images Helps Select Important Ions and Characterize Confidence in the Resulting Segments. Molecular & Cellular Proteomics, 15(5), 1761–1772.",br(),
              "http://doi.org/10.1074/mcp.O115.053918"

                     
        ))
      ),
          box( width = 12, title =h2("Group comparison",icon('chart-simple')),status = "success",solidHeader=TRUE,
            fluidRow(column(width=12,"This page apply parametric/non parametric pairwise group comparison for a given mass"
                     
        ))
      ),
          box( width = 12, title =h2("Protein Identification",icon("dna")),status = "success",solidHeader=TRUE,
            fluidRow(column(width=12,
              strong("Differential Analysis"),br(),
              " This part use the meansTest() function from Cardinal package to fit linear models with the most basic summarization. The samples must be specified with samples. Each sample is summarized by its mean, and then a linear model is fit to the summaries. In this case, each sample is a separate run.

Here, we specify condition as the sole fixed effect. Internally, the model will call either lm() or lme() depending on whether any random effects are provided.",
br(),
strong("Gene Set Enrichment Analysis"),br(),
"For identify protein enriched in a condition, an enrichment test could be applied on peptide list. 
Gene Set Enrichment Analysis is a widely used method in functional enrichment analysis. It is based on a list of peptides indexed by a numeric value, such as differential analysis statistic, representing their association with a biological phenomenon of interest. These numeric values are then used as input for GSEA. The algorithm performs a statistical comparison by sorting the peptide list based on their numerical values. This sorting process allows for the identification of sets of peptides that exhibit similar order of magnitude
or enrichment patterns, which may indicate their belonging to the same protein."

                     
        ))
      ),




          )),
      tabItem(tabName = "tab1",
        fluidRow(column(width=12,box(width=NULL,status='primary',solidHeader=TRUE,title=h1("Settings",icon("cogs")),
          column(width=6,
          selectInput('org', 'Choose your species', choices = list(Human='human_protein_gsea.rds', Mouse='mouse_protein_gsea.rds'))),#, Other='oth'))
          column(width=6,
          fileInput('msi-files',"Load your imzml & ibd files", multiple=TRUE))

          ))),
        fluidRow(
          column(width=12,
            box(width=NULL, status = 'info', solidHeader = TRUE, title = h2("File uploaded", icon('table')),
              withSpinner( DT::dataTableOutput("dtFiles"), type = 8, color = "#CDCDE6", size = 1)
            )
          ),
          column(width=12,
            box(width=NULL, status = 'info', solidHeader = TRUE, title = h2("File grouped", icon('table')),
              withSpinner( DT::dataTableOutput('groupe'), type = 8, color = "#CDCDE6", size = 1)
            )
          )
        )
      ),   
     tabItem(tabName = "tab2",
        fluidRow(
          column(width=6,
            box(width=12, status = 'success', solidHeader = TRUE, title = h2("Mean Condition Boxplot", icon('chart-simple')),
              withSpinner(plotOutput('bpMeanCond'), type = 8, color = "#CDCDE6", size = 1)
            )
          ),
          column(width=6,
            box(width=12, status = 'success', solidHeader = TRUE, title = h2("Mean Replicat Boxplot", icon('chart-simple')),
              withSpinner(plotOutput('bpMeanReplicat'), type = 8, color = "#CDCDE6", size = 1)
            )
          ),
          column(width=6,
            box(width=12, status = 'success', solidHeader = TRUE, title = h2("Upset Plot", icon('chart-simple')),
              withSpinner(plotOutput('upsetPlot'), type = 8, color = "#CDCDE6", size = 1)
            )
          )
         )
      ),
      tabItem(tabName = "tab3",

        fluidRow(
          box(width = 2, status = 'primary', title = h2("Settings", icon('cogs')), solidHeader = TRUE, collapsible=TRUE,
            uiOutput('mzChoiceUI'),
            selectInput('groupes2','Please choose between experimental group and cluster',c('Group','Cluster')),
            numericInput('ts_padj_pt', "Provide pvalue threshold",min=0, max=0.1, step = 0.01,value=0.05),
            #selectInput('pixel_or_mean','Please choose if the analysis is run on : ',c('Pixel','Mean')),
            selectInput('indep_or_dep','Please choose if your sample are : ',c('Independant','Dependant'))

          ),
          box(width=10, status = 'info', solidHeader = TRUE, title = h2("Comparison test results", icon('chart-simple')),
              tableOutput('pairwiseText'),
              column(width=6,
              h2('Pairwise t-test results'),
              withSpinner(tableOutput('pairwiseTableTTest'), type = 8, color = "#CDCDE6", size = 1)),
              column(width=6,h2('Pairwise wilcoxon results'),withSpinner(tableOutput('pairwiseTableWilcox'), type = 8, color = "#CDCDE6", size = 1)) 
            
          )
        ),
        fluidRow(
          box(width=12, status = 'success', solidHeader = TRUE, title = h2("Boxplot of mean", icon('chart-simple')),
            withSpinner(plotOutput('bpPairwise'), type = 8, color = "#CDCDE6", size = 1) 
          )
        )
      ),
      tabItem(tabName = "tab4",
        fluidRow(
          tabBox(
          tabPanel("Global PCA",
            fluidRow(
          box(width = 2, status = 'primary', title = h2("Settings", icon('cogs')), solidHeader = TRUE, collapsible=TRUE,
            selectInput("dim1", label = "Choose your first dimension",
                  choices = list("Dim1" = 1, "Dim2" = 2,"Dim3" = 3, "Dim4" =4, "Dim5" = 5),selected = 1),
            selectInput("dim2", label = "Choose your second dimension",
                  choices = list( "Dim2" = 2,"Dim3" = 3, "Dim4" =4, "Dim5" = 5),selected = 2),
          ),
          box(width=5, status='success', title = h2('Individu graph of PCA',icon('chart-simple')), solidHeader = TRUE,collapsible=TRUE,
            withSpinner(plotlyOutput("pcaGroup"), type = 8, color = "#CDCDE6", size = 1)
          ),
          box(width=5, status='success', title = h2('Variable graph of PCA',icon('chart-simple')), solidHeader = TRUE,collapsible=TRUE,
            withSpinner(plotlyOutput("pcaMz"), type = 8, color = "#CDCDE6", size = 1)
          )
        )),
        tabPanel("Mean  PCA",
        fluidRow(
          box(width = 2, status = 'primary', title = h2("Settings", icon('cogs')), solidHeader = TRUE, collapsible=TRUE,
            selectInput("dim1-2", label = "Choose your first dimension",
                  choices = list("Dim1" = 1, "Dim2" = 2,"Dim3" = 3, "Dim4" =4, "Dim5" = 5),selected = 1),
            selectInput("dim2-2", label = "Choose your second dimension",
                  choices = list( "Dim2" = 2,"Dim3" = 3, "Dim4" =4, "Dim5" = 5),selected = 2)
          ),
          box(width= 10, status='success', title = h2('Individu graph of PCA (mean)',icon('chart-simple')), solidHeader = TRUE,collapsible=TRUE,
            withSpinner(plotlyOutput("pcaGroupMean"), type = 8, color = "#CDCDE6", size = 1)
          )
        )),tabPanel("Projection PCA",
        fluidRow(
          box(width = 2, status = 'primary', title = h2("Settings", icon('cogs')), solidHeader = TRUE, collapsible=TRUE,
            selectInput("pca_dim", label = "Choose your first dimension",
                  choices = list("Dim1" = 1, "Dim2" = 2,"Dim3" = 3, "Dim4" =4, "Dim5" = 5),selected = 1)
          ),
          box(width= 10, status='success', title = h2('Projection of PCA',icon('chart-simple')), solidHeader = TRUE,collapsible=TRUE,
              withSpinner(plotOutput("pcaObj"), type = 8, color = "#CDCDE6", size = 1)
          )
        )
      ),tabPanel("UMAP",
      fluidRow(
          box(width = 2, status = 'primary', title = h2("Settings", icon('cogs')), solidHeader = TRUE, collapsible=TRUE,
            selectInput("col_umap", label = "Color by ",
                  choices = list("Condition" = 'condition', "Replicat" = 'replicat'))
          ),
          box(width= 10, status='success', title = h2('UMAP visualization',icon('chart-simple')), solidHeader = TRUE,collapsible=TRUE,
              withSpinner(plotOutput("umap"), type = 8, color = "#CDCDE6", size = 1)
          )

          )

      ),       id="tabBox",    width = 12))),
      tabItem(tabName = "tab5",
        fluidRow(
          box(width = 2, status = 'primary', title = h2("Settings", icon('cogs')), solidHeader = TRUE, collapsible=TRUE,
            uiOutput('nbClustUI'),
              numericInput("sparsity", "Please provide the sparsity parameter",min=0,step=1,value=0),
              numericInput("smoothing_radius","Please provide the smoothing radius", min=0,step=1,value=0)
          ),
          box(width=5,status='success',title = h2('Projection of Classification spatial shrunken centroids (pixel)',icon('chart-simple')),solidHeader = TRUE,collapsible=TRUE,
              withSpinner(plotOutput("ssc"), type = 8, color = "#CDCDE6", size = 1)
          ),
          box(width=5,status='success',title = h2('Projection of Classification spatial shrunken centroids (m/z)',icon('chart-simple')),solidHeader = TRUE,collapsible=TRUE,
            withSpinner(plotOutput("SSCPlot"), type = 8, color = "#CDCDE6", size = 1)
          )
        )
      ),
      tabItem(tabName = "tab6",
        fluidRow(
          box(width = 2, status = 'primary', title = h2("Settings", icon('cogs')), solidHeader = TRUE, collapsible=TRUE,
            selectInput('groupes','Please choose between experimental group and cluster',c('Group','Cluster')),
            conditionalPanel(condition = "input.groupes == 'Group'",
                  uiOutput('condChoiceUI'),
                  uiOutput('condChoiceUI2'),
            ),
            conditionalPanel(condition = "input.groupes == 'Cluster'",

                uiOutput('clustChoiceUI'),
                uiOutput('clustChoiceUI2'),
            ),
            
            numericInput('ts_padj', "Provide pvalue threshold",min=0, max=0.1, step = 0.01,value=0.05)
          ),
          box(width=10, status='success', title = h2('Volcano Plot',icon('chart-simple')), solidHeader = TRUE,collapsible=TRUE,
            withSpinner(plotlyOutput("volcanoPlot"), type = 8, color = "#CDCDE6", size = 1),
          )
        ),
        fluidRow(
          column(width=6,
            box(width=12, status = 'info', solidHeader = TRUE, title = h2("Table Differential Analysis results", icon('table')),
              withSpinner(DT::dataTableOutput("tableAD"), type = 8, color = "#CDCDE6", size = 1)
            )
          ),
          column(width=6,
            box(width=12, status = 'info', solidHeader = TRUE, title = h2("Table Protein Enrichment Analysis results", icon('table')),
              withSpinner(DT::dataTableOutput("resEnrichment"), type = 8, color = "#CDCDE6", size = 1)
            )
          )
        )
      )
    )
  )
)

