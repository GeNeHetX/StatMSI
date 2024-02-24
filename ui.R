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
            
    title = "MStatI"

  
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home",icon=shiny::icon('home')),
      fileInput('msi-files',"Load your imzml & ibd files", multiple=TRUE),
      menuItem("Overview", tabName = "tab1",icon=icon('arrows-rotate')),
      menuItem("Boxplot", tabName = "tab2",icon=icon('arrows-rotate')),
      menuItem("Group Comparison", tabName = "tab3",icon=icon('arrows-rotate')),
       menuItem("PCA", tabName = "tab4",icon=icon('arrows-rotate')),
      menuItem("Classification", tabName = "tab5",icon=icon('arrows-rotate')),
      menuItem("Differential Analysis", tabName = "tab6",icon=icon('arrows-rotate'))
      
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
        background:#262686
      }
      .box.box-solid.box-info{
        border-bottom-color:#262686;
        border-left-color:#262686;
        border-right-color:#262686;
        border-top-color:#262686;
      }

      .box.box-solid.box-primary>.box-header {
        color:#fff;
        background:#AAAAAA
      }
      .box.box-solid.box-primary{
        border-bottom-color:#AAAAAA;
        border-left-color:#AAAAAA;
        border-right-color:#AAAAAA;
        border-top-color:#AAAAAA;
      }

      .box.box-solid.box-success>.box-header {
        color:#fff;
        background:#CDCDE6;
      }

      .box.box-solid.box-success{
        border-bottom-color:#CDCDE6;
        border-left-color:#CDCDE6;
        border-right-color:#CDCDE6;
        border-top-color:#CDCDE6;
      }
      
      
      .progress-bar {
        background-color: #777;
      }
      .nav-tabs-custom>.nav-tabs>li.active {
        border-top-color: #262686;
      }
              
    ")),     

    tabItems(
      tabItem(tabName ="home",
        fluidRow(         
          box(width = 12, title = h2('Presentation', icon('display')), status = 'success', solidHeader = TRUE, collapsible = TRUE,
            fluidRow(
              column(width = 12,
               strong("Authors :"), "Audrey Beaufils(GeNeHetX & IMAP )", br(),
               strong("Date :"), "February 2024", br(),
               strong("Contact :"), "audrey1.beaufils@inserm.fr", br(),
               strong("GitHub :"), a("GeNeHetX", href = "https://github.com/GeNeHetX/"), br(), br()
              )
            )
          )
        )
      ),
      tabItem(tabName = "tab1",
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
            selectInput('pixel_or_mean','Please choose if the analysis is run on : ',c('Pixel','Mean')),
            selectInput('indep_or_dep','Please choose if your sample are : ',c('Dependant','Independant'))

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

