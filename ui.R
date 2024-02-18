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
            
    title = "StatMSI"

  
),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home",icon=shiny::icon('home')),
     
      menuItem("Analysis", tabName = "tab1",icon=icon('arrows-rotate'))
      
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
        background-color: #CDCDE6;
      }
              
      .btn-default {
        background-color: #CDCDE6;
        color: #262686;
        border-color: #ddd;
      }

      .nav-tabs-custom>.nav-tabs>li.active {
        border-top-color: #262686;
      }
              
    ")),     

     tabItems(
  tabItem(tabName ="home",

          fluidRow(
                   
                      
          box(width = 12, title = h1('Presentation', icon('display')), status = 'success', solidHeader = TRUE, collapsible = TRUE,
              fluidRow(
                column(width = 12,
                       strong("Authors :"), "Audrey Beaufils(GeNeHetX)", br(),
                       strong("Date :"), "February 2024", br(),
                       strong("Contact :"), "audrey1.beaufils@inserm.fr", br(),
                       strong("GitHub :"), a("GeNeHetX", href = "https://github.com/GeNeHetX/"), br(), br()

       
        )))
          

    )),
    tabItem(tabName = "tab1",
        box(width = 12, status = 'info', title = h1("Settings", icon('cogs')), solidHeader = TRUE, collapsible=TRUE,
          column(width=3,
        fileInput('msi-files',"Load your imzml files", multiple=TRUE))
          #,        numericInput('nb_group',"Enter the number of groupe",2, min=2,step=1)
        ),
        fluidRow(
        tabBox(
    tabPanel("Overwiew",
      withSpinner( DT::dataTableOutput("dtFiles"), type = 8, color = "#CDCDE6", size = 1)
      #tableOutput('groupe'),
    
        ),
     
        
    tabPanel("Boxplot",
        fluidRow(

        column(width=6,
        box(width=12, status = 'success', solidHeader = TRUE, title = h3("Mean Condition Boxplot", icon('chart-simple')),
          withSpinner(plotOutput('bpMeanCond'), type = 8, color = "#CDCDE6", size = 1)
        )
        ),
        column(width=6,
        box(width=12, status = 'success', solidHeader = TRUE, title = h3("Mean Replicat Boxplot", icon('chart-simple')),
            
          withSpinner(plotOutput('bpMeanReplicat'), type = 8, color = "#CDCDE6", size = 1)
        )
        ),
        column(width=6,
        box(width=12, status = 'success', solidHeader = TRUE, title = h3("Upset Plot", icon('chart-simple')),
            
          withSpinner(plotOutput('upsetPlot'), type = 8, color = "#CDCDE6", size = 1)
        )
        )

      
    )
        ),
    tabPanel(" Group comparison",
      
      
      fluidRow(

        box(width=12, status = 'success', solidHeader = TRUE, title = h3("Comparison test results", icon('chart-simple')),
           column(width=3,
        uiOutput('mzChoiceUI')),
           column(width=9,
          tableOutput('pairwiseText'),
          withSpinner(tableOutput('pairwiseTable'), type = 8, color = "#CDCDE6", size = 1),
          withSpinner(plotOutput('bpPairwise'), type = 8, color = "#CDCDE6", size = 1)

          )
        )
        ),
        

      
    


    ),
    tabPanel("PCA",
      fluidRow(
        box(width=12, status='success', title = h2('Graph of PCA',icon('chart-simple')), solidHeader = TRUE,collapsible=TRUE,
          column(width=2,
            selectInput("dim1", label = "Choose your first dimension",
                choices = list("Dim1" = 1, "Dim2" = 2,"Dim3" = 3, "Dim4" =4, "Dim5" = 5),selected = 1),
            selectInput("dim2", label = "Choose your second dimension",
                choices = list( "Dim2" = 2,"Dim3" = 3, "Dim4" =4, "Dim5" = 5),selected = 2),
          ),
          column(width=5, 
            withSpinner(plotlyOutput("pcaGroup"), type = 8, color = "#CDCDE6", size = 1)
          ),
          column(width=5,
            withSpinner(plotlyOutput("pcaMz"), type = 8, color = "#CDCDE6", size = 1)
          ) 
        ),
        box(width=12,status='success',title = h2('Graph of PCA on mean groups',icon('chart-simple')),solidHeader = TRUE,collapsible=TRUE,
          column(width=2,
            selectInput("dim1-2", label = "Choose your first dimension",
                choices = list("Dim1" = 1, "Dim2" = 2,"Dim3" = 3, "Dim4" =4, "Dim5" = 5),selected = 1),
            selectInput("dim2-2", label = "Choose your second dimension",
                choices = list( "Dim2" = 2,"Dim3" = 3, "Dim4" =4, "Dim5" = 5),selected = 2),

          ),
          column(width=10, 
            withSpinner(plotlyOutput("pcaGroupMean"), type = 8, color = "#CDCDE6", size = 1)
          )
        ),
        box(width=12,status='success',title = h2('Projection of PCA',icon('chart-simple')),solidHeader = TRUE,collapsible=TRUE,
          column(width=2,
            selectInput("pca_dim", label = "Choose your first dimension",
                choices = list("Dim1" = 1, "Dim2" = 2,"Dim3" = 3, "Dim4" =4, "Dim5" = 5),selected = 1)
          ),
          column(width=10, 
            withSpinner(plotOutput("pcaObj"), type = 8, color = "#CDCDE6", size = 1)
          )
        )
      )
    ),
    tabPanel('Classification',
      fluidRow(
      box(width=12,status='success',title = h2('Projection of Classification spatial shrunken centroids',icon('chart-simple')),solidHeader = TRUE,collapsible=TRUE,
          column(width=2,
            uiOutput('nbClustUI'),
            numericInput("sparsity", "Please provide the sparsity parameter",min=0,step=1,value=0),
            numericInput("smoothing_radius","Please provide the smoothing radius", min=0,step=1,value=0)
          ),
          column(width=10, 
            withSpinner(plotOutput("ssc"), type = 8, color = "#CDCDE6", size = 1),
            withSpinner(plotOutput("SSCPlot"), type = 8, color = "#CDCDE6", size = 1)
          )
        )
      )),
    tabPanel("Differential Analysis" ,
      fluidRow(
        box(width=12,status='success',title = h2('Graph of PCA on mean groups',icon('chart-simple')),solidHeader = TRUE,collapsible=TRUE,
          column(width=2,
            uiOutput('condChoiceUI'),
            uiOutput('condChoiceUI2'),
            numericInput('ts_padj', "Provide pvalue threshold",min=0, max=0.1, step = 0.01,value=0.05)

          ),
          column(width=10, 
            withSpinner(DT::dataTableOutput("tableAD"), type = 8, color = "#CDCDE6", size = 1),
            withSpinner(plotlyOutput("volcanoPlot"), type = 8, color = "#CDCDE6", size = 1)
          )
        ))

      ),id="tabBox",width = 12

    )
  )))))
