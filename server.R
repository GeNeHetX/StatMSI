#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
packages <- c("shiny", "DT","shinydashboard","shinycssloaders","BiocManager", "ggplot2", "plotly", "reshape2", "factoextra", "FactoMineR", "devtools", "ggupset", "Cardinal","ggpubr")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
if (!require("BiocManager", quietly = TRUE) && "BiocManager" %in% new_packages)
  install.packages("BiocManager")
BiocManager::install(new_packages,update=FALSE)

options(shiny.maxRequestSize=100000*1024^2)
library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(ggpubr)
library(factoextra)
library(FactoMineR)
library(ggupset)
library(Cardinal)

# Define server logic required to draw a histogram
function(input, output, session) {
# HOME Page



# Overview
filesInput <- reactive({
 req(input$'msi-files')
 data = input$'msi-files'
extension = sapply(input$'msi-files'$datapath,tools::file_ext)
data$type = extension
return(data)


})

output$dtFiles <- DT::renderDT(server = FALSE, {
  req(input$'msi-files')
      DT::datatable(
        filesInput(),
        extensions = c("Buttons"),
        options = list(
          dom = 'Bfrtip'
         
        )
      )
    })


  files<- reactive({
    req(input$'msi-files')

    data = filesInput()
    extension = data$type
    name_file = sapply(data$name,tools::file_path_sans_ext)
    condition = sapply(data$name,function(x) strsplit(x,'-')[[1]][1])
    replicat = apply(cbind(condition, sapply(data$name,function(x) strsplit(x,'-')[[1]][2])),1,paste,collapse='-')



    table= as.data.frame(
      cbind(
        type = extension,
        groupe= condition,
        replicat = replicat,
        name = data$name,
        datapath = data$datapath,
        filename = name_file
      )
    )
    
    imzml = table[which(table$type == 'imzML'),c('groupe',  'datapath',  'filename','name', 'replicat')]
    ibd = table[which(table$type == 'ibd'),c(  'datapath',  'filename','name')]

    colnames(imzml)=c('groupe',  'imzml_datapath',  'filename','imzml_name','replicat')
    colnames(ibd)=c('ibd_datapath',  'filename','ibd_name')
    
    df = merge(imzml,ibd,by='filename')

    return(list(table= df))

  })

  output$groupe <- DT::renderDT(server = FALSE, {
  req(input$'msi-files')
      DT::datatable(
        files()$table[,-c(3,6)][,c(2,4,1,3,5)],
        extensions = c("Buttons"),
        options = list(
          dom = 'Bfrtip'
         
        )
      )
    })




  combineObject<-reactive({
    req(input$'msi-files')
    datas = files()$table

    cardin_object = apply(datas,1,function(x) readMSIData(x[4]))
 objects = cardin_object[[1]]

  mzs = mz(objects)
  centroided(objects)=TRUE
  cond_prec = strsplit(as.vector(run(objects))[1],'-')[[1]][1]
  xadd = max(as.data.frame(coord(objects))$x) + 50
  yadd = 0
  
  

  for(i in 2:length(cardin_object)){
    
    obj = cardin_object[[i]]
    current_cond = strsplit(as.vector(run(obj))[1],'-')[[1]][1]
    mz(obj) = mzs
    centroided(obj)=TRUE


    if( cond_prec == current_cond){
      coord = as.data.frame(coord(obj))
      coord$x = coord$x +  xadd
      coord$y = coord$y +  yadd
      xadd = max(coord$x) + 50
      coord(obj) = coord(PositionDataFrame(coord))
    }
    else{
      coord = as.data.frame(coord(obj))
        yadd = max(as.data.frame(coord(objects))$y) +50
      coord$y = coord$y +  yadd
      xadd = max(coord$x) + 50

      coord(obj) = coord(PositionDataFrame(coord))
    }
    cond_prec = current_cond
    objects= cbind(objects,obj)
    image(objects)
    
  }
  
  run(objects) = sapply(as.vector(run(objects)),function(x) paste(strsplit(x,'-')[[1]][1:2],collapse='-'))
  condition = sapply(as.vector(run(objects)),function(x) strsplit(x,'-')[[1]][1])
  objects$condition=as.factor(condition)
  
  
  
  fData(objects) = summarizeFeatures(objects, "mean", as="DataFrame",groups=objects$condition)
  fData(objects)  = summarizeFeatures(objects, "mean", as="DataFrame",groups=run(objects))
  
  
  spectra = as.matrix(spectra(objects))
  colnames(spectra) = run(objects)
  rownames(spectra) = mz(objects)
  return(list(msiObject=objects,matrix=spectra))



  })


  output$Images <-renderPlot({
    req(input$'msi-files')
    image(combineObject()$msiObject)
  })


meanData <- reactive({
  datas = files()$table
  object = combineObject()
  data = object$msiObject
  condition = unique(datas$groupe)
  f_data = as.data.frame(fData(data))

  

  means = as.data.frame(f_data[,-1])
  colnames(means)= datas$replicat
  
  means.melt = data.frame(melt(means))
colnames(means.melt)= c('replicat','mean_intensity')

means.melt$condition = sapply(as.vector(means.melt$replicat),function(x) strsplit(x,'-')[[1]][1])


return(means.melt)


})
dataUpset <- reactive({
  datas = files()$table
  object = combineObject()
  data = object$msiObject
  mm = as.data.frame(summarizeFeatures(data, "mean", as="DataFrame",groups=data$condition))[,-1]
  colnames(mm) = unique(data$condition)


tt = lapply(1:ncol(mm),function(x) ifelse(mm[,x] > 1,colnames(mm)[x],''))
yy = Reduce(cbind, tt)



mm$all=apply(yy,1, function(x) paste(x,collapse='-'))


  mmComb = mm %>%
mutate(
all_list = as.list(strsplit(all, "-"))
)

return(mmComb)


})

 output$upsetPlot <- renderPlot(

   ggplot(data = dataUpset(),mapping = aes(x = all_list)) +
    geom_bar(fill = '#262686') +
    scale_x_upset(order_by = "degree")+
    geom_text(stat='count', aes(label=..count..),vjust=-0.5, size=5, color="black") +
    labs(x='Annotation combination', y='Distribution') +
    theme(axis.title = element_text(size=15))+
    theme_combmatrix(combmatrix.label.text = element_text(color ='#262686', size=12)) + 
    axis_combmatrix(levels = levels(as.factor(unlist(dataUpset()$all_list)))) 
    
  )


output$bpMeanReplicat <- renderPlot({
    
    res = meanData()
    
    ggboxplot(res, 'replicat','mean_intensity',col='condition',shape='condition')+
      rotate_x_text(45)
  })

  output$bpMeanCond <- renderPlot({
    res = meanData()
    
    ggboxplot(res, 'condition','mean_intensity',col='condition',shape='condition')+
      rotate_x_text(45)
  })









#PCA
  pca <- reactive({
    object = combineObject()
    spectra = object$matrix

    res_pca = prcomp(t(spectra), scale. = TRUE)


    pca_group = get_pca_ind(res_pca)

    
    pca_mz =  get_pca_var(res_pca)

    coord_gp = pca_group$coord[,1:5] # Keep just 5th first PC
    coord_mz = pca_mz$coord[,1:5]
    pca_df_gp = as.data.frame(
      cbind(
        coord_gp, 
        colnames(spectra), 
        sapply(colnames(spectra),function(x) strsplit(x,'-')[[1]][1])
        
      )
    )
    colnames(pca_df_gp)=c(paste0("Dim",1:5),"Replicat","Condition")

  pca_df_mz = as.data.frame(
      cbind(
        coord_mz, 
        rownames(spectra)
        
      )
    )
    colnames(pca_df_mz)=c(paste0("Dim",1:5),"mz")
    
    eigen_value = get_eig(res_pca)[1:5,2]

    res = list(
      pca_df_gp = pca_df_gp, # For ggplot
      pca_df_mz = pca_df_mz,
      pca_sample = pca_group, # For contribution
      pca_gene = pca_mz, # For contribution
      eig = round(eigen_value,2), # For percentage variance explained
      contrib_gp = pca_group$contrib[,1:5]
    )
    return(res)

  })

  pcaMean <- reactive({
    object = combineObject()
    spectra = as.data.frame(fData(object$msiObject))[,-1]

    res_pca = prcomp(t(spectra), scale. = TRUE)


    pca_group = get_pca_ind(res_pca)



    coord_gp = pca_group$coord[,1:5] # Keep just 5th first PC

    pca_df_gp = as.data.frame(
      cbind(
        coord_gp, 
        colnames(spectra), 
        sapply(colnames(spectra),function(x) strsplit(x,'[.]')[[1]][2])
        
      )
    )
    colnames(pca_df_gp)=c(paste0("Dim",1:5),"Replicat","Condition")

  
    eigen_value = get_eig(res_pca)[1:5,2]

    res = list(
      pca_df_gp = pca_df_gp, # For ggplot
      pca_sample = pca_group, # For contribution
      eig = round(eigen_value,2), # For percentage variance explained
      contrib_gp = pca_group$contrib[,1:5]
    )
    return(res)

  })

  pcaGraphGroup <- reactive({
    req(input$dim1,input$dim2)
    res.pca = pca()
    pca_df = res.pca$pca_df_gp

    eigen_value =res.pca$eig

    pca_sub = pca_df[,c(as.numeric(input$dim1), as.numeric(input$dim2), 6, 7)]
    colnames(pca_sub)= c("x","y","Replicat","Condition")
    pca_sub$x = as.numeric(pca_sub$x)
    pca_sub$y = as.numeric(pca_sub$y)
    
    plot=ggplot(data=pca_sub, aes(x=x, y=y, col=Condition, tooltip=Replicat)) +
      geom_point(size=1) + theme_minimal() +
      labs(x = paste0("Dimension ", input$dim1," (",eigen_value[as.numeric(input$dim1)],"%)"),y = paste0("Dimension ", input$dim2," (",eigen_value[as.numeric(input$dim2)],"%)")) + 
      geom_vline(xintercept=0, linetype="dashed", color = "grey") +
      geom_hline(yintercept=0, linetype="dashed", color = "grey") +
      theme(legend.position="bottom")
    return(plot)
  })

  output$pcaGroup<- renderPlotly({ 
    ggplotly(pcaGraphGroup()) %>% 
      layout(legend = list(orientation = 'h', x = 0.45, y = 1.1)) 
  })

  pcaGraphGroupMean <- reactive({
    req(input$'dim1-2',input$'dim2-2')
    res.pca = pcaMean()
    pca_df = res.pca$pca_df_gp

    eigen_value =res.pca$eig

    pca_sub = pca_df[,c(as.numeric(input$'dim1-2'), as.numeric(input$'dim2-2'), 6, 7)]
    colnames(pca_sub)= c("x","y","Replicat","Condition")
    pca_sub$x = as.numeric(pca_sub$x)
    pca_sub$y = as.numeric(pca_sub$y)
    
    plot=ggplot(data=pca_sub, aes(x=x, y=y, col=Condition, tooltip=Replicat)) +
      geom_point(size=1) + theme_minimal() +
      labs(x = paste0("Dimension ", input$'dim1-2'," (",eigen_value[as.numeric(input$'dim1-2')],"%)"),y = paste0("Dimension ", input$'dim2-2'," (",eigen_value[as.numeric(input$'dim2-2')],"%)")) + 
      geom_vline(xintercept=0, linetype="dashed", color = "grey") +
      geom_hline(yintercept=0, linetype="dashed", color = "grey") +
      theme(legend.position="bottom")
    return(plot)
  })

  output$pcaGroupMean<- renderPlotly({ 
    ggplotly(pcaGraphGroupMean()) %>% 
      layout(legend = list(orientation = 'h', x = 0.45, y = 1.1)) 
  })



    pcaGraphMz <- reactive({
    req(input$dim1,input$dim2)
    res.pca = pca()
    pca_df = res.pca$pca_df_mz

    eigen_value =res.pca$eig

    pca_sub = pca_df[,c(as.numeric(input$dim1), as.numeric(input$dim2), 6)]
    colnames(pca_sub)= c("x","y","mz")
    pca_sub$x = as.numeric(pca_sub$x)
    pca_sub$y = as.numeric(pca_sub$y)
    
    plot=ggplot(data=pca_sub, aes(x=x, y=y, tooltip=mz)) +
      geom_point(size=1) + theme_minimal() +
      labs(x = paste0("Dimension ", input$dim1," (",eigen_value[as.numeric(input$dim1)],"%)"),y = paste0("Dimension ", input$dim2," (",eigen_value[as.numeric(input$dim2)],"%)")) + 
      geom_vline(xintercept=0, linetype="dashed", color = "grey") +
      geom_hline(yintercept=0, linetype="dashed", color = "grey") +
      theme(legend.position="bottom")
    return(plot)
  })

   pcaGraphProj <- reactive({
    object = combineObject()$msiObject
    res.pca = pca()
    contrib = res.pca$contrib_gp

    df = data.frame(cbind(as.data.frame(coord(object))[,1:2],contrib=as.vector(contrib[,as.numeric(input$pca_dim)])))
  
    plot = ggplot(df, aes(x, y, fill = contrib)) + geom_tile()


    return(plot)
  })


  output$pcaMz<- renderPlotly({ 
    ggplotly(pcaGraphMz()) %>% 
      layout(legend = list(orientation = 'h', x = 0.45, y = 1.1)) 
  })

  output$pcaObj<- renderPlot({ 
    pcaGraphProj()
  })

  mzChoice <- reactive({
      object = combineObject()
  data = object$msiObject

    mz = round(mz(data),3)

    name = mz
    num  = c(1:length(name))

    choiceTable = data.frame(name, num)
    choix = setNames(as.numeric(choiceTable$num), choiceTable$name)

    return(choix)

  })
    output$mzChoiceUI <-renderUI({
   
    return(selectInput("mzTarget", label = "Choose mz target to compare",
                  choices = mzChoice()))

  })

      condChoice <- reactive({
      object = combineObject()
  data = object$msiObject

    cond= unique(data$condition)

    name = cond
    num  = c(1:length(name))

    choiceTable = data.frame(name, num)
    choix = setNames(as.numeric(choiceTable$num), choiceTable$name)

    return(choix)

  })
    output$condChoiceUI <-renderUI({
   
    return(selectInput("condTarget", label = "Choose mz target to compare",
                  choices = condChoice()))

})

         condChoice2 <- reactive({
      object = combineObject()
  data = object$msiObject

    cond= unique(data$condition)

    name = cond
    num  = c(1:length(name))

    choiceTable = data.frame(name, num)
    choix = setNames(as.numeric(choiceTable$num), choiceTable$name)

    return(choix)

  })
    output$condChoiceUI2 <-renderUI({
   
    return(selectInput("condTarget2", label = "Choose mz target to compare",
                  choices = condChoice2(),selected =2))

})


  output$nbClustUI <-renderUI({
   req(input$'msi-files')
    return(numericInput("nbClust", label = "Choose nb class",
                  length(unique(files()$table$groupe)),step=1))

  })

pairwize <- reactive({
      req(input$mzTarget)
  datas = files()$table
  object = combineObject()
  data = object$matrix

  f_data = data[as.numeric(input$mzTarget),]

  table = as.data.frame(cbind(replicat= colnames(data), intensity = as.vector(f_data)))

  table$intensity = as.numeric(table$intensity)
  table$condition = sapply(table$replicat,function(x) strsplit(x,'-')[[1]][1])
 
  return(table)



})
output$bpPairwise <- renderPlot({
    
    res = pairwize()
    
    ggboxplot(res, 'condition','intensity',col='condition',shape='condition')+
      rotate_x_text(45)
  })


anova <- reactive({

  data = pairwize()
  aov1 = aov(intensity ~ condition, data = data)
  normality = nortest::ad.test(resid(aov1))
  inter = ifelse(normality$p.value < 0.05,'=> Reject H0, Residus are not normally distributed','=> No reject H0, Residus are  normally distributed')
  
  text = c('normality','Anderson Darling','H0 : Residus are normally distributed', normality$p.value,normality$statistic,inter)
  
  var = bartlett.test(intensity ~ condition, data = data)
  inter = ifelse(var$p.value < 0.05, '=> Reject H0, At least two samples variances are not equal','=> No reject H0, Samples variances are equal')

  text = rbind(text,c('Homoscedasticity','Bartlett','H0 : Samples variances are equal',var$p.value,var$statistic,inter))

  if( normality$p.value > 0.05 || var$p.value > 0.5){
    test = 'anova'
    sm = summary(aov1)[[1]]
    stat = sm[["F value"]][1]
    padj = sm[["Pr(>F)"]][1]
    inter = ifelse(padj < 0.05,'=> Reject H0, At least two samples means are not equal','=> No reject H0, Samples means are equal')
    text = rbind(text,c('Group comparison','ANOVA','H0 : Samples means are equal',padj,stat,inter))
    pairwise_test = 't.test'
    pairwise_table = as.data.frame(pairwise.t.test(as.numeric(data$intensity), as.factor(data$condition), p.adjust.method = "BH")$p.value)
  }
  else{
    test = 'kruskal'
    res= kruskal.test(intensity ~ condition, data = data)
    stat = res$statistic
    padj = res$p.value 
    inter = ifelse(padj < 0.05,'=> Reject H0, At least two samples means are not equal','=> No reject H0, Samples means are equal')

    text = rbind(text,c('Group comparison','Kruskal-Wallis','H0 : Samples means are equal',padj,stat,inter))
    pairwise_test = 'wilcoxon'
    pairwise_table = as.data.frame(pairwise.wilcox.test(as.numeric(data$intensity), as.factor(data$condition), p.adjust.method = "BH")$p.value)

  }
  text= as.data.frame(text)
  colnames(text)= c('Test','Name','H0','p-value','Statistic','Interpretation')
rownames(pairwise_table) = unique(data$condition)[-1]  
  return(list(pairwise_table=pairwise_table,text=text))

})

output$pairwiseTable <- renderTable({

     
        anova()$pairwise_table
        
    },rownames=TRUE)

output$pairwiseText <-renderTable({
  anova()$text
  })

ssc_test <-reactive({
  req(input$nbClust)
   datas = combineObject()
  spatialShrunkenCentroids(datas$msiObject,  r=1, s=0, k = as.numeric(input$nbClust))

  })
output$ssc <- renderPlot({
  req(input$'msi-files')
  image(ssc_test())

})

output$SSCPlot <- renderPlot({
  req(input$'msi-files')
  plot(ssc_test())

})


anadiff <- reactive({
  req(input$condTarget,input$condTarget2)

  datas = combineObject()$msiObject
  condition = unique(datas$condition)

  choiceCond = condition[c(as.numeric(input$condTarget),as.numeric(input$condTarget2))]

    sub = datas[, which(datas$condition %in% as.vector(choiceCond))] 

    mean = as.data.frame(summarizeFeatures(sub, "mean", as="DataFrame",groups=sub$condition))[,c(as.numeric(input$condTarget)+1,as.numeric(input$condTarget2)+1)]
    log2FC = log2(mean[,1]/mean[,2])

  
  mtest <- meansTest(sub, ~ condition)

df=data.frame(cbind(mz=mz(sub),log2FC= log2FC,mean,summary(mtest)))

return(df)

  })

output$tableAD <- DT::renderDT(server = FALSE, {

      DT::datatable(
        anadiff(),
        caption = 'Table : Results of differential analysis',
        extensions = c("Buttons"),
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(extend = "copy", text = "Copy", filename = "pairwise_table",
                 exportOptions = list(
                   modifier = list(page = "all")
                 )
            ),
            list(extend = "csv", text = "CSV", filename = "pairwise_table",
                 exportOptions = list(
                   modifier = list(page = "all")
                 )
            ),
            list(extend = "excel", text = "Excel", filename = "pairwise_table",
                 exportOptions = list(
                   modifier = list(page = "all")
                 )
            )
          )
        )
      )
    })
  volcano <- reactive({

    table = anadiff()
    tsPadj = as.numeric(input$ts_padj)
    tsFC = 2

          
    table$diffexpressed = "NO"
    table$diffexpressed[table$log2FC > tsFC & table$FDR < tsPadj] = "UP"
    table$diffexpressed[table$log2FC < -tsFC & table$FDR < tsPadj] = "DOWN"

    cols=c("#CDCDE6", "grey","#262686")

    if(nrow(table[table$diffexpressed == 'NO', ])==0) { cols = c("#CDCDE6" , "#262686")}
    if(nrow(table[table$diffexpressed == 'DOWN', ]) == 0) { cols = c('grey', "#262686")}
    if(nrow(table[table$diffexpressed == 'UP', ])   ==0)  { cols = c('grey', "#CDCDE6")}

    plot = ggplot(data=as.data.frame(table), aes(x=log2FC, y=-log10(FDR), col=diffexpressed, tooltip=mz)) +
      geom_point(size=1) + theme_minimal()+
      geom_vline(xintercept=c(-tsFC, tsFC), col="red") +
      geom_hline(yintercept=-log10(tsPadj), col="red")+
      scale_color_manual(values=cols)
     
    return(plot)
  })

  output$volcanoPlot <- renderPlotly({ ggplotly(volcano()) })




}





