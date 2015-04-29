library(shiny)

wideDir <- '~/DataAnalysis//WISE//Datasets//wide//'
wideFiles <- list.files(wideDir)
longDir <- '~/DataAnalysis//WISE//Datasets//long//'
longFiles <- list.files(longDir)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # reactive features of ui
  dataForm <- reactive({
    input$dataForm
  })
  #feature <- reactive({
  #  input$feature
  #})
  observe({
    updateSelectInput(session, "dataset", choices=switch(dataForm(),"wide"=wideFiles,"long"=longFiles), selected=switch(dataForm(),"wide"=wideFiles[1],"long"=longFiles[1]))
  })
  datasetInput <- reactive({
    readRDS(paste0(wideDir,input$dataset))
  })
  getFeatures <- reactive({
    data <- datasetInput()
    cnames <- names(data)
    # only numeric
    cnames <- cnames[sapply(data,is.numeric)]
    
    features <- gsub("(.*)?(min|max|first|last|mean|median|sum|first\\.to\\.last)(.*)$","\\1",cnames)
    #features <- gsub("\\.", " ", features)
    return(features)
  })
  getFuns <- reactive({
    cnames <- names(datasetInput())
    feature <- input$feature
    #feature <- gsub(" ", "\\.", feature)
    cnames.fe <- grep(feature, cnames, value=TRUE)
    funs <- unique(gsub("(.*)?(min|max|first|last|mean|median|sum|first\\.to\\.last)(.*)$","\\2",cnames.fe))
    funs <- funs[funs %in% c("min","max","first","last","mean","median","sum", "first.to.last")]
    #funs <- gsub("\\.", " ", funs)
    return(funs)
  })
  getItems <- reactive({
    cnames <- names(datasetInput())
    feature <- input$feature
    #feature <- gsub(" ", "\\.", feature)
    cnames.fe <- grep(feature, cnames, value=TRUE)
    fun <- input$fun
    #fun <- gsub(" ", "\\.", fun)
    cnames.fu <- grep(fun, cnames.fe, value=TRUE)
    items <- unique(gsub("(.*)?(min|max|first|last|mean|median|sum|first\\.to\\.last)(.*)$","\\3",cnames.fu))
    #items <- gsub("\\.", " ", items)
    return(items)
  })
  
  output$view <- renderPlot({
    data <- datasetInput()
    full <- paste(input$feature,input$fun,input$item,sep="")
    hist(data[,full])
  })
  output$feature <- renderUI({
    selectInput('feature','Feature',getFeatures())
  })
  output$fun <- renderUI({
    selectInput('fun', "Function", getFuns())
  })
  output$item <- renderUI({
    selectInput('item', "Item", getItems())
  })
})
