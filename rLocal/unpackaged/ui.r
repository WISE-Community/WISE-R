wideDir <- '~/DataAnalysis//WISE//Datasets//wide//'
wideFiles <- list.files(wideDir)
longDir <- '~/DataAnalysis//WISE//Datasets//long//'
longFiles <- list.files(longDir)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
   
  # Application title
  titlePanel("View WISE data"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("dataForm",label="What form of data?",choices=c("wide","long"), selected="wide"),
      selectInput("dataset",label="Choose your dataset",choices=wideFiles,selected=wideFiles[1]),
      uiOutput('feature'),
      uiOutput('fun'),
      uiOutput('item') 
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("view")
    )
  )
))