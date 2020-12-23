library(shinyMatrix)
library(sp)
library(sf)
library(shinythemes)
library(rgdal)  # for vector work; sp package should always load with rgdal. 
library(dplyr)
library(shiny)
library(lubridate)

zipfinal <- read.csv('map_data/zipfinal.csv', stringsAsFactors = FALSE)

validzip <- function(mat){
  check <- ''
  for(i in c(1:5)){
    if(is.na(mat[i, 1]) | (mat[i, 1] %in% zipfinal$Zip)){}
    else{
      check <- paste0(check," ", i)
      print(check)
    }
  }
  return(check)
}

calc_risk_zip <- function(mat){
  print(mat)
  riskall <- 1
  check <- validzip(mat)
  print(check)
  if(nchar(check)>1){
    return(paste0('Invalid zipcode: row', check))
  }else{
    for (i in c(1:5)){
      if(!is.na(mat[i,1])){
        print(mat[i,1])
        if(is.na(mat[i,2])){return('Enter the number of attendees')}
        risk <- zipfinal$pnI[zipfinal$Zip==mat[i, 1]] ** mat[i, 2]
        print(risk)
        riskall <- riskall*risk
      }
      else{
        if(!is.na(mat[i,2])){return('Enter the zip code')}
      }
    }
    return(paste0('Risk: ', round(100*(1-riskall),1), '%'))
  }
}

# Probably of NOT infected = 1-[(cases*ascbias)*scalefactor / population]
# PerfectSituation = Probably of NOT infected*Probably of NOT infected*Probably of NOT infected*Probably of NOT infected*Probably of NOT infected

ui <- fluidPage(
  
  #theme = shinytheme("sandstone"),
  # Application title
  titlePanel(titlePanel(div(
    column(width = 12, h2("COVID-19 Event Risk Assessment Planning tool 222"))
    
  ))), #windowTitle = "COVID-19 Event Risk Assessment Planning tool"),
  HTML(paste0(
    "<p>Enter the zip codes of where your guests are coming from. Include attendees from your household.</p>"
  )
  ),
  # Place controls here
  
  
  fluidRow(
    
    column(width = 6,
           matrixInput("calcMatrix", 
                       value = matrix(data = NA, nrow = 5, ncol = 2, byrow = FALSE,
                                      dimnames = list(c("1", "2", "3", "4", "5"),
                                                      c("Zip Code", "Attendees"))),
                       class = "numeric",
                       rows = list(
                         names = FALSE,
                         editableNames = FALSE),
                       cols = list(
                         names = TRUE,
                         editableNames = FALSE)
           )
           
           
           
    ),
    column(width = 6,
           actionButton("button", "Calculate the risk"),
           HTML(paste0(
             "<p></p>"
           )),
           htmlOutput("risk")
           
    )
  )
)


server <- function(input,output){
  
  #getDataZip()
  
  observeEvent(input$button, {
    output$risk <- renderText({
      paste("<font color=\"#FF0000\"><b>", calc_risk_zip(input$calcMatrix), "</b></font>")
    })
  })
  
} # end of server


shinyApp(ui = ui, server = server)
