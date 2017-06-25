

library(shiny)

levels <- read.csv("final_Utility.csv",header = TRUE,check.names = FALSE)
features <- read.csv("final_Feature.csv",header=TRUE,check.names = FALSE)
averages <- read.csv("final_AvgUtilities.csv",header = TRUE,check.names = FALSE)
attributes <- read.csv("ProjectAttributes.csv",header = TRUE,check.names = FALSE)
utilities <- read.csv("HB_Utilities.csv",header = TRUE,check.names = FALSE)
#covariates <- read.csv("covariates.csv",header=TRUE,check.names = FALSE)

require(ggplot2)
require(tidyr)
require(ggthemes)
require(stringr)


# Feature Matrix
atts <- attributes[,-2]
cntr <- 1
featmatrix <- data.frame(matrix(nrow=length(unique(attributes[,1])),ncol=4))
cntr.col <- 1
for (i in 1:(nrow(atts))){
  if(atts[i,2] == 1){
    featmatrix[cntr,3] <- cntr.col
  }
  if (i == nrow(atts)){
    featmatrix[cntr,1] <- atts[i,1]
    featmatrix[cntr,2] <- atts[i,2]
    featmatrix[cntr,4] <- cntr.col
    cntr <- cntr + 1
  }
  else if ((atts[i,2] > atts[(i+1),2])){
    featmatrix[cntr,1] <- atts[i,1]
    featmatrix[cntr,2] <- atts[i,2]
    featmatrix[cntr,4] <- cntr.col
    cntr <- cntr + 1
  }
  cntr.col <- cntr.col + 1
}
featmatrix[,1] <- as.character(unique(attributes[,2]))
colnames(featmatrix) <- c("Feature","Levels","Start_Column","End_Column")

attributes[,"Keys"] <- paste0(attributes[,"Feature"],"_",attributes[,"LevelNumber"])

if(exists("covariates")){
covariates[,ncol(covariates)+1] <- rep(1,nrow(covariates))
covariates <- covariates[,c(ncol(covariates),1:(ncol(covariates)-1))]
colnames(covariates)[1] <- "Overall"
} else {
  covariates <- data.frame("Overall"=rep(1,nrow(utilities)))
}

col_width <- floor(12/nrow(featmatrix))


ui <- fluidPage(
   
   titlePanel("Example Project"),
   
   tabsetPanel(
     
     tabPanel("Overview",
              
              h2("Conjoint Analysis Metrics"),
              tags$div(
                tags$ul(
                  tags$li("The utilities are the framework of conjoint analysis.  Utilities are calculated through running the survey data through a Hierarchical Bayesian statistical model
."),
                  tags$ul(
                    tags$li("The Hierarchical Bayesian model implements a Markov Chain Monte Carlo (MCMC) algorithm to sample draws from the posterior distribution of each model parameter.  We use the median of the random draws as our estimate of utility for that parameter for the respondent.
"),
                    tags$li("It is a machine learning algorithm that loops through the model many times and uses past models to become increasingly smarter and more accurate.
")
                  ),
                  tags$br(),
                  tags$li("Utilities are an ordinal measurement.
"),
                  tags$br(),
                  tags$li("Feature Importance scores are a representation of how much the overall feature was in decision-making – derived from the range of that feature’s levels
."),
                  tags$br(),
                  tags$li("Level Preference share is the percentage of respondents who had their highest level utility for that individual level."),
                  tags$br(),
                  tags$li("All metrics are important and should be used in combination.   The most important levels are those with high preference scores in the feature with the highest importance score.
")
                )
              )
            
   
      ),
     tabPanel("Feature Importance",
            
              fluidRow(
                
              column(3,  
              selectInput("segment", label = "Segment:",
                          choices = levels[,1], selected = levels[,1][1]))
              
              ),
              
              plotOutput('feature')
     ),
     tabPanel("Level Preference",
              
              fluidRow(
                
              column(3,  
              selectInput("segment_level", label = "Segment:",
                          choices = levels[,1], selected = levels[,1][1])),
              column(3,
              selectInput("feature", label = "Feature:",
                          choices = featmatrix[,1], selected = featmatrix[,1][1]))
              
              ),
              
              plotOutput('level')
              
       
     ),
     tabPanel("Average Utility",
              
              fluidRow(
              
              column(3,    
              selectInput("segment_average", label = "Segment:",
                          choices = levels[,1], selected = levels[,1][1])),
              column(3,
              selectInput("feature_average", label = "Feature:",
                          choices = featmatrix[,1], selected = featmatrix[,1][1]))
              
              ),
              
              plotOutput('utility')

       
     ),
     
     tabPanel("Simulator",
              
              fluidRow(
                
                column(3,
                selectInput("segment_simulator","Segment:",colnames(covariates),selected = 1)),
                column(3,
                selectInput("number","Number of Packages:",2:6,selected = 1))
              
              ),
              
              
              fluidRow(
                
                
                column(col_width, 
                       h4("Package #1"),
                       lapply(1:nrow(featmatrix),function(i) {
                         selectInput(paste0("1","_",featmatrix[i,"Feature"],"_",i),featmatrix[i,"Feature"],choices = attributes[featmatrix[i,3]:featmatrix[i,4],"LevelText"], selected = 1)
                       })),
                
                
                column(col_width,
                       h4("Package #2"),
                       lapply(1:nrow(featmatrix),function(i) {
                         
                         selectInput(paste0("2","_",featmatrix[i,"Feature"],"_",i),featmatrix[i,"Feature"],choices = attributes[featmatrix[i,3]:featmatrix[i,4],"LevelText"], selected = 1)
                         
                       })
                ),
                
                
                
                conditionalPanel(
                  condition = "input.number >= 3",
                  
                  column(col_width,
                         h4("Package #3"),
                         lapply(1:nrow(featmatrix),function(i) {
                           selectInput(paste0("3","_",featmatrix[i,"Feature"],"_",i),featmatrix[i,"Feature"],choices = attributes[featmatrix[i,3]:featmatrix[i,4],"LevelText"], selected = 1)
                         })
                  )),
                
                conditionalPanel(
                  condition = "input.number >= 4",
                  
                  column(col_width,
                         h4("Package #4"),
                         lapply(1:nrow(featmatrix),function(i) {
                           selectInput(paste0("4","_",featmatrix[i,"Feature"],"_",i),featmatrix[i,"Feature"],choices = attributes[featmatrix[i,3]:featmatrix[i,4],"LevelText"], selected = 1)
                         })
                         
                  )),
                
                
                
                conditionalPanel(
                  condition = "input.number >= 5",
                  
                  column(col_width,
                         h4("Package #5"),
                         lapply(1:nrow(featmatrix),function(i) {
                           selectInput(paste0("5","_",featmatrix[i,"Feature"],"_",i),featmatrix[i,"Feature"],choices = attributes[featmatrix[i,3]:featmatrix[i,4],"LevelText"], selected = 1)
                         })
                         
                  )),
                
                conditionalPanel(
                  condition = "input.number == 6",
                  
                  column(col_width,
                         h4("Package #6"),
                         lapply(1:nrow(featmatrix),function(i) {
                           selectInput(paste0("6","_",featmatrix[i,"Feature"],"_",i),featmatrix[i,"Feature"],choices = attributes[featmatrix[i,3]:featmatrix[i,4],"LevelText"], selected = 1)
                         })
                  ))),
              
              fluidRow(
                
                column(9, align = "center",
                       plotOutput('plot'))

                # column(2,
                #        downloadButton('download_simulation','Download Image'),
                #        offset = 9)
              )
       
     )
   )
)

server <- function(input, output) {
  
  output$feature <- renderPlot({
    sub <- features[which(features[,1] == input$segment),]
    
    dat <- sub %>% gather("Feature","Importance",2:ncol(sub))
    
    ggplot(data = dat, aes(x=Feature,y=Importance)) + geom_bar(stat="identity",width = .5,fill = "dodgerblue3") + theme_hc() + 
      scale_y_continuous(name = "",limits = c(0,1),breaks = seq(from = 0,to = 1,by=.1),label=function(x){return(paste0(x*100,'%'))},expand = c(0, .1)) +
      scale_x_discrete(name="",labels = function(x) str_wrap(x, width = 20)) + geom_text(aes(label = paste(round(Importance * 100,digits = 1),'%')), vjust = -1,size=6) + 
      theme(plot.title = element_text(hjust = 0.5,size = 18),axis.text.x = element_text(size=16),axis.text.y = element_text(size=11))
    
  })
  
  output$level <- renderPlot({
    cntr <- which(featmatrix[,1] == input$feature)
    sub <- levels[which(levels[,1] == input$segment_level),c(1,(featmatrix[cntr,3]+1):(featmatrix[cntr,4]+1))]
    
    dat <- sub %>% gather("Level","Preference",2:ncol(sub))
    
    ggplot(data = dat, aes(x=Level,y=Preference)) + geom_bar(stat="identity",width = .5,fill = "dodgerblue3") + theme_hc() + 
      scale_y_continuous(name = "",limits = c(0,1),breaks = seq(from = 0,to = 1,by=.1),label=function(x){return(paste0(x*100,'%'))},expand = c(0, .1)) +
      scale_x_discrete(name="",labels = function(x) str_wrap(x, width = 20)) + geom_text(aes(label = paste(round(Preference * 100,digits = 1),'%')), vjust = -1,size=6) + 
      theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=16),axis.text.y = element_text(size=11))
    
  })
  
  output$utility <- renderPlot({
    cntr <- which(featmatrix[,1] == input$feature_average)
    sub <- averages[which(averages[,1] == input$segment_average),c(1,(featmatrix[cntr,3]+1):(featmatrix[cntr,4]+1))]
    
    dat <- sub %>% gather("Level","Average",2:ncol(sub))
    
    ggplot(data = dat, aes(x=Level,y=Average,group=1)) + geom_point(color = 'dodgerblue3',size = 5) + geom_line(color='dodgerblue3') + 
      theme_hc() + geom_text(aes(label = round(Average,3), vjust = -1),size=6) + scale_x_discrete(name="",labels = function(x) str_wrap(x, width = 20)) + 
      scale_y_continuous(name = "",expand = c(.1, .1)) + theme(plot.title = element_text(hjust = 0.5,size = 20),axis.text.x = element_text(size=16),axis.text.y = element_text(size=11)) 
    
  })
  
  
  
  Simulation <- reactive({
    
    myvalues <- NULL
    
    for(i in 1:(length(names(input)) - 7)){

      
      row_feat <- as.numeric(substr(names(input)[i],start = nchar(names(input)[i]),stop = nchar(names(input)[i])))
      start_col <- featmatrix[row_feat,3]
      end_col <- featmatrix[row_feat,4]
      
      temp <- attributes[start_col:end_col,]
      
      key <- temp[which(temp[,4] == input[[names(input)[i]]]),5]
      
      now <- (cbind(names(input)[i],input[[names(input)[i]]], key))
      
      names(now) <- names(myvalues)
      
      myvalues <- as.data.frame(rbind(myvalues,now))
      
    }
    
    names(myvalues) <- c("Input","Value","Key")

    
    
    pack <- lapply(seq(1,nrow(myvalues),by=nrow(featmatrix)), function(x) {
      myvalues[x:(x+nrow(featmatrix)-1),3]
    })
    
    names(pack) <- lapply(1:6, function(y){
      paste0("Package #",y)
    })
    
    individuals <- data.frame(matrix(0,nrow=nrow(utilities),ncol = as.numeric(input$number)))
    
    
    
    for(j in 1:as.numeric(input$number)){
      
      cols <- lapply(seq_along(pack[[j]]), function(x) {
        which(colnames(utilities) == pack[[j]][x])
      })
      cols <- unlist(cols)
      
      temp <- utilities[,cols]
      
      individuals[,j] <- apply(temp,1,sum)

    }
    
    maxes <- data.frame(matrix(0,nrow = nrow(individuals),ncol = as.numeric(input$number)))
    
    for (i in 1:nrow(maxes)){
      ptr <- which(individuals[i,] == max(individuals[i,]))
      maxes[i,ptr] <- 1
    }
    
    totals <- data.frame(matrix(0,nrow = 1,ncol = as.numeric(input$number)))
    
    for(i in 1:ncol(totals)){
      totals[1,i] <- sum(maxes[which(covariates[,which(colnames(covariates) == input$segment_simulator)] == 1),i])/sum(covariates[,which(colnames(covariates) == input$segment_simulator)])
    }
    colnames(totals) <- lapply(1:(as.numeric(input$number)), function(y){
      paste0("Package #",y)
    })
    
    dat <- totals %>% gather("Package","Share",1:ncol(totals))
    
    ggplot(data = dat,aes(x=Package,y=Share)) + geom_bar(stat="identity",width = .5,fill = "dodgerblue3") + theme_hc() + 
      scale_y_continuous(name = "Projected Market Share",limits = c(0,1),breaks = seq(from = 0,to = 1,by=.1),label=function(x){return(paste0(x*100,'%'))},expand = c(0, .1)) +
      scale_x_discrete(name="") + geom_text(aes(label = paste(round(Share * 100,digits = 1),'%')), vjust = -1,size=6)  + 
      theme(plot.title = element_text(hjust = 0.5,size = 18),axis.text.x = element_text(size=16),axis.text.y = element_text(size=11))
    
    
  })
  
  
  output$plot <- renderPlot({
    Simulation()
  })
  
  
  # output$download_simulation <- downloadHandler(
  #   filename <- function(){
  #     paste("simulation",Sys.Date(),".png",sep="")},
  #   content <- function(file){
  #     ggsave(file,Simulation())
  #     dev.off()
  #   },
  #   contentType = 'image/png'
  # )
  

}

shinyApp(ui = ui, server = server)

