# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# -----------------------------------------------------------------------------
# Preamble 
library(shiny)
library(eegkit)
library(ggplot2)

# -----------------------------------------------------------------------------
# Define UI for application
ui <- fluidPage(
  
  # Enables MathJax / LaTeX 
  withMathJax(),
  
  # App title ----
  titlePanel(
    
    # "Title Text",
    img(src = "rstudio.png", 
        height = 35, 
        width = 100
    ), 
    windowTitle = "Electrode App"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      conditionalPanel(
        condition = "input.conditionedPanels == 'Introduction'", 

        h3("Control Panel"),
        
        helpText("The control panel can help you interact with display. The control panel will change depending on the tab you select.")
      ),
      
      conditionalPanel(
        condition = "input.conditionedPanels == 'Electrode Cap'",       
        
        h3("Control Panel"),
        
        helpText("Click the play button or move the slider manually. Green electrodes are statistically significant."),
        
        a("Shiny homepage", href = "http://shiny.rstudio.com"),
        
        br()
        
      ),
      
      conditionalPanel(
        condition = "input.conditionedPanels == 'Indiviudal Electrodes'", 
        
        h3("Control Panel"),
        
        helpText("Select an electrode channel from the pulldown list to view."),
        
        selectInput(inputId = "channel", 
                    label = "Electrode Channel", 
                    choices = c("FP1", "AF7", "AF3", "F1", "F3", "F5", "F7", "FT7", "FC5", "FC3", "FC1", "C1", "C3", "C5", "T7", "TP7", "CP5", "CP3", "CP1", "P1", "P3", "P5", "P7", "P9", "PO7", "PO3", "O1", "IZ", "OZ", "POZ", "PZ", "CPZ", "FPZ", "FP2", "AF8", "AF4", "AFZ", "FZ", "F2", "F4", "F6", "F8", "FT8", "FC6", "FC4", "FC2", "FCZ", "CZ", "C2", "C4", "C6", "T8", "TP8", "CP6", "CP4", "CP2", "P2", "P4", "P6", "P8", "P10", "PO8", "PO4", "O2", "F9", "F10", "FT9", "FT10", "NOSE", "NZ", "TP9", "TP10"), 
                    multiple = FALSE)
      ),
      
      conditionalPanel(
        condition = "input.conditionedPanels == 'Dependence Summary'",       
        
        h3("Control Panel"),
        
        helpText("Summary Control Pannel. Slider bar for time. 1:100"),
        
        a("Shiny homepage", href = "http://shiny.rstudio.com"),
        
        br()
        
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Introduction",
          br(),
          h3("Problem Statement:"),
          "Loudness-dependence of auditory evoked potentials (LDAEP) is a biomarker that may help identify personalized treatment to depression. We want to determine the response of EEG data to the loudness of auditory tone stimulus using functional data techniques. We think of EEG response as a function of a continuous scalar variable, loudness, measured in decibels."
        ),
      
        tabPanel(
          "Indiviudal Electrodes",  
          
          plotOutput(outputId = "eegTime"),
          
          verbatimTextOutput(outputId = "summary")
        ),
        
        tabPanel(
          "Electrode Cap",
          
          br(),
          
          plotOutput(outputId = "eegCap")                  
        ),
        
        tabPanel(
          "Dependence Summary",  
          
          helpText("Some stuff about depenceny goes here. Put an electorde cap. Control pannel has time.")
        ),
        
        
        id = "conditionedPanels"                
      )
    )  
  )
)   

# -----------------------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  load("data/eeg.app.data.interest.Rdata")
  
  elc.labels <- c("FP1", "AF7", "AF3", "F1", "F3", "F5", "F7", "FT7", "FC5", "FC3", "FC1", "C1", "C3", "C5", "T7", "TP7", "CP5", "CP3", "CP1", "P1", "P3", "P5", "P7", "P9", "PO7", "PO3", "O1", "IZ", "OZ", "POZ", "PZ", "CPZ", "FPZ", "FP2", "AF8", "AF4", "AFZ", "FZ", "F2", "F4", "F6", "F8", "FT8", "FC6", "FC4", "FC2", "FCZ", "CZ", "C2", "C4", "C6", "T8", "TP8", "CP6", "CP4", "CP2", "P2", "P4", "P6", "P8", "P10", "PO8", "PO4", "O2", "F9", "F10", "FT9", "FT10", "NOSE", "NZ", "TP9", "TP10")
  
  output$eegCap <- renderPlot({
    eegcap(elc.labels, "2d", col.point = )
  })
  
  output$eegTime <- renderPlot({
    i <- which(elc.labels %in% input$channel)
    
    se <- eeg.app.data.interest[[i]]$plots[[2]]$se
    
    dat <- data.frame(x = eeg.app.data.interest[[i]]$plots[[2]]$x, 
                      y = eeg.app.data.interest[[i]]$plots[[2]]$fit)
    
    min <- dat$y - 1.96*se
    max <- dat$y + 1.96*se
    
    ggplot(dat, aes(x, y)) + 
      geom_ribbon(aes(ymin = min, 
                      ymax = max), 
                  fill="steelblue2", 
                  color="steelblue2") +
      geom_line(color="steelblue4", lwd=1) +
      geom_hline(mapping = NULL, data = NULL, yintercept = 0) +
      ylab("Microvolts") +
      xlab("Time after stimulus (mS)") +
      ylim(-.15,.15)
  })
}

# -----------------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)

# runApp for showcase mode, must compile under comment first. 
# runApp(appDir = "~/NYU Projects/R Scripts/eeg_app/", display.mode = "showcase")
