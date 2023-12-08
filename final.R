library(shiny)
library(dplyr)



dat <- read.csv('cancer.csv', header = T)
dat$BMI <- round(dat$BMI)
dat$Classification <- ifelse(dat$Classification == 1, 0, 1)
dat$Classification <- as.numeric(as.character(dat$Classification))

ui <- fluidPage(
  titlePanel("Cancer Diagnosis"),
  sidebarLayout(
    position = "left",
    mainPanel("main panel",
              fluidRow(
                splitLayout(style = "border: 1px solid silver:", 
                            cellWidths = c(400,400,400),
                            plotOutput("res_plot"),
                            plotOutput('gluc_plot'))
              ),
              plotOutput("homa_plot")
    ),
                            

    sidebarPanel(
      numericInput(inputId='Age', label='Age', value = 50,min = 24, max = 90, 
                   step = NA,width = NULL),
      numericInput(inputId='BMI', label='BMI', value = 28,min = 18, max = 39, 
                   step = NA,width = NULL),
      numericInput(inputId='HOMA', label='HOMA', value = 1.38,min = 0.3, max = 26,
                   step = NA,width = NULL),
      numericInput(inputId='Resistin', label='Resistin', value = 11, min = 3.2, 
                   max = 83, step = NA, width = NULL),
      numericInput(inputId='Glucose', label='Glucose', value = 92, min = 60, 
                   max = 201, step = NA,width = NULL),
      textOutput(outputId = 'Pred'),
      tags$head(tags$style("#Pred{color: red;
                                 font-size: 30px;
                                 font-style: bold;
                           }"))
  )))




server <- function(input, output){
  output$res_plot <- renderPlot({
    ################################### Resistin ###################################
    mod1 <- glm(Classification ~ Resistin, family = binomial(link = "logit"),
                data = dat)
    pred1 <- predict(mod1, newdata = dat, 
                     type = 'response')
    res_plot <- plot(dat$Resistin, pred1, xlim = c(0, 90), pch = 16, col = 'red', 
                     xlab = 'Resistin', ylab = 'Probability of Cancer',
                     cex = 2)
  }, height = 300, width = 400)
  
  output$gluc_plot <- renderPlot({
    ################################# GLUCOSE ######################################
    mod2 <- glm(Classification ~ Glucose, family = binomial(link = "logit"),
                data = dat)
    pred2 <- predict(mod2, newdata = dat, type = 'response')
    gluc_plot <- plot(dat$Glucose, pred2, xlim = c(60, 150), pch = 16, col = "red",
                      xlab = "Glucose Level", ylab = 'Probability of Cancer',
                      cex = 2)
  }, height = 300, width = 400)
  
  output$homa_plot <- renderPlot({
    ################################ HOMA #######################################
    mod3 <- glm(Classification ~ HOMA, family = binomial(link = "logit"),
                data = dat)
    pred3 <- predict(mod3, newdata = dat, type = 'response')
    homa_plot <- plot(dat$HOMA, pred3, xlim = c(0,26), pch = 16, col = "red",
                      xlab = 'HOMA', ylab = 'Probability of Cancer', 
                      cex = 2)
    
  },  height = 500, width = 800)
    ################################ FULL MODEL ####################################
  mod <- glm(Classification ~ Age + BMI + Glucose + Resistin + HOMA, 
             data = dat)
  data <- reactive({
    new_dat  <- data.frame(Age = input$Age, 
                           BMI=input$BMI,
                           Glucose=input$Glucose,
                           Resistin=input$Resistin,
                           HOMA=input$HOMA)
    })
  pred <- reactive({round(predict(mod,data(), type = 'response'), 2)*100
    })
  output$Pred <- renderPrint(paste0("Model predicts probability of Cancer = ",
                                    pred(), '%', "(All Biomakers Used)"))
}

shinyApp(ui=ui, server = server)
