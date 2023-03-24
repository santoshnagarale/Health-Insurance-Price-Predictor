library(shiny)
ui <- fluidPage(
  titlePanel("Health Insurance Price Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age:", 25, min = 0, max = 100),
      numericInput("bmi", "BMI:", 25, min = 0, max = 100),
      numericInput("sex", "SEX:(Male=1, Female=0)", 0, min = 0, max = 1),
      numericInput("smoker", "SMOKER: (Yes=1, No=0)", 0, min = 0, max = 1),
      numericInput("region", "REGION:(East=0, West=1, South=2, North=3)", 0, min = 0, max = 3),
      
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput("prediction_plot"),
      textOutput("prediction"),
      plotOutput("ggplot1"),
      plotOutput("ggplot2"),
      plotOutput("ggplot3"),
      plotOutput("ggplot4"),
      
      plotOutput("ggplot5")
      
    )
  )
)

server <- function(input, output) {
  
  # Use eventReactive to only perform the prediction when the submit button is clicked
  prediction <- eventReactive(input$submit, {
    # Load the insurance dataset
    data<- read.csv("C:\\Users\\snaga\\OneDrive\\Desktop\\sem-2\\Programming for spatial data\\insurance_encoded.csv")
    
    # Fit a linear regression model
    fit <- lm(charges ~ age + bmi+ sex + smoker + region, data =data)
    
    # Use the model to make a prediction based on the input values
    predict(fit, newdata = data.frame(age = input$age, bmi = input$bmi, sex=input$sex, smoker=input$smoker, region=input$region))
  })
  
  # Display the prediction
  output$prediction <- renderText({
    paste("The predicted health insurance price is ₹", round(prediction(), 2))
  })
  
  # Display the plot
  output$prediction_plot <- renderPlot({
    # Load the insurance dataset
    data<- read.csv("C:\\Users\\snaga\\OneDrive\\Desktop\\sem-2\\Programming for spatial data\\insurance_encoded.csv")
    
    # Fit a linear regression model
    fit <- lm(charges ~ age + bmi + sex + smoker + region, data = data)
    
    # Plot the model predictions against the observed data
    plot(data$age, data$charges, col = "green", pch = 16)
    abline(fit, col = "blue")
    points(input$age, prediction(), col = "red", pch=16)
    
  })
  
  output$ggplot1 <- renderPlot({
    # Load the insurance dataset
    data<- read.csv("C:\\Users\\snaga\\OneDrive\\Desktop\\sem-2\\Programming for spatial data\\insurance_encoded.csv")
    
    library(ggplot2)
    ggplot(data,aes(children,charges))+geom_point(col="blue")+labs(title="Number of Dependants vs. Charges")+xlab("No.of Children")+ylab("Charges")
    
  })
  
  output$ggplot2 <- renderPlot({
    # Load the insurance dataset
    data<- read.csv("C:\\Users\\snaga\\OneDrive\\Desktop\\sem-2\\Programming for spatial data\\insurance_encoded.csv")
    
    library(ggplot2)
    ggplot(data,aes(x=age, y=charges, color=smoker))+geom_point()+labs(title="Age*Smoker vs. charges")+xlab("AGE")+ylab("Charges")
    
  })
  
  output$ggplot3 <- renderPlot({
    # Load the insurance dataset
    data<- read.csv("C:\\Users\\snaga\\OneDrive\\Desktop\\sem-2\\Programming for spatial data\\insurance_encoded.csv")
    
    library(ggplot2)
    ggplot(data,aes(x=bmi, y=charges, color=smoker))+geom_point()+
      labs(title="BMI*Smoker vs. Expenses")+xlab("BMI")+ylab("Expenses")
  })
  
  output$ggplot4 <- renderPlot({
    # Load the insurance dataset
    data<- read.csv("C:\\Users\\snaga\\OneDrive\\Desktop\\sem-2\\Programming for spatial data\\insurance_encoded.csv")
    
    library(ggplot2)
    ggplot(data,aes(as.factor(sex),charges,fill=sex))+geom_boxplot()+labs(title="Gender ~ Charges")+xlab("Sex")+ylab("Charges")
    
  })
  
  
  output$ggplot5 <- renderPlot({
    # Load the insurance dataset
    data<- read.csv("C:\\Users\\snaga\\OneDrive\\Desktop\\sem-2\\Programming for spatial data\\insurance_encoded.csv")
    
    data1<-data.frame(
      bmi=c(data$bmi),
      children=c(data$children),
      age=c(data$age),
      sex=c(data$sex),
      smoker=c(data$smoker),
      region=c(data$region),
      charges=c(data$charges)
      
    )
    
    #correlation Matrix of all the variables
    
    library("corrplot")
    corrplot(cor(data1),method="circle",title="Correlation Matrix")
    
    
  })
  
  
  
  
}
shinyApp(ui, server)




