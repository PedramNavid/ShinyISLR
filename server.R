# Global setup ----
library(shiny)
library(ggplot2)
library(ISLR)
library(splines)
library(gridExtra)
source("theme.R")

# Set theme
theme_set(theme_islr)

# Read Datasets
Advertising <- read.csv("data/Advertising.csv")
Income <- read.csv("data/Income1.csv")
Income2 <- read.csv("data/Income2.csv")

# 2.1 What is Statistical Learning ----
islr21 <- function(input, output, session) {

  # Create model for Income
  mdl_income <- loess(Income ~ Education, data = Income, span = 0.8)
  mdl_income2_loess <- loess(Income ~ Education + Seniority, data=Income2)
  mdl_income2_linear <- lm(Income ~ Education + Seniority, data = Income2)
  mdl_income2_spline <- lm(Income ~ splines::ns(Education, df = 4) + splines::ns(Seniority, df = 4), Income2)
  mdl_income2_spline_rough <- lm(Income ~ splines::ns(Education, df = 13) + splines::ns(Seniority, df = 13), Income2)

  Income$pred <- predict(mdl_income)
  
  # Function to create 3d plot using a model for Income dataset
  plot_3d_model <- function(model, data=Income2, theta=25, phi=25, col="lightblue") {
    # With thanks to :https://tinyurl.com/lvlm6oz
    x <- range(Income2$Education)
    x <- seq(x[1], x[2], length.out=50)    
    y <- range(Income2$Seniority)
    y <- seq(y[1], y[2], length.out=50)
    
    z <- outer(x,y, 
               function(Education,Seniority)
                 predict(model, data.frame(Education,Seniority)))
    
    p <- persp(x,y,z, theta=theta, phi=phi, 
               col=col,expand = 0.5,shade = 0.2,
               xlab="Years of Education", ylab="Seniority", zlab="Income")
    
    obs <- trans3d(Income2$Education, Income2$Seniority,Income2$Income,p)
    pred <- trans3d(Income2$Education, Income2$Seniority,fitted(model),p)
    points(obs, col="red",pch=16)
    segments(obs$x, obs$y, pred$x, pred$y)
  }
  
  output$plot21 <- renderPlot({
    
    ggplot(Advertising, aes_(as.name(input$var21), quote(Sales))) + 
      geom_point(shape=1, color='red') +
      geom_smooth(method='lm', se=FALSE, color = 'darkblue') +
      scale_y_continuous(limits = c(0,30), breaks = seq(0,30,5))
    
  })
  
  output$plot22 <- renderPlot({
    
    if(input$observed22) {
      ggplot(Income, aes(Education,Income)) + 
        geom_point(color='red') +
        geom_smooth(method='loess', se=FALSE, color = 'black', span = 0.8) +
        geom_segment(aes(xend = Education, yend = pred), alpha = .2) +
        scale_x_continuous(breaks=seq(10,22,2)) +
        scale_y_continuous(breaks=seq(20,80,10)) 
    } else
    {
      ggplot(Income, aes(Education,Income)) + 
        geom_point(color='red') +
        scale_x_continuous(breaks=seq(10,22,2)) +
        scale_y_continuous(breaks=seq(20,80,10))
    }
    
  })
  
  output$plot23 <- renderPlot({
    plot_3d_model(mdl_income2_loess)
  })
  
  output$plot24 <- renderPlot({
    
    if(input$param24=="linear")
      plot_3d_model(mdl_income2_linear)
    if(input$param24=='smooth')
      plot_3d_model(mdl_income2_spline, col='yellow')
    if(input$param24=='rough')
      plot_3d_model(mdl_income2_spline_rough, col='yellow')
    
  })
}

# 2.2 Asessing Model Accuracy ----
islr22 <- function(input, output, session) {
  q <- 1:100
  
  # Function generator
  get_function <- reactive({
   
    switch(input$func29,
           "func1" = (8000 + 20*q - 5*q^2 + 0.045*q^3) / 1000,
           "func2" = 2 + 0.05*q + 0.00006*q^2,
           "func3" = -(10 - 10*q + 10.5*q^2 - 0.54*q^3 + 0.00435*q^4) / 1500
    )
    
    
  })
  
  # Create random dataset from function
  create_data <- function() {
    set.seed(1234)
    
    f <- get_function()
    
    noise <- rnorm(length(q), mean = 0, sd = 1)
    data <- data.frame(x = q, y = f + noise)
    
    idx <- sample(q, length(q) * 0.5, replace = F)
    train <- data[idx,]
    test <- data[-idx,]
    list(
      train = train,
      test = test
    )
  }
  
  # Creates a train and test model and calculates MSE
  create_model <- function(train, test, power = 2) {
    
    fml <- as.formula(y ~ poly(x, power))
    fit_lm <- lm(fml, data = train)
    
    pred_lm <- predict(fit_lm)
    train_mse <- round(Metrics::mse(pred_lm, train$y), 3)
    
    # Test MSE
    pred_lm_test <- predict(fit_lm, newdata = test)
    test_mse <- round(Metrics::mse(pred_lm_test, test$y), 3)
    
    list(
      fml = fml,
      power = power,
      train_mse = train_mse,
      test_mse = test_mse,
      train = train,
      test = test
    )
  }
  
  output$plot29 <- renderPlot({
    data <- create_data()
    mdl <- create_model(data$train, data$test, power = input$df29)
    fit = lm(mdl$fml, data = mdl$train)
    pred_train = predict(fit)
    
    lower_bound <- floor(min(mdl$train$y, mdl$test$y))
    upper_bound <- ceiling(max(mdl$train$y, mdl$test$y))
    
    p1 <- ggplot(mdl$train, aes(x, y)) + 
      geom_point() + 
      geom_smooth(method='lm', se = F, formula = mdl$fml, color = 'orange') +
      geom_segment(aes(xend = x, yend = pred_train), color = 'grey80') +
      ylim(lower_bound, upper_bound) +
      geom_label(aes(x = 25, y = lower_bound, 
                     label = paste0("Train MSE: ", mdl$train_mse)))
    
    pred_test= predict(fit, newdata = mdl$test)
    
    p2 <- ggplot(mdl$test, aes(x, y)) + 
      geom_point() + 
      geom_smooth(method='lm', se = F, formula = mdl$fml, data = mdl$train, color = 'orange') +
      geom_segment(aes(xend = x, yend = pred_test), color = 'grey80') +
      ylim(lower_bound, upper_bound) + 
      geom_label(aes(x = 25, y = lower_bound, 
                     label = paste0("Test MSE: ", mdl$test_mse)))
    
    gridExtra::grid.arrange(p1, p2, ncol = 2)
    
  })
  
  
}

# 3.1 Simple Linear Regression ----
islr31 <- function(input, output, session) {
  output$plot31 <- renderPlot({qplot(1,2)})
  output$RSS <- renderText({123})
}

# Server Call ----
shinyServer(function(input, output, session) {

  callModule(islr21, "21")
  callModule(islr22, "22")
  callModule(islr31, "31")

})
