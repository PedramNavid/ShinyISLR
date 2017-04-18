
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

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

# Refactor section
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
shinyServer(function(input, output, session) {

  callModule(islr21, "21")
  callModule(islr22, "22")


})
