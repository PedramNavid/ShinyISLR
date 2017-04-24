library(shiny)

# 2.1 What is Statistical Learning ----
islr21UI <- function(id, label = "What is Statistical Learning") {
  ns <- NS(id)
  navbarMenu(label, 
    tabPanel("2.1 Advertising",
           sidebarLayout(
             sidebarPanel(radioButtons(ns("var21"), "Outcome Variable",
               c("TV" = "TV",
                 "Radio" = "Radio",
                 "Newspaper" = "Newspaper"
               )
             )),
             
             mainPanel(
               plotOutput(ns("plot21")),
               p("The Advertising data set. The plot displays sales, in thousands
                 of units, as a function of TV, radio, and newspaper budgets, in thousands of
                 dollars, for 200 different markets. In each plot we show the simple least squares
                 fit of sales to that variable, as described in Chapter 3. In other words, each blue
                 line represents a simple model that can be used to predict sales using TV, radio,
                 and newspaper, respectively")
               )
              )),
    
    tabPanel("2.2 Income",
           sidebarLayout(
             sidebarPanel(radioButtons(ns("observed22"), "Select Model",
               c("Observed" = F,
                 "Underlying" = T)
             )),
             
             mainPanel(
               plotOutput(ns("plot22")),
               p("The Income data set. Left: The red dots are the observed values of income
                 (in tens of thousands of dollars) and years of education for 30 individuals.
                 Right: The blue curve represents the true underlying relationship between income
                 and years of education, which is generally unknown (but is known in this case
                 because the data were simulated). The black lines represent the error associated
                 with each observation. Note that some errors are positive (if an observation lies
                 above the blue curve) and some are negative (if an observation lies below the curve).
                 Overall, these errors have approximately mean zero."
                ),
               p("Note: for this Shiny example the underlying distribution was estimated using a loess curve."
                )
               )
           )),
    
  tabPanel("2.3 Income Plane",
           mainPanel(
             plotOutput(ns("plot23")),
             p(
               "The plot displays income as a function of years of education
               and seniority in the Income data set. The blue surface represents the true underlying
               relationship between income and years of education and seniority,
               which is known since the data are simulated. The red dots indicate the observed
               values of these quantities for 30 individuals"
             ),
             p(
               "Note: for this Shiny example the underlying distribution was estimated using a loess curve."
             )
             )),
  
  tabPanel("2.4 - 2.6 Parametric/Non-Parametric Model Fit",
           sidebarLayout(
             sidebarPanel(radioButtons(ns("param24"), "Select Model Type",
               c("Parametric (Linear)" = 'linear',
                 "Non-Parametric (Smooth Spline)" = 'smooth',
                 "Non-Parametric (Rough Spline)" = 'rough')
             )),
             
             mainPanel(
               plotOutput(ns("plot24")),
               p("A linear model fit by least squares to the Income data from Figure 2.3. 
                 The observations are shown in red, and the yellow plane indicates the
                 least squares fit to the data. A smooth thin-plate spline fit to the Income data 
                 from Figure 2.3 is shown in yellow; the observations are displayed in red. 
                 A rough thin-plate spline fit to the Income data from Figure 2.3.
                 This fit makes zero errors on the training data."
                )
               )
              )),
           
  tabPanel("2.8 Unsupervised Learning",
           mainPanel(
             plotOutput(ns("plot28")),
             p("A clustering data set involving three groups. Each group is shown using a different 
               colored symbol. Left: The three groups are well-separated. In
               this setting, a clustering approach should successfully identify the three groups.
               Right: There is some overlap among the groups. Now the clustering task is more
               challenging"
              )
             )
            )
  )
}

# 2.2 Assessing Model Accuracy ----
islr22UI <- function(id, label = "Assessing Model Accuracy") {
  ns <- NS(id)
  navbarMenu(label, 
             tabPanel("2.9 - 2.11 Flexibility",
                      sidebarLayout(
                        sidebarPanel(radioButtons(ns("func29"), "Originating Function",
                                                   c('Polynomial Function (3 Degrees)' = 'func1', 
                                                     'Polynomial Function (2 Degrees)' = 'func2',
                                                     'Polynomial Function (4 Degrees)' = 'func3')),
                                      sliderInput(ns("df29"), "Degrees of Freedom", 
                                                     min = 1, max = 20, value = 1, step = 1, round = TRUE)
                        ),
                        mainPanel(
                          plotOutput(ns("plot29"))
                        )
                      )
             )
)
}

islr31UI <- function(id, label = "Simple Linear Regression") {
  ns <- NS(id)
  navbarMenu(label,
    tabPanel("3.1 Simple Linear Regression",
      sidebarLayout(
        sidebarPanel(
          sliderInput(ns("bhat0"), "Estimate for Intercept (B0)",
            min = 5, max = 9, value = 7, step = 0.5, round = TRUE)
          ,
          sliderInput(ns("bhat1"), "Estimate for TV Coefficient (B1)",
            min = 0.03, max = 0.06, value = 0.048, step = 0.002)
        ),
      mainPanel(
        plotOutput(ns("plot31")),
        textOutput(ns("RSS"))
      )
    )
  ))
}

# UI Calls ----
shinyUI(navbarPage(
  "ISLR 2",
  islr21UI("21"),
  islr22UI("22"),
  islr31UI("31")
  )
  
)