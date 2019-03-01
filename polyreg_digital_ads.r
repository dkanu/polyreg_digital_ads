###############################
## Imports
###############################
imports <- c('ggplot2','scales','rio')
invisible(lapply(imports, require, character.only = TRUE))
##########################################
## HELPER FUNCTIONS
##########################################
EstimateMetric <- function(B, x){
  # Use a second order polynoial function to estimate a metric
  # Args:
  #   B: Vector of estimated coefficients used in second order polynomial regression
  #   x: Integer representing spend level
  round(B[1] + (B[2]*x) + (B[3]*(x)^2))
}
ObjectiveFunction <- function(B, x){
  # Define second order polynomial as an objective function. This function will be used for optimization.
  # Args: 
  #   B: Vector of estimated coefficients used in second order polynomial regression
  #   x: Integer representing spend level
  B[1] + (B[2]*x) + (B[3]*(x)^2)
}
FitPolynomial <- function(dat, y, x, d = 2, xlabel = 'x', ylabel = 'y', 
                          title='title', sub.title="x vs. y", caption = "test data"){
  # Perfrom second order polynomial regression of y on x.
  # Args:
  #   dat: data frame containg variables of interest
  #   y: string provding the column name of the dependent variable
  #   x: string providing the column name of the independent variable
  #   d: degree of polynomial to be estimated
  y <- dat[[y]]
  x <- dat[[x]]
  
  poly.fit <- lm(y ~ poly(x, degree = d, raw=TRUE)) # Second order polynomial regression model
  
  plot.out <- ggplot(dat, aes(y=y, x=x)) + # Scatter plot and regression line
    geom_point(alpha = .5) +
    stat_smooth(method="lm", formula = y  ~ poly(x, d, raw = TRUE), se=FALSE)+
    scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + theme_minimal() +
    labs(subtitle = sub.title, y = ylabel, x = xlabel, title = title, caption = caption)
  
  list(poly.fit, plot.out)
}
###############################
## Regression, Estimation, and Optimization
###############################
yourdata <- import('path_to_your_data')
y <- 'Impressions'  #name of column containing dependent variable
x <- 'Revenue (Adv Currency)'  #name of column containing independet variable

fit <- FitPolynomial(yourdata, y, x, d = 2) #Fit the model
fit[[2]] #Plot function
#Optimize the objective function given estimated coefficients
fit.optim <- optimize(ObjectiveFunction, B = unname(coef(fit[[1]])), interval = c(100, 10000), maximum = TRUE)
fit.optim
#Estimate value of the objective function at the maximum
EstimateMetric(B = unname(coef(fit[[1]])), x = fit1.optim$maximum)
