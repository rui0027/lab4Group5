### this is an example about how to use this package

## IF NOT DOWNLOAD YET:
# library(devtools)
# devtools::install_github("Crispy-tuna/lab4Group5/lab4Group5")
## package name is lab4Group5，which is on the Crispy-tuna/lab4Group5/lab4Group5

### load package
library(lab4Group5)
library(ggplot2)

# create a linreg object
linreg_object <- linreg(Petal.Length~Species, data = iris)

# print()
linreg_object$print()

# plot()
linreg_object$plot()

# resid() which will return the vector of residuals e.
linreg_object$resid()

# return the predicted values y hat
linreg_object$pred()

# return the coefficients as a named vector.
linreg_object$coef()

# Supplementary information about linear models
linreg_object$summary()

