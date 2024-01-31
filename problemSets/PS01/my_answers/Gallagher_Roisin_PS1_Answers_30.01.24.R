#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stringr", "datasets", "stats", "graphics", "grDevices", "utils", "methods", "base"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
ks_test <- function(data) {
  # Create the empirical CDF
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  # Calculate test statistic
  D <- max(abs(empiricalCDF - pnorm(data)))
  
  # Compute the p-value 
  p_value <- 2 * exp(-2 * D^2 * length(data))
  
  return(list(D = D, p_value = p_value))
}

# Set seed for reproducibility
set.seed(123)

# Generate 1000 Cauchy random variables
data <- rcauchy(1000, location = 0, scale = 1)

# Perform the Kolmogorov-Smirnov test
test_results <- ks_test(data)
print(test_results)

#####################
# Problem 2
#####################
# Create the data
set.seed(123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75 * data$x + rnorm(200, 0, 1.5)
plot(data$x, data$y)

# Estimate the model using lm for comparison
result_lm <- lm(y ~ x, data = data)


# Define the cost function for OLS
cost_function <- function(params, data) {
  predictions <- params[1] + params[2] * data$x
  sum((data$y - predictions)^2)
}


# Initial parameter guesses for Newton-Raphson method
initial_params <- c(0, 0)

# Use BFGS method in optim to minimize the cost function
result_bfgs <- optim(initial_params, cost_function, data = data, method = "BFGS")


# Compare the results
print("BFGS results:")
print(result_bfgs$par)
print("LM results:")
print(coef(result_lm))
