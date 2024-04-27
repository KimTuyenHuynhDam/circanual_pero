library(glmnet)  # for elastic net regression
library(caret)   # general modeling and machine learning
library(tidyverse)
library(readxl)
library(broom)

setwd("~/Library/CloudStorage/OneDrive-UniversityofSouthCarolina/Kiaris lab/peromyscus/data for MS seasonality 2024/circanual_2024/circanual/methylation")

# Reading and preparing the data
normalized_betas_sesame <- read_xlsx("normalized_betas_sesame.xlsx")
SS <- read_xlsx("all_DNAm_mice_all_tissues.xlsx") %>%
  filter(CanBeUsedForAgingStudies == 'yes')

# Preparing the data: example of merging and structuring
data_merged <- normalized_betas_sesame %>%
  pivot_longer(!CGid, names_to = "Basename", values_to = "ProMet") %>%
  pivot_wider(names_from = CGid, values_from = ProMet) %>%
  inner_join(SS) %>%
  # there is NO universal naming scheme for "loci", but these three letters seem to cover it
  pivot_longer(starts_with(c("cg","rs","ch")),names_to = "CGnum" , values_to = "ProMet")



# Elastic Net Model
set.seed(123)  # for reproducibility
model_fit <- train(
  ProMet ~ Age + Tissue, data = data_merged,
  method = "glmnet",
  trControl = trainControl("cv", number = 10),  # 10-fold cross-validation
  tuneLength = 10
)

# Checking the variable importance
importance <- varImp(model_fit, scale = FALSE)

# Plotting the most important CpGs
plot(importance)


myscan <- data_merged %>%
  mutate(
    Sex = as.factor(Sex), 
    SpeciesAbbreviation = as.factor(SpeciesAbbreviation),
    Tissue = as.factor(Tissue)
  ) %>%
  group_by(CGnum) %>%
  nest() %>%
  mutate(model_elasticnet = map(data, ~ {
    clean_data <- .x %>%
      filter(!is.na(Age) & !is.na(ProMet) & !is.na(Sex) & !is.na(SpeciesAbbreviation) & !is.na(Tissue))
    
    if(nrow(clean_data) > 10) {
      model_matrix <- model.matrix(ProMet ~ Age + Sex + SpeciesAbbreviation + Tissue - 1, data = clean_data)
      response_vector <- clean_data$ProMet
      
      # Fitting the model within tryCatch to handle potential errors gracefully
      cv_model <- tryCatch({
        cv.glmnet(model_matrix, response_vector, alpha = 0.5)  # alpha=0.5 for elastic net, no comma needed here
      }, error = function(e) {
        warning(paste("Failed to fit model for CGnum:", unique(clean_data$CGnum), "- Error:", e$message))
        return(NULL)  # return NULL on error in model fitting
      })
      return(list(cv_model=cv_model))  # Ensure cv_model is always wrapped in a list
    } else {
      warning(paste("Insufficient data for CGnum:", unique(clean_data$CGnum)))
      return(list(cv_model=NULL))  # Explicitly return a list with NULL if data is insufficient
    }
  })) %>%
  filter(!map_lgl(model_elasticnet, ~ is.null(.x$cv_model)))  # Remove groups where model is NULL

# Example of extracting results safely
results <- myscan %>%
  mutate(
    lambda_min = map_dbl(model_elasticnet, ~ {
      if (!is.null(.x$cv_model) && !is.null(.x$cv_model$lambda.min)) {
        return(.x$cv_model$lambda.min)
      } else {
        return(NA_real_)
      }
    }),
    lambda_1se = map_dbl(model_elasticnet, ~ {
      if (!is.null(.x$cv_model) && !is.null(.x$cv_model$lambda.1se)) {
        return(.x$cv_model$lambda.1se)
      } else {
        return(NA_real_)
      }
    })
  )

# Assuming `results` has been computed as previously described
coef_min <- map(results$model_elasticnet, ~ coef(.x$cv_model, s = "lambda.min"))
coef_1se <- map(results$model_elasticnet, ~ coef(.x$cv_model, s = "lambda.1se"))

# Example of looking at coefficients for the first CGnum (for simplicity in demonstration)
print(coef_min[[1]])  # Coefficients at lambda.min
print(coef_1se[[1]])  # Coefficients at lambda.1se


library(glmnet)
library(ggplot2)

# Function to plot coefficient paths
library(glmnet)
library(ggplot2)

# Revised function to plot coefficient paths
plot_coefficient_paths <- function(cv_model) {
  if (is.null(cv_model)) return(NULL)  # Skip if the model is NULL
  
  # Extracting the coefficient matrix at a sequence of lambda values used in the model
  lambda_sequence <- cv_model$lambda
  coefficient_matrix <- as.matrix(coef(cv_model, s = lambda_sequence))
  
  # Preparing the data for ggplot
  df_coef <- as.data.frame(coefficient_matrix[-1, , drop = FALSE])  # Exclude the intercept row
  names(df_coef) <- lambda_sequence
  df_coef$Feature <- rownames(df_coef)  # rownames here are the variable names
  long_coef_df <- reshape2::melt(df_coef, id.vars = "Feature", variable.name = "Lambda", value.name = "Coefficient")
  long_coef_df$Lambda <- as.numeric(as.character(long_coef_df$Lambda))
  
  # Creating the plot
  return(ggplot(long_coef_df, aes(x = log(Lambda), y = Coefficient, color = Feature)) +
           geom_line() +
           labs(title = "Coefficient Path per Lambda", x = "Log(Lambda)", y = "Coefficient") +
           theme_minimal())
}

# Apply the plotting function safely to each model
myscan$coef_path_plots <- lapply(myscan$model_elasticnet, function(x) {
  tryCatch({
    plot_coefficient_paths(x$cv_model)  # Ensure to access cv_model correctly
  }, error = function(e) {
    warning("Failed to plot coefficient paths; returning NULL.")
    return(NULL)
  })
})


# Function to plot cross-validated errors
plot_cv_errors <- function(cv_model) {
  if (is.null(cv_model)) return(NULL)  # Handle NULL models gracefully
  
  df_errors <- data.frame(Lambda = cv_model$lambda, MSE = cv_model$cvm)
  ggplot(df_errors, aes(x = log(Lambda), y = MSE)) +
    geom_line(color = "blue") +
    geom_point(aes(x = log(cv_model$lambda.min), y = min(cv_model$cvm)), color = "red", size = 2) +
    geom_point(aes(x = log(cv_model$lambda.1se), y = cv_model$cvm[which.min(cv_model$cvm)]), color = "green", size = 2) +
    labs(title = "CV Error vs. Log(Lambda)", x = "Log(Lambda)", y = "Mean Squared Error") +
    theme_minimal()
}

# Apply plotting functions to each model and store the plots for easy access

myscan$cv_error_plots <- lapply(myscan$model_elasticnet, plot_cv_errors)

# Viewing plots in RStudio
# You can click through the list elements in RStudio's viewer or use the following to render in the Plots pane:
if (!is.null(myscan$coef_path_plots[[1]])) print(myscan$coef_path_plots[[1]])  # Replace 1 with other indices as needed
if (!is.null(myscan$cv_error_plots[[1]])) print(myscan$cv_error_plots[[1]])  # Replace 1 with other indices as needed

##########
# Hypothetical scenario where we also have se's available
library(glmnet)
library(dplyr)

# Assuming myscan is already populated with cv.glmnet models
myscan2 <- myscan %>%
  mutate(age_impact = map(model_elasticnet, ~ {
    if (!is.null(.x$cv_model)) {
      # Extract coefficients at lambda.1se
      coeffs <- coef(.x$cv_model, s = "lambda.1se")
      # Coefficients return as a named vector, extract 'Age' if it exists
      if ("Age" %in% names(coeffs)) {
        return(as.numeric(coeffs["Age"]))
      } else {
        return(NA_real_)  # Return NA if Age is not a predictor in the model
      }
    } else {
      return(NA_real_)  # Return NA if the model is NULL
    }
  }))

