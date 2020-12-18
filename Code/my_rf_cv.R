#' Random Forest Cross-Validation
#'
#' The purpose of this function is to test how good the random forest model fits in the
#'   penguins dataset.
#'
#' @param k number of folds
#'
#' @keywords prediction
#'
#' @return a numeric with the cross-validation error
#'
#' @import randomForest
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # Get the penguins data frame and remove the NA values
  penguins_data <- na.omit(my_penguins[, 3:6])
  fold <- sample(rep(1:k, length = nrow(penguins_data)))
  # Add the "split" column to the data frame
  penguins_data$split <- fold

  mse <- rep(NA, k)

  for (i in 1:k) {
    # Use one fold as test data while using all other folds as the training data.
    data_train <- penguins_data %>% dplyr::filter(split != i)
    data_test <- penguins_data %>% dplyr::filter(split == i)

    # Train a random forest model with 100 trees to predict body_mass_g
    ran_forest_model <- randomForest(body_mass_g ~ bill_length_mm +
                                       bill_depth_mm +
                                       flipper_length_mm,
                                     data_train, ntree = 100)
    # Make prediction
    pred <- predict(ran_forest_model, data_test[, 1:3])
    # Evaluate teh average squared difference
    mse[i] <- mean((pred - data_test$body_mass_g)^2)
  }
  return(mean(mse))
}

my_rf_cv(2)
