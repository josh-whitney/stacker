library(stacker)

#For some reason, this next test doesn't work.
test_that("Training and testing sets for iris have the correct dimension",{
  set.seed(1)
  model_wrapper_1 <- function(training_frame, validation_frame)
    runif(nrow(validation_frame), 1,2)
  model_wrapper_2 <- function(training_frame, validation_frame)
    runif(nrow(validation_frame), 2,3)
  training <- iris[iris$Species %in% c("virginica", "versicolor"),]
  testing <- iris[!iris$Species %in% c("virginica", "versicolor"),]
  lv1 <- get_level_1_data(training, testing_frame = testing, response = "Petal.Length",
                          n_folds = 5,
                          model_wrappers = c(model_wrapper_1, model_wrapper_2))
  expect_equal(dim(lv1$level_1_training), c(100,3))
  expect_equal(dim(lv1$level_1_testing), c(50,3))
})

test_that("An error is thrown when n_folds == 1",{
  model_wrapper_1 <- function(training_frame, validation_frame)
    runif(nrow(validation_frame), 1,2)
  expect_error(get_level_1_data(iris, iris, "Petal.Length", n_folds = 1,
                                model_wrappers = c(model_wrapper_1)),
               "n_folds must be at least 2 for stacking to work correctly.")
})