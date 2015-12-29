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
  expect_equal(dim(lv1$level_1_testing), c(50,2))
})

test_that("An error is thrown when n_folds == 1",{
  model_wrapper_1 <- function(training_frame, validation_frame)
    runif(nrow(validation_frame), 1,2)
  expect_error(get_level_1_data(iris, iris, "Petal.Length", n_folds = 1,
                                model_wrappers = c(model_wrapper_1)),
               "n_folds must be at least 2 for stacking to work correctly.")
})

test_that("If training_frame already has a cv column, that is used instead of a new one being defined",{
  training_frame <- get_cv_folds(iris, n_folds = 5)
  model_wrapper_1 <- function(training_frame, validation_frame)
    runif(nrow(validation_frame), 1,2)
  #Since each tick mark represents a fold, there should be a series of
  #five tickmarks in the output but not 10
  expect_output(get_level_1_data(training_frame, model_wrappers = c(model_wrapper_1),
                                  response = "Petal.Length", n_folds = 10),
                 "|=====|", fixed = TRUE)

})

test_that("A simple linear model with iris dataset works.",{
  set.seed(1)
  iris_model_wrapper <- function(training_frame, validation_frame){
    linear_model <- lm(Petal.Length ~ ., data = training_frame)
    predict(linear_model, newdata = validation_frame) #the output
  }
  iris_training <- iris[1:100,-5]
  iris_testing <- iris[101:150,-5]
  #define a CV fold column for iris_training so we can use it for all future
  #model wrappers.
  iris_training <- get_cv_folds(iris_training, n_folds = 10)
  lv1_data <- get_level_1_data(iris_training,
                               response = "Petal.Length",
                               model_wrappers = c(iris_model_wrapper = iris_model_wrapper),
                               testing_frame = iris_testing)
  expect_false(anyNA(lv1_data$level_1_training))
  expect_equal(head(lv1_data$level_1_training$iris_model_wrapper),
               c(1.415958, 1.521141, 1.354233, 1.367999, 1.311796, 1.756137),
               tolerance = 1e-6)
})
