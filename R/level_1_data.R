#' Add a cv folds column to a data frame.
#'
#' @param training_frame data frame, the frame to which to
#' append the cv folds column
#' @param n_folds integer, number of folds
#'
#' @return A data frame identical to training frame with one extra column
#' called "fold_id" that gives the fold of each column.  Also, the attribute
#' "cv_column" is added to indicate that a cv column has been defined.  If
#' the "cv_column" attribute is already present, training_frame is returned
#' unchanged.
#' @export
#'
#' @examples
#' get_cv_folds(iris, 5)
get_cv_folds <- function(training_frame, n_folds){
  #Check to see if training_frame already has a CV column.  If not, add one.
  if(!n_folds > 1 | !is.integer(n_folds))
    stop("n_folds must be an integer greater than 1.")
  if(is.null(attributes(training_frame)$cv_column)){
    num_obs <- nrow(training_frame)
    # Cross-validation folds
    folds <- sample(rep(seq(n_folds), ceiling(num_obs/n_folds)))[1:num_obs]
    training_frame$fold_id <- folds
    attributes(training_frame)$cv_column <- "fold_id"
  }
  training_frame
}


#' Internal function to get level 1 data to stack on.
#'
#' This function does the work for get_level_1_data.  See the documentation
#' for get_level_1_data for details on the functionality.
#'
#' @param training_frame
#' @param response
#' @param model_wrappers
#' @param n_folds
#' @import data.table
#' @return
#'
#' @examples
.get_level_1_data <- function(training_frame,
                              response,
                              model_wrappers,
                              testing_frame = NULL,
                              n_folds = 5){
  if(n_folds == 1 & is.null(testing_frame))
    stop("If n_folds is 1, testing_frame must be provided.")

  #Add a folds column to keep track of CV folds if it's not present already.
  training_frame <- get_cv_folds(training_frame, n_folds = n_folds)
  train <- as.data.table(training_frame)
  test <- as.data.table(testing_frame)

  for(i in 1:length(model_wrappers)){
    model_wrapper <- names(model_wrappers)[i] #The name of the wrapper.
    if(is.null(model_wrapper))
      model_wrapper <- paste0("Model_", i)
    wrapper_function <- model_wrappers[[i]] #The wrapper itself.
    #track progress with bar.
    if(n_folds != 1){
      cat(paste0("Cross validating and training ", model_wrapper,
                 ". Each tick represents a cross validation.\n" ))
    } else {
      cat(paste0("Generating level 1 data for test set using ", model_wrapper, "\n"))
    }
    progress <- txtProgressBar(max = n_folds, width = n_folds, style = 3)

    #Each loop adds predictions for the fold under consideration, while all
    #subsequent folds have NA placeholders.  Each time the loop repeats the NA's
    #for the current fold are overwritten (by reference, thanks data.table!).
    #You can watch this process occur with the debugger.
    for(fold in 1:n_folds){
      #If n_folds is at least two, the validation frame will be the fold
      #outside the training set.
      if(n_folds != 1){
        train[fold_id == fold,
              (model_wrapper) := wrapper_function(training_frame = train[fold_id != fold,],
                                                  validation_frame = train[fold_id == fold,])]
      } else {
        test[, (model_wrapper) := wrapper_function(training_frame = train,
                                                   validation_frame = testing_frame)]
      }
      setTxtProgressBar(progress, fold)
    }
    close(progress)
  }
  if(n_folds != 1){
    train[,(setdiff(names(training_frame), response)) := NULL] #Remove original columns
    setDF(train)
    output <- train
  } else {
    test[,(setdiff(names(testing_frame), response)) := NULL] #Remove original columns
    setDF(test)
    output <- test
  }
  output
}

#' Get level 1 training and testing data
#'
#' @param training_frame
#' @param testing_frame
#' @param response string, name of the response column
#' @param model_wrappers (named) vector or list of functions. Names are optional, and
#' in their absence names will be automatically generated.  See details below
#' for defining model wrappers.
#' @param n_folds integer, number of cross-validation folds.  May be omitted if
#' training_frame already contains a fold column defined by get_cv_folds
#'
#' @return
#' @export
#'
#' @examples
get_level_1_data <- function(training_frame,
                             testing_frame,
                             response,
                             model_wrappers,
                             n_folds = 5){
  if(is.null(attributes(training_frame)$cv_column) & !n_folds > 1)
    stop("n_folds must be at least 2 for stacking to work correctly.")
  #Check for a folds column in training frame and use that instead of making a
  #new one.
  if(!is.null(attributes(training_frame)$cv_column)){
    print(paste("Detected a cv fold column already present in training_frame. Ignoring n_folds argument and using",
                attributes(training_frame)$cv_column,
                "as the folds column."))
    #Get the number of folds previously defined.
    n_folds <- max(training_frame[[attributes(training_frame)$cv_column]])
  }
  level_1_training <- .get_level_1_data(training_frame =  training_frame,
                                        response = response,
                                        model_wrappers = model_wrappers,
                                        n_folds = n_folds)
  level_1_testing <- .get_level_1_data(training_frame = training_frame,
                                       response = response,
                                       model_wrappers = model_wrappers,
                                       n_folds = 1,
                                       testing_frame = testing_frame)
  list(level_1_training = level_1_training, level_1_testing = level_1_testing)
}
