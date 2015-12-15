#Add CV folds to the input data frame.
get_cv_folds <- function(training_frame, n_folds){
  #Check to see if training_frame already has a CV column.  If not, add one.
  if(is.null(attributes(training_frame)$CV_column)){
    num_obs <- nrow(training_frame)
    # Cross-validation folds
    folds <- sample(rep(seq(n_folds), ceiling(num_obs/n_folds)))[1:num_obs]
    training_frame$fold_id <- folds
    attributes(training_frame)$CV_column <- "fold_id"
  }
  training_frame
}


get_level_1_data <- function(training_frame, model_wrappers, n_folds = 5){
  #Add a folds column to keep track of CV folds.
  training_frame <- get_cv_folds(training_frame, n_folds = n_folds)
  setDT(training_frame)

  for(model_wrapper in model_wrappers){
    for(fold in 1:n_folds){

    }
  }
}
