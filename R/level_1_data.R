#Add CV folds to the input data frame.
get_cv_folds <- function(training_frame, n_folds){
  #Check to see if training_frame already has a CV column.  If not, add one.
  if(is.null(attributes(training_frame)$cv_column)){
    num_obs <- nrow(training_frame)
    # Cross-validation folds
    folds <- sample(rep(seq(n_folds), ceiling(num_obs/n_folds)))[1:num_obs]
    training_frame$fold_id <- folds
    attributes(training_frame)$cv_column <- "fold_id"
  }
  training_frame
}


get_level_1_data <- function(training_frame,
                             response,
                             model_wrappers,
                             n_folds = 5){
  #Add a folds column to keep track of CV folds if it's not present already.
  training_frame <- get_cv_folds(training_frame, n_folds = n_folds)
  dt <- as.data.table(training_frame)

  for(model_wrapper in model_wrappers){
    #track progress with bar.
    cat(paste0("Cross validating and training ", model_wrapper,
               ". Each tick represents a cross validation.\n" ))
    progress <- txtProgressBar(max = n_folds, width = n_folds, style = 3)

    #Each loop adds predictions for the fold under consideration, while all
    #subsequent folds have NA placeholders.  Each time the loop repeats the NA's
    #for the current fold are overwritten (by reference, thanks data.table!).
    #You can watch this process occur with the debugger.
    for(fold in 1:n_folds){
      dt[fold_id == fold,
         (model_wrapper) := match.fun(model_wrapper)(training_frame = dt[fold_id != fold,],
                                                     validation_frame = dt[fold_id == fold,])]
      setTxtProgressBar(progress, fold)
    }
    close(progress)
  }
}
