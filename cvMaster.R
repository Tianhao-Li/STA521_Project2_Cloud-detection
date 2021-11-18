library(caret)
library(data.table)
source("dataSplit.R")
library(pROC)


missclass_rate = function(y_true,y_pred) {
  return (mean(y_true != y_pred))
}

accuracy = function(y_true,y_pred) {
  return (mean(y_true == y_pred))
}


cvMaster = function(trainData, label_col = "label", folds=5, classifier="qda", metric="Accuracy",
                   split=NA, split_param=c(), tune_param = c(-1), verbose = T, seed=520) {
   set.seed(seed)
   # Dealing input is a list of images instead of pre-splitted blocks
   # Use the inputed split functions to split data
   # requires split_param input
   if (!is.na(split)) {
     # expecting input as a list of images
     if (length(split_param) <= 0) {
       stop("Please input split parameter")
     }
     
     if (split == "block") {
       # all other parameters are default
       trainData = block_wise_split(trainData,
                                    k_x=split_param[1], 
                                    k_y=split_param[2], 
                                    percentage=c(split_param[3], split_param[4]))$train_blocks
     } else if (split == "vert") {
       trainData = vert_wise_split(trainData,
                                   k=split_param[1],
                                   percentage=c(split_param[2], split_param[3]))$train_blocks
     }
   }
   # Shuffle the pre-splited blocks/stripes
   # if the data come in as iid examples, we need to shuffle anyway
   trainData_index = sample(1:length(trainData))
   trainData = trainData[trainData_index]
  
  # split folds, assuming there is no validation set
  # the following applies generically, not just to block-wise splits
  block_length = length(trainData)
  fold_length = floor(block_length/folds)
  start = 1; end = fold_length
  
  accuracy_vector = matrix(0, folds, length(tune_param))
  threshold = matrix(0, folds, length(tune_param))
  for (i in 1:folds) {
    
    # build train, val dataset
    val_index = start:min(end, block_length)
    val_set = data.frame(rbindlist(trainData[val_index])[,-c("x", "y")])
    train_set = data.frame(rbindlist(trainData[-val_index])[,-c("x", "y")])
    # Training using Caret Wrapper
    for (j in 1:length(tune_param)) {
      tp = tune_param[j]
      fm = as.formula(paste0(label_col, " ~ ."))
      caretctrl = trainControl(method = "none")
      # since different model we use intake different preprocess option and different hyper parameter,
      # here we use if-else to specify the training process
      if (classifier == "glmnet") {
        tune=data.frame(alpha=1,lambda=tp)
        cvModel = train(
          form = fm,
          data = train_set,
          method = classifier,
          family = "binomial",
          preProcess = c("center","scale"),
          tuneLength = 1,
          tuneGrid = tune,
          metric = metric,
          trControl = caretctrl
        )
      }
      else if (classifier == "rpart") {
        tune=data.frame(cp=tp)
        cvModel = train(
          form = fm,
          data = train_set,
          method = classifier,
          tuneLength = 1,
          tuneGrid = tune,
          metric = metric,
          trControl = caretctrl
        )
      } else if (classifier == "rf") {
        tune=data.frame(mtry=tp)
        fm = as.formula(paste0(as.factor(label_col), " ~ ."))
        cvModel = train(
          form = fm,
          data = train_set,
          method = classifier,
          tuneLength = 1,
          tuneGrid = tune,
          metric = metric,
          trControl = caretctrl
        )
      }
      else { # for lda and qda, which do not have hyper parameter
        cvModel = train(
          form = fm,
          data = train_set,
          method = classifier,
          preProcess = c("center","scale"),
          tuneLength = 1,
          metric = metric,
          trControl = caretctrl
        )
      }
      y_pred_prob = predict(cvModel, newdata = val_set, type ="prob")
      roc_obj=roc(val_set[[label_col]] ~ y_pred_prob[,"Cloud"], smoothed=TRUE, plot=FALSE)
      
      #get the "best" "threshold"
      thres = as.numeric(coords(roc_obj, "best", "threshold")['threshold'])
      # Loss computation
      y_pred <- as.character(ifelse(y_pred_prob[,"Cloud"] > thres, "Cloud", "Not Cloud"))
      l = accuracy(y_true = c(val_set[, label_col]), y_pred = c(y_pred))
      if (verbose) {
        print(sprintf("The average accuracy of %s classifier with %f tunning parameter at %f fold is %f", classifier, tp, i, l))
      }
      threshold[i,j] = thres
      accuracy_vector[i,j] = l
    }
    
    # rotate the data
    start = end + 1
    end = start + fold_length
  }
  return (list("accuracy" = accuracy_vector, "threshold" = threshold))
}
