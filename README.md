# STA521 Project 2: Cloud Detection
## Author: Tianhao Li, Haoming Yang

In this project, we implemented the following:
Image splitting algorithm: The image splitting functions is written in dataSplit.R file. In the file, there are two functions called block_wise_split() and vertical_wise_split(). Each function will in take a list of images and return a list with 3 sublists: train,validation, and test dataset. The specific algorithm that we used to find splitting point is described in our report.

Another file is cvMaster.R, which we implemented the cross validation function that train the user defined model with feeded data. The data input can be a list of images and a string value that specify the split algorithm, or a pre-splitted training data that contains list of blocks/vertical strips of the data. Users are also given the liberty to choose the metric that they wish to optimize on, the specific classifier they want to fit, and the tuning parameter. Once a model is fitted, it is evaluated first with ROC to find its best threshold, then it is evaluated by accuracy. The thresholds and accuracy will be each stored in a matrix and outputed. 

To reproduce our result, one can use set.seed(520) for the training. During diagnostic, we have set the following seed: 520, 1, 130, 602. 

To address the package dependencies, our training pipeline utilized pROC, caret, and specific algorithms such as lda,qda from MASS, decision tree from rpart, and random forest from randomForest package. For plotting, we used a mixture of ggplot2 and the vanilla plotting function of r. 
