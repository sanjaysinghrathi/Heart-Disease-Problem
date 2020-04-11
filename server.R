
server <- function(input, output) {
  
  ## Define max file size
  options(shiny.maxRequestSize=530*1024^2)
  options(DT.options = list(pageLength = 15))
  options(DT.options = list(scrollX = TRUE))
  source('global.R', local = TRUE)
  
  ## Read Attribute names
  names <- reactive({
    names <- c("Age",
               "Sex",
               "Chest_Pain_Type",
               "Resting_Blood_Pressure",
               "Serum_Cholesterol",
               "Fasting_Blood_Sugar",
               "Resting_ECG",
               "Max_Heart_Rate_Achieved",
               "Exercise_Induced_Angina",
               "ST_Depression_Exercise",
               "Peak_Exercise_ST_Segment",
               "Num_Major_Vessels_Flouro",
               "Thalassemia",
               "Diagnosis_Heart_Disease")
    return(names)
  })
  names_factor <- reactive({
    names <- c("Resting_ECG", 
               "Fasting_Blood_Sugar", 
               "Sex", 
               "Diagnosis_Heart_Disease", 
               "Exercise_Induced_Angina",
               "Peak_Exercise_ST_Segment", 
               "Chest_Pain_Type",
               "Thalassemia")
    return(names)
  })
  names_numeric <- reactive({
    names <- c("Age", 
               "Resting_Blood_Pressure", 
               "Serum_Cholesterol", 
               "Max_Heart_Rate_Achieved", 
               "ST_Depression_Exercise",
               "Num_Major_Vessels_Flouro")
    return(names)
  })
  
  ## Read csv data file
  e <- reactive({
    #Read in data
    heart_disease_dataset <- read.csv(file = "processed.cleveland.data", header = F)
    
    #Prepare column names
    names <- names()
    
    #Apply column names to the dataframe
    colnames(heart_disease_dataset) <- names
    return(heart_disease_dataset)
  })
  output$e2 = DT::renderDataTable(e(), server = FALSE)
  
  ## Plot original data
  output$plot <- renderPlot({
    if (!requireNamespace("ggpubr", quietly = TRUE)){
      install.packages("ggpubr")}
    library("ggpubr")
    e <- e()
    names <- names()
    names_factor <- names_factor()
    names_numeric <- names_numeric()
    if(!is.null(e) && sum(input$xcol1 ==names_numeric)==1){
      if (!requireNamespace("ggplot2", quietly = TRUE)){
        install.packages("ggplot2")}
      library("ggplot2")
      g <- ggplot(data=e, aes(x= e[,c(input$xcol1)], fill = e[,c(input$xcol1)] )) +
        geom_bar() +
        ggtitle(paste("Plot for", input$xcol1, sep=" ")) +
        xlab(input$xcol1)  +
        ylab("Count") +
        labs(fill=input$xcol1)+
        geom_text(stat='count', aes(label=..count..), vjust=-1)
      #return(g)
    }else if(!is.null(e) && sum(input$xcol1 ==names_factor)==1){
      if (!requireNamespace("ggplot2", quietly = TRUE)){
        install.packages("ggplot2")}
      library("ggplot2")
      e[,c(input$xcol1)] <- as.factor(e[,c(input$xcol1)])
      g <- ggplot(data=e, aes(x= e[,c(input$xcol1)], fill = e[,c(input$xcol1)], label=input$xcol1 )) +
        geom_bar() +
        ggtitle(paste("Plot for", input$xcol1, sep=" ")) +
        xlab(input$xcol1)  +
        ylab("Count") +
        labs(fill=input$xcol1)+
        geom_text(stat='count', aes(label=..count..), vjust=-1)
      #return(g)
    }else{
      return(NULL)
    }
    
    if(!is.null(e) && sum(input$xcol1 ==names_numeric)==1){
      if (!requireNamespace("ggplot2", quietly = TRUE)){
        install.packages("ggplot2")}
      library("ggplot2")
      g1 <- ggplot(data=e, aes(x= as.factor(e[,dim(e)[2]]) )) +
        geom_boxplot(aes(y = e[,c(input$xcol1)],  fill = as.factor(e[,dim(e)[2]])), alpha  = .6, fatten = .7) +
        ggtitle(paste("Plot for", input$xcol1, sep=" ")) +
        xlab(colnames(e)[dim(e)[2]])  +
        ylab(input$xcol1) +
        labs(fill=colnames(e)[dim(e)[2]])
      #geom_text(stat='count', aes(label=..count..), vjust=-1)
      #return(g)
    }else if(!is.null(e) && sum(input$xcol1 ==names_factor)==1){
      if (!requireNamespace("ggplot2", quietly = TRUE)){
        install.packages("ggplot2")}
      library("ggplot2")
      #e[,c(input$xcol1)] <- as.factor(e[,c(input$xcol1)])
      #e[,dim(e)[2]] <- as.factor(e[,dim(e)[2]])
      g1 <- ggplot(data=e, aes(x= as.factor(e[,dim(e)[2]]), fill = as.factor(e[,c(input$xcol1)]), label= as.factor(e[,c(input$xcol1)]) )) +
        geom_bar() +
        ggtitle(paste("Plot for", input$xcol1, sep=" ")) +
        xlab(colnames(e)[dim(e)[2]])  +
        ylab("Count") +
        labs(fill=input$xcol1)+
        geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=0.5))
      #return(g)
    }else{
      return(NULL)
    }
    
    return(ggarrange(g,g1,nrow = 1, ncol = 2))
    
  })
  
  
  ############################################################# Plot processed data
  #process data
  e_clean <- reactive({
    heart_disease_dataset <- e()
    
    if (!requireNamespace("tidyverse", quietly = TRUE)){
      install.packages("tidyverse")}
    library("tidyverse")
    
    heart_dataset_clean_tbl <- heart_disease_dataset %>% 
      drop_na() %>%
      mutate_at(c("Resting_ECG", 
                  "Fasting_Blood_Sugar", 
                  "Sex", 
                  "Diagnosis_Heart_Disease", 
                  "Exercise_Induced_Angina",
                  "Peak_Exercise_ST_Segment", 
                  "Chest_Pain_Type"), as_factor) %>%
      mutate(Num_Major_Vessels_Flouro = as.numeric(Num_Major_Vessels_Flouro)) %>%
      mutate(Diagnosis_Heart_Disease = fct_lump(Diagnosis_Heart_Disease, other_level = "1")) %>% 
      filter(Thalassemia != "?") %>%
      select(Age, 
             Resting_Blood_Pressure, 
             Serum_Cholesterol, 
             Max_Heart_Rate_Achieved, 
             ST_Depression_Exercise,
             Num_Major_Vessels_Flouro,
             everything())
    return(heart_dataset_clean_tbl)
  })
  ## plot process data
  output$plot_processed <- renderPlot({
    e <- e_clean()
    names <- names()
    names_factor <- names_factor()
    names_numeric <- names_numeric()
    
    if(!is.null(e) && sum(input$ycol1 ==names_numeric)==1){
      if (!requireNamespace("ggplot2", quietly = TRUE)){
        install.packages("ggplot2")}
      library("ggplot2")
      g <- ggplot(data=e, aes(x= e[,c(input$ycol1)], fill = e[,c(input$ycol1)] )) +
        geom_bar() +
        ggtitle(paste("Plot for", input$ycol1, sep=" ")) +
        xlab(input$ycol1)  +
        ylab("Count") +
        labs(fill=input$ycol1)+
        geom_text(stat='count', aes(label=..count..), vjust=-1)
      #return(g)
    }else if(!is.null(e) && sum(input$ycol1 ==names_factor)==1){
      if (!requireNamespace("ggplot2", quietly = TRUE)){
        install.packages("ggplot2")}
      library("ggplot2")
      e[,c(input$ycol1)] <- as.factor(e[,c(input$ycol1)])
      g <- ggplot(data=e, aes(x= e[,c(input$ycol1)], fill = e[,c(input$ycol1)], label=input$ycol1 )) +
        geom_bar() +
        ggtitle(paste("Plot for", input$ycol1, sep=" ")) +
        xlab(input$ycol1)  +
        ylab("Count") +
        labs(fill=input$ycol1)+
        geom_text(stat='count', aes(label=..count..), vjust=-1)
      #return(g)
    }else{
      return(NULL)
    }
   
    if(!is.null(e) && sum(input$ycol1 ==names_numeric)==1){
      if (!requireNamespace("ggplot2", quietly = TRUE)){
        install.packages("ggplot2")}
      library("ggplot2")
      g1 <- ggplot(data=e, aes(x= as.factor(e[,dim(e)[2]]) )) +
        geom_boxplot(aes(y = e[,c(input$ycol1)],  fill = as.factor(e[,dim(e)[2]])), alpha  = .6, fatten = .7) +
        ggtitle(paste("Plot for", input$ycol1, sep=" ")) +
        xlab(colnames(e)[dim(e)[2]])  +
        ylab(input$ycol1) +
        labs(fill=colnames(e)[dim(e)[2]])
      #geom_text(stat='count', aes(label=..count..), vjust=-1)
      #return(g)
    }else if(!is.null(e) && sum(input$ycol1 ==names_factor)==1){
      if (!requireNamespace("ggplot2", quietly = TRUE)){
        install.packages("ggplot2")}
      library("ggplot2")
      #e[,c(input$ycol1)] <- as.factor(e[,c(input$ycol1)])
      #e[,dim(e)[2]] <- as.factor(e[,dim(e)[2]])
      g1 <- ggplot(data=e, aes(x= as.factor(e[,dim(e)[2]]), fill = as.factor(e[,c(input$ycol1)]), label= as.factor(e[,c(input$ycol1)]) )) +
        geom_bar() +
        ggtitle(paste("Plot for", input$ycol1, sep=" ")) +
        xlab(colnames(e)[dim(e)[2]])  +
        ylab("Count") +
        labs(fill=input$ycol1)+
        geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=0.5))
      #return(g)
    }else{
      return(NULL)
    }
    
    return(ggarrange(g,g1,nrow = 1, ncol = 2))
    
  })
  
  
  ########################################################### Correlation matrix and Plot
  corre <- reactive({
    e <- e_clean()
    for(i in 1:dim(e)[2]){
      e[,i] <- as.numeric(e[,i])
    }
    if(input$corr=="Integer"){
      return(cor(e))
    }else if(input$corr=="Logical"){
      #return(cor(e))
      return(abs(cor(e))>input$corr_cutoff)
    }
  })
  output$corre = DT::renderDataTable(corre(), server = FALSE)
  
  output$correplot <- renderPlot({
    # if (!requireNamespace("corrplot", quietly = TRUE)){
    #   install.packages("corrplot")}
    # library('corrplot')
    # for(i in 1:dim(e)[2]){
    #   e[,i] <- as.numeric(e[,i])
    # }
    # if(input$corr=="Integer"){
    #   return(corrplot(cor(e)))
    # }else if(input$corr=="Logical"){
    #   return(corrplot(abs(cor(e))>input$corr_cutoff))
    # }
    if (!requireNamespace("GGally", quietly = TRUE)){
      install.packages("GGally")}
    library(GGally)
    heart_dataset_clean_tbl <- e_clean()
    #Correlation matrix using Pearson method, default method is Pearson
    g1 <- heart_dataset_clean_tbl %>% ggcorr(high       = "#20a486ff",
                                       low        = "#fde725ff",
                                       label      = TRUE, 
                                       hjust      = .75, 
                                       size       = 3, 
                                       label_size = 3,
                                       nbreaks    = 5) +
      labs(title = "Correlation Matrix", subtitle = "Pearson Method Using Pairwise Obervations")
    #Correlation matrix using Kendall method
    g2 <- heart_dataset_clean_tbl %>% ggcorr(method     = c("pairwise", "kendall"),
                                         high       = "#20a486ff",
                                         low        = "#fde725ff",
                                         label      = TRUE, 
                                         hjust      = .75, 
                                         size       = 3, 
                                         label_size = 3,
                                         nbreaks    = 5) +
      labs(title = "Correlation Matrix", subtitle = "Kendall Method Using Pairwise Observations")
    return(ggarrange(g1,g2, nrow = 1, ncol = 2))
  })
  
  
  ############################################ train a model
  ots <- reactive({
    
    heart_dataset_clean_tbl<- e_clean()
    colnames(heart_dataset_clean_tbl)[dim(heart_dataset_clean_tbl)[2]] <- "response"
    heart_dataset_clean_tbl <- heart_dataset_clean_tbl %>% 
      mutate(response = recode(response, 
                               "0" = "no",
                               "1" = "yes"))
    
    if (!requireNamespace("rsample", quietly = TRUE)){
      install.packages("rsample")}
    library('rsample')
      
    #set seed for repeatability
    set.seed(1333)
    
    #create split object 
    train_test_split <- heart_dataset_clean_tbl %>% initial_split(prop = input$model_cutoff , strata = "response")
    
    #pipe split obj to training() fcn to create training set
    training <- train_test_split %>% training()
    
    #pipe split obj to testing() fcn to create test set
    testing <- train_test_split %>% testing()
    
    ## Model
    if (!requireNamespace("doParallel", quietly = TRUE)){
      install.packages("doParallel")}
    library('doParallel')
    require(doParallel)
    registerDoParallel(cores = 16)
    
    if (!requireNamespace("caret", quietly = TRUE)){
      install.packages("caret")}
    library('caret')
    if (!requireNamespace("e1071", quietly = TRUE)){
      install.packages("e1071")}
    library('e1071')
    if (!requireNamespace("kernlab", quietly = TRUE)){
      install.packages("kernlab")}
    library('kernlab')
    if (!requireNamespace("randomForest", quietly = TRUE)){
      install.packages("randomForest")}
    library('randomForest')
    if (!requireNamespace("Boruta", quietly = TRUE)){
      install.packages("Boruta")}
    library('Boruta')
    library(glmnet)
    
    if(input$model=="Elastic-Net"){
      
      ##################################################### find model general accuracy elastic net for test data
      set.seed(123)
      lambda <- 10^seq(-3, 3, length = 100)
      model <- train(response ~., data = training, method = "glmnet", trControl = trainControl(method = "LOOCV", allowParallel = TRUE), tuneGrid = expand.grid(alpha = seq(0,1,0.1), lambda = lambda))
      model
      int_accuracy <- max(model$results$Accuracy, na.rm = TRUE)
      
      predictions <- predict(model, newdata = testing, type = "prob")
      row.names(predictions) <- row.names(testing)
      predictions_prob <- data.frame(predictions)
      predictions <- predict(model, newdata = testing)
      predictions
      predictions_prob$Predicted_class <- predictions
      predictions_prob$Actual_class <- testing$response
      
      predictions <- model %>% predict(testing)
      predictions
      if(nlevels(as.factor(predictions))==nlevels(as.factor(testing$response))){
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing$response))
      }else if(nlevels(as.factor(predictions))>=nlevels(as.factor(testing$response))){
        levels(testing$response) <- levels(predictions)
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing$response))
      }else if(nlevels(as.factor(predictions))<=nlevels(as.factor(testing$response))){
        levels(predictions) <- levels(testing$response)
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing$response))
      }
      
      
    }else if(input$model=="Support Vector Machines"){
      
      ##################################################### find model general accuracy svm for test data
      ## CV with repeated Cv
      trctrl <- trainControl(method = "LOOCV", allowParallel = TRUE, classProbs =  TRUE)
      ##grid based
      #training2 <- training[,-126]
      grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
      set.seed(2345)
      model <- train(response ~., data = training, method = "svmLinear", trControl=trctrl, preProcess = c("center", "scale"), tuneGrid = grid, tuneLength = 10)
      model
      #gene_importance <- varImp(model)
      #gene_importance <- gene_importance$importance
      int_accuracy <- max(model$results$Accuracy, na.rm = TRUE)
      
      predictions <- predict(model, newdata = testing, type = "prob")
      row.names(predictions) <- row.names(testing)
      predictions_prob <- data.frame(predictions)
      predictions <- predict(model, newdata = testing)
      predictions
      predictions_prob$Predicted_class <- predictions
      predictions_prob$Actual_class <- testing$response
      
      test_pred_grid <- predict(model, newdata = testing)
      test_pred_grid
      confusionMatrix(test_pred_grid, testing$response )
      ext_accuracy <- confusionMatrix(test_pred_grid, testing$response )
      
      
    }else if(input$model=="Neural Networks"){
      ##################################################### Model accuracy for neural networks
      
      numFolds <- trainControl(method = 'LOOCV', allowParallel = TRUE, verbose=TRUE , search = "grid")
      grid <- expand.grid(size=c(seq(from = 1, to = 10, by = 1)),
                          decay=c(seq(from = 0.0, to = 0.5, by = 0.1)))
      
      set.seed(567)
      model <- train(response ~ ., training, method='nnet', trace = FALSE, preProcess = c('center', 'scale','pca'), metric="Accuracy", trControl = numFolds, linout=FALSE, tuneGrid=grid)
      model
      int_accuracy <- max(model$results$Accuracy)
      
      predictions <- predict(model, newdata = testing, type = "prob")
      row.names(predictions) <- row.names(testing)
      predictions_prob <- data.frame(predictions)
      predictions <- predict(model, newdata = testing)
      predictions
      predictions_prob$Predicted_class <- predictions
      predictions_prob$Actual_class <- testing$response
      
      predictions <- model %>% predict(testing)
      predictions
      
      if(nlevels(as.factor(predictions))==nlevels(as.factor(testing$response))){
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing$response))
      }
      
    }else if(input$model=="Random Forest"){
      
      ##################################################### find model general accuracy Random Forest for test data
      
      # Tune random forest
      set.seed(567)
      bestmtry <- as.data.frame(tuneRF(training[,1:(dim(training)[2]-1)], training$response, stepFactor=1.5, improve=1e-5, ntree=2000))
      print(bestmtry)
      numFolds <- trainControl(method = 'LOOCV', allowParallel = TRUE, verbose=TRUE , search = "grid")
      grid <- expand.grid(.mtry=bestmtry$mtry)
      
      set.seed(567)
      model <- train(response ~ ., training, method='rf', trace = FALSE, preProcess = c('center', 'scale'), metric="Accuracy", trControl = numFolds, linout=FALSE, tuneGrid=grid)
      model
      int_accuracy <- max(model$results$Accuracy)
      
      predictions <- predict(model, newdata = testing, type = "prob")
      row.names(predictions) <- row.names(testing)
      predictions_prob <- data.frame(predictions)
      predictions <- predict(model, newdata = testing)
      predictions
      predictions_prob$Predicted_class <- predictions
      predictions_prob$Actual_class <- testing$response
      
      predictions <- model %>% predict(testing)
      predictions
      
      #confusionMatrix(as.factor(predictions),as.factor(testing$response))
      if(nlevels(as.factor(predictions))==nlevels(as.factor(testing$response))){
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing$response))
      }else if(nlevels(as.factor(predictions))>=nlevels(as.factor(testing$response))){
        levels(testing$response) <- levels(predictions)
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing$response))
      }else if(nlevels(as.factor(predictions))<=nlevels(as.factor(testing$response))){
        levels(predictions) <- levels(testing$response)
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing$response))
      }
      
    }else if(input$model=="Elastic-Net with feature selection"){
      
      ####################################################### model accuracy using elastic net
      ##  feature selection using random forest
      set.seed(123)
      boruta.train <- Boruta(response~., data = training, doTrace = 2,  maxRuns= 100, pValue = 0.05)
      print(boruta.train)
      final.boruta <- TentativeRoughFix(boruta.train)
      print(final.boruta)
      bestest1 <- data.frame(significant_probes =getSelectedAttributes(final.boruta, withTentative = TRUE))
      rm(boruta.train)
      rm(final.boruta)
      
      library(tidyr)
      bestest1 <- separate_rows(bestest1, 1, sep = "`")
      bestest1 <- bestest1[bestest1$significant_probes!="",]
      training1 <- training[,c(bestest1,"response")]
      testing1 <-  testing[,c(bestest1,"response")]
      rm(bestest1)
      
      set.seed(123)
      lambda <- 10^seq(-3, 3, length = 100)
      model <- train(response ~., data = training1, method = "glmnet", trControl = trainControl(method = "LOOCV", allowParallel = TRUE), tuneGrid = expand.grid(alpha = seq(0,1,0.1), lambda = lambda))
      model
      int_accuracy <- max(model$results$Accuracy, na.rm = TRUE)
      
      predictions <- predict(model, newdata = testing1, type = "prob")
      row.names(predictions) <- row.names(testing1)
      predictions_prob <- data.frame(predictions)
      predictions <- predict(model, newdata = testing1)
      predictions
      predictions_prob$Predicted_class <- predictions
      predictions_prob$Actual_class <- testing1$response
      
      
      predictions <- model %>% predict(testing1)
      predictions
      
      if(nlevels(as.factor(predictions))==nlevels(as.factor(testing1$response))){
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing1$response))
      }else if(nlevels(as.factor(predictions))>=nlevels(as.factor(testing1$response))){
        levels(testing1$response) <- levels(predictions)
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing1$response))
      }else if(nlevels(as.factor(predictions))<=nlevels(as.factor(testing1$response))){
        levels(predictions) <- levels(testing1$response)
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing1$response))
      }
      
      
    }else if(input$model=="Support Vector Machines with feature selection"){
      
      ##################################################### find model general accuracy svm for test data using feature selection
      
      ##################################################### find model general accuracy svm for test data
      ## CV with repeated Cv
      trctrl <- trainControl(method = "LOOCV", allowParallel = TRUE, classProbs =  TRUE)
      ##grid based
      #training2 <- training[,-126]
      grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
      set.seed(2345)
      model <- train(response ~., data = training, method = "svmLinear", trControl=trctrl, preProcess = c("center", "scale"), tuneGrid = grid, tuneLength = 10)
      model
      #gene_importance <- varImp(model)
      #gene_importance <- gene_importance$importance
      int_accuracy <- max(model$results$Accuracy, na.rm = TRUE)
      
      predictions <- predict(model, newdata = testing, type = "prob")
      row.names(predictions) <- row.names(testing)
      predictions_prob <- data.frame(predictions)
      predictions <- predict(model, newdata = testing)
      predictions
      predictions_prob$Predicted_class <- predictions
      predictions_prob$Actual_class <- testing$response
      
      test_pred_grid <- predict(model, newdata = testing)
      test_pred_grid
      confusionMatrix(test_pred_grid, testing$response )
      ext_accuracy <- confusionMatrix(test_pred_grid, testing$response )
      
      
    }else if(input$model=="Neural Networks with feature selection"){
      ##################################################### Model accuracy for neural networks using feature selection
      
      ##  feature selection using random forest
      set.seed(123)
      boruta.train <- Boruta(response~., data = training, doTrace = 2,  maxRuns= 100, pValue = 0.05)
      print(boruta.train)
      final.boruta <- TentativeRoughFix(boruta.train)
      print(final.boruta)
      bestest1 <- data.frame(significant_probes =getSelectedAttributes(final.boruta, withTentative = TRUE))
      rm(boruta.train)
      rm(final.boruta)
      
      library(tidyr)
      bestest1 <- separate_rows(bestest1, 1, sep = "`")
      bestest1 <- bestest1[bestest1$significant_probes!="",]
      training1 <- training[,c(bestest1,"response")]
      testing1 <-  testing[,c(bestest1,"response")]
      rm(bestest1)
      
      numFolds <- trainControl(method = 'LOOCV', allowParallel = TRUE, verbose=TRUE , search = "grid")
      grid <- expand.grid(size=c(seq(from = 1, to = 10, by = 1)),
                          decay=c(seq(from = 0.0, to = 0.5, by = 0.1)))
      
      set.seed(567)
      model <- train(response ~ ., training1, method='nnet', trace = FALSE, preProcess = c('center', 'scale','pca'), metric="Accuracy", trControl = numFolds, linout=FALSE, tuneGrid=grid)
      model
      int_accuracy <- max(model$results$Accuracy)
      
      predictions <- predict(model, newdata = testing1, type = "prob")
      row.names(predictions) <- row.names(testing1)
      predictions_prob <- data.frame(predictions)
      predictions <- predict(model, newdata = testing1)
      predictions
      predictions_prob$Predicted_class <- predictions
      predictions_prob$Actual_class <- testing1$response
      
      predictions <- model %>% predict(testing1)
      predictions
      
      if(nlevels(as.factor(predictions))==nlevels(as.factor(testing1$response))){
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing1$response))
      }else if(nlevels(as.factor(predictions))>=nlevels(as.factor(testing1$response))){
        levels(testing1$response) <- levels(predictions)
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing1$response))
      }else if(nlevels(as.factor(predictions))<=nlevels(as.factor(testing1$response))){
        levels(predictions) <- levels(testing1$response)
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing1$response))
      }
      
    }else if(input$model=="Random Forest with feature selection"){
      
      ##################################################### find model general accuracy Random Forest for test data using feature selection
      
      ##  feature selection using random forest
      set.seed(123)
      boruta.train <- Boruta(response~., data = training, doTrace = 2,  maxRuns= 100, pValue = 0.05)
      print(boruta.train)
      final.boruta <- TentativeRoughFix(boruta.train)
      print(final.boruta)
      bestest1 <- data.frame(significant_probes =getSelectedAttributes(final.boruta, withTentative = TRUE))
      rm(boruta.train)
      rm(final.boruta)
      
      library(tidyr)
      bestest1 <- separate_rows(bestest1, 1, sep = "`")
      bestest1 <- bestest1[bestest1$significant_probes!="",]
      training1 <- training[,c(bestest1,"response")]
      testing1 <-  testing[,c(bestest1,"response")]
      rm(bestest1)
      
      # Tune random forest
      set.seed(567)
      bestmtry <- as.data.frame(tuneRF(training1[,1:(dim(training1)[2]-1)], training1$response, stepFactor=1.5, improve=1e-5, ntree=2000))
      print(bestmtry)
      numFolds <- trainControl(method = 'LOOCV', allowParallel = TRUE, verbose=TRUE , search = "grid")
      grid <- expand.grid(.mtry=bestmtry$mtry)
      
      set.seed(567)
      model <- train(response ~ ., training1, method='rf', trace = FALSE, preProcess = c('center', 'scale'), metric="Accuracy", trControl = numFolds, linout=FALSE, tuneGrid=grid)
      model
      int_accuracy <- max(model$results$Accuracy)
      
      predictions <- predict(model, newdata = testing1, type = "prob")
      row.names(predictions) <- row.names(testing1)
      predictions_prob <- data.frame(predictions)
      predictions <- predict(model, newdata = testing1)
      predictions
      predictions_prob$Predicted_class <- predictions
      predictions_prob$Actual_class <- testing1$response
      
      predictions <- model %>% predict(testing1)
      predictions
      
      if(nlevels(as.factor(predictions))==nlevels(as.factor(testing1$response))){
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing1$response))
      }else if(nlevels(as.factor(predictions))>=nlevels(as.factor(testing1$response))){
        levels(testing1$response) <- levels(predictions)
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing1$response))
      }else if(nlevels(as.factor(predictions))<=nlevels(as.factor(testing1$response))){
        levels(predictions) <- levels(testing1$response)
        ext_accuracy <- confusionMatrix(as.factor(predictions),as.factor(testing1$response))
      }
      
    }else{
      return(NULL)
    }
    
    if(input$model=="No Model"){
      return(NULL)
    }
    if (!requireNamespace("pROC", quietly = TRUE)){
      BiocManager::install("pROC")}
    library(pROC)
    if(!requireNamespace("DMwR", quietly = TRUE)){
      BiocManager::install("DMwR")}
    library(DMwR)
    if(!requireNamespace("ggplotify", quietly = TRUE)){
      BiocManager::install("ggplotify")}
    library("ggplotify")
    
    predictions_prob <- predictions_prob
    predictions_prob <- predictions_prob[order(predictions_prob$yes),]
    roc_curve <- roc(predictions_prob$Actual_class, predictions_prob$yes, levels=c("no", "yes"), direction="<", positive="yes")
    roc_curve
    
    results <- data.frame(Internal_Accuracy=c(0), External_Accuracy=c(0), External_Kappa=c(0), AUC=c(0), P_Value=c(0), Sensitivity=c(0),
                           Specificity=c(0), Pos_Pred_Value=c(0), Neg_Pred_Value=c(0), Precision=c(0), Recall=c(0), F1=c(0), Prevalence=c(0),
                           Detection_Rate=c(0), Detection_Prevalence=c(0), Balanced_Accuracy=c(0))
    results[1,1] <- int_accuracy
    results[1,2] <- ext_accuracy$overall[1]
    results[1,3] <- ext_accuracy$overall[2]
    results[1,4] <- roc_curve$auc
    results[1,5] <- ext_accuracy$overall[6]
    results[1,6] <- ext_accuracy$byClass[1]
    results[1,7] <- ext_accuracy$byClass[2]
    results[1,8] <- ext_accuracy$byClass[3]
    results[1,9] <- ext_accuracy$byClass[4]
    results[1,10] <- ext_accuracy$byClass[5]
    results[1,11] <- ext_accuracy$byClass[6]
    results[1,12] <- ext_accuracy$byClass[7]
    results[1,13] <- ext_accuracy$byClass[8]
    results[1,14] <- ext_accuracy$byClass[9]
    results[1,15] <- ext_accuracy$byClass[10]
    results[1,16] <- ext_accuracy$byClass[11]
    
    
    return(list(training,testing, predictions_prob, int_accuracy, ext_accuracy, results))
  })
  
  output$ots1 = DT::renderDataTable(as.data.frame(ots()[[5]]$table), server = FALSE)
  output$ots3 = DT::renderDataTable(as.data.frame(ots()[[6]]), server = FALSE)
  output$ots2 <- renderPlot({
    predictions_prob<- ots()[[3]]
    if(input$model=="No Model"){
      return(NULL)
    }
    if (!requireNamespace("pROC", quietly = TRUE)){
      BiocManager::install("pROC")}
    library(pROC)
    if(!requireNamespace("DMwR", quietly = TRUE)){
      BiocManager::install("DMwR")}
    library(DMwR)
    if(!requireNamespace("ggplotify", quietly = TRUE)){
      BiocManager::install("ggplotify")}
    library("ggplotify")
    
    predictions_prob <- predictions_prob
    predictions_prob <- predictions_prob[order(predictions_prob$yes),]
    roc_curve <- roc(predictions_prob$Actual_class, predictions_prob$yes, levels=c("no", "yes"), direction="<", positive="yes")
    roc_curve
    #roc_plot <- as.ggplot(~plot(roc_curve, col="blue", lwd=3, main="ROC curve for Testing set", print.thres = "best"))
    # g1 <- ggroc(roc_curve, alpha = 0.5, colour = "red", linetype = 2, size = 2)
    # g1 + theme_minimal() + ggtitle("ROC curve") + 
    #   geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")
    gl <- ggroc(roc_curve, legacy.axes = TRUE) + xlab("False Positive Rate") + ylab("True Positive Rate") + 
      geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="red", linetype="dashed")
    g2 <- predictions_prob %>% 
      ggplot(aes(x=c(1:dim(predictions_prob)[1]), y=yes, col=Actual_class)) + 
      scale_color_manual(values=c("red", "black")) + 
      geom_point() + 
      geom_rug() + 
      xlab("Testing Sample")+
      ylab("Response")+
      ggtitle("Response scores for Testing set")
    return(ggarrange(gl,g2, nrow = 1, ncol = 2))
  })
  
  
  output$pca <- renderPlot({
    
    if(!requireNamespace("ggplotify", quietly = TRUE)){
      BiocManager::install("ggplotify")}
    library("ggplotify")
    
    e1 <- e_clean()
    colnames(e1)[dim(e1)[2]] <- "response"
    e1 <- e1 %>% 
      mutate(response = recode(response, 
                               "0" = "no",
                               "1" = "yes"))
    
    pData <- e1
    for(i in 1:dim(e1)[2]){
      e1[,i] <- as.numeric(as.character(e1[,i]))
    }
    e <- t(e1[,1:(dim(e1)[2]-1)])
    if(!is.null(e) && !is.null(pData) && dim(e)[2]==dim(pData)[1] && input$pca==TRUE){
      if (!requireNamespace("ggfortify", quietly = TRUE)){
        install.packages("ggfortify")}
      library(ggfortify)
      pc <- prcomp(t(e),scale.=TRUE)
      proportion_0f_variance <- pc$sdev^2/sum(pc$sdev^2)
      plot(proportion_0f_variance, main="Proportion of Variance for all PCA Components", xlab= "PCA Index", ylab= "Variance %")
      pcDat <- prcomp(t(e),scale.=TRUE)
      a1 <-autoplot(pcDat,
                   data = pData, 
                   colour="response", 
                   shape="response",
                   size=2)
      a2 <-autoplot(pcDat,
                   data = pData, 
                   x=2,
                   y=3,
                   colour="response", 
                   shape="response",
                   size=2)
      a <- ggarrange(a1, a2, nrow = 1, ncol = 2)
      if(!is.null(a)){
        return(a)
      }else{
        return(NULL)
      }
    }else if(!is.null(e) && !is.null(pData) && dim(e)[2]==dim(pData)[1] && input$pca_select==TRUE){
      if(!requireNamespace("ggplotify", quietly = TRUE)){
        BiocManager::install("ggplotify")}
      library("ggplotify")
      if (!requireNamespace("ggfortify", quietly = TRUE)){
        install.packages("ggfortify")}
      library(ggfortify)
      pc <- prcomp(t(e),scale.=TRUE)
      proportion_0f_variance <- pc$sdev^2/sum(pc$sdev^2)
      plot(proportion_0f_variance, main="Proportion of Variance for all PCA Components", xlab= "PCA Index", ylab= "Variance %")
      pcDat <- prcomp(t(e),scale.=TRUE)
      a1 <-autoplot(pcDat,
                    data = pData, 
                    colour="response", 
                    shape="response",
                    size=2)
      a2 <-autoplot(pcDat,
                   data = pData, 
                   x=as.integer(input$pca1),
                   y=as.integer(input$pca2),
                   colour="response", 
                   shape="response",
                   size=2)
      
      a <- ggarrange(a1, a2, nrow = 1, ncol = 2)
      if(!is.null(a)){
        return(a)
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
  })
  
}
