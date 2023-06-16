library(arcgisbinding)


arc.check_portal()
#arc.open("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/ACS_Total_Population_Boundaries/FeatureServer/2")

#ozone.arc.dataset@extent
#ozone.arc.dataset@fields
ozone.dataframe <- arc.select(object = ozone.arc.dataset, fields = c("FID", "Shape", "LATITUDE", "LONGITUDE", "ELEVATION", "OZONE"), where_clause = "ELEVATION >= 1000")
head(ozone.dataframe)
ozone.path <- system.file("extdata", "ca_ozone_pts.shp", package="arcgisbinding")
ozone.arc.dataset <- arc.open(ozone.path)




#ozone.dataframe <- arc.select(object = ozone.arc.dataset, fields = c("FID", "Shape", "LATITUDE", "LONGITUDE", "ELEVATION", "OZONE"), where_clause = "ELEVATION >= 1000")
#head(ozone.dataframe)


# Data Analytics Using ArcGIS Pro and R









  
  require(MKmisc)
  require(ROCR)
  require(survey)
  require(pROC)
  require(ROCR)
  require(caret)
  
  ##################################################################################################### 
  ### Define input/output parameters
  #####################################################################################################
  input_data <- in_params[[1]]
  train_percentage_size <- (in_params[[2]])/100
  dependent_variable <- in_params[[3]]
  independent_variables <- in_params[[4]]
  output_prediction_data <- out_params[[1]]
  
  ##################################################################################################### 
  ### Load Data and Create Dataframe R Object 
  #####################################################################################################
  arc.progress_label("Loading data...")
  arc.progress_pos(40)
  
  d <- arc.open(input_data)
  fields_list <- append(c(dependent_variable), independent_variables)
  d_df_full <- arc.select(d)
  d_df <- arc.select(d, fields = fields_list)
  
  #####################################################################################################
  ### Create Training and Testing Datasets
  #####################################################################################################
  arc.progress_label("Creating training and testing datasets...")
  arc.progress_pos(60)
  smp_size <- floor(train_percentage_size * nrow(d_df))
  
  set.seed(1234)
  train_ind <- sample(seq_len(nrow(d_df)), size = smp_size)
  
  d_df_train <- d_df[train_ind, ]
  d_df_test <- d_df[-train_ind, ]
  
  #####################################################################################################
  ### Fit Logistic Regression Model
  #####################################################################################################
  arc.progress_label("Creating training and testing datasets...")
  arc.progress_pos(80)
  
  response <- d_df_train[, 1]
  predictors <- d_df_train[, -1]
  
  d_df_train.log <- glm(response ~ .,  family = binomial(link = 'logit'), data = predictors)
  
  d_df_full$Seagrass_Prediction <- predict(d_df_train.log, newdata = d_df_full, type = "response")
  
  #####################################################################################################
  ### Run Diagnostics on Logistic Regression Model
  #####################################################################################################
  arc.progress_label("Running diagnostics on fitted model...")
  arc.progress_pos(80)
  
  #Summary of model fit
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n"))
  cat(paste0("\n", "Summary of Fitted Logistic Regression Model", "\n"))
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n"))
  print(summary(d_df_train.log))
  
  #Hosmer-Lemeshow Test
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n"))
  cat(paste0("\n", "Hosmer-Lemeshow Goodness of Fit Test Results", "\n"))
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n"))
  HL <- HLgof.test(fit = fitted(d_df_train.log), obs = d_df_train$Seagrass)
  print(HL)
  
  #Prediction Accuracy 
  d_df_test.log.pred <- predict(d_df_train.log, newdata = d_df_test, type = 'response')
  d_df_test.log.pred <- ifelse(d_df_test.log.pred > 0.5, 1, 0)
  misClassificError <- mean(d_df_test.log.pred != d_df_test$Seagrass)
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n"))
  cat(paste0("\n", "Prediction Accuracy for Test Dataset", "\n"))
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n"))
  print(paste('Accuracy Percentage:', round((1-misClassificError)*100, 2)))
  cat(paste0("\n"))
  
  #ROC Curve
  d_df_test.log.pred <- predict(d_df_train.log, newdata = d_df_test, type = 'response')
  pred <- prediction(d_df_test.log.pred, d_df_test$Seagrass)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  plot(perf)
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n"))
  cat(paste0("\n", "Area Under the ROC Curve", "\n"))
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n"))
  print(paste('Area:', auc))
  cat(paste0("\n"))
  cat(paste0("\n", "............................................", "\n"))
  cat(paste0("\n"))
  
  #####################################################################################################
  ### write out
  #####################################################################################################
  arc.progress_label("Writing output...")
  arc.progress_pos(80)
  
  if(!is.null(output_prediction_data) && output_prediction_data != "NA")
    arc.write(output_prediction_data, d_df_full, shape_info = arc.shapeinfo(d))
  
  arc.progress_pos(100)
}
