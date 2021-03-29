###### Models #####

#Regression tree rpart
model_rt = function(fmla_vars, train_data, val_data, test_data){
  # convert vectors to string then formula
  rt_fmla = as.formula(paste("COURSE_GRADE ~ ", paste(fmla_vars, collapse=" + "), sep=""))
  rt = rpart(rt_fmla, data = train_data, parms = list(split = 'gini'))
  if(predict_on_test){
    predy = rpart.predict(rt, test_data)
  }else{
    predy = rpart.predict(rt, val_data)
  }
  return(as.data.frame(predy))
}

# GLM
model_glm = function(fmla_vars, train_data, val_data, test_data){
  glm_l1 = cv.glmnet(x = as.matrix(train_data[, fmla_vars]),
                     y = as.numeric(train_data[, 'COURSE_GRADE']), alpha = 1
  )
  if(predict_on_test){
    predy = predict(glm_l1, as.matrix(test_data[, fmla_vars]), s = 'lambda.min')
  }else{
    predy = predict(glm_l1, as.matrix(val_data[, fmla_vars]), s = 'lambda.min')
  }
  result = as.data.frame(predy)
  colnames(result)[which(colnames(result) == '1')] = 'predy'
  return(result)
}

# Naive Bayes
model_naiveBayes = function(fmla_vars, train_data, val_data, test_data){
  bayes_fmla = as.formula(paste("COURSE_GRADE ~ ", paste(fmla_vars, collapse=" + "), sep=""))
  bayes = naiveBayes(bayes_fmla, data = train_data)
  if(predict_on_test){
    predy = predict(bayes, test_data)
  }else{
    predy = predict(bayes, val_data)
  }
  result = as.data.frame(predy)
  colnames(result)[which(colnames(result) == '1')] = 'predy'
  return(result)
}

# ROC
plot_roc = function(predy, test_data){
  test_roc = roc(test_data$COURSE_GRADE>=50, factor(predy>=50, ordered = T),
                 smoothed = TRUE,
                 # arguments for ci
                 ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                 # arguments for plot
                 plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                 print.auc=TRUE, show.thres=TRUE)
  
  sens.ci <- ci.se(test_roc)
  plot(sens.ci, type="shape", col="lightblue")
  plot(sens.ci, type="bars")
  
  sum((test_data$COURSE_GRADE >= 50) == (unname(predy) >= 50)) / length(predy)
}

# Calculate model accuracy
accuracy = function(validate_data, pred, pred_col_name){
  tmp = merge(x = pred, y = validate_data[,c("student_id","COURSE_GRADE")], by = "student_id", all.x = TRUE)
  tmp$orig_result = ifelse(tmp$COURSE_GRADE >= 50, "Pass", "Fail")
  
  true_pos = sum(tmp$orig_result == "Pass" & tmp[[pred_col_name]] == "Pass")
  true_neg = sum(tmp$orig_result == "Fail" & tmp[[pred_col_name]] == "Fail")
  false_pos = sum(tmp$orig_result == "Fail" & tmp[[pred_col_name]] == "Pass")
  false_neg = sum(tmp$orig_result == "Pass" & tmp[[pred_col_name]] == "Fail")
  accuracy = (true_pos + true_neg)/ (true_pos + true_neg + false_pos + false_neg)
  
  result = data.frame(true_pos, true_neg, false_pos, false_neg, accuracy)
  return (result)
}