#### Main.R #####

#### Packages ####
library(doBy)
library(caret)
library(rpart)
library(glmnet)
library(rpart.plot)
library(pROC)
library(e1071)
library(reconstructr)
library(plyr)
library(tidyverse)

##### Inputs #####
Target_course = 'UO Foundations of Human Biology 1' #### set for target course in long description
start_date = '2017-01-01'
week_range = 3
# Create validation data?
cross_validate = T
# Use bi-gram conditional probability features? (increases processing time, feature selection for later work?)
use_bigram_probs = T
# Use models to predict on test data? if F predicts on validate data
predict_on_test = F

##### End inputs #####
#setwd("~/rds/UniSA Online/New_project")

### SQL Connection ####
#source('R_SQL_Conn.R')

#### Extracting datasets from SQL databases ####
source('Data_extract.R')

#### Feature extraction ####
source('Feature_extract.R')

# Split dataset into train/test
# use older courses as train and latest course as test
# data may not always be present for first few weeks in specified range

if(cross_validate){
  # Get latest course as test, 2nd latest as validation
  test_term = max(courses$term_code)
  val_term = sort(courses$term_code, partial = length(courses$term_code) - 1) [length(courses$term_code)-1]
  
  test_courseid = max(courses$moodle_course_id)
  val_courseid = sort(courses$moodle_course_id, partial = length(courses$moodle_course_id) - 1) [length(courses$moodle_course_id)-1]
  
  #track
  tracking_p = p_tracking(tracking, week_range)
  track_test = subset(tracking_p, tracking_p$term_code == test_term)
  track_val = subset(tracking_p, tracking_p$term_code == val_term)
  track_train = subset(tracking_p, tracking_p$term_code != test_term & tracking_p$term_code != val_term)
  
  #panopto
  pan_use_p = p_panopto(pan_use, week_range)
  pan_test = subset(pan_use_p, pan_use_p$TERM_CODE == test_term)
  pan_val = subset(pan_use_p, pan_use_p$TERM_CODE == val_term)
  pan_train = subset(pan_use_p, pan_use_p$TERM_CODE != test_term & pan_use_p$TERM_CODE != val_term)
  
  #h5p
  h5p_p = p_h5p(h5p, week_range)
  h5p_test = subset(h5p_p, h5p_p$Course_ID == test_courseid)
  h5p_val = subset(h5p_p, h5p_p$Course_ID == val_courseid)
  h5p_train = subset(h5p_p, h5p_p$Course_ID != test_courseid & h5p_p$Course_ID != val_courseid)
  
  #m_log
  m_log_p = p_m_log(m_log, week_range)
  if(use_bigram_probs){
    m_log_bigrams_p = p_m_log_bigrams(m_log, week_range)
    m_log_p = merge(m_log_p, m_log_bigrams_p)
  }
  m_log_test = subset(m_log_p, m_log_p$term_code == test_term)
  m_log_val = subset(m_log_p, m_log_p$term_code == val_term)
  m_log_train = subset(m_log_p, m_log_p$term_code != test_term & m_log_p$term_code != val_term)
  
}else{
  track_train = p_tracking(subset(tracking, tracking$term_code %in% setdiff(courses$term_code, max(courses$term_code))), week_range)
  track_test = p_tracking(subset(tracking, tracking$term_code == max(courses$term_code)), week_range)
  
  pan_train = p_panopto(subset(pan_use, pan_use$TERM_CODE %in% setdiff(courses$term_code, max(courses$term_code))), week_range)
  pan_test = p_panopto(subset(pan_use, pan_use$TERM_CODE == max(courses$term_code)), week_range)
  
  h5p_train = p_h5p(subset(h5p, h5p$Course_ID %in% setdiff(courses$moodle_course_id, max(courses$moodle_course_id))), week_range)
  h5p_test = p_h5p(subset(h5p, h5p$Course_ID == max(courses$moodle_course_id)), week_range)
  
  # m_log_train = p_m_log(subset(m_log, m_log$term_code %in% setdiff(courses$term_code, max(courses$term_code))), week_range)
  # m_log_test = p_m_log(subset(m_log, m_log$term_code == max(courses$term_code)), week_range)
  #m_log
  m_log_p = p_m_log(m_log, week_range)
  if(use_bigram_probs){
    m_log_bigrams_p = p_m_log_bigrams(m_log, week_range)
    m_log_p = merge(m_log_p, m_log_bigrams_p)
  }
  m_log_test = subset(m_log_p, m_log_p$term_code == test_term)
  m_log_train = subset(m_log_p, m_log_p$term_code != test_term)
}

#### Merge different features
# Train Data
#track
track_train = merge(track_train, moodle_users[, c("id", "idnumber")], by.x = "userid", by.y = 'id')
colnames(track_train)[which(colnames(track_train) == 'idnumber')] = 'student_id'
colnames(track_train)[which(colnames(track_train) == 'num_sessions')] = 'num_sessions_track'

#h5p
h5p_train = merge(h5p_train, courses[,c('moodle_course_id', 'term_code')], by.x = 'Course_ID', by.y = 'moodle_course_id')
train = merge(track_train, h5p_train, by.x = c("term_code", "student_id" ), by.y = c("term_code", "Student_ID"), all = T)

#m_log
m_log_train = merge(m_log_train, moodle_users[, c("id", "idnumber")], by.x = "userid", by.y = 'id')
colnames(m_log_train)[which(colnames(m_log_train) == 'idnumber')] = 'student_id'
train = merge(train, m_log_train, by.x = c("term_code", "student_id" ), by.y = c("term_code", "student_id"), all = T)

#panopto
if (exists("pan_train") || (dim(pan_train)[1] == 0)) {
  colnames(pan_train)[which(colnames(pan_train) == 'num_sessions')] = 'num_sessions_pan'
  train = merge(train, pan_train, by.x = c("term_code", "student_id" ), by.y = c("TERM_CODE", "STUDENT_ID"), all = T)
}

# Test Data
#track
track_test = merge(track_test, moodle_users[, c("id", "idnumber")], by.x = "userid", by.y = 'id')
colnames(track_test)[which(colnames(track_test) == 'idnumber')] = 'student_id'
colnames(track_test)[which(colnames(track_test) == 'num_sessions')] = 'num_sessions_track'

#h5p
h5p_test = merge(h5p_test, courses[,c('moodle_course_id', 'term_code')], by.x = 'Course_ID', by.y = 'moodle_course_id')
test = merge(track_test, h5p_test, by.x = c("term_code", "student_id" ), by.y = c("term_code", "Student_ID"), all = T)

#m_log
m_log_test = merge(m_log_test, moodle_users[, c("id", "idnumber")], by.x = "userid", by.y = 'id')
colnames(m_log_test)[which(colnames(m_log_test) == 'idnumber')] = 'student_id'
test = merge(test, m_log_test, by.x = c("term_code", "student_id" ), by.y = c("term_code", "student_id"), all = T)

#panopto
if (exists("pan_test") || (dim(pan_test)[1] == 0)) {
  test = merge(test, pan_test, by.x = c("term_code", "student_id" ), by.y = c("TERM_CODE", "STUDENT_ID"), all = T)
  colnames(pan_test)[which(colnames(pan_test) == 'num_sessions')] = 'num_sessions_track'
}

#Validation Data
if (cross_validate){
  track_val = merge(track_val, moodle_users[, c("id", "idnumber")], by.x = "userid", by.y = 'id')
  colnames(track_val)[which(colnames(track_val) == 'idnumber')] = 'student_id'
  colnames(track_val)[which(colnames(track_val) == 'num_sessions')] = 'num_sessions_track'
  
  #h5p
  h5p_val = merge(h5p_val, courses[,c('moodle_course_id', 'term_code')], by.x = 'Course_ID', by.y = 'moodle_course_id')
  validate = merge(track_val, h5p_val, by.x = c("term_code", "student_id" ), by.y = c("term_code", "Student_ID"), all = T)
  
  #m_log
  m_log_val = merge(m_log_val, moodle_users[, c("id", "idnumber")], by.x = "userid", by.y = 'id')
  colnames(m_log_val)[which(colnames(m_log_val) == 'idnumber')] = 'student_id'
  validate = merge(validate, m_log_val, by.x = c("term_code", "student_id" ), by.y = c("term_code", "student_id"), all = T)
  
  #panopto
  if (exists("pan_val") || (dim(pan_val)[1] == 0)) {
    validate = merge(validate, pan_val, by.x = c("term_code", "student_id" ), by.y = c("TERM_CODE", "STUDENT_ID"), all = T)
    colnames(pan_val)[which(colnames(pan_val) == 'num_sessions')] = 'num_sessions_track'
  }
}

##### Merge grades #####
train = merge(train, grades[,c("STUDENT_ID", "TERM_CODE", "COURSE_GRADE", "COURSE_GRADE_LETTER")], 
              by.x = c("term_code", "student_id"), by.y = c("TERM_CODE", "STUDENT_ID"))

test = merge(test, grades[,c("STUDENT_ID", "TERM_CODE", "COURSE_GRADE", "COURSE_GRADE_LETTER")], 
              by.x = c("term_code", "student_id"), by.y = c("TERM_CODE", "STUDENT_ID"))

if(cross_validate){
  validate = merge(validate, grades[,c("STUDENT_ID", "TERM_CODE", "COURSE_GRADE", "COURSE_GRADE_LETTER")], 
                   by.x = c("term_code", "student_id"), by.y = c("TERM_CODE", "STUDENT_ID"))
}
### fill 0 to replace NA  ####
train[is.na(train)] = 0
test[is.na(test)] = 0
if(cross_validate) validate[is.na(validate)] = 0

# train = train[train$COURSE_GRADE != 'I',]
train$COURSE_GRADE = as.numeric(train$COURSE_GRADE)
train = train[!is.na(train$COURSE_GRADE),]

test$COURSE_GRADE = as.numeric(test$COURSE_GRADE)
test = test[!is.na(test$COURSE_GRADE),]

if(cross_validate){
  validate$COURSE_GRADE = as.numeric(validate$COURSE_GRADE)
  validate = validate[!is.na(validate$COURSE_GRADE),]
}

###### Model #####
## Build formula/features list to pass to model functions

# base formula, can be broken down further by table
fmla_vars = c("avg_duration_page.viewing_per_session", "avg_num_page.viewing_per_session", "num_sessions_track",
              "no_quiz", "no_try.mean", "Student_Score.max.mean", "time.Session_No", "time.Session_Time_Mean",
              "time.Morning_Session", "time.Day_Session", "time.Night_Session", "activity.Assessment", "activity.Informing_Orienteering", 
              "activity.Learning_Content_Access", "activity.Metacognitive_Monitoring", "activity.Metacognitive_Planning",
              "activity.Social_Interaction_Passive", "activity.Social_Interaction_Active")

# panopto vars
pan_vars = c("avg_duration_video.viewing_per_session", "avg_num_video.viewing_per_session", "num_sessions_pan")


# check data exists and is not empty then merge var names
if( exists("pan_train") && dim(pan_train)[1] != 0 && exists("pan_test") && dim(pan_test)[1] != 0 && (exists("pan_val") && dim(pan_test)[1] != 0)) fmla_vars = c(fmla_vars, pan_vars)

if(use_bigram_probs){
  # Bi-gram conditional probability vars
  prob_vars = c()
  for(ac in c("A", "I", "L", "M", "P", "R", "W")){
    newcol = paste("Start", ac, sep = ".to.")
    prob_vars = c(prob_vars, newcol)
    for (ac2 in c("A", "I", "L", "M", "P", "R", "W", "End")){
      if (ac != ac2){
        newcol = paste(ac, ac2, sep = ".to.")
        prob_vars = c(prob_vars, newcol)
      }
    }
  }
  
  fmla_vars = c(fmla_vars, prob_vars)
}

#### Model ###
source('Model.R')

## merge demographic information
merge_demographics = function(predy, test_data){
  prediction = test_data[c("student_id")]
  # Generalise test data and predictions to pass and fail only, needed for majority voting
  prediction = cbind(prediction, predy)
  if("predy" %in% colnames(prediction)){
    prediction$result_predy = ifelse(prediction$predy >= 50, "Pass", "Fail")
  }
  return(prediction)
}

# Majority voting
majority_vote = function(list_predictions){
  model_count = length(list_predictions)
  maj_pred = data.frame()
  for (df in list_predictions){
    if(is_empty(maj_pred)){
      maj_pred = as.data.frame(df$result_predy)
    }else{
      maj_pred = cbind(maj_pred, df$result_predy)
    }
  }
  #Count number of "Pass" per row, i.e number of models that predicted Pass
  pass_votes = rowSums(maj_pred == "Pass")
  maj_pred = cbind(maj_pred, pass_votes)
  maj_pred$maj_vote = ifelse(pass_votes > model_count /2, "Pass", "Fail")
  return(maj_pred)
}

# Regression tree
rt_pred = model_rt(fmla_vars, train, validate, test)

# GLM
glm_pred = model_glm(fmla_vars, train, validate, test)

# Naive Bayes
bayes_pred = model_glm(fmla_vars, train, validate, test)

# Merge demographics
if(predict_on_test){
  rt_pred = merge_demographics(rt_pred, test)
  glm_pred = merge_demographics(glm_pred, test)
  bayes_pred = merge_demographics(bayes_pred, test)
}else{
  rt_pred = merge_demographics(rt_pred, validate)
  glm_pred = merge_demographics(glm_pred, validate)
  bayes_pred = merge_demographics(bayes_pred, validate)
}

#Majority voting
list_preds = list(rt_pred, glm_pred, bayes_pred)
maj_pred = majority_vote(list_preds)
if(predict_on_test){
  maj_pred = merge_demographics(maj_pred, test)
}else{
  maj_pred = merge_demographics(maj_pred, validate)
}

#Confusion matrix and accuracy on validation data
if(!predict_on_test){
  rt_acc = accuracy(validate, rt_pred, "result_predy")
  glm_acc = accuracy(validate, glm_pred, "result_predy")
  bayes_acc = accuracy(validate, bayes_pred, "result_predy")
  maj_acc = accuracy(validate, maj_pred, "maj_vote")
}