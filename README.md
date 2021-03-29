# UniSA Online Predictive Analytics Project
This project's aim is to develop a pipeline in R for predicting student performance in a course based on data available early in the course, by using historical course data to train predictive models.

## Structure
The pipeline is structured as follows:
* Main.R: wrapper for all other modules, used for setting inputs, splitting train/test datasets, merging features, perform predictions and organising output.
    * R_SQL_Connection.R: Connects to databases and retrieves required data
    * Data_extract.R: Extract required tables for analysis
    * Feature_extract.R: Contains functions for extracting features from retrieved tables
    * Model_R.: Contains functions for performing predictive analytics on data

## Inputs
The following inputs can be set in Main.R:
* Target_course: String, name of the course.
* start_date: String, in 'YYYY-MM-DD' format. Date of course start.
* week_range: Int, number of weeks of data to be used for training predictive model.
* cross_validate: Boolean, create cross-validation data? If true, latest course offering data is used as test, 2nd latest as validate and all older offerings as train.
* use_bigram_probs: Boolean, Extract bi-gram conditional probability features from m_log? (Heavily increases processing time)
* predict_on_test: Boolean, use models to predict on test data? if F predicts on validate data (Recommend default FALSE, TRUE should only be used for testing changes)

## Functions
### Main.R Functions
#### merge_demographics = function(predy, test_data)
Function for merging student information (student ID) with model predictions, and generalising grade predictions into "Pass" and "Fail" values

**Inputs:**

* predy: Dataframe containing model prediction result, as outputted by model_rt(), model_glm() and model_naiveBayes()
* test_data: Datframe containing test data, required for student IDs

**Output:**

Dataframe of 3 columns containing student ID, grade prediction and pass or fail value

**Example usage:**

```
rt_pred = model_rt(fmla_vars, train, validate, test)
merge_demographics(rt_pred, test)

head(rt_pred)

  student_id    predy result_predy
1  XXXXXXXXX 75.90688         Pass
2  XXXXXXXXX 59.36207         Pass
3  XXXXXXXXX 75.90688         Pass
4  XXXXXXXXX 81.54255         Pass
5  XXXXXXXXX 75.90688         Pass
6  XXXXXXXXX 75.90688         Pass
```

#### majority_vote = function(list_predictions)
Function for majority voting between multiple models, based on models' predicted "Pass" and "Fail" values

**Input:**

* list_predictions: list of dataframes containing model predictions, as outputted by merge_demographics()

**Output:**

Dataframe containing student ID, columns containing Pass/Fail result from each predictive model, number of "Pass" votes accross all inputted results, and Pass/Fail result of majority vote

**Example usage:**

```
list_preds = list(rt_pred, glm_pred, bayes_pred)
maj_pred = majority_vote(list_preds)

head(maj_pred)
  student_id df$result_predy df$result_predy df$result_predy pass_votes maj_vote
1   XXXXXXXX            Pass            Pass            Pass          3     Pass
2  XXXXXXXXX            Pass            Pass            Pass          3     Pass
3  XXXXXXXXX            Pass            Pass            Pass          3     Pass
4  XXXXXXXXX            Pass            Pass            Pass          3     Pass
5  XXXXXXXXX            Pass            Pass            Pass          3     Pass
6  XXXXXXXXX            Pass            Pass            Pass          3     Pass
```


### Feature_Extract.R Functions
#### p_m_log_bigrams = function(m_log, week_range = 3)
Extracts conditional bi-gram probability features from m_log. An implementation of technique described in: Li, Xiao, Wang, Ting & Wang, Huaimin 2017, ‘Exploring N-gram Features in Clickstream Data for MOOC Learning Achievement Prediction’, in Database Systems for Advanced Applications, Springer International Publishing, Cham, pp. 328–339.
NOTE: Very long processing time

**Inputs:**

* m_log: dataframe of m_log data, as processed by Data_extract.R
* week_range: number of weeks of course data to analyse

**Output:**

Dataframe containing conditional bi-gram probability features for each activity to each other activity and end state, as well as from start state to each activity, for each student per course.
```
head(m_log_bigrams_p)
  userid term_code Start.to.A      A.to.I    A.to.L     A.to.M     A.to.P     A.to.R    A.to.W   A.to.End Start.to.I I.to.A I.to.L
1      X      1812          0 0.000000000 0.0000000 0.00000000 0.00000000 0.00000000 0.0000000 0.00000000          0      0    0.0
2      X      1812          0 0.000000000 0.0000000 0.00000000 0.00000000 0.00000000 0.0000000 0.00000000          0      0    0.0
3    XXX      1812          0 0.000000000 0.0000000 0.00000000 0.00000000 0.00000000 0.0000000 0.00000000          0      0    0.0
4    XXX      1812          0 0.000000000 0.3333333 0.00000000 0.00000000 0.00000000 0.6666667 0.00000000          0      0    0.0
5    XXX      1812          0 0.000000000 0.0000000 0.00000000 0.00000000 0.00000000 0.0000000 0.00000000          0      0    0.0
6   XXXX      1812          0 0.009174312 0.1376147 0.02752294 0.02752294 0.04587156 0.7155963 0.03669725          0      0    0.5
```

### Model.R Functions
#### model_rt = function(fmla_vars, train_data, val_data, test_data)
Make student grade predictions using Regression Tree technique.

**Inputs:**

* fmla_vars: vector of column names of features to use in prediction
* train_data: dataframe of training data
* val_data: dataframe of validation data
* test_data: dataframe of test data

**Output:**

Dataframe of 1 column containing predicted grades, ordered in same student order as test_data.

**Example usage:**

```
fmla_vars = c("avg_duration_page.viewing_per_session", "avg_num_page.viewing_per_session", "num_sessions_track",
              "no_quiz", "no_try.mean", "Student_Score.max.mean", "time.Session_No", "time.Session_Time_Mean",
              "time.Morning_Session", "time.Day_Session", "time.Night_Session", "activity.Assessment", "activity.Informing_Orienteering", 
              "activity.Learning_Content_Access", "activity.Metacognitive_Monitoring", "activity.Metacognitive_Planning",
              "activity.Social_Interaction_Passive", "activity.Social_Interaction_Active")

model_rt = function(fmla_vars, train_data, val_data, test_data)
```

#### model_glm = function(fmla_vars, train_data, val_data, test_data)
Make student grade predictions using Generalised Linear Model technique

**Inputs:**

* fmla_vars: vector of column names of features to use in prediction
* train_data: dataframe of training data
* val_data: dataframe of validation data
* test_data: dataframe of test data

**Output:**

Dataframe of 1 column containing predicted grades, ordered in same student order as test_data.

**Example usage:**

See model_rt above

#### model_naiveBayes = function(fmla_vars, train_data, val_data, test_data)
Make student grade predictions using Naïve Bayes technique.

**Inputs:**
* fmla_vars: vector of column names of features to use in prediction
* train_data: dataframe of training data
* val_data: dataframe of validation data
* test_data: dataframe of test data

**Output:**

Dataframe of 1 column containing predicted grades, ordered in same student order as test_data.

**Example usage:**

See model_rt above

#### accuracy = function(validate_data, pred, pred_col_name)
Compares model prediction results with validation data and outputs confusion matrix and accuracy values

**Inputs:**

* validate_data: dataframe of validation data
* pred: dataframe of prediction model results, which was passed through merge_demographics() to produce column with "Pass" and "Fail" values
* pred_col_name: column name which contains "Pass" and "Fail values"

**Output:**

Dataframe of 5 columns containing confusion matrix values and accuracy of inputted model results.

**Example usage:**

```
head(rt_pred)

  student_id    predy result_predy
1  XXXXXXXXX 75.90688         Pass
2  XXXXXXXXX 59.36207         Pass
3  XXXXXXXXX 75.90688         Pass
4  XXXXXXXXX 81.54255         Pass
5  XXXXXXXXX 75.90688         Pass
6  XXXXXXXXX 75.90688         Pass

rt_acc = accuracy(validate, rt_pred, "result_predy")
```
