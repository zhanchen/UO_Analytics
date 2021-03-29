##### Tracking and panopto video data #######
p_tracking = function(data, week_range = 3){
  # extract sp start date
  data$sp_start_date = unlist(sapply(data$term_code, function(x){ return(courses[courses$term_code == x, 'study_period_start_date'])}))
  
  # calculate week index, it seems records in database will be one week prior to actual start date
  data$'week_no' = paste('Week', ceiling(difftime(data$landingtime, as.Date(data$sp_start_date), units = 'weeks'))-1)
  
  # Extract data from desired week range
  data = data[data$week_no %in% paste('Week', 1:week_range),] # week1 to 3
  
  # sort data
  data = orderBy(~ term_code + userid + landingtime + duration, data)
  
  # calculate actual duration
  data$gap = abs(append(data$landingtime[-1], Sys.time()) - data$landingtime)
  data$act_duration = pmin(data$duration, data$gap)
  
  # rm invalid records
  data = data[data$act_duration != 0, ]
  
  # assign session 
  data$session = unlist(
    lapplyBy(
      formula = ~ term_code + userid, data = data, 
      FUN = function(x){
        res = c()
        ind = 1
        test = x$landingtime
        repeat{
          temp = split(test, test > test[1] + 60*30) # session interval
          res = append(res, rep(ind, length(temp$`FALSE`)))
          if (length(temp$`TRUE`)){
            test = temp$`TRUE`
            ind = ind + 1
          }else{
            break
          }
        }
        res
      }
    )
  )
  
  data_summary1 = summaryBy(
    formula = act_duration ~ term_code + userid + session + pagetitle,
    data = data,
    FUN = sum
  )
  colnames(data_summary1)[ncol(data_summary1)] = 'duration_per_page'
  
  data_summary2 = summaryBy(
    formula = duration_per_page ~ term_code + userid + session,
    data = data_summary1,
    FUN = c(median, length)
  )
  colnames(data_summary2)[ncol(data_summary2)] = 'num_pages'
  
  data_summary3 = summaryBy(
    formula = duration_per_page.median + num_pages ~ term_code + userid,
    data = data_summary2,
    FUN = c(mean, length)
  )
  data_summary3 = data_summary3[,-ncol(data_summary3)]
  colnames(data_summary3)[ncol(data_summary3)-2:0] =c(
    'avg_duration_page.viewing_per_session',
    'avg_num_page.viewing_per_session',
    'num_sessions'
  )
  
  return(data_summary3)
} 


###### Panopto video #####
p_panopto = function(data, week_range = 3){
  
  # extract sp start date
  data$sp_start_date = unlist(sapply(data$TERM_CODE, function(x){ return(courses[courses$term_code == x, 'study_period_start_date'])}))
  
  # caculate week index, it seems records in database will be one week prior to actual start date
  # TODO: Week_no wrongly calculated for later courses
  data$'week_no' = paste('Week', ceiling(difftime(data$VIEW_TIME, as.Date(data$sp_start_date), units = 'weeks'))-1)
  
  # Extract data from desired week range
  data = data[data$week_no %in% paste('Week', 1:week_range),] # week1 to 3
  
  # sort data
  data = orderBy(~ TERM_CODE + STUDENT_ID + VIEW_TIME + SECONDS_VIEWED, data )
  
  # caculate actual duration
  data$gap = abs(append(data$VIEW_TIME[-1], Sys.time()) - data$VIEW_TIME)
  data$act_duration = pmin(data$SECONDS_VIEWED, data$gap)
  
  # rm invalid records
  data = data[data$act_duration != 0, ]
  
  # assign session 
  data$session = unlist(
    lapplyBy(
      formula = ~ TERM_CODE + STUDENT_ID, data = data, 
      FUN = function(x){
        res = c()
        ind = 1
        test = x$VIEW_TIME
        repeat{
          temp = split(test, test > test[1] + 60*30) # session interval
          res = append(res, rep(ind, length(temp$`FALSE`)))
          if (length(temp$`TRUE`)){
            test = temp$`TRUE`
            ind = ind + 1
          }else{
            break
          }
        }
        res
      }
    )
  )
  
  data_summary1 = summaryBy(
    formula = act_duration ~ TERM_CODE + STUDENT_ID + session + PANOPTO_SESSION_KEY,
    data = data,
    FUN = sum
  )
  colnames(data_summary1)[ncol(data_summary1)] = 'duration_per_video'
  
  data_summary2 = summaryBy(
    formula = duration_per_video ~ TERM_CODE + STUDENT_ID + session,
    data = data_summary1,
    FUN = c(median, length)
  )
  colnames(data_summary2)[ncol(data_summary2)] = 'num_videos'
  
  data_summary3 = summaryBy(
    formula = duration_per_video.median + num_videos ~ TERM_CODE + STUDENT_ID,
    data = data_summary2,
    FUN = c(mean, length)
  )
  data_summary3 = data_summary3[,-ncol(data_summary3)]
  colnames(data_summary3)[ncol(data_summary3)-2:0] =c(
    'avg_duration_video.viewing_per_session',
    'avg_num_video.viewing_per_session',
    'num_sessions'
  )
  
  return(data_summary3)
  
}


###### H5P embedded quiz ####
p_h5p = function(h5p, week_range = 3){
  
  # extract sp start date
  h5p$sp_start_date = unlist(sapply(h5p$`Course_ID`, function(x){ return(courses[courses$moodle_course_id == x, 'study_period_start_date'])}))
  
  # calculate week index, it seems records in database will be one week prior to actual start date
  h5p$'week_no' = paste('Week', ceiling(difftime(h5p$`H5P_Action_Time`, as.Date(h5p$sp_start_date), units = 'weeks'))-1)
  
  # Extract data from desired week range
  h5p = h5p[h5p$week_no %in% paste('Week', 1:week_range),] # week1 to 3
  
  # no. of try
  summary1 = summaryBy(
    formula = H5P_Action_Time ~ Course_ID + Student_ID + H5P_Activity_Name,
    data = h5p,
    FUN = function(x){length(unique(x))}
  )
  colnames(summary1)[ncol(summary1)] = 'no_try'
  
  
  # max score student achieved
  h5p$Student_Score = h5p$Student_Score/h5p$Max_Score
  h5p[is.na(h5p)] = 1
  summary2 = summaryBy(
    formula = Student_Score ~ Course_ID + Student_ID + H5P_Activity_Name,
    data = h5p,
    FUN = max,
    na.rm = T
  )
  
  # merge
  summary3 = merge(summary1, summary2)
  
  # no. of quiz
  summary4 = summaryBy(
    formula = H5P_Activity_Name ~ Course_ID + Student_ID,
    data = summary3,
    FUN = length
  )
  colnames(summary4)[3] = 'no_quiz'
  
  # mean of ...
  summary5 = summaryBy(
    formula = no_try + Student_Score.max ~ Course_ID + Student_ID,
    data = summary3,
    FUN = mean,
    na.rm = T
  )
  
  # merge
  summary5 = merge(summary4, summary5)
  
  return(summary5)
}

###### Moodle log #######
p_m_log = function(m_log, week_range = 3){
  
  # activity labels
  m_log$activity = paste0(m_log$module, ', ', m_log$action)
  
  # extract sp start date
  m_log$sp_start_date = unlist(sapply(m_log$term_code, function(x){ return(courses[courses$term_code == x, 'study_period_start_date'])}))
  
  # calculate week index, it seems records in database will be one week prior to actual start date
  m_log$'week_no' = paste('Week', ceiling(difftime(m_log$time, as.Date(m_log$sp_start_date), units = 'weeks'))-1)
  
  # Extract data from desired week range
  m_log = m_log[m_log$week_no %in% paste('Week', 1:week_range),] # week1 to 3
  
  # activity labels
  Assessment = c('assign, view', 'assign, submit for grading','assign, submissioncopied','quiz, continue attempt',
                 'quiz, view','quiz, attempt')
  
  Informing_Orienteering = c('assign, view all','assign, view','assign, view submit assignment form','forum, search',
                             'forum, view all','quiz, view all','zoom, view all')
  
  Learning_Content_Access = c('course, view section','lti, view','page, view','resource, view','url, view')
  
  Metacognitive_Monitoring = c('feedback, view','course, user report','forum, user report','grade, update','quiz, review',
                               'quiz, view summary')
  
  Metacognitive_Planning = c('calendar, add','deadline_extensions, create','deadline_extensions, view','forum, subscribe',
                             'forum, unsubscribe','forum, start tracking','forum, stop tracking')
  
  Social_Interaction_Passive = c('forum, view discussion','forum, view forum','forum, view')
  
  Social_Interaction_Active = c('forum, add discussion','forum, delete discussion','forum, delete post','forum, add post',
                                'forum, update post')
  
  m_log = sessionise(x = m_log, timestamp = time, user_id = userid, threshold = 1800)
  
  #Codify activities as follows
  # Assessment = A
  # Informing_Orienteering = I
  # Learning_Content_Access = L
  # Metacognitive_Monitoring = M
  # Metacognitive_Planning = P
  # Social_Interaction_Passive = R
  # Social_Interaction_Active = W
  m_log$activity_type = ifelse(m_log$activity %in% Assessment, "A",
                               ifelse(m_log$activity %in% Informing_Orienteering, "I",
                                      ifelse(m_log$activity %in% Learning_Content_Access,"L", 
                                             ifelse(m_log$activity %in% Metacognitive_Monitoring, "M",
                                                    ifelse(m_log$activity %in% Metacognitive_Planning, "P",
                                                           ifelse(m_log$activity %in% Social_Interaction_Passive, "R", "W"))))))

  m_log = m_log[with(m_log, order(session_id, time)), ]
  
  m_log = m_log %>% group_by(session_id) %>% mutate(activity_sequence = paste0(activity_type, collapse = ""))
  
  summary1 = summaryBy(
    formula = time ~ term_code + userid, data = m_log,
    FUN = function(test){
      test = sort(unique(test), decreasing = F)
      res = list()
      repeat{
        temp = split(test, test > test[1] + 60*30) # session interval
        res[[length(res)+1]] = temp$`FALSE`
        if (length(temp$`TRUE`)){
          test = temp$`TRUE`
        }else{
          break
        }
      }

      resH = unlist(lapply(res, function(x){as.numeric(format(as.POSIXct(x[1], origin = '1970-01-01'),'%H'))}))

      c(
        Session_No = length(res),
        Session_Time_Mean = mean(unlist(lapply(res, function(x){x[length(x)] - x[1]}))),
        Morning_Session = sum(resH >= 4 & resH < 12),
        Day_Session = sum(resH >= 12 & resH < 20),
        Night_Session = sum(resH >= 20 | resH < 4)
      )

    }
  )

  #Activity sums
  summary2 = summaryBy(
    activity ~ term_code + userid, data = m_log,
    FUN = function(ac){
      c(
        Assessment = sum(ac %in% Assessment),
        Informing_Orienteering = sum(ac %in% Informing_Orienteering),
        Learning_Content_Access = sum(ac %in% Learning_Content_Access),
        Metacognitive_Monitoring = sum(ac %in% Metacognitive_Monitoring),
        Metacognitive_Planning = sum(ac %in% Metacognitive_Planning),
        Social_Interaction_Passive = sum(ac %in% Social_Interaction_Passive),
        Social_Interaction_Active = sum(ac %in% Social_Interaction_Active)
      )
    }
  )
  
  result = merge(summary1, summary2)
  
  ##### Extract n-gram features #####
  # if(use_bigram_probs){
  #   summary3 = p_m_log_bigrams(m_log)
  #   result = merge(result, summary3)
  # }
  
  return(result)
}

##### Extract n-gram features #####
p_m_log_bigrams = function(m_log, week_range = 3){
  # activity labels
  m_log$activity = paste0(m_log$module, ', ', m_log$action)
  
  # extract sp start date
  m_log$sp_start_date = unlist(sapply(m_log$term_code, function(x){ return(courses[courses$term_code == x, 'study_period_start_date'])}))
  
  # calculate week index, it seems records in database will be one week prior to actual start date
  m_log$'week_no' = paste('Week', ceiling(difftime(m_log$time, as.Date(m_log$sp_start_date), units = 'weeks'))-1)
  
  # Extract data from desired week range
  m_log = m_log[m_log$week_no %in% paste('Week', 1:week_range),] # week1 to 3
  
  # activity labels
  Assessment = c('assign, view', 'assign, submit for grading','assign, submissioncopied','quiz, continue attempt',
                 'quiz, view','quiz, attempt')
  
  Informing_Orienteering = c('assign, view all','assign, view','assign, view submit assignment form','forum, search',
                             'forum, view all','quiz, view all','zoom, view all')
  
  Learning_Content_Access = c('course, view section','lti, view','page, view','resource, view','url, view')
  
  Metacognitive_Monitoring = c('feedback, view','course, user report','forum, user report','grade, update','quiz, review',
                               'quiz, view summary')
  
  Metacognitive_Planning = c('calendar, add','deadline_extensions, create','deadline_extensions, view','forum, subscribe',
                             'forum, unsubscribe','forum, start tracking','forum, stop tracking')
  
  Social_Interaction_Passive = c('forum, view discussion','forum, view forum','forum, view')
  
  Social_Interaction_Active = c('forum, add discussion','forum, delete discussion','forum, delete post','forum, add post',
                                'forum, update post')
  
  m_log = sessionise(x = m_log, timestamp = time, user_id = userid, threshold = 1800)
  
  #Codify activities as follows
  # Assessment = A
  # Informing_Orienteering = I
  # Learning_Content_Access = L
  # Metacognitive_Monitoring = M
  # Metacognitive_Planning = P
  # Social_Interaction_Passive = R
  # Social_Interaction_Active = W
  m_log$activity_type = ifelse(m_log$activity %in% Assessment, "A",
                               ifelse(m_log$activity %in% Informing_Orienteering, "I",
                                      ifelse(m_log$activity %in% Learning_Content_Access,"L", 
                                             ifelse(m_log$activity %in% Metacognitive_Monitoring, "M",
                                                    ifelse(m_log$activity %in% Metacognitive_Planning, "P",
                                                           ifelse(m_log$activity %in% Social_Interaction_Passive, "R", "W"))))))
  
  m_log = m_log[with(m_log, order(session_id, time)), ]
  
  m_log = m_log %>% group_by(session_id) %>% mutate(activity_sequence = paste0(activity_type, collapse = ""))
  
  summary3 = m_log %>% group_by(activity_sequence, session_id, userid, term_code) %>% tally()
  #remove repeated activities
  summary3$activity_sequence = gsub('([[:alpha:]])\\1+', '\\1', summary3$activity_sequence)
  #aggregate all activities per student and course
  summary3 = summary3 %>% group_by(userid, term_code) %>% mutate(mean_ac_per_session = mean(n))
  summary3 = aggregate(activity_sequence ~ userid + term_code, summary3, FUN=toString)
  summary3$activity_sequence = gsub('\\s+', '', summary3$activity_sequence)
  #add "," for final session's end
  summary3$activity_sequence = paste(summary3$activity_sequence, ",", sep="")
  
  # Get list of subsequent activities for each activity type
  for (row in 1:nrow(summary3)) {
    afterA = ""
    afterI = ""
    afterL = ""
    afterM = ""
    afterP = ""
    afterR = ""
    afterW = ""
    firstAcs = ""
    ac_sequences = summary3[row, 3]
    
    ac_sequences_split = strsplit(ac_sequences, "")[[1]]
    # colon character "," delimits each session, if previous char is colon, next char is first activity,
    #if colon is next char then prev char is last activity in session
    prev_ac = ","
    for (ac in ac_sequences_split) {
      switch(prev_ac, 
             "A"={
               # case 'A'
               afterA = paste(afterA, ac, sep="")
             },
             "I"={
               # case 'I' 
               afterI = paste(afterI, ac, sep="")
             },
             "L"={
               # case 'L'
               afterL = paste(afterL, ac, sep="")
             },
             "M"={
               # case 'M'
               afterM = paste(afterM, ac, sep="")
             },
             "P"={
               # case 'P'
               afterP = paste(afterP, ac, sep="")
             },
             "R"={
               # case 'R'
               afterR = paste(afterR, ac, sep="")
             },
             "W"={
               # case 'W'
               afterW = paste(afterW, ac, sep="")
             },
             {
               # case ","
               firstAcs = paste(firstAcs, ac, sep="")
             }
      )
      prev_ac = ac
    }
    # Add last "," for final session
    summary3[row, 4] = afterA
    summary3[row, 5] = afterI
    summary3[row, 6] = afterL
    summary3[row, 7] = afterM
    summary3[row, 8] = afterP
    summary3[row, 9] = afterR
    summary3[row, 10] = afterW
    summary3[row, 11] = firstAcs
  }
  summary3 = summary3 %>% rename(afterA = V4, afterI = V5, afterL = V6, afterM = V7, afterP = V8, afterR = V9, afterW = V10, firstAcs = V11)    
  
  #Calculate all Markov chain bi-gram conditional probabilities
  # make list of new column names and add
  new_colnames = c()
  for(ac in c("A", "I", "L", "M", "P", "R", "W")){
    newcol = paste("Start", ac, sep = ".to.")
    new_colnames = c(new_colnames, newcol)
    for (ac2 in c("A", "I", "L", "M", "P", "R", "W", "End")){
      if (ac != ac2){
        newcol = paste(ac, ac2, sep = ".to.")
        new_colnames = c(new_colnames, newcol)
      }
    }
  }
  summary3[new_colnames] = 0
  
  #Calculate probabilities of an activity given the previous activity
  for (row in 1:nrow(summary3)) {
    for(col in 4:11){
      toA = toI = toL = toM = toP = toR = toW = toEnd = 0
      col_name = colnames(summary3)[col]
      start_colname = ""
      
      if (col_name == "firstAcs"){
        start_colname = "Start"
      }else{
        # last char in colname correspond to starting activity e.g afterA -> A.to.I, A.to.L etc.
        start_colname = str_sub(col_name, start = -1)
      }
      
      ac_sequences = summary3[row, col]
      ac_sequences_split = strsplit(ac_sequences, "")[[1]]
      for (ac in ac_sequences_split) {
        switch(ac, 
               "A"={
                 # case 'A'
                 toA = toA + 1
               },
               "I"={
                 # case 'I' 
                 toI = toI + 1
               },
               "L"={
                 # case 'L'
                 toL = toL + 1
               },
               "M"={
                 # case 'M'
                 toM = toM + 1
               },
               "P"={
                 # case 'P'
                 toP = toP + 1
               },
               "R"={
                 # case 'R'
                 toR = toR + 1
               },
               "W"={
                 # case 'W'
                 toW = toW + 1
               },
               {
                 # case ","
                 toEnd = toEnd + 1
               }
        )
      }
      #Calculate probabilities and assign to corresponding columns
      total_ac = total_ac = toA + toI + toL + toM + toP + toR + toW + toEnd
      
      if(start_colname == "Start"){
        # Start -> End sequence not possible
        total_ac = total_ac - toEnd
      }
      
      if(start_colname != "A"){
        dest_col = paste(start_colname, "A", sep=".to.")
        summary3[row, dest_col] = as.numeric(ifelse(total_ac == 0, 0, toA/total_ac))
      }
      if(start_colname != "I"){
        dest_col = paste(start_colname, "I", sep=".to.")
        summary3[row, dest_col] = as.numeric(ifelse(total_ac == 0, 0, toI/total_ac))
      }
      if(start_colname != "L"){
        dest_col = paste(start_colname, "L", sep=".to.")
        summary3[row, dest_col] = as.numeric(ifelse(total_ac == 0, 0, toL/total_ac))
      }
      if(start_colname != "M"){
        dest_col = paste(start_colname, "M", sep=".to.")
        summary3[row, dest_col] = as.numeric(ifelse(total_ac == 0, 0, toM/total_ac))
      }
      if(start_colname != "P"){
        dest_col = paste(start_colname, "P", sep=".to.")
        summary3[row, dest_col] = as.numeric(ifelse(total_ac == 0, 0, toP/total_ac))
      }
      if(start_colname != "R"){
        dest_col = paste(start_colname, "R", sep=".to.")
        summary3[row, dest_col] = as.numeric(ifelse(total_ac == 0, 0, toR/total_ac))
      }
      if(start_colname != "W"){
        dest_col = paste(start_colname, "W", sep=".to.")
        summary3[row, dest_col] = as.numeric(ifelse(total_ac == 0, 0, toW/total_ac))
      }
      if(start_colname !="Start"){
        dest_col = paste(start_colname, "End", sep=".to.")
        summary3[row, dest_col] = as.numeric(ifelse(total_ac == 0, 0, toEnd/total_ac))
      }
    }
  }
  
  #drop unnecessary columns
  summary3 = subset(summary3, select=-c(activity_sequence, afterA, afterI, afterL, afterM, afterP, afterR, afterW, firstAcs))
  
  return(summary3)
}
