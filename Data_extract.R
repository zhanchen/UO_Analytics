####### SQL query connection ############
#source('~/rds/Unisa Online/Code/Rcode/R_SQL_Conn.R')

# ##### Inputs #####
# Target_course = 'UO Foundations of Human Biology 1' #### set for target course in long description
# start_date = '2017-01-01'
# ##################

##### Common Step #####
# Load Data files
load('./Data/Intern_data210202.RData')
load('./Data/non_sensi_moodle_users.RData')
load('./Data/student_demo_INTERN.RData')

#### Demographics  ####
# sqlres = dbSendQuery(Oracle_con, "SELECT * FROM LEARNING_TEACHING_DW.S_STUDENT")

all_student_Demographics = student_Demograhpics_INTERN

# sqlres = dbSendQuery(
#   UO_moodle_con,
#   '
#   SELECT * FROM public.m_user
#   '
# )
moodle_users = non_sensi_moodle_users

# m_users? or student_demographics?

### Course info ###

# sqlres = dbSendQuery(
#   UO_moodle_con, 
#   paste0("SELECT x.* FROM public.m_unisa_course x 
#   WHERE x.course_desc_long = '", Target_course, "'")
# )
# 
# courses = fetch(sqlres, n = -1)

courses$study_period_start_date = substr(as.character(as.POSIXct(courses$study_period_start_date, origin = '1970-01-01')),1, 10)
#courses = courses[as.Date(courses$study_period_start_date) >= as.Date(start_date),]

##### Query of tracking/time_on_task data #####
# sqlres = dbSendQuery(
#   EDW_Cloud_con, 
#   paste0("SELECT * FROM unisa_edw.uo_tracking edw WHERE edw.courseid IN (", paste(courses$moodle_course_id, collapse = ','), ")")
# )
# 
# tracking = fetch(sqlres, n = -1)

# add term_code (already merged in RData file)
#tracking = merge(tracking, courses[, c('moodle_course_id', 'term_code')], by.x = 'courseid', by.y = 'moodle_course_id')

tracking$landingtime = as.POSIXct(tracking$landingtime, origin = '1970-01-01') # convert log time

###### Grades #####
# sqlres = dbSendQuery(
#   Oracle_con,
#   paste0("SELECT DISTINCT ss.STUDENT_ID, csfgf.* FROM LEARNING_TEACHING_DW.C_STUDENT_FINAL_GRADE_FT csfgf, LEARNING_TEACHING_DW.S_STUDENT ss , LEARNING_TEACHING_DW.C_COURSE_OFFERING cco 
#   WHERE csfgf.STUDENT_KEY = ss.STUDENT_KEY AND csfgf.COURSE_OFFERING_KEY = cco.COURSE_OFFERING_KEY AND cco.COURSE_NAME = '", Target_course, "'")
#          
# )
# grades = fetch(sqlres, n = -1)

###### Moodle log #####
# sqlres = dbSendQuery(
#   UO_moodle_con, 
#   paste0(
#   "SELECT * FROM m_log 
#   WHERE time >= ", as.integer(as.POSIXct(start_date)), " and course in (", paste(courses$moodle_course_id, collapse = ','), ")"
#   )
# )
# m_log = fetch(sqlres, n = -1)
# add term_code
# m_log = merge(m_log, courses[, c('moodle_course_id', 'term_code')], by.x = 'course', by.y = 'moodle_course_id')
# convert datatime
m_log$time = as.POSIXct(m_log$time, origin = '1970-01-01')


###### Panopto video ########
# sqlres = dbSendQuery(
#   Oracle_con, 
#   paste0(
#     "SELECT DISTINCT CCO.TERM_CODE, ss.STUDENT_ID, cpsuf.*
# FROM LEARNING_TEACHING_DW.C_PANOPTO_STUDENT_USAGE_FT cpsuf, LEARNING_TEACHING_DW.C_PANOPTO_SESSION_FT cpsf, LEARNING_TEACHING_DW.C_COURSE_OFFERING cco , LEARNING_TEACHING_DW.S_STUDENT ss 
# WHERE cpsuf.PANOPTO_SESSION_KEY = CPSF.PANOPTO_SESSION_KEY AND CPSUF.STUDENT_KEY = ss.STUDENT_KEY AND CPSF.COURSE_OFFERING_KEY = cco.COURSE_OFFERING_KEY AND  cco.COURSE_NAME IN ('", Target_course, "')"
#   )
# )
#pan_use = fetch(sqlres, n = -1)

######## Learning planner #########
# sqlres = dbSendQuery(
#   MSSQL_con,
#   paste(
#     '
#   SELECT * 
#   FROM dbo.StudentKnow sk, dbo.Week w, dbo.NeedToKnow ntk 
#   WHERE w.Id = ntk.WeekId AND sk.NeedToKnowId = ntk.Id AND w.CourseId in (
#   ',
#     paste(courses$course_id, collapse = ','),
#     ')
#   '
#   )
# )
# 
# studentknow = fetch(sqlres, n = -1)


####### H5P ##########
# sqlres = dbSendQuery(
#   UO_moodle_con,
#   paste(
#     '
#   select
# 	hvp.course                                               as "Course_ID",
# 	hvp."name"                                               as "H5P_Activity_Name",
# 	hvp.id                                                   as "H5P_Activity_ID",
# 	u.firstname                                              as "Student_Firstname",
# 	u.lastname                                               as "Student_Lastname",
# 	u.idnumber                                               as "Student_ID",
# 	l.userid                                                 as "Student_Moodle_ID", 
# 	u.email                                                  as "Student_Email",
# 	l.ip                                                     as "H5P_IP_Address", 
# 	to_timestamp(l."time") at time zone \'Australia/Adelaide\' as "H5P_Action_Time", 
# 	l."time"                                                 as "H5P_Action_Timestamp", 
# 	hr.id                                                    as "H5P_Log_ID",
# 	hr.parent_id                                             as "H5P_Parent_Log_ID",
# 	hr.interaction_type                                      as "Interaction_Type",
# 	hr.description                                           as "Interaction_Description",
# 	hr.additionals                                           as "Additional_Info",
# 	hr.max_score                                             as "Max_Score",
# 	hr.correct_responses_pattern                             as "Correct_Responses",
# 	hr.response                                              as "Student_Responses",
# 	hr.raw_score                                             as "Student_Score"
# from
# 	public.m_hvp_xapi_results hr,
# 	m_log l,
# 	m_hvp hvp,
# 	m_user u
# where
# 	hr.content_id = hvp.id          and
# 	hr.content_id::varchar = l.info and 
# 	hvp.course = l.course           and
# 	hr.user_id = l.userid           and
# 	hr.user_id = u.id               and
# 	hvp.course in 
#   (', 
#     paste(courses$moodle_course_id, collapse = ','), 
#     ')
# 	order by hvp.id, 
#          userid, 
#          l."time", 
#          hr.id
#   '
#   )
# 
# )
# h5p = fetch(sqlres, n = -1)


