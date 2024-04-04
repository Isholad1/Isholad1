setwd("C:/Users/ifunanya/OneDrive/Desktop/Survey_data_files")
library(readxl)  
library(ggplot2)
library(dplyr)   
library(skimr)
library(stats)
library(rpart)
library(rpart.plot)

winter_data<- read.csv("WorkingAtHome_Survey_Winter2020-21_commissioned_data_collection.csv")

skim(winter_data)
sum(is.na(winter_data))

###Data Inspection and variable renaming#####
##Auditory distraction##
table(winter_data$Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.experience.auditory.distractions.whilst.working.)

winter_data <- rename(winter_data,auditory_distraction= Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.experience.auditory.distractions.whilst.working.)

table(winter_data$auditory_distraction,useNA = "ifany")

##Visual distraction##
table(winter_data$Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.experience.visual.distractions.in.my.work.area.)

winter_data <- rename(winter_data,visual.distraction=Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.experience.visual.distractions.in.my.work.area.)

table(winter_data$visual.distraction,useNA = "ifany")

##concentration difficulty##
table(winter_data$Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.find.it.difficult.to.concentrate.on.my.work )

winter_data <- rename(winter_data, concen_difficulty = Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.find.it.difficult.to.concentrate.on.my.work.)

table(winter_data$concen_difficulty,useNA = "ifany")

####home distraction##
table(winter_data$Please.answer.the.questions.below.in.relation.to.the.current.COVID.19.homeworking.conditions....I.experience.fewer.distractions.when.working.from.home.)

winter_data <- rename(winter_data, home_distract = Please.answer.the.questions.below.in.relation.to.the.current.COVID.19.homeworking.conditions....I.experience.fewer.distractions.when.working.from.home.)

table(winter_data$home_distract,useNA = "ifany")

#work interruption##
table(winter_data$Please.answer.the.questions.below.in.relation.to.the.current.COVID.19.homeworking.conditions....Working.from.home.I.experience.fewer.interruptions.)
winter_data <- rename(winter_data, interruption = Please.answer.the.questions.below.in.relation.to.the.current.COVID.19.homeworking.conditions....Working.from.home.I.experience.fewer.interruptions.)

table(winter_data$interruption,useNA = "ifany")

##productivity##
table(winter_data$Please.answer.the.questions.below.in.relation.to.the.current.COVID.19.homeworking.conditions....Working.from.home.makes.me.more.productive.)

winter_data <- rename(winter_data,productivity= Please.answer.the.questions.below.in.relation.to.the.current.COVID.19.homeworking.conditions....Working.from.home.makes.me.more.productive.)

table(winter_data$productivity,useNA = "ifany")

##Work life imbalance###
table(winter_data$Under.COVID.19.measures..I.feel.......that.in.the.homeworking.environment.I.am.always.working.)

winter_data <- rename(winter_data, WL_imbalance = Under.COVID.19.measures..I.feel.......that.in.the.homeworking.environment.I.am.always.working.)

table(winter_data$WL_imbalance,useNA = "ifany")

##Social/work isolation##
table(winter_data$Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...Teamwork.and.camaraderie.suffer.as.a.result.of.homeworking.)

winter_data <- rename(winter_data, SW_isolation =Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...Teamwork.and.camaraderie.suffer.as.a.result.of.homeworking.)
table(winter_data$SW_isolation,useNA = "ifany")

##job_autonomy##
table(winter_data$ Please.rate.the.following.statements.in.relation.to.your.current.working.arrangements....How.much.autonomy.do.you.have.in.your.work.)

winter_data <- rename(winter_data, Job_autonomy = Please.rate.the.following.statements.in.relation.to.your.current.working.arrangements....How.much.autonomy.do.you.have.in.your.work.)

table(winter_data$ Job_autonomy, useNA = "ifany")

##Resilience##
table(winter_data$ Using.the.scale.provided..please.indicate.the.extent.to.which.you.agree.or.disagree...with.each.of.the.following.statements....I.am.able.to.adapt.to.change.)

winter_data <- rename(winter_data, Resilience = Using.the.scale.provided..please.indicate.the.extent.to.which.you.agree.or.disagree...with.each.of.the.following.statements....I.am.able.to.adapt.to.change.)

table(winter_data$Resilience, useNA = "ifany")

##Gender##
table(winter_data$How.do.you.define.your.gender..5.Male..6.Female..7.Other..8.Prefer.not.to.say)

winter_data <- rename(winter_data, Gender = How.do.you.define.your.gender..5.Male..6.Female..7.Other..8.Prefer.not.to.say)

table(winter_data$Gender, useNA = "ifany")

#Age##
table(winter_data$How.old.are.you.)
winter_data <- rename(winter_data, Age = How.old.are.you.)
table(winter_data$Age, useNA = "ifany")

#Managerial jobs##
table(winter_data$My.job.role...title.is.)
winter_data <- rename(winter_data, Managerial_jobs = My.job.role...title.is.)
table(winter_data$ Managerial_jobs, useNA = "ifany")

##HBW_satisfaction##
table(winter_data$Please.answer.the.following.questions.in.terms.of.your.overall.experience.with.homeworking.under.COVID.19.measures....All.in.all..I.am.satisfied.with.remote.work.)

winter_data <- rename(winter_data, HBW_satisfaction = Please.answer.the.following.questions.in.terms.of.your.overall.experience.with.homeworking.under.COVID.19.measures....All.in.all..I.am.satisfied.with.remote.work.)

table(winter_data$HBW_satisfaction)

##Employment_status##
table(winter_data$Which.of.the.below.best.describes.your.employment.status)
winter_data <- rename(winter_data, Employment_status =Which.of.the.below.best.describes.your.employment.status)

table(winter_data$Employment_status, useNA = "ifany")

## Age of children##
table(winter_data$Do.you.have.any.children..If.yes..please.indicate.the.number.of.children.you.have.within.each.category..Please.include.all.children.whether.or.not.they.are.living.with.you.at.the.moment....Aged.17.20...Text)

table(winter_data$Do.you.have.any.children..If.yes..please.indicate.the.number.of.children.you.have.within.each.category..Please.include.all.children.whether.or.not.they.are.living.with.you.at.the.moment....Aged.0.4...Text)

table(winter_data$ Do.you.have.any.children..If.yes..please.indicate.the.number.of.children.you.have.within.each.category..Please.include.all.children.whether.or.not.they.are.living.with.you.at.the.moment....Aged.11.16...Text)

table(winter_data$Do.you.have.any.children..If.yes..please.indicate.the.number.of.children.you.have.within.each.category..Please.include.all.children.whether.or.not.they.are.living.with.you.at.the.moment....Aged.5.10...Text )

##############################################################
##creating dataset for factor analysis for Envirinmental distraction:Environ_distraction##
## Map category labels to numeric values##
##########################################################################

##work interruption##
class(winter_data$interruption)

table(winter_data$interruption)

#Recode the values using the correct mapping
winter_data$interruption_numeric[winter_data$interruption == "Strongly disagree"] <- 1

winter_data$interruption_numeric[winter_data$interruption == "Disagree"] <- 2

winter_data$interruption_numeric[winter_data$interruption == "Somewhat disagree"] <- 3

winter_data$interruption_numeric[winter_data$interruption == "Neither agree nor disagree"] <- 4

winter_data$interruption_numeric[winter_data$interruption == "Somewhat agree"] <- 5

winter_data$interruption_numeric[winter_data$interruption == "Agree"] <- 6

winter_data$interruption_numeric[winter_data$interruption == "Strongly agree"] <- 7

table(winter_data$interruption_numeric,useNA = "ifany")

##home distraction####
class(winter_data$home_distract)

table(winter_data$home_distract)

#Recode the values using the correct mapping
winter_data$home_distract_numeric[winter_data$home_distract == "Strongly disagree"] <- 1

winter_data$home_distract_numeric[winter_data$home_distract == "Disagree"] <- 2

winter_data$home_distract_numeric[winter_data$home_distract == "Somewhat disagree"] <- 3

winter_data$home_distract_numeric[winter_data$home_distract == "Neither agree nor disagree"] <- 4

winter_data$home_distract_numeric[winter_data$home_distract == "Somewhat agree"] <- 5

winter_data$home_distract_numeric[winter_data$home_distract == "Agree"] <- 6

winter_data$home_distract_numeric[winter_data$home_distract == "Strongly agree"] <- 7

table(winter_data$home_distract_numeric,useNA = "ifany")


##Visual distraction##
class(winter_data$visual.distraction)

table(winter_data$visual.distraction)

#Recode the values using the correct mapping
winter_data$visual.distraction_numeric[winter_data$visual.distraction == "Strongly disagree"] <- 1

winter_data$visual.distraction_numeric[winter_data$visual.distraction == "Disagree"] <- 2

winter_data$visual.distraction_numeric[winter_data$visual.distraction == "Neither agree nor disagree"] <- 3

winter_data$visual.distraction_numeric[winter_data$visual.distraction == "Agree"] <- 4

winter_data$visual.distraction_numeric[winter_data$visual.distraction == "Strongly agree"] <- 5

table(winter_data$visual.distraction_numeric,useNA = "ifany")

##Auditory distraction##
class(winter_data$auditory_distraction)

table(winter_data$auditory_distraction,useNA = "ifany")

#Recode the values using the correct mapping
winter_data$auditory_distraction_numeric[winter_data$auditory_distraction == "Strongly disagree"] <- 1

winter_data$auditory_distraction_numeric[winter_data$auditory_distraction == "Disagree"] <- 2

winter_data$auditory_distraction_numeric[winter_data$auditory_distraction == "Neither agree nor disagree"] <- 3

winter_data$auditory_distraction_numeric[winter_data$auditory_distraction == "Agree"] <- 4

winter_data$auditory_distraction_numeric[winter_data$auditory_distraction == "Strongly agree"] <- 5

table(winter_data$auditory_distraction_numeric,useNA = "ifany")

##concentration difficulty##
class(winter_data$concen_difficulty)

table(winter_data$concen_difficulty,useNA = "ifany")

winter_data$concen_difficulty_numeric[winter_data$concen_difficulty == "Strongly disagree"] <- 1

winter_data$concen_difficulty_numeric[winter_data$concen_difficulty == "Disagree"] <- 2

winter_data$concen_difficulty_numeric[winter_data$concen_difficulty == "Neither agree nor disagree"] <- 3

winter_data$concen_difficulty_numeric[winter_data$concen_difficulty == "Agree"] <- 4
winter_data$concen_difficulty_numeric[winter_data$concen_difficulty == "Strongly agree"] <- 5

table(winter_data$concen_difficulty_numeric,useNA = "ifany")


### Factor analysis for environmental distraction###
#Subset the variables
Environ.distraction <- winter_data %>% 
  select(concen_difficulty_numeric, auditory_distraction_numeric, visual.distraction_numeric, home_distract_numeric,interruption_numeric)

# Remove observations with missing values
Environ.distraction <- na.omit(Environ.distraction)
skim(Environ.distraction)

# Standardize the variables
Environ.distraction <- scale(Environ.distraction)
summary(Environ.distraction)

# Perform factor analysis
factor_analysis <- factanal(Environ.distraction, factors = 1, rotation = "varimax")

# Print factor analysis results
print(factor_analysis)
##Based on the loadings provided in the factor analysis results, the variable "auditory_distraction_numeric" has the highest loading and lowest uniqueness on the single factor extracted. This suggests that "auditory_distraction_numeric" is the variable that best represents the common underlying factor related to environ_distraction in your data.##


##Cronbach's alpha assess the internal consistency or reliability of the 3 variables with positive loading as shown in the output of the load factor analysis##
#Subset the variables
Environ.distraction <- winter_data %>% 
  select(concen_difficulty_numeric, auditory_distraction_numeric, visual.distraction_numeric)

# Calculate Cronbach's alpha
alpha_result <- psych::alpha(Environ.distraction)

# Print the Cronbach's alpha coefficient
print(alpha_result$total$raw_alpha) ##value on 0.88 suggests a strong internal consistency 

###the variable auditory_distraction will be used to represent environmental distrcation##

winter_data <- rename(winter_data, Environ_distraction = auditory_distraction)
table(winter_data$Environ_distraction)


###Variables transformation#####
###productivity###
class(winter_data$productivity)
table(winter_data$productivity,useNA = "ifany")##1 outlier "Q89_1" and 1 blank category with value "3" will be changed to NA##

# Transform productivity and make as.integer##
winter_data$productivity <- as.integer(factor(winter_data$productivity,
                                             levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

######WL_imbalance######
class(winter_data$WL_imbalance)
table(winter_data$WL_imbalance,useNA = "ifany")#blank value of "8" dictated#

winter_data$WL_imbalance <- as.integer(factor(winter_data$WL_imbalance,
                levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))
                    

######Social/work_isolation######
class(winter_data$SW_isolation)
table(winter_data$SW_isolation,useNA = "ifany")#unlabeled value "3" dictated##

winter_data$SW_isolation <- as.integer(factor(winter_data$SW_isolation, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5)))

######Environmental_distraction######
class(winter_data$Environ_distraction)
table(winter_data$Environ_distraction,useNA = "ifany")#black value "2" detected

winter_data$Environ_distraction <- as.integer(factor(winter_data$Environ_distraction, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5)))

###### Job_autonomy#####
class(winter_data$Job_autonomy)
table(winter_data$Job_autonomy,useNA = "ifany")##unlabeled value "5" detected

winter_data$Job_autonomy <- as.integer(factor(winter_data$Job_autonomy,
                                              levels = c("Extremely low", "Low", "Somewhat low", "Neither low nor high", "Somewhat high", "High", "Extremely high"),labels = c(1, 2, 3, 4, 5, 6, 7)))

####Resilience#####
class(winter_data$Resilience)
table(winter_data$Resilience,useNA = "ifany")

winter_data$Resilience <- as.integer(factor(winter_data$Resilience,
                                             levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

###Gender####
table(winter_data$Gender,useNA = "ifany")
class(winter_data$Gender)
# Recode values and set appropriate levels
winter_data$Gender <- ifelse(winter_data$Gender %in% c(5, 7), "Other", ifelse(winter_data$Gender == 6, "Female", NA))

winter_data$Gender <- as.factor(winter_data$Gender)

###Age####
table(winter_data$Age,useNA = "ifany")##unlabeled value "5"#
class(winter_data$Age)

winter_data$Age <- ifelse(winter_data$Age %in% c("18-25", "26-35"), "18-35",ifelse(winter_data$Age %in% c("36-45", "46-55"), "36-55",
    ifelse(winter_data$Age %in% c("56-65", "66 and over"), "56 and over", ifelse(winter_data$Age == "Prefer not to say", NA, NA))))

winter_data$Age <- as.factor(winter_data$Age)

table(winter_data$Age,useNA = "ifany")#Prefer not to say was added to the unlabeled value "5" and assigned as NA#


####HBW_satisfaction####
class(winter_data$HBW_satisfaction)

table(winter_data$HBW_satisfaction,useNA = "ifany")

winter_data$HBW_satisfaction <- ifelse(winter_data$HBW_satisfaction %in% c("Agree", "Somewhat agree", "Strongly agree"), "Yes",
ifelse(winter_data$HBW_satisfaction %in% c("Neither agree nor disagree", "Disagree", "Somewhat disagree", "Strongly disagree"), "No", NA))

winter_data$HBW_satisfaction <- as.factor(winter_data$HBW_satisfaction)

####Employment_status####
table(winter_data$Employment_status,useNA = "ifany")

class(winter_data$Employment_status)

winter_data$Employment_status <- ifelse(winter_data$Employment_status == "Business owner", "Business owner", ifelse(winter_data$Employment_status == "Employee", "Employee",  ifelse(winter_data$Employment_status == "Self-employed", "Self-employed", ifelse(winter_data$Employment_status == "Unpaid family worker", "Unpaid family worker", NA))))

winter_data$Employment_status <- factor(winter_data$Employment_status)

##Young_children; Ages:0-10##
winter_data$young_children <- ifelse(winter_data$`Do.you.have.any.children..If.yes..please.indicate.the.number.of.children.you.have.within.each.category..Please.include.all.children.whether.or.not.they.are.living.with.you.at.the.moment....Aged.0.4...Text` %in% c("1", "2", "4", "9") | winter_data$`Do.you.have.any.children..If.yes..please.indicate.the.number.of.children.you.have.within.each.category..Please.include.all.children.whether.or.not.they.are.living.with.you.at.the.moment....Aged.5.10...Text` %in% c("1", "1 living with me", "2", "3", "4"), "Yes", "No")

winter_data$young_children <- as.factor(winter_data$young_children)
table(winter_data$young_children,useNA = "ifany")

##Teenagers- Ages:11-20##
winter_data$Teenagers <- ifelse(
  winter_data$Do.you.have.any.children..If.yes..please.indicate.the.number.of.children.you.have.within.each.category..Please.include.all.children.whether.or.not.they.are.living.with.you.at.the.moment....Aged.11.16...Text %in% c("1", "1 living with me", "2", "3") |
    winter_data$Do.you.have.any.children..If.yes..please.indicate.the.number.of.children.you.have.within.each.category..Please.include.all.children.whether.or.not.they.are.living.with.you.at.the.moment....Aged.17.20...Text %in% c("1", "2", "3"), "Yes", "No")

winter_data$Teenagers <- as.factor(winter_data$Teenagers)

##Managerial jobs##
table(winter_data$Managerial_jobs,useNA = "ifany")

class(winter_data$Managerial_jobs)

winter_data <- winter_data %>%
mutate(Managerial_jobs = case_when(
Managerial_jobs %in% c("manager", "director","Director","Deputy director ", "president", "executive", "ceo","CEO","Ceo", "chief", "Vice President","Quality Manager","Training manager","Technology Manager", "technical purchasing manager", "Tech Refresh Manager","Technology Refresh Manager","Team Manager","Administrator", "Supply chain manager ","Sr. Manager", "Software developer manager", "SM Manager","Service Delivery Manager", "serive manager","Business development manager","Business support manager ","Finance & HR Administrator" , "Admin manager", "Administration Manager ","admin", "ADMIN","invigilator  admin","Database administrator","Commercial manager","Administrator l", "Office Administrator","Administration", "sales administrator","Cso", "SEO Account Manager","Senior project manager", "Senior Manager","Senior manager", "Senior First Line Manager", "Science support manager ", "School business manager ", "Sales manager ", "Sales Director","sales director","sales and business development manager", "Retail store manager", "Publishing Operations Managerq","Property Manager","Project manager","project manager", "project engineer / manager","Program director","Production Manager","Procurement manager","Practice manager","Policy manager","Planning Manager","Performance Manager","Performance Improvement Manager", "Payroll Manager","Partnerships manager", "Partnership manager","operations manager","Operations Manager", "Operation Manager","Operations Director","Operations director","Office Manager", "Office manager","Office and culture manager"," MD","Marketing manager", "Marketing Manager", "Managing Director", "Manger", "Managing director", "MANAGING DIRECTOR","Manager", "MANAGER", "Manager/Team Leader","local government manager","Line manager", "IT Service Manager","it programme manager","Directorate manager","It manager", "IT MANAGER ","IT Manager","It diretecor","IT DIRECTOR","Inter Manager","Information technology manager","Information Manager","Income Manager","i.t. manager", "Human Resources manager","HR Manager", "HR manager","Hr manager", "hr manager","Hotel director","Head receptionist","Head Receptionist and Administrator","Head of Operations","Head of Global Marketing","Head of department", "head of department", "Head Of Customer Service ","Head of Commonwealth unit","head of Commonwealth unit", "H r manager","Global head of Business Development","General Manager","General manager","Floor manager ","Financial services manager","Financial manger","FINANCE MANAGER","Finance Manager ","Finance director","Finance Assistant Manager","FACILITIES MANAGER", "executive IT manager","Estates Maintenance Manager","Engineering Manager","Directorate manager","Delivery manager","Customer manager","Completions manager ", "Accounts Manager", "associate director", "Director of sales and marketing","Director of it","Director of engineering","director of engineering","Director of Assurance ","Director IT ", "director autoparts","Vice President Business Development","vice president","translator/director","sales director","Sales Director ","Creative diractor","System Manager NHS","Senior Sales Executive","Senior management","Senior executive","Project Manager (Business Change Consultancy)","Project executive","Night manager of hotel","middle manger", "Middle management","Manager/Team Leader","Manager of personnel", "investment executive","HR executive ","Account executive","Fundraising executive","Marketing executive","Marketing Manager","Senior Data Recovery engineer","Work coach","Feature Delivery Leader","Tech Lead","Team leader change management team","Team leader", "Product support team leader","Lead Business Analyst ", "Supervisor  office floor","Office supervisor","Supervisor","Owner","SUPERVISOR","supervisor","Superviser","Software and business integration lead", "Senior quantity surveyor", "Senior Strategy and Design Officer", "Senior scientist", "Senior Scientist", "Senior Registered Clinical Psychologist/ Establishment Lead Psychologist","Senior Process Specialist","Senior business travel management") ~ "Managerial",
    TRUE ~ "Other" ))

winter_data$Managerial_jobs <- as.factor(winter_data$Managerial_jobs)


##Variable selection##                                         
winter_data <-
  winter_data %>% select(productivity, WL_imbalance, SW_isolation, Environ_distraction, Job_autonomy, Resilience, Gender, Age, Managerial_jobs, HBW_satisfaction, Employment_status, young_children, Teenagers)


skim(winter_data)
sum(is.na(winter_data))
summary(winter_data)


##Frequency distribution##
table(winter_data$productivity,useNA = "ifany")##4 NA##
table(winter_data$WL_imbalance,useNA = "ifany")## 9 NA ##
table(winter_data$SW_isolation,useNA = "ifany")## 4 NA##
table(winter_data$Environ_distraction,useNA = "ifany")##6 NA##
table(winter_data$Job_autonomy,useNA = "ifany")## 6 NA ##
table(winter_data$Resilience,useNA = "ifany")## 5 NA##
table(winter_data$Gender,useNA = "ifany")## 7 NA ##
table(winter_data$Age,useNA = "ifany")##7 NA##
table(winter_data$HBW_satisfaction,useNA = "ifany")## 5 NA##
table(winter_data$Employment_status,useNA = "ifany")## 10 NA##
table(winter_data$Managerial_jobs,useNA = "ifany")
table(winter_data$young_children,useNA = "ifany")
table(winter_data$Teenagers,useNA = "ifany")

#######################################################
##Descriptive statistics for dummy variables##

# Select the variables of interest
dummy_vars <- c("Gender", "Age", "Managerial_jobs", "HBW_satisfaction", "Employment_status", "young_children", "Teenagers")


# Calculate the descriptive statistics
num_observations <- sapply(dummy_vars, function(x) sum(!is.na(winter_data[[x]])))
num_na <- sapply(dummy_vars, function(x) sum(is.na(winter_data[[x]])))
value_frequency <- sapply(dummy_vars, function(x) table(addNA(winter_data[[x]])))
percentage <- sapply(value_frequency, function(x) round(prop.table(x) * 100, 2))

# Create the table
table_dummy <- data.frame(Variable = dummy_vars, `Number ofObservations` = num_observations, `Number of NA's` = num_na, Value = sapply(value_frequency, function(x) paste(names(x), collapse = ", ")), Frequency = sapply(value_frequency, function(x) paste(x, collapse = ", ")), `Percentage %` = sapply(percentage, function(x) paste(round(x, 2), collapse = ", ")))

# Print the table
print(table_dummy)

############################
####Discriptive statistics for continous variables###
# Select the variables to included in the table##

continous_vars<-winter_data %>% select(productivity,FW_conflict, SW_isolation, Environ_distraction,Job_autonomy,Resilience)


#######Graphs#####
#Gender by age group##
table(winter_data$Age, winter_data$Gender)
ggplot(data = winter_data, aes(x = Age, fill = Gender)) + geom_bar(position = "stack")+ 
  theme(legend.title = element_blank()) +
  labs(y = "Number", x = "Age Group", title = "Gender composition by age group")

### Gender by job autonomy##
table(winter_data$Gender, winter_data$Job_autonomy)

ggplot(data = winter_data, aes(x = Job_autonomy, fill = Gender)) + geom_bar(position = "stack")+ 
  theme(legend.title = element_blank()) +
  labs(y = "Number", x = "Job Autonomy", title = " Gender by job autonomy")

##HBW satisfaction based on gender##
table(winter_data$Gender, winter_data$HBW_satisfaction)
ggplot(data = winter_data, aes(x = HBW_satisfaction, fill = Gender)) + geom_bar(position = "dodge")+ 
  theme(legend.title = element_blank()) +
  labs(y = "Number", x = "HBW satisfaction", title = " HBW satifaction by gender")
 ######histogram of Family-work conflict by gender####
table(winter_data$FW_conflict, winter_data$Gender)
ggplot(data = winter_data, mapping = aes(x = FW_conflict,fill=Gender)) + geom_histogram()

###############################################
# Fit the regression model
# Set "Gender" as a factor variable with "Other" as the reference category
winter_data$Gender <- relevel(winter_data$Gender, ref = "Other")

##multicollinearity check : variance inflation factor (VIF)###
model_VIF <- lm(productivity ~ WL_imbalance+ SW_isolation + Environ_distraction +
               Job_autonomy + Resilience ,
             data = winter_data)

# Calculate the VIF for each predictor variable
vif_results <- vif(model_VIF)

# View the VIF values
print(vif_results)
####Overall, based on the VIF values, there is no strong evidence of high multicollinearity among the predictor variables in the model. This indicates that the estimated coefficients for each predictor variable are not significantly affected by multicollinearity.#####

# Multiple regression analysis####
Reg1 <- lm(productivity ~ SW_isolation+ WL_imbalance + Environ_distraction +
              Job_autonomy + Resilience + Age+Managerial_jobs+
              HBW_satisfaction + Employment_status + young_children +
              Teenagers+ Gender,
            data = winter_data)

# View the regression results
summary(Reg1)

##Moderaton analysis,moderator: Gender##
# Fit the regression model with interaction terms
Reg2 <- lm(productivity ~ SW_isolation + WL_imbalance + Environ_distraction +
                              Job_autonomy + Resilience + Age + Managerial_jobs +
                              HBW_satisfaction + Employment_status + young_children +
                              Teenagers + Gender +
                              SW_isolation:Gender + WL_imbalance:Gender + Environ_distraction:Gender +
                              Job_autonomy:Gender + Resilience:Gender,
                            data = winter_data)

# Perform the moderation analysis using summary()
summary(Reg2)

#######################################
####Hierarchical multiple regressions###
##Stepwise regression, including the moderator "Gender":

# Step 1: Fit the first model with control variables only
Reg3a <- lm(productivity ~ Age + Managerial_jobs + HBW_satisfaction + Employment_status + young_children + Teenagers + Gender, data = winter_data)

# Step 2: Fit the second model with control variables and job demands, namely work_life balance, social/work isolation, and distracting work environment.
Reg3b <- lm(productivity ~ Age + Managerial_jobs + HBW_satisfaction + Employment_status + young_children + Teenagers + WL_imbalance + SW_isolation + Environ_distraction + Gender, data = winter_data)

# Step 3: Fit the third model with all predictors (including control variables,job demands and the resources of job autonomy and resilience)
Reg3c <- lm(productivity ~ Age + Managerial_jobs + HBW_satisfaction + Employment_status + young_children + Teenagers + WL_imbalance + SW_isolation + Environ_distraction + Job_autonomy + Resilience + Gender, data = winter_data)

# Step 4: Compare the models using summary statistics and model fit indices
summary(Reg3a)
summary(Reg3b)
summary(Reg3c)

#Moderation analysis of model based on hierachy#
Reg4 <- lm(productivity ~ Age + Managerial_jobs + HBW_satisfaction + Employment_status + young_children + Teenagers + WL_imbalance + SW_isolation + Environ_distraction + Job_autonomy + Resilience + Gender +
                    Gender:WL_imbalance + Gender:SW_isolation + Gender:Environ_distraction + Gender:Job_autonomy + Gender:Resilience, 
                  data = winter_data)

summary(Reg4)

####To determine the important variables based on AIC#####
# Step 1: Create the predictor matrix (X) and the response vector (Y) from the winter_data
predictor_vars <- c("WL_imbalance", "SW_isolation", "Environ_distraction", "Job_autonomy", "Resilience")
X <- as.matrix(winter_data[, predictor_vars])
Y <- winter_data$productivity

# Step 2: Calculate the number of observations (n)
n <- nrow(winter_data)

# Step 3: Fit the linear regression models
model1 <- lm(Y ~ SW_isolation + WL_imbalance + Environ_distraction + Job_autonomy + Resilience + Age + Managerial_jobs + HBW_satisfaction + Employment_status + young_children + Teenagers + Gender, data = winter_data)

model2 <- lm(Y ~ WL_imbalance + SW_isolation + Environ_distraction + Job_autonomy + Resilience + Age + Managerial_jobs + HBW_satisfaction + Employment_status, data = winter_data)

model3 <- lm(Y ~ WL_imbalance + SW_isolation + Environ_distraction + Job_autonomy + Resilience + Age + Managerial_jobs + HBW_satisfaction, data = winter_data)

Reg3c <- lm(Y ~ Age + Managerial_jobs + HBW_satisfaction + Employment_status + young_children + Teenagers + WL_imbalance + SW_isolation + Environ_distraction + Job_autonomy + Resilience + Gender, data = winter_data)

Reg4 <- lm(Y ~ Age + Managerial_jobs + HBW_satisfaction + Employment_status + young_children + Teenagers + WL_imbalance + SW_isolation + Environ_distraction + Job_autonomy + Resilience + Gender +
             Gender:WL_imbalance + Gender:SW_isolation + Gender:Environ_distraction + Gender:Job_autonomy + Gender:Resilience, 
           data = winter_data)

# Step 4: Calculate the residual sum of squares (RSS) for each model
RSS_model1 <- sum(model1$residuals^2)
RSS_model2 <- sum(model2$residuals^2)
RSS_model3 <- sum(model3$residuals^2)
RSS_model_Reg3c <- sum(Reg3c$residuals^2)
RSS_model_Reg4 <- sum(Reg4$residuals^2)

# Step 5: Calculate the number of parameters in each model
k_model1 <- length(model1$coefficients) - 1  # -1 to exclude the intercept
k_model2 <- length(model2$coefficients) - 1
k_model3 <- length(model3$coefficients) - 1
k_model_Reg3c <- length(Reg3c$coefficients) - 1
k_model_Reg4 <- length(Reg4$coefficients) - 1

# Step 6: Calculate the AIC for each model
AIC_model1 <- n * log(RSS_model1/n) + 2 * k_model1
AIC_model2 <- n * log(RSS_model2/n) + 2 * k_model2
AIC_model3 <- n * log(RSS_model3/n) + 2 * k_model3
AIC_model_Reg3c <- n * log(RSS_model_Reg3c/n) + 2 * k_model_Reg3c
AIC_model_Reg4 <- n * log(RSS_model_Reg4/n) + 2 * k_model_Reg4

# Print the AIC values for each model
cat("AIC for model1:", AIC_model1, "\n")
cat("AIC for model2:", AIC_model2, "\n")
cat("AIC for model3:", AIC_model3, "\n")
cat("AIC for Reg3c:", AIC_model_Reg3c, "\n")
cat("AIC for Reg4:", AIC_model_Reg4, "\n")

##Decision Tree Regression;to identify importatnt variables##

# Define predictor variables
predictor_vars <- c("WL_imbalance", "SW_isolation", "Environ_distraction", "Job_autonomy", "Resilience")


# Define moderator variable
moderator_var <- "Gender"

# Define control variables
control_vars <- c("Age", "Managerial_jobs", "HBW_satisfaction", "Employment_status", "young_children", "Teenagers")

# Define outcome variable
outcome_var <- "productivity"

# Combine all variables for modeling
model_vars <- c(predictor_vars, moderator_var, control_vars, outcome_var)

# Fit the decision tree model
decision_tree_model <- rpart(as.formula(paste(outcome_var, "~ .")), data = winter_data[, model_vars], method = "anova")

# Visualize the decision tree
rpart.plot(decision_tree_model)
summary(decision_tree_model)

# Make predictions using the decision tree model
predictions <- predict(decision_tree_model, winter_data[, model_vars])

# Add the predictions to the winter_data dataframe
winter_data$predicted_productivity <- predictions



#########################THE END#########################################




