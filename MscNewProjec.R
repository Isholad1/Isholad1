setwd("C:/Users/user/OneDrive - University of Lagos/Documents")

library(readxl)  
library(ggplot2)
library(psych)
library(tidyverse) ###data manipulation
library(skimr) ###data summarization
library(rpart) ###For Decision Tree
library(rpart.plot) ###For Decision Tree visualization
library(randomForest) #For Forest Tree Model

winter_data<- read.csv("WorkingAtHome_Survey_Winter2020-21_commissioned_data_collection.csv")

skim(winter_data)
sum(is.na(winter_data))

write.csv(winter_data, file = "winter_data.csv")

#Variables under Home-Based Working (HBW)


#HBW_1(Autonomy)
table(winter_data$Please.rate.the.following.statements.in.relation.to.your.current.working.arrangements....How.much.autonomy.do.you.have.in.your.work.)

winter_data <- rename(winter_data, HBW_1 = Please.rate.the.following.statements.in.relation.to.your.current.working.arrangements....How.much.autonomy.do.you.have.in.your.work.)
winter_data$HBW_1 <- as.integer(factor(winter_data$HBW_1,
                                       levels = c("Extremely low", "Low", "Somewhat low", "Neither low nor high", "Somewhat high", "High", "Extremely high"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_1,useNA = "ifany")

#HBW_2(Concentration)
table(winter_data$Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.find.it.difficult.to.concentrate.on.my.work.)

winter_data <- rename(winter_data, HBW_2 = Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.find.it.difficult.to.concentrate.on.my.work.)
winter_data$HBW_2 <- as.integer(factor(winter_data$HBW_2,
                                       levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_2,useNA = "ifany")

#HBW_3(Auditory Distraction)
table(winter_data$Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.experience.auditory.distractions.whilst.working.)

winter_data <- rename(winter_data, HBW_3 = Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.experience.auditory.distractions.whilst.working.)
winter_data$HBW_3 <- as.integer(factor(winter_data$HBW_3,
                                       levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5)))

table(winter_data$HBW_3,useNA = "ifany")

#HBW_4(Internet Access)
table(winter_data$Please.rate.the.following.questions.regarding.your.current.work.set.up..How.satisfied.are.you.with.......your.access.to.the.internet.at.home.)

winter_data <- rename(winter_data, HBW_4 = Please.rate.the.following.questions.regarding.your.current.work.set.up..How.satisfied.are.you.with.......your.access.to.the.internet.at.home.)
winter_data$HBW_4 <- as.integer(factor(winter_data$HBW_4,
                                       levels = c("Extremely dissatisfied", "Extremely satisfied", "Moderately dissatisfied", "Moderately satisfied", "Neither satisfied nor dissatisfied", "Slightly dissatisfied", "Slightly satisfied"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_4,useNA = "ifany")


#HBW_5(Visual Distraction)
table(winter_data$Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.experience.visual.distractions.in.my.work.area.)

winter_data <- rename(winter_data, HBW_5 = Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.experience.visual.distractions.in.my.work.area.)
winter_data$HBW_5 <- as.integer(factor(winter_data$HBW_5,
                                       levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_5,useNA = "ifany")


#HBW_6(Comfortability)
table(winter_data$Please.rate.the.following.questions.regarding.your.current.work.set.up..How.satisfied.are.you.with.......the.comfort.of.your.homeworking.space..chair..desk..computer.equipment..etc...)

winter_data <- rename(winter_data, HBW_6 = Please.rate.the.following.questions.regarding.your.current.work.set.up..How.satisfied.are.you.with.......the.comfort.of.your.homeworking.space..chair..desk..computer.equipment..etc...)
winter_data$HBW_6 <- as.integer(factor(winter_data$HBW_6,
                                       levels = c("Extremely dissatisfied", "Slightly dissatisfied", "Moderately dissatisfied", "Neither satisfied nor dissatisfied", "Slightly satisfied", "Moderately satisfied", "Extremely satisfied"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_6,useNA = "ifany")

# Combine variables HBW1 to HBW 9 to create Home based working variable
winter_data$Home_based_work <- (winter_data$HBW_1 + 
                                  winter_data$HBW_2 +
                                  winter_data$HBW_3 + 
                                  winter_data$HBW_4 + 
                                  winter_data$HBW_5 +
                                  winter_data$HBW_6 )/6

# Rename combined variable as productivity
colnames(winter_data)[colnames(winter_data) == "Home_based_work"] <- "Home_based_work"

# View updated dataframe
head(winter_data)

#########################################################
#Variables under Productivity                           ###########################################
#######################################################

#P1(Efficiency)
table(winter_data$Please.answer.the.following.questions.in.terms.of.your.overall.experience.with.homeworking.under.COVID.19.measures....Remote.working.allows.me.to.perform.my.job.better.than.I.ever.could.when.I.worked.in.the.office.)

winter_data <- rename(winter_data, P_1 = Please.answer.the.following.questions.in.terms.of.your.overall.experience.with.homeworking.under.COVID.19.measures....Remote.working.allows.me.to.perform.my.job.better.than.I.ever.could.when.I.worked.in.the.office.)
winter_data$P_1 <- as.integer(factor(winter_data$P_1,
                                     levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$P_1,useNA = "ifany")


#P2(Job productive)
table(winter_data$Please.answer.the.questions.below.in.relation.to.the.current.COVID.19.homeworking.conditions....Working.from.home.makes.me.more.productive.)

winter_data <- rename(winter_data, P_2 = Please.answer.the.questions.below.in.relation.to.the.current.COVID.19.homeworking.conditions....Working.from.home.makes.me.more.productive.)
winter_data$P_2 <- as.integer(factor(winter_data$P_2,
                                     levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$P_2,useNA = "ifany")


#P3(Job Effectiveness)
table(winter_data$Please.answer.the.questions.below.in.relation.to.the.current.COVID.19.homeworking.conditions....Working.from.home.enhances.my.job.effectiveness.)

winter_data <- rename(winter_data, P_3 = Please.answer.the.questions.below.in.relation.to.the.current.COVID.19.homeworking.conditions....Working.from.home.enhances.my.job.effectiveness.)
winter_data$P_3 <- as.integer(factor(winter_data$P_3,
                                     levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$P_3,useNA = "ifany")

#P4(Optimality)
table(winter_data$This.technology.......forces.me.to.work.much.faster.that.I.am.used.to.)

winter_data <- rename(winter_data, P_4 = This.technology.......forces.me.to.work.much.faster.that.I.am.used.to.)
winter_data$P_4 <- as.integer(factor(winter_data$P_4,
                                     levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$P_4,useNA = "ifany")


# Combine variables P1, P2, P3, P4 to create productivity variable
winter_data$productivity <- (winter_data$P_1 + winter_data$P_2 + winter_data$P_3 + winter_data$P_4)/4

# Rename combined variable as productivity
colnames(winter_data)[colnames(winter_data) == "productivity"] <- "Productivity"

# View updated dataframe
head(winter_data)



