setwd("C:/Users/isholad/Documents")

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


#Variables under Home-Based Working (HBW)

#HBW_1(Condusiveness)
table(winter_data$Under.COVID.19.measures..I.feel.......that.in.the.homeworking.environment.I.am.always.working.)

winter_data <- rename(winter_data, HBW_1 = Under.COVID.19.measures..I.feel.......that.in.the.homeworking.environment.I.am.always.working.)
winter_data$HBW_1 <- as.integer(factor(winter_data$HBW_1,
                                     levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_1,useNA = "ifany")

#HBW_2(Supportive Workers)
table(winter_data$Please.answer.these.questions.in.terms.of.your.relationship.with.colleagues.under.current..remote..working.arrangements....I.have.people.around.me.whilst.working.from.home.or.and.in.the.office.under.new.social.distancing.measures.)

winter_data <- rename(winter_data, HBW_2 = Please.answer.these.questions.in.terms.of.your.relationship.with.colleagues.under.current..remote..working.arrangements....I.have.people.around.me.whilst.working.from.home.or.and.in.the.office.under.new.social.distancing.measures.)
winter_data$HBW_2 <- as.integer(factor(winter_data$HBW_2,
                                       levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_2,useNA = "ifany")

#HBW_3(Availability)
table(winter_data$Please.answer.these.questions.in.terms.of.your.relationship.with.colleagues.under.current..remote..working.arrangements....I.have.co.workers.available.whom.I.can.depend.on.when.I.have.a.problem.)

winter_data <- rename(winter_data, HBW_3 = Please.answer.these.questions.in.terms.of.your.relationship.with.colleagues.under.current..remote..working.arrangements....I.have.co.workers.available.whom.I.can.depend.on.when.I.have.a.problem.)
winter_data$HBW_3 <- as.integer(factor(winter_data$HBW_3,
                                       levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_3,useNA = "ifany")

#HBW_4(Flexibility)
table(winter_data$Please.respond.to.the.questions.in.relation.to.your.current.working.arrangements..with.1...Not.at.all.and.5...A.great.deal....In.total..how.much.flexibility.do.you.feel.you.have.in.your.work.)

winter_data <- rename(winter_data, HBW_4 = Please.respond.to.the.questions.in.relation.to.your.current.working.arrangements..with.1...Not.at.all.and.5...A.great.deal....In.total..how.much.flexibility.do.you.feel.you.have.in.your.work.)

table(winter_data$HBW_4,useNA = "ifany")

#HBW_5(Autonomy)
table(winter_data$Please.rate.the.following.statements.in.relation.to.your.current.working.arrangements....How.much.autonomy.do.you.have.in.your.work.)

winter_data <- rename(winter_data, HBW_5 = Please.rate.the.following.statements.in.relation.to.your.current.working.arrangements....How.much.autonomy.do.you.have.in.your.work.)
winter_data$HBW_5 <- as.integer(factor(winter_data$HBW_5,
                                              levels = c("Extremely low", "Low", "Somewhat low", "Neither low nor high", "Somewhat high", "High", "Extremely high"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_5,useNA = "ifany")

#HBW_6(Concentaction)
table(winter_data$Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.find.it.difficult.to.concentrate.on.my.work.)

winter_data <- rename(winter_data, HBW_6 = Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.find.it.difficult.to.concentrate.on.my.work.)
winter_data$HBW_6 <- as.integer(factor(winter_data$HBW_6,
                                       levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_6,useNA = "ifany")

#HBW_7(Auditory Distraction)
table(winter_data$Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.experience.auditory.distractions.whilst.working.)

winter_data <- rename(winter_data, HBW_7 = Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.experience.auditory.distractions.whilst.working.)
winter_data$HBW_7 <- as.integer(factor(winter_data$HBW_7,
                                       levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_7,useNA = "ifany")

#HBW_8(Internet Access)
table(winter_data$Please.rate.the.following.questions.regarding.your.current.work.set.up..How.satisfied.are.you.with.......your.access.to.the.internet.at.home.)

winter_data <- rename(winter_data, HBW_8 = Please.rate.the.following.questions.regarding.your.current.work.set.up..How.satisfied.are.you.with.......your.access.to.the.internet.at.home.)
winter_data$HBW_8 <- as.integer(factor(winter_data$HBW_8,
                                       levels = c("Extremely dissatisfied", "Extremely satisfied", "Moderately dissatisfied", "Moderately satisfied", "Neither satisfied nor dissatisfied", "Slightly dissatisfied", "Slightly satisfied"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_8,useNA = "ifany")


#HBW_9(Visual Distraction)
table(winter_data$Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.experience.visual.distractions.in.my.work.area.)

winter_data <- rename(winter_data, HBW_9 = Please.rate.the.following.statements.relating.to.your.current.work..1...Strongly.disagree.and.5...Strongly.agree...I.experience.visual.distractions.in.my.work.area.)
winter_data$HBW_9 <- as.integer(factor(winter_data$HBW_9,
                                       levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"),labels = c(1, 2, 3, 4, 5, 6, 7)))

table(winter_data$HBW_9,useNA = "ifany")

# Combine variables HBW1 to HBW 9 to create Home based working variable
winter_data$Home_based_working <- (winter_data$HBW_1 + 
                                     winter_data$HBW_2 +
                                     winter_data$HBW_3 + 
                                     winter_data$HBW_4 + 
                                     winter_data$HBW_5 +
                                     winter_data$HBW_6 +
                                     winter_data$HBW_7 +
                                     winter_data$HBW_8 +
                                     winter_data$HBW_9)/9

# Rename combined variable as productivity
colnames(winter_data)[colnames(winter_data) == "Home_based_working"] <- "Home_based_working"

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

#P4(Technology)
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


#Gender
table(winter_data$How.do.you.define.your.gender..5.Male..6.Female..7.Other..8.Prefer.not.to.say)

winter_data <- rename(winter_data, Gender = How.do.you.define.your.gender..5.Male..6.Female..7.Other..8.Prefer.not.to.say)
winter_data$Gender <- ifelse(winter_data$Gender %in% c(5, 7), "Other", ifelse(winter_data$Gender == 6, "Female", NA))

winter_data$Gender <- as.factor(winter_data$Gender)
table(winter_data$Gender, useNA = "ifany")

########################################################3
#Control Variables
########################################
#Age
table(winter_data$How.old.are.you.)
winter_data <- rename(winter_data, Age = How.old.are.you.)
table(winter_data$Age, useNA = "ifany")

##Age####
table(winter_data$Age,useNA = "ifany")##unlabeled value "5"#
class(winter_data$Age)

winter_data$Age <- ifelse(winter_data$Age %in% c("18-25", "26-35"), "18-35",ifelse(winter_data$Age %in% c("36-45", "46-55"), "36-55",
                                                                                   ifelse(winter_data$Age %in% c("56-65", "66 and over"), "56 and over", ifelse(winter_data$Age == "Prefer not to say", NA, NA))))

winter_data$Age <- as.factor(winter_data$Age)

table(winter_data$Age,useNA = "ifany")#Prefer not to say was added to the unlabeled value "5" and assigned as NA#


##Young_children; Ages:0-10##
winter_data$young_children <- ifelse(winter_data$`Do.you.have.any.children..If.yes..please.indicate.the.number.of.children.you.have.within.each.category..Please.include.all.children.whether.or.not.they.are.living.with.you.at.the.moment....Aged.0.4...Text` %in% c("1", "2", "4", "9") | winter_data$`Do.you.have.any.children..If.yes..please.indicate.the.number.of.children.you.have.within.each.category..Please.include.all.children.whether.or.not.they.are.living.with.you.at.the.moment....Aged.5.10...Text` %in% c("1", "1 living with me", "2", "3", "4"), "Yes", "No")

winter_data$young_children <- as.factor(winter_data$young_children)
table(winter_data$young_children,useNA = "ifany")

##Teenagers- Ages:11-20##
winter_data$Teenagers <- ifelse(
  winter_data$Do.you.have.any.children..If.yes..please.indicate.the.number.of.children.you.have.within.each.category..Please.include.all.children.whether.or.not.they.are.living.with.you.at.the.moment....Aged.11.16...Text %in% c("1", "1 living with me", "2", "3") |
    winter_data$Do.you.have.any.children..If.yes..please.indicate.the.number.of.children.you.have.within.each.category..Please.include.all.children.whether.or.not.they.are.living.with.you.at.the.moment....Aged.17.20...Text %in% c("1", "2", "3"), "Yes", "No")
winter_data$Teenagers <- as.factor(winter_data$Teenagers)
table(winter_data$Age,useNA = "ifany")



#Productive in Total(PIT)
productivity <- winter_data %>% 
  select(P_1, P_2, P_3, P_4)

#Combining the above variables and renaming it with "Home_based_Working"

Home_based_working <- winter_data %>% 
  select(HBW_1,HBW_2, HBW_3, HBW_4, HBW_5, HBW_6, HBW_7, HBW_8, HBW_9)


### Factor analysis for Productivity and HBW

# Remove observations with missing values
productivity <- na.omit(productivity)
skim(productivity)

# Standardize the variables
productivity <- scale(productivity)
summary(productivity)

# Perform factor analysis
factor_analysis <- factanal(productivity, factors = 1, rotation = "varimax")
# Perform the factor analysis
fa_result <- fa(productivity, nfactors = 3, rotate = "varimax")

# Print factor analysis results
print(factor_analysis)
print(fa_result)

##Cronbach's alpha assess the internal consistency or reliability of the 4 variables with positive loading as shown in the output of the load factor analysis##
#Subset the variables
productivity <- winter_data %>% 
  select(P_1, P_2, P_3, P_4)

# Calculate Cronbach's alpha
alpha_result <- psych::alpha(productivity)

# Print the Cronbach's alpha coefficient
print(alpha_result$total$raw_alpha) ##value on 0.80 suggests a strong internal consistency 

##################################################################
################FOR Home-based Working
#################################################################

# Remove observations with missing values
Home_based_working <- na.omit(Home_based_working)
skim(Home_based_working)

# Standardize the variables
Home_based_working <- scale(Home_based_working)
summary(Home_based_working)

# Perform factor analysis
factor_analysis <- factanal(Home_based_working, factors = 1, rotation = "varimax")

# Perform the factor analysis
fa_result <- fa(Home_based_working, nfactors = 3, rotate = "varimax")


# Print factor analysis results
print(factor_analysis)
print(fa_result)

##Cronbach's alpha assess the internal consistency or reliability of the 9 variables with positive loading as shown in the output of the load factor analysis
#Subset the variables
Home_based_working <- winter_data %>% 
  select(HBW_1,HBW_2, HBW_3, HBW_4, HBW_5, HBW_6, HBW_7, HBW_8, HBW_9)

# Calculate Cronbach's alpha
alpha_result <- psych::alpha(Home_based_working, check.keys = TRUE)

# Print the Cronbach's alpha coefficient
print(alpha_result$total$raw_alpha) ##value on 0.63 suggests a internal consistency 

##Variable selection##   
winter_data <- winter_data %>% 
  select (Home_based_working, Gender, Productivity, young_children, Age, Teenagers )

skim(winter_data)
sum(is.na(winter_data))
summary(winter_data)



##Frequency distribution##
table(winter_data$Productivity,useNA = "ifany")
table(winter_data$Gender, useNA = "ifany") 
table(winter_data$Age,useNA = "ifany")##7 NA##
table(winter_data$young_children,useNA = "ifany")
table(winter_data$Teenagers,useNA = "ifany")  

###############################################
# Fit the regression model
# Set "Gender" as a factor variable with "Other" as the reference category
winter_data$Gender <- relevel(winter_data$Gender, ref = "Other")


# Multiple regression analysis####
Reg1 <- lm(Productivity ~ Home_based_working + Gender,
           data = winter_data)

# View the regression results
summary(Reg1)


##Expanded Model Analysis with Control Variables 

Reg2 <- lm(Productivity ~ Home_based_working + Gender + young_children + Age + Teenagers,
           data = winter_data)


# Perform the expanded model analysis using summary()
summary(Reg2)

#Interactive Model
Reg3 <- lm(Productivity ~ Home_based_working + Gender + Home_based_working*Gender,
           data = winter_data)

#Perform the interactive analysis using summary()
summary(Reg3)

##Descriptive statistics for dummy variables##

# Select the variables of interest
dummy_vars <- c("Gender", "Age", "young_children", "Teenagers")


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

continous_vars<-winter_data %>% select(Home_based_working)

# Calculate descriptive statistics
stats <- summary(continous_vars)

# Print the statistics
print(stats)


####Discriptive statistics for ordinal variable###
# Select the variable to included in the table##
ordinal_var<-winter_data %>% select(Productivity)

# Calculate descriptive statistics
stats <- summary(ordinal_var)

# Print the statistics
print(stats)


# Step 1: Create the predictor matrix (X) and the response vector (Y) from the winter_data
predictor_vars <- c("Home_based_working")
X <- as.matrix(winter_data[, predictor_vars])
Y <- winter_data$Productivity

# Step 2: Calculate the number of observations (n)
n <- nrow(winter_data)

###Step 3: Fit the linear regression models
model1 <- lm(Y ~ Home_based_working + Gender,
             data = winter_data)

model2 <- lm(Y ~ Home_based_working + Age + young_children + Teenagers + Gender,
             data = winter_data)

model3 <- lm(Y ~ Home_based_working + Gender + Home_based_working*Gender,
             data = winter_data)


# Step 4: Calculate the residual sum of squares (RSS) for each model
RSS_model1 <- sum(model1$residuals^2)
RSS_model2 <- sum(model2$residuals^2)
RSS_model3 <- sum(model3$residuals^2)


# Step 5: Calculate the number of parameters in each model
k_model1 <- length(model1$coefficients) - 1  # -1 to exclude the intercept
k_model2 <- length(model2$coefficients) - 1
k_model3 <- length(model3$coefficients) - 1


# Step 6: Calculate the AIC for each model
AIC_model1 <- n * log(RSS_model1/n) + 2 * k_model1
AIC_model2 <- n * log(RSS_model2/n) + 2 * k_model2
AIC_model3 <- n * log(RSS_model3/n) + 2 * k_model3


# Print the AIC values for each model
cat("AIC for model1:", AIC_model1, "\n")
cat("AIC for model2:", AIC_model2, "\n")
cat("AIC for model3:", AIC_model3, "\n")



##Decision Tree Regression;to identify importatnt variables##

# Define predictor variables
predictor_var <- c("Home_based_working")


# Define interactor variable
interactor_var <- c("Gender")

# Define control variables
control_vars <- c("Age", "young_children", "Teenagers")

# Define outcome variable
outcome_var <- "Productivity"

# Combine all variables for modeling
model_vars <- c(predictor_var, interactor_var, control_vars, outcome_var)

# Fit the decision tree model
decision_tree_model <- rpart(as.formula(paste(outcome_var, "~ .")), data = winter_data[, model_vars], method = "anova")

# Visualize the decision tree
rpart.plot(decision_tree_model)
summary(decision_tree_model)

# Make predictions using the decision tree model
predictions <- predict(decision_tree_model, winter_data[, model_vars])

# Add the predictions to the winter_data dataframe
winter_data$predicted_productivity <- predictions


###Data Visualisation 
#Plot 1
winter_data %>%
  filter(!is.na(Home_based_working)) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(Productivity)) %>%
  filter(!is.na(Age)) %>%
  ggplot() +
  aes(x = Home_based_working, y = Productivity) +
  geom_jitter(size = 1.5) +
  geom_smooth(size = 1.5) +
  labs(
    title = "Figure 4.1",
    subtitle = "Productivity on Home Based Working"
  ) +
  theme_linedraw() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold.italic")
  )


#plot 2

winter_data %>%
  filter(!is.na(Home_based_working)) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(Productivity)) %>%
  filter(!is.na(Age)) %>%
  ggplot() +
  aes(x = Productivity) +
  geom_density(adjust = 1L, fill = "#112446") +
  geom_line(stat="density", aes(color = Gender)) +
  labs(
    title = "Figure 4.2",
    subtitle = "Productivity on Gender"
  ) +
  theme_linedraw() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold.italic")
  ) +
  facet_wrap(vars(Gender))

#plot 3
winter_data %>%
  filter(!is.na(Home_based_working)) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(Productivity)) %>%
  filter(!is.na(Age)) %>%
  ggplot() +
  aes(x = Home_based_working, y = predicted_productivity) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, linetype = "solid", colour = "#0C4C8A") +
  labs(title = "Figure 4.3", subtitle = "Home based Working on Predicted Productivity") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

#Plot 4
winter_data %>%
  filter(!is.na(Home_based_working)) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(Productivity)) %>%
  filter(!is.na(Age)) %>%
  ggplot() +
  aes(x = Productivity, fill = Gender) +
  geom_histogram(bins = 10L) +
  geom_line(aes(y = Age), color = "blue") +
  scale_fill_hue(direction = 1) +
  labs(title = "Figure 4.4", subtitle = "Productivity on Age") +
  coord_flip() +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold.italic")
  ) +
  facet_wrap(vars(Age))


##############################################
####Appendices
###############################

##Plot 1
winter_data %>%
  filter(!is.na(Home_based_working)) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(Productivity)) %>%
  filter(!is.na(Age)) %>%
  ggplot() +
  aes(x = Home_based_working) +
  geom_histogram(bins = 14L, fill = "#112446") +
  labs(title = "HBW on Gender") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  facet_wrap(vars(Gender), ncol = 1L)

#Plot 2

winter_data %>%
  filter(!is.na(Home_based_working)) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(Productivity)) %>%
  filter(!is.na(Age)) %>%
  ggplot() +
  aes(x = Home_based_working) +
  geom_histogram(bins = 30L, fill = "#112446") +
  labs(title = "HBW on Age") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold")) +
  facet_wrap(vars(Age), nrow = 2L)

##Plot 3
winter_data %>%
  filter(!is.na(Home_based_working)) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(Productivity)) %>%
  filter(!is.na(Age)) %>%
  ggplot() +
  aes(x = predicted_productivity, fill = Gender) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(title = "Predicted Productivity on Gender") +
  coord_flip() +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

#Plot 4
winter_data %>%
  filter(!is.na(Home_based_working)) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(Productivity)) %>%
  filter(!is.na(Age)) %>%
  ggplot() +
  aes(x = predicted_productivity) +
  geom_histogram(bins = 30L, fill = "#112446") +
  labs(title = "Predicted Productivity on Age") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold")) +
  facet_wrap(vars(Age))

#plot 5
winter_data %>%
  filter(!is.na(Home_based_working)) %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(Productivity)) %>%
  filter(!is.na(Age)) %>%
  ggplot() +
  aes(x = predicted_productivity) +
  geom_density(fill = "#112446") +
  labs(title = "Predicted Productivity on Age") +
  theme_gray() +
  theme(plot.title = element_text(face = "bold")) +
  facet_wrap(vars(Age))

###########################################################
##########END
#################################################################

