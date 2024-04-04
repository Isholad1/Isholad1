library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(dplyr)

#Data cleaming
str(driver_102)
summary(driver_102)
# No NA
# id is identifier
# No obvious errors

# the parcel_102
str(parcel_102)
summary(parcel_102)
# No NA
# id is identifier
# No obvious errors

#Joining both data
Olamide_2023 <- inner_join(driver_102,parcel_102,
                           by = "driver_id")
#Data restriction
Olamide_2023 %>% filter(parcel_returned ==0)
#Removing the outcome zero under the parcel arrived and returned
olamide_2023_parcel_return <- Olamide_2023 %>% 
  filter(parcel_arrived !=0,
         parcel_returned !=0) %>%
  #Removing the kinder option under the time of delivery
  filter(time_of_delivery != "kinder") %>% 
  #Removing the zero hours contract under the work pattern
filter(work_pattern != "zero hours contract") %>% 
  #Removing the dress as an outcome on parcel status
filter(parcel_status != "dress")


#Descriptive statistics
summary(olamide_2023_parcel_return$parcel_status)
summary(olamide_2023_parcel_return$parcel_returned)
summary(olamide_2023_parcel_return$parcel_arrived)
summary(olamide_2023_parcel_return$parcel_value)


#Standard deviation 
sd(olamide_2023_parcel_return$parcel_returned)
sd(olamide_2023_parcel_return$parcel_arrived)
sd(olamide_2023_parcel_return$parcel_value)


#plot on parcel status
ggplot(olamide_2023_parcel_return) +
  aes(x = parcel_status) +
  geom_bar(fill = "#EF562D") +
  labs(title = "Parcel Status") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0.5)
  )

#Plot on parcel status and delivery location 
ggplot(olamide_2023_parcel_return) +
  aes(x = parcel_status, fill = delivery_location) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title = "Parcel Status") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0.5)
  )

#Plot on parcel status and delivery time
ggplot(olamide_2023_parcel_return) +
  aes(x = parcel_status) +
  geom_bar(fill = "#EF562D") +
  labs(title = "Parcel Status on Time of Delivery ") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0.5)
  ) +
  facet_wrap(vars(time_of_delivery))


#Plot on location and van type
ggplot(olamide_2023_parcel_return) +
  aes(x = van_type) +
  geom_bar(fill = "#EF562D") +
  labs(title = "Delivery Location on Van Type") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0.5)
  ) +
  facet_wrap(vars(delivery_location))


#Plot on delivery time and work pattern
ggplot(olamide_2023_parcel_return) +
  aes(x = time_of_delivery) +
  geom_bar(fill = "#EF562D") +
  labs(title = "Delivery Time and Work Pattern") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0.5)
  ) +
  facet_wrap(vars(work_pattern))

#plots under appendices accordingly 
#1
ggplot(olamide_2023_parcel_return) +
  aes(x = parcel_value, fill = delivery_location) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(title = "Parcel Value on Parcel status") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  facet_wrap(vars(parcel_status))

#2
ggplot(olamide_2023_parcel_return) +
  aes(x = parcel_value) +
  geom_histogram(bins = 30L, fill = "#EF562D") +
  labs(title = "Parcel value on Parcel status") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  facet_wrap(vars(parcel_status))

#3
ggplot(olamide_2023_parcel_return) +
  aes(x = experience) +
  geom_histogram(bins = 30L, fill = "#EF562D") +
  labs(title = "Driver id and Experience") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  facet_wrap(vars(driver_id))

#4
ggplot(olamide_2023_parcel_return) +
  aes(x = driver_id) +
  geom_histogram(bins = 30L, fill = "#EF562D") +
  theme_classic() +
  facet_wrap(vars(experience))

#5
ggplot(olamide_2023_parcel_return) +
  aes(x = van_type) +
  geom_bar(fill = "#EF562D") +
  labs(title = "Delivery Location on Van Type") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  facet_wrap(vars(delivery_location))

#6
ggplot(olamide_2023_parcel_return) +
  aes(x = parcel_status) +
  geom_bar(fill = "#EF562D") +
  labs(title = "Parcel Status on Driver Id") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  facet_wrap(vars(driver_id))

#Summary of the observation:
count(Olamide_2023)
count(olamide_2023_parcel_return)
str(count(olamide_2023_parcel_return))

#Total omitted observations:
count(Olamide_2023) - count(olamide_2023_parcel_return)
