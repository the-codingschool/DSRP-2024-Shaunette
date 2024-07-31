#Risk vs Facility Type
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)

csf_no_blank <- csf_new[csf_new$Risk != '', ]
csf_select <- csf_no_blank %>% filter(Facility_Type == c('Restaurant', 'Bar', 'School', 'Daycare'))

#Finding proportion of pass or fail depending for each specifc location ####
length(get_dupes(csf_select, 'Location'))
length(unique(csf_select$Address))

#buisnesses <- unique(csf_select$Address)

#var = ifelse(csf_select$Address %in% buisnesses & csf_select$Results == 'Pass'|'Pass w/ Conditions', pass + 1, total_inspections + 1, pass, total_inspections)
      #WORK ON THIS:: else(csf_select$Results != 'Pass'|'Pass w/ Conditions', total_inspections + 1, total_inspections)

# Creating a clolumn of proption of Passes a business has ####
csf_select <- csf_select %>%
  mutate(Pass_Rate = )

#csf_resturant %>% 
  #ggplot(aes(Risk, fill = Facility_Type)) + 
  #geom_point() 

csf_no_blank %>% 
  ggplot(aes(Risk, State)) + 
  geom_point() 

# How have risk levels changed over the years in Restaurants, Bars, School and Daycare

# Time series plot
tail(csf_select)

#Risk 1 throughout the years
csf_select %>% 
  filter(Risk == "Risk 1 (High)") %>%
  ggplot(aes(x = Year, fill = Facility_Type)) +
  geom_boxplot() +
  


#Risk 2 throughout the years
csf_select %>% 
  filter(Risk == "Risk 2 (Medium)") %>%
  ggplot(aes(x = Year, fill = Facility_Type)) +
  geom_bar()s


# Daycare risk levels throuought the year
csf_select %>% 
  filter(Facility_Type == 'Daycare', Results == 'Fail') %>%
  ggplot(aes(x = Year, fill = Risk)) +
  geom_bar() + 
  ggtitle("Daycare Risk levels throuought the Years")

# Bar graphs
names(csf_select)
#Risk #1

csf_select %>% 
  filter(Risk == 'Risk 1 (High)') %>%
  ggplot(aes(Facility_Type, fill= Facility_Type)) +
  geom_bar() +
  ggtitle("Prevalenace of Risk 1 In Specfic Facilities")

csf_select %>% 
  filter(Risk == 'Risk 1 (High)') %>%
  ggplot(aes(Year, fill= Facility_Type)) +
  geom_bar() +
  ggtitle("Prevalenace of Risk 1 In Specfic Facilities")

# csf_select %>% 
#   filter(Facility_Type == 'Restaurant') %>%
#   ggplot(aes(fill= Facility_Type)) +
#   geom_bar(stat = "Risk", width = 1) +
#   coord_polar("y", start=0) +
#   ggtitle("Pie Chart")

#Risk #2
csf_select %>% 
  filter(Risk == 'Risk 2 (Medium)') %>%
  ggplot(aes(Facility_Type, fill= Facility_Type)) +
  geom_bar() +
  ggtitle("Prevalenace of Risk 2 In Specfic Facilities")

#Risk #3
csf_select %>% 
  filter(Risk == 'Risk 3 (Low)') %>%
  ggplot(aes(Facility_Type, fill= Facility_Type)) +
  geom_bar() +
  ggtitle("Prevalenace of Risk 3 In Specfic Facilities")

mode(csf_select$Risk)

# Time Series Plot 
csf_select %>%
  group_by(Facility_Type) %>%
  ggplot(aes(Risk, Year)) +
  geom_line(median(csf_select$Risk))

# Finding COunts ####

#Number of Each facility type at Risk 1 - using group_by() 
csf_select %>% 
  group_by(Facility_Type) %>%
  filter(Risk == "Risk 1 (High)") %>%
  summarise(number = length(Risk))

#Number of Each facility type at Risk 2 - using group_by() 
csf_select %>% 
  group_by(Facility_Type) %>%
  filter(Risk == "Risk 2 (Medium)") %>%
  summarise(number = length(Risk))

#Number of Each facility type at Risk 3 - using group_by() 
csf_select %>% 
  group_by(Facility_Type) %>%
  filter(Risk == "Risk 3 (Low)") %>%
  summarise(number = length(Risk))

## creating a new risk column where risk has a numeric value

#csf_select$Binary_Risk <- csf_select$Risk 
csf_select$Binary_Risk[csf_select$Binary_Risk == "Risk 1 (High)"] <- 1
csf_select$Binary_Risk[csf_select$Binary_Risk == "Risk 2 (Medium)"] <- 2
csf_select$Binary_Risk[csf_select$Binary_Risk == "Risk 3 (Low)"] <- 3
csf_select$Binary_Risk[csf_select$Binary_Risk == "All"] <- 0
csf_select$Binary_Risk = as.numeric(csf_select$Binary_Risk)

#Risk 'Coef' #####
csf_select %>% 
  group_by(Facility_Type) %>%
  #filter(Risk == "Pass") %>%
  summarise(avg = mean(Binary_Risk))

#graphing risk per year
csf_select %>% 
  group_by(Facility_Type, Year) %>%
  summarise(avg = mean(Binary_Risk)) %>%
  ggplot(aes(Year, avg, fill = Facility_Type)) +
  geom_line()
  #filter(Risk == "Pass") %>%



#######################################################################
#Number of Each facility type That Passes - using group_by() 
passes <- csf_select %>% 
  group_by(Facility_Type) %>%
  filter(Results == "Pass") %>%
  summarise(number = length(Results))

#Number of Each facility type That Passes with Conditions - using group_by() 
passes_cond <- csf_select %>% 
  group_by(Facility_Type) %>%
  filter(Results == "Pass w/ Conditions") %>%
  summarise(number = length(Results))

#Number of Each facility type That did not Passes with Conditions or Pass - using group_by() 

not_pass <- csf_select %>% 
  group_by(Facility_Type) %>%
  filter(Results != "Pass" & Results != "Pass w/ Conditions") %>%
  summarise(number = length(Results))

pass_rate_data <- data.frame(
  facility_type = c("Daycare", "Bar", "Resturant", "School"),
  pass_rate = c(1224/1730, 54/121, 31298/46294, 3159/4190 ),
  stringsAsFactors = TRUE
)
pass_rate_data


