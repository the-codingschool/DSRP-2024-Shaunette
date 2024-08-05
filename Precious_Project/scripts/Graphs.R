#Risk vs Facility Type
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)
#library(streamgraph)
#install.packages(streamgraph)
library(viridis)
library(hrbrthemes)
library(plotly)

csf_no_blank <- csf_new[csf_new$Risk != '', ]
csf_select <- csf_no_blank %>% filter(Facility_Type == c('Restaurant', 'Bar', 'School', 'Daycare'))

#Finding proportion of pass or fail depending for each specifc location ####
length(get_dupes(csf_select, 'Location'))
length(unique(csf_select$Address))

#buisnesses <- unique(csf_select$Address)


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
  #geom_line(csf_select$Bink))

# Finding Counts ####

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

csf_select %>% 
  group_by(Facility_Type, Year) %>%
  filter(Risk == "Risk 3 (Low)") %>%
  summarise(number = length(Risk))
## creating a new risk column where risk has a numeric value

csf_select$Binary_Risk <- csf_select$Risk 
csf_select$Binary_Risk[csf_select$Binary_Risk == "Risk 1 (High)"] <- 1
csf_select$Binary_Risk[csf_select$Binary_Risk == "Risk 2 (Medium)"] <- 2
csf_select$Binary_Risk[csf_select$Binary_Risk == "Risk 3 (Low)"] <- 3
csf_select$Binary_Risk[csf_select$Binary_Risk == "All"] <- -1
csf_select$Binary_Risk = as.numeric(csf_select$Binary_Risk)
csf_select$Binary_Risk[csf_select$Binary_Risk == 1] <- 3
csf_select$Binary_Risk[csf_select$Binary_Risk == 2] <- 2
csf_select$Binary_Risk[csf_select$Binary_Risk == 3] <- 1
csf_select$Binary_Risk[csf_select$Binary_Risk == -1] <- 0
#Risk 'Coef' #####
risk_coef <- data.frame(csf_select %>% 
  group_by(Facility_Type) %>%
  summarise(avg = mean(Binary_Risk)))

risk_coef_yrly <- data.frame(csf_select %>% 
  group_by(Facility_Type, Year) %>%
  summarise(avg = mean(Binary_Risk)))
#graphing risk per year
csf_select %>% 
  group_by(Facility_Type, Year) %>%
  summarise(Average_Risk = mean(Binary_Risk)) %>%
  ggplot(aes(Year, Average_Risk, fill = Facility_Type, color = Facility_Type), ylim = c(0, 4)) +
  geom_line() +
  ggtitle("Avgerage Risk Score Over the Years")
  #filter(Risk == "Pass") %>%

#Graph for Lighting Talk 1 ####
csf_select %>% 
  group_by(Facility_Type, Year) %>%
  summarise(Average_Risk = mean(Binary_Risk)) %>%
  ggplot(aes(Year, Average_Risk, fill = Facility_Type, color = Facility_Type), ylim = c(0, 4)) +
  geom_point() +
  geom_line() +
  #scale_y_continuous(expand=c(0,0.10)) +
  ggtitle("Avgerage Risk Score Over the Years")
#filter(Risk == "Pass") %>%


#######################################################################
#Number of Each facility type That Passes - using group_by() 
passes <- csf_select %>% 
  group_by(Facility_Type) %>%
  filter(Results == "Pass") %>%
  summarise(number = length(Results))

bar_passes <- passes$number[1]
dayCare_passes <- passes$number[2]
rest_passes <- passes$number[3]
school_passes <- passes$number[4]

#Number of Each facility type That Passes with Conditions - using group_by() 
passes_cond <- csf_select %>% 
  group_by(Facility_Type) %>%
  filter(Results == "Pass w/ Conditions") %>%
  summarise(number = length(Results))

bar_passes <- bar_passes + passes_cond$number[1]
dayCare_passes <- dayCare_passes + passes_cond$number[2]
rest_passes <- rest_passes + passes_cond$number[3]
school_passes <- school_passes + passes_cond$number[4]

#Number of Each facility type That did not Passes with Conditions or Pass - using group_by() 

not_pass <- csf_select %>% 
  group_by(Facility_Type) %>%
  filter(Results != "Pass" & Results != "Pass w/ Conditions") %>%
  summarise(number = length(Results))

bar_not_pass <- not_pass$number[1]
dayCare_not_pass <- not_pass$number[2]
rest_not_pass <- not_pass$number[3]
school_not_pass <- not_pass$number[4]

# The pass rate for each kind of facility
pass_rate_data <- data.frame(
  facility_type = c("Daycare", "Bar", "Resturant", "School"),
  pass_rate = c(dayCare_passes/(dayCare_not_pass+dayCare_passes), bar_passes/(bar_passes+bar_not_pass), 
                rest_passes/(rest_not_pass+rest_passes), school_passes/(school_not_pass+school_passes)),
  stringsAsFactors = TRUE
)

line(pass_rate_data) #finds that the corealtion between facility type and pass rate is 0.51046 annd 0.05805

csf_select %>% 
  group_by(Facility_Type, Year) %>%
  ggplot(aes(Year, pass_rate, fill = Facility_Type)) %>% 
  passRate = (passes + passes_cond)/(not_pass + passes + passes_cond) +
  #pass_rate = c(dayCare_passes/(dayCare_not_pass+dayCare_passes), bar_passes/(bar_passes+bar_not_pass), 
   #             rest_passes/(rest_not_pass+rest_passes), school_passes/(school_not_pass+school_passes)) %>%
  #summarise(pass_rate) %>%s
  geom_line(passRate)

csf_select %>% 
  group_by(Facility_Type, Year) %>%
  filter(Results != "Pass" & Results != "Pass w/ Conditions") %>%
  summarise(number = length(Results)) 

#passRate = (passes + passes_cond)/(not_pass + passes + passes_cond)
passRate = bar_passes/(bar_passes+bar_not_pass)
csf_select %>%
  group_by(Year) %>%
  filter(Facility_Type == 'Bar' & Year >= 2023) %>%
  hist(passRate)
  #ggplot(aes(Year, passRate, )) + 
  #geom_line(passRate)
  #passRate = (passes + passes_cond)/(not_pass + passes + passes_cond)

# Finding a Correlation between number of passes and risk ####

pass_data <- csf_select %>% 
  group_by(Facility_Type, Year) %>%
  #filter(Facility_Type == "Bar") %>%
  summarise(Passes = sum(Results == 'Pass') + sum(Results == 'Pass w/ Conditions'), 
            Failures = sum(Results == 'Fail') + sum(Results == 'No Entry') + 
              sum(Results == 'Out of Business') + sum(Results == 'Not Ready') +
              sum(Results == 'Business Not Located')) %>%
  mutate(Pass_Rate = (Passes)/(Failures + Passes))

pass_data %>% ggplot(aes(x = Year, y = Pass_Rate)) %>%
  filter(Facility_Type == 'Bar') %>%
  geom_point()

plot(risk_coef_yrly$avg, pass_rate_data$pass_rate)

# rate of risk 1, 2, 3
risk_rate <- csf_select %>% 
  group_by(Facility_Type, Year) %>%
  #filter(Facility_Type == "Daycare") %>%
  summarise(riskOne = sum(Risk == 'Risk 1 (High)'), 
          riskTwo = sum(Risk == 'Risk 2 (Medium)'), 
          riskThree = sum(Risk == 'Risk 3 (Low)'),
          All = sum(Risk == 'All')) %>%
  mutate(Risk_One_Rate = (riskOne)/(riskOne + riskTwo +riskThree + All),
         Risk_Two_Rate = (riskTwo)/(riskOne + riskTwo +riskThree + All),
         Risk_Three_Rate = (riskThree)/(riskOne + riskTwo +riskThree + All))

#correlation graph between two variables 
line(risk_rate$riskOne, pass_data$Pass_Rate)
pass_data
risk_rate
# risk_rate %>% filter(Facility_Type == 'Bar') %>% ggplot()  +
#   geom_point(mapping = aes(x = Year, y = Risk__Rate))

combine_data_risk_pass <- data.frame(Facility_Type = pass_data$Facility_Type,
                                     Year = pass_data$Year,
                                     Pass_Rate = pass_data$Pass_Rate,
                                     Risk_One_Rate = risk_rate$Risk_One_Rate,
                                     Risk_Two_Rate = risk_rate$Risk_Two_Rate, 
                                     Risk_Three_Rate = risk_rate$Risk_Three_Rate)
combine_data_risk_pass
# Scatter plot for each faciltiy type
### Line of best fit used

# Daycare###
combine_data_risk_pass %>% filter(Facility_Type == 'Daycare') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_One_Rate, color = Year)) +
  geom_point() +
  geom_smooth(aes(Pass_Rate,Risk_One_Rate), method="lm", se=F) +
  ggtitle("Correaltion Between Daycare Risk 1 Rate and Overall PassRate from 2010 - 2024")


combine_data_risk_pass %>% filter(Facility_Type == 'Daycare') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_Two_Rate, color = Year)) +
  geom_point() +
  geom_smooth(aes(Pass_Rate,Risk_Two_Rate), method="lm", se=F)


combine_data_risk_pass %>% filter(Facility_Type == 'Daycare') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_Three_Rate, color = Year)) +
  geom_point() +
  geom_smooth(aes(Pass_Rate,Risk_Three_Rate), method="lm", se=F)


## Bar
combine_data_risk_pass %>% filter(Facility_Type == 'Bar') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_One_Rate, color = Year)) +
  geom_point() +
  geom_smooth(aes(Pass_Rate,Risk_One_Rate), method="lm", se=F)

combine_data_risk_pass %>% filter(Facility_Type == 'Bar') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_Two_Rate, color = Year)) +
  geom_point() + 
  geom_smooth(aes(Pass_Rate,Risk_Two_Rate), method="lm", se=F) +
  ggtitle("Correaltion Between Bar Risk 2 Rate and Overall PassRate from 2010 - 2024")


combine_data_risk_pass %>% filter(Facility_Type == 'Bar') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_Three_Rate, color = Year)) +
  geom_point() + 
  geom_smooth(aes(Pass_Rate,Risk_Three_Rate), method="lm", se=F)

## School
combine_data_risk_pass %>% filter(Facility_Type == 'School') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_One_Rate, color = Year)) +
  geom_point() + 
  geom_smooth(aes(Pass_Rate,Risk_One_Rate), method="lm", se=F)

combine_data_risk_pass %>% filter(Facility_Type == 'School') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_Two_Rate, color = Year)) +
  geom_point() + 
  geom_smooth(aes(Pass_Rate,Risk_Two_Rate), method="lm", se=F)

combine_data_risk_pass %>% filter(Facility_Type == 'School') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_Three_Rate, color = Year)) +
  geom_point() + 
  geom_smooth(aes(Pass_Rate,Risk_Three_Rate), method="lm", se=F)

## restaurants 
combine_data_risk_pass %>% filter(Facility_Type == 'Restaurant') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_One_Rate, color = Year)) +
  geom_point() + 
  geom_smooth(aes(Pass_Rate,Risk_One_Rate), method="lm", se=F)

combine_data_risk_pass %>% filter(Facility_Type == 'Restaurant') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_Two_Rate, color = Year)) +
  geom_point() + 
  geom_smooth(aes(Pass_Rate,Risk_Two_Rate), method="lm", se=F)

#smooth density plot
combine_data_risk_pass %>% filter(Facility_Type == 'Restaurant') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_Two_Rate, color = Year)) +
  geom_point() + 
  geom_smooth()

combine_data_risk_pass %>% filter(Facility_Type == 'Restaurant') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_Three_Rate, color = Year)) +
  geom_point() + 
  geom_smooth(aes(Pass_Rate,Risk_Three_Rate), method="lm", se=F, color = 'red')

#smooth density plot
combine_data_risk_pass %>% filter(Facility_Type == 'Restaurant') %>% 
  ggplot(aes(x = Pass_Rate, y = Risk_Three_Rate, color = Year)) +
  geom_point() + 
  geom_smooth(color = 'red')


