library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)
library(readr)

#install.packages('lubridate')
library(lubridate)

csf <-  read.csv("C:/Users/preci/Desktop/W O N D E R/DSRP-2024-Shaunette/Precious_Project/data/Food_Inspections.csv")
head(csf)

#Cleaning the city column
get_dupes(csf, 'Location')
clean_names(csf, 'title')

filter(csf, City != "chicago")

csf$City[which(csf$City== "CHicago")] = "Chicago"
csf$City[which(csf$City== "CHicago")] = "Chicago"
csf$City[which(csf$City== "CHICAGO")] = "Chicago"
csf$City[which(csf$City== "123chicago")] = "Chicago"
csf$City[which(csf$City== "CHICAGO.")] = "Chicago"
csf$City[which(csf$City== "Chicago")] = "Chicago"
csf$City[which(csf$City== "LANSING")] = "Chicago"
csf$City[which(csf$City== "CHICAGOCHICAGO")] = "Chicago"
csf$City[which(csf$City== "CCHICAGO")] = "Chicago"
csf$City[which(csf$City== "CHCHICAGO")] = "Chicago"
csf$City[which(csf$City== "CHICAGOO")] = "Chicago"
csf$City[which(csf$City== "CHICAGOI")] = "Chicago"

csf$City[which(csf_new$City== "312CHICAGO")] = "Chicago"
csf$City[which(csf_new$City== "CHICAGOC")] = "Chicago"
csf$City[which(csf_new$City== "chicago")] = "Chicago"
csf$City[which(csf_new$City== "CHCICAGO")] = "Chicago"
csf$City[which(csf_new$City== "CH")] = "Chicago"
csf$City[which(csf_new$City== "CHICAGO HEIGHTS")] = "Chicago"
csf$City[which(csf_new$City== "MAYWOOD")] = "Maywood"
csf$City[which(csf_new$City== "Summit")] = "Chicago"
csf$City[which(csf_new$City== "SUMMIT")] = "Chicago"
csf$City[which(csf_new$City== "OAK LAWN")] = "Chicago"
csf$City[which(csf_new$City== "MATTESON")] = "Chicago"

# removing villages and labeling them as chicago
csf$City[which(csf$City== "ELK GROVE VILLAGE")] = "chicago" 
csf$City[which(csf$City== "SCHAUMBURG")] = "chicago" 
csf$City[which(csf$City== "ELMHURST")] = "chicago" 
csf$City[which(csf$City== "NILES NILES")] = "chicago" 
csf$City[which(csf$City== "chicagoBEDFORD PARK")] = "chicago" 
csf$City[which(csf$City== "HIGHLAND PARK")] = "chicago" 
csf$City[which(csf$City== "BLOOMINGDALE")] = "chicago" 
csf$City[which(csf$City== "NAPERVILLE")] = "chicago" 
csf$City[which(csf$City== "alsip")] = "chicago"
csf$City[which(csf$City== "WESTMONT")] = "chicago"
csf$City[which(csf$City== "TINLEY PARK")] = "chicago"

#remove resturants not in IL
nrow(csf)
csf_IL_only <- csf[csf$State != 'CA', ]
csf_IL_only <- csf[csf$State != 'CO', ]
csf_IL_only <- csf[csf$State != 'WI', ]
csf_IL_only <- csf[csf$State != 'NY', ]
csf_IL_only <- csf[csf$State != 'IN', ]
csf_IL_only <- csf[csf$State != '', ]

names(csf_IL_only)

#inserting NA's
csf_IL_only$City[which(csf_IL_only$City== "")] = NaN
csf_IL_only$AKA.Name[which(csf_IL_only$AKA.Name== "")] = NaN
csf_IL_only$DBA.Name[which(csf_IL_only$DBA.Name== "")] = NaN
csf_IL_only$Violations[which(csf_IL_only$Violations== "")] = NaN
csf_IL_only
# find Buisness Not located Location, see if there 
df_new <- filter(csf_IL_only, Results == "Business Not Located")
df_new$AKA.Name
df_new$Zip
get_dupes(df_new, 'Zip')

#change column names
csf_new <- csf_IL_only %>%
  rename(
    License = License..,
    Inspection_ID = Inspection.ID,
    Inspection_Date = Inspection.Date, 
    Inspection_Type = Inspection.Type,    
    DBA_Name = DBA.Name,                        
    AKA_Name = AKA.Name,
    Facility_Type = Facility.Type
  )
colnames(csf_new)
## Clean Facility type
csf_new <- csf_new %>% 
  mutate(Facility_Type = case_when(Facility_Type == 'SHELTER' ~ 'Shelter',
                   Facility_Type == 'Daycare Above and Under 2 Years' ~ 'Daycare',
                   Facility_Type == 'Daycare (2 - 6 Years)' ~ 'Daycare',
                   Facility_Type == 'Daycare (2 Years)' ~ 'Daycare',
                   Facility_Type == 'DAYCARE 2 YRS TO 12 YRS' ~ 'Daycare',
                   Facility_Type == 'Daycare (Under 2 Years)' ~ 'Daycare',
                   Facility_Type == 'DAYCARE 2-6, UNDER 6' ~ 'Daycare',
                   Facility_Type == 'DAY CARE 102' ~ 'Daycare',
                   Facility_Type == 'DAYCARE 1586' ~ 'Daycare',
                   Facility_Type == 'DAYCARE 1023' ~ 'Daycare',
                   Facility_Type == 'DAYCARE COMBO' ~ 'Daycare',
                   Facility_Type == 'DAYCARE' ~ 'Daycare',
                   Facility_Type == 'DAYCARE 6 WKS-5YRS' ~ 'Daycare',
                   Facility_Type == 'Daycare Combo 1586' ~ 'Daycare',
                   Facility_Type == 'Daycare Night' ~ 'Daycare',
                   Facility_Type == 'Day Care Combo (1586)' ~ 'Daycare',
                   Facility_Type == 'CHURCH/DAY CARE' ~ 'Daycare',
                   Facility_Type == 'DAY CARE' ~ 'Daycare',
                   Facility_Type == 'DAY CARE 1023' ~ 'Daycare',
                   Facility_Type == '1584-DAY CARE ABOVE 2 YEARS' ~ 'Daycare',
                   Facility_Type == 'DAY CARE 2 - 14' ~ 'Daycare',
                   Facility_Type == 'Day Care Facility' ~ 'Daycare',
                   
                   Facility_Type == 'Coffe Shop' ~ 'Cafe',
                   Facility_Type == 'coffe shop' ~ 'Cafe',
                   Facility_Type == 'cafe' ~ 'Cafe',
                   Facility_Type == 'Kids Cafe' ~ 'Cafe',
                   Facility_Type == 'CAFE' ~ 'Cafe',
                   Facility_Type == 'COFFEE/TEA' ~ 'Cafe',
                   Facility_Type == 'TEA STORE' ~ 'Cafe',
                   Facility_Type == 'TAVERN' ~ 'Bar',
                   Facility_Type == 'TAVERN/LIQUOR' ~ 'Bar',
                   Facility_Type == 'Tavern' ~ 'Bar',
                   Facility_Type == 'HOOKA BAR' ~ 'Bar',
                   Facility_Type == 'Tavern/Bar' ~ 'Bar',
                   Facility_Type == 'LIQUOR' ~ 'Bar', 
                   Facility_Type == 'LIQUOR/GROCERY STORE/BAR' ~ 'Bar', 
                   Facility_Type == 'bar' ~ 'Bar',
                   Facility_Type == 'TAVERN-LIQUOR' ~ 'Bar',
                   Facility_Type == 'Liquor' ~ 'Bar',
                   
                   Facility_Type == 'hair salon' ~ 'Hair Salon',
                   Facility_Type == 'HAIR SALON' ~ 'Hair Salon',
                   Facility_Type == 'NON-PROFIT' ~ 'Non-Profit',
                   Facility_Type == 'NON -PROFIT' ~ 'Non-Profit',
                   Facility_Type == 'NOT FOR PROFIT' ~ 'Non-Profit',
                   Facility_Type == 'NON-FOR PROFIT BASEMENT' ~ 'Non-Profit',
                   Facility_Type == 'NON-FOR PROFIT CLUB' ~ 'Non-Profit',
                   Facility_Type == 'liquor store' ~ 'Bar',
                   Facility_Type == 'GYM STORE' ~ 'Gym',
                   Facility_Type == 'GYM' ~ 'Gym',
                   Facility_Type == 'liquor store' ~ 'Bar',
                   Facility_Type == 'LIQUORE STORE/BAR' ~ 'Bar',
                   Facility_Type == 'TAVERN/1006' ~ 'Bar',
                   Facility_Type == '(convenience store)' ~ 'Convenience Store',
                   Facility_Type == 'GROCERY & RESTAURANT' ~ 'Restaurant',
                   Facility_Type == 'RESTAURANT/BAKERY' ~ 'Restaurant',
                   Facility_Type == 'RESTAURANT.BANQUET HALLS' ~ 'Restaurant',
                   Facility_Type == 'GROCERY STORE/ RESTAURANT' ~ 'Restaurant',
                   Facility_Type == 'GROCERY & RESTAURANT' ~ 'Restaurant',
                   
                   Facility_Type == 'bakery/restaurant' ~ 'Restaurant',
                   Facility_Type == 'TENT RSTAURANT' ~ 'Restaurant',
                   TRUE ~ Facility_Type
                   
  ))
#unique(csf_new$Facility_Type)
#colnames(csf_new)



#Split Violations

violations = read_csv("C:/Users/preci/Desktop/W O N D E R/DSRP-2024-Shaunette/Precious_Project/data/violations.csv")
head(violations)

#Drop columns
csf_new <- csf_new[, -20]
csf_new <- csf_new[, -19]
csf_new <- csf_new[, -18]

colnames(csf_new)

#Create a Date & Year Column
csf_new$Inspection_Date <- as.Date(csf_new$Inspection_Date[],format = '%m/%d/%Y')
csf_new$Year <- as.numeric(format(csf_new$Inspection_Date,'%Y'))
head(csf_new$Year)
tail(csf_new$Inspection_Date)
