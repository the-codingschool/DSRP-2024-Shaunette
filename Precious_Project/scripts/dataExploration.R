
## read in dataset
csf <-  read.csv("C:/Users/preci/Desktop/W O N D E R/DSRP-2024-Shaunette/Precious_Project/data/Food_Inspections.csv")
head(csf)
tail(csf)
typeof(csf$Inspection.ID)
typeof(csf$DBA.Name)
typeof(csf$License..)
typeof(csf$AKA.Name)
typeof(csf$Facility.Type)
typeof(csf$Risk)
typeof(csf$Address)
typeof(csf$City)
typeof(csf$State)
typeof(csf$Zip)
typeof(csf$Inspection.Date)
typeof(csf$Inspection.Type)
typeof(csf$Results)
typeof(csf$Violations)
typeof(csf$Longitude)
typeof(csf$Latitude)
typeof(csf$Location)


nrow(csf)
ncol(csf)

unique(csf$Risk)
table(csf$Risk)

unique(csf$City)
table(csf$City)

unique(csf$City)
table(csf$City)


unique(csf$State)
table(csf$State)

unique(csf$Zip)
table(csf$Zip)

unique(csf$Results)
table(csf$Results)

unique(csf$Inspection.Type)
table(csf$Inspection.Type)

unique(csf$Violations)
table(csf$Violations)

unique(csf$Longitude)
table(csf$Longitude)

unique(csf$Latitude)
table(csf$Latitude)

table(csf$Location)
unique(csf$Location)

length(unique(csf$Inspection.ID))
length(unique(csf$DBA.Name))
length(unique(csf$License..))
length(unique(csf$AKA.Name))
length(unique(csf$Facility.Type))
length(unique(csf$Risk))
length(unique(csf$Address))
length(unique(csf$City)) ## chicago is spelt 84 diffrent ways
length(unique(csf$State))
length(unique(csf$Zip))
length(unique(csf$Inspection.Date))
length(unique(csf$Inspection.Type))
length(unique(csf$Results))
length(unique(csf$Violations))
length(unique(csf$Longitude))
length(unique(csf$Latitude))
length(unique(csf$Location))

mean(csf$Inspection.ID)
median(csf$Inspection.ID)
sd(csf$Inspection.ID)
var(csf$Inspection.ID)

median(csf$License..)

mean(csf$Zip)
median(csf$Zip)

mean(csf$Longitude)
median(csf$Longitude)

mean(csf$Latitude)
median(csf$Latitude)
