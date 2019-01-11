

# age <- round(rtruncnorm(n=5000, a=20, b=85, mean=50, sd=1),0)
# family.size <- round(rtruncnorm(n=10, a=3, b=4, mean=2, sd=1),0)
# 
# gender <- c("Female","Male")
# 
# marital.status <- c("Married","Single","Divorced")
# 
# locality.type <- c("Village","City","Town")
# 
# Education <- c("Under Graduate", "Graduate", "Masters", "PhD")
# 
# Profession <- c("Student","Teacher","Doctor","Lawyer","Engineer","Admin - operation","Fisher","Shop Keeper","Not Working","Retired","Driver","Industry Worker")
# 
# Employment.Status <- c("Unemployed","Active","Terminated","Retired")
# 
# Purchase.Channel <- c("Direct","Independent Agent","Broker","TPA")
# 
# state <- read.csv(url("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv"))
# 
# cl.State <- sample(state$State,5000,replace = T)
# 
# cl.Profession <- sample(Profession,5000,replace = T)



library(readxl)

#Input Data from excel with lookup table
lookupTable <- read_excel("C:\\Users\\rahul\\Downloads\\Codeathon Next OneDrive_1_08-12-2018\\LookupTable.xlsx")

str(lookupTable)

#Locally assign lookup values 
Gender_ls <- lookupTable$Gender
Marital_Status_ls <- lookupTable$'Marital Status'
Locality_Type_ls <- lookupTable$`Locality Type`
State_ls = lookupTable$State
Education_ls = lookupTable$Education
Profession_ls = lookupTable$Profession
Organisation_ls = lookupTable$Organisation
Employment_Status_ls = lookupTable$'Employment Status'
Smoking_ls = lookupTable$Smoking
Health_Condition_ls = lookupTable$'Health Condition'
Purchase_Channel_ls = lookupTable$'Purchase Channel'


#Removing NA values
Gender_ls = Gender_ls[is.na(Gender_ls)==FALSE]
Marital_Status_ls = Marital_Status_ls[is.na(Marital_Status_ls)==FALSE]
Locality_Type_ls = Locality_Type_ls[is.na(Locality_Type_ls)==FALSE]
State_ls = State_ls[is.na(State_ls)==FALSE]
Education_ls = Education_ls[is.na(Education_ls)==FALSE]
Profession_ls = Profession_ls[is.na(Profession_ls)==FALSE]
Organisation_ls = Organisation_ls[is.na(Organisation_ls)==FALSE]
Employment_Status_ls = Employment_Status_ls[is.na(Employment_Status_ls)==FALSE]
Smoking_ls = Smoking_ls[is.na(Smoking_ls)==FALSE]
Health_Condition_ls = Health_Condition_ls[is.na(Health_Condition_ls)==FALSE]
Purchase_Channel_ls = Purchase_Channel_ls[is.na(Purchase_Channel_ls)==FALSE]

#Package to generate positive random normals
require(truncnorm)


df <- data.frame(ds.Gender_ls = sample(Gender_ls,5000,replace = T),
                 ds.Marital_Status_ls = sample(Marital_Status_ls,5000,replace = T,prob = c(0.7,0.2,0.1)),
                 ds.Locality_Type_ls = sample(Locality_Type_ls,5000,replace = T),
                 ds.State_ls = sample(State_ls,5000,replace = T),
                 ds.Education_ls = sample(Education_ls,5000,replace = T,prob = c(0.3,0.4,0.2,0.1)),
                 ds.Primary_Purchase_Channel_ls = sample(Purchase_Channel_ls,5000,replace = T,prob = c(0.3,0.3,0.2,0.2)))
df =  cbind(df,
            ds.Organisation_ls = sample(Organisation_ls,5000,replace = T,prob = c(0.2,0.14,0.15,0.12,0.15,0.01,0.08,0.1,0.05)),
            ds.Employment_Status_ls = sample(Employment_Status_ls,5000,replace = T, prob = c(0.05,0.6,0.15,0.2)),
            ds.Health_Condition_ls = sample(Health_Condition_ls,5000,replace = T, prob = c(0.25,0.3,0.45)),
            ds.Secondary_Purchase_Channel_ls = sample(Purchase_Channel_ls,5000,replace = T, prob = c(0.3,0.3,0.2,0.2)))

#colnames(df)

df$ds.Profession_ls = ifelse(df$ds.Employment_Status_ls == "Retired","Retired","NA")


df$ds.Profession_ls[df$ds.Profession_ls=="NA"] = sample(Profession_ls,4061,replace = T,prob = c(0.01,0.1,0.05,0.09,0.15,0.1,0.05,0.05,0.05,0.08,0.02,0.00,0.1,0.15))

#table(df$ds.Employment_Status_ls)

df$ds.age = ifelse(df$ds.Employment_Status_ls == "Retired",round(rtruncnorm(939,a=40, b=85, mean=55, sd=5),0),
                   ifelse(df$ds.Profession_ls == "Student",round(rtruncnorm(38 ,a=20, b=45, mean=28, sd=5),0),
                          round(rtruncnorm(4023 ,a=23, b=70, mean=45, sd=5),0)))

df$ds.Parents = ifelse(df$ds.Employment_Status_ls == "Retired",round(rtruncnorm(939,a=0, b=1, mean=0, sd=1),0),
                       ifelse(df$ds.Profession_ls == "Student",round(rtruncnorm(38 ,a=1, b=2, mean=2, sd=1),0),
                              round(rtruncnorm(4023 ,a=1, b=2, mean=1.25, sd=1),0)))

df$ds.family.size = ifelse(df$ds.age >= 20 & df$ds.age < 30, sample(c(3,4,5),replace=T,prob = c(0.2,0.6,0.2)),
                           ifelse(df$ds.age >= 30 & df$ds.age < 40, sample(c(4,5,6),replace=T,prob = c(0.1,0.7,0.2)),
                                  sample(c(3,4,5),replace=T,prob = c(0.1,0.6,0.3))))


df$ds.Children <- ifelse(df$ds.age >= 20 & df$ds.age < 30, sample(c(0,1,2),replace=T,prob = c(0.2,0.45,0.35)),
                      ifelse(df$ds.age >= 30 & df$ds.age < 40, sample(c(0,1,2),replace=T,prob = c(0.1,0.6,0.3)),
                             sample(c(2,3,4),replace=T,prob = c(0.5,0.35,0.15))))

#hist(df$ds.Children, breaks = 4, plot = T)

length(df$ds.age >= 20 & df$ds.age < 30)

df$ds.Avg.Family.Age <- ifelse(df$ds.age >= 20 & df$ds.age < 30 & df$ds.Children > 2 , rtruncnorm(5000,a=30, b=40, mean=35, sd=3),
                      ifelse(df$ds.age >= 30 & df$ds.age < 45 & df$ds.Children > 1, rtruncnorm(5000,a=30, b=50, mean=40, sd=3),
                      rtruncnorm(5000,a=40, b=60, mean=50, sd=3)))

df$ds.Annual.income = round(rtruncnorm(n=5000, a=20000, b=185000, mean=50000, sd=10000),0)

df$ds.Avg.annual.inc = round(rtruncnorm(n=5000, a=20000, b=185000, mean=50000, sd=10000),0)

df$ds.Annual.Expenses  = df$ds.Annual.income - round(rtruncnorm(n=5000, a=10000, b=105000, mean=35000, sd=7000),0)

#max(df$ds.Annual.Expenses)


df$ds.Saving.Amount = df$ds.Annual.income - df$ds.Annual.Expenses

df$ds.Credit.Cards = round(rtruncnorm(n=5000, a=0, b=15, mean=4, sd=1),0)

df$ds.Two.Wheelers = round(rtruncnorm(n=5000, a=0, b=4, mean=2, sd=1),0)

df$ds.Four.Wheelers = round(rtruncnorm(n=5000, a=0, b=2, mean=0, sd=0.5),0)

df$ds.Bank.Accounts = round(rtruncnorm(n=5000, a=1, b=6, mean=3, sd=1),0)

df$ds.Houses = round(rtruncnorm(n=5000, a=1, b=4, mean=1, sd=1),0)

df$ds.Estates  = round(rtruncnorm(n=5000, a=1, b=3, mean=1, sd=0.5),0)

df$ds.Yearly.Travel.Dist.Air = ifelse(round(rtruncnorm(n=5000, a=5000, b=50000, mean=15000, sd=1000),0)<0,round(rtruncnorm(n=5000, a=5000, b=50000, mean=15000, sd=1000),0)*-1,round(rtruncnorm(n=5000, a=5000, b=50000, mean=15000, sd=1000),0))

df$ds.Yearly.Travel.Dist.Road = ifelse(round(rtruncnorm(n=5000, a=20000, b=80000, mean=50000, sd=10000),0)<0,round(rtruncnorm(n=5000, a=20000, b=80000, mean=50000, sd=10000),0)*-1,round(rtruncnorm(n=5000, a=20000, b=80000, mean=50000, sd=10000),0))

df$ds.Policy.Face.Value = round(rtruncnorm(n=5000, a=200000, b=2550000, mean=100000, sd=20000),0)

df$ds.Claimed.Amount = df$ds.Policy.Face.Value*sample(c(0,0.2,0.5,0.8,1),replace=T,prob = c(0.55,0.15,0.15,0.1,0.05))

df$ds.Yearly.Premium = df$ds.Annual.Expenses * round(rtruncnorm(n=5000, a=5, b=20, mean=10, sd=2),0)/100

df$ds.YearOfPurchase.1st_Prod = sample(c(2000:2009), 5000, replace = TRUE)

df$ds.YearOfPurchase.LastProd = df$ds.YearOfPurchase.1st_Prod + sample(0:8, 5000, replace = TRUE)

#Children Plan
df$ds.prod1 = ifelse(df$ds.Children == 0, sample(0:1,replace=T,prob = c(0.9,0.1)),sample(0:1,replace=T,prob = c(0.1,0.9)))

#Senior Citizen Plan
df$ds.prod2 = ifelse(df$ds.age < 60, sample(0:1,replace=T,prob = c(0.9,0.1)),sample(0:1,replace=T,prob = c(0.2,0.8)))

#Car Insurance Plan
df$ds.prod3 = ifelse(df$ds.Four.Wheelers > 0, sample(0:1,replace=T,prob = c(0,0.9)),sample(0:1,replace=T,prob = c(1,0)))

#Bike Insurance Plan
df$ds.prod4 = ifelse(df$ds.Two.Wheelers > 0, sample(0:1,replace=T,prob = c(0,0.9)),sample(0:1,replace=T,prob = c(1,0)))

#Professional Insurance Plan
df$ds.prod5 = ifelse(df$ds.Profession_ls == "Student", sample(0:1,replace=T,prob = c(1,0)),sample(0:1,replace=T,prob = c(0.15,0.85)))

#Farmer Insurance Plan
df$ds.prod6 = ifelse(df$ds.Profession_ls == "Farmer", sample(0:1,replace=T,prob = c(1,0)),sample(0:1,replace=T,prob = c(0.15,0.85)))

#High Income Insurance Plan
df$ds.prod7 = ifelse(df$ds.Annual.income <= 50000, sample(0:1,replace=T,prob = c(0.8,0.2)),sample(0:1,replace=T,prob = c(0.2,0.8)))

#Estate Insurance Plan
df$ds.prod8 = ifelse(df$ds.Estates < 2, sample(0:1,replace=T,prob = c(0.95,0.15)),sample(0:1,replace=T,prob = c(0.15,0.95)))

#Health Insurance Plan
df$ds.prod9 = ifelse(df$ds.Health_Condition_ls == "Below Avg", sample(0:1,replace=T,prob = c(0.2,0.8)),sample(0:1,replace=T,prob = c(0.1,0.9)))

#Retirement Plan
df$ds.prod10 = sample(0:1,5000,replace = TRUE,prob = c(0.10,0.90))
  
#getwd()

write.csv(df, file = "C:\\Users\\rahul\\Downloads\\Codeathon Next OneDrive_1_08-12-2018\\codeathon_input1.csv")

View(df)
