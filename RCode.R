library(openxlsx)
library(plyr)

##Population##
pop <- read.xlsx("14100DS0002_2017-03.xlsx",
                 sheet ="Population and People _LGA_1540",
                 startRow = 5)
pop[pop == "-"]<- NA

#Percentage Age distribution of Population
percent_age_dist <- subset(pop, select = c(2:12))
colnames(percent_age_dist) = percent_age_dist[2, ]
percent_age_dist <- percent_age_dist[-c(1:3),]

#Age distribution of population
age_dist <- subset(pop, select = c(2,3,51:69))
colnames(age_dist) = age_dist[2,]
age_dist <- age_dist[-c(1:3),]

#Age distribution of males
age_dist_m <- subset(pop, select = c(2,3,13:31))
colnames(age_dist_m) = age_dist_m[2,]
age_dist_m <- age_dist_m[-c(1:3),]

#Age distribution of females
age_dist_f <- subset(pop, select = c(2,3,32:50))
colnames(age_dist_f) = age_dist_f[2,]
age_dist_f <- age_dist_f[-c(1:3),]

#Percentage of working age population per area
work_age <- subset(pop, select = c(2,3,70))
colnames(work_age) = work_age[2,]
work_age <- work_age[-c(1:3),]

##Median Age##
#Median age per area
med_age <- subset(pop, select = c(2,3,71:73))
colnames(med_age) = med_age[2,]
med_age <- med_age[-c(1:3),]

##Births and Deaths##
#Births per area
birth <- subset(pop, select =c(2,3,74))
colnames(birth) = birth[1,]
birth <- birth[-c(1:3),]
#Fertility per female
fertility <- subset(pop, select =c(2,3,75))
colnames(fertility) = fertility[1,]
fertility <- fertility[-c(1:3),]
#Deaths per area
death <- subset(pop, select =c(2,3,76))
colnames(death) = death[1,]
death <- death[-c(1:3),]
#Standardised death per area
death_st <- subset(pop, select =c(2,3,77))
colnames(death_st) = death_st[1,]
death_st <- death_st[-c(1:3),]

##Population Desity##
pop_den <- subset (pop, select = c(2,3,78))
colnames(pop_den) = pop_den[3,]
pop_den <- pop_den[-c(1:3),]

#Percentage of Aboriginal people per area
perc_ind <- subset(pop, select =c(2,3,79))
colnames(perc_ind) = perc_ind[1,]
perc_ind <- perc_ind[-c(1:3),]

#Overseas Population
overseas_pop <- subset(pop, select =c(2,3,80:89), X81 != "-")
colnames(overseas_pop) = overseas_pop[1,]
overseas_pop <- overseas_pop[-c(1:3),]

##Migration##
#Arrivals
arrivals <- subset(pop, select =c(2,3,90))
colnames(arrivals) = arrivals[1,]
arrivals <- arrivals[-c(1:3),]

#Departures
departures <- subset(pop, select =c(2,3,91))
colnames(departures) = departures[1,]
departures <- departures[-c(1:3),]

#Net arrivals
net <- subset(pop, select =c(2,3,92))
colnames(net) = net[1,]
net <- net[-c(1:3),]

##Industry and Sectors##
ind <- read.xlsx("14100DS0004_2017-03.xlsx",
                 sheet = "Economy and Industry _LGA_15424",
                 startRow = 6)
ind[ind == "-"] <- NA

#Businesses with number of employees
num_emp <- subset(ind, select = c(2,3,4:8))
colnames(num_emp) = num_emp[1,]
num_emp <- num_emp[-c(1:2),]

#Business entries
bus_ent <- subset(ind, select = c(2,3,9:13))
colnames(bus_ent) = bus_ent[1,]
bus_ent <- bus_ent[-c(1:2),]

#Business exits
bus_exit <- subset(ind, select = c(2,3,14:18))
colnames(bus_exit) = bus_exit[1,]
bus_exit <- bus_exit[-c(1:2),]

#Businesses per industry
bus_ind <- subset(ind, select = c(2,3,19:39))
colnames(bus_ind) = bus_ind[1,]
bus_ind <- bus_ind[-c(1:2),]

#Building approvals
bud_app <- subset(ind, select = c(2,3,40:49))
colnames(bud_app) = bud_app[1,]
bud_app <- bud_app[-c(1:2),]

#Bankrupts
bankrupts <- subset(ind, select = c(2,3,54:56))
colnames(bankrupts) = bankrupts[1,]
bankrupts <- bankrupts[-c(1:2),]

#Percentage of workforce per industry
work_ind <- subset(ind, select = c(2,3,57:76))
colnames(work_ind) = work_ind[1,]
work_ind <- work_ind[-c(1:2),]
work_ind <- work_ind[work_ind$Manufacturing != "-",]

#Type of vehicle
vec_type <- subset(ind, select =c(2,3,77:86))
colnames(vec_type) = vec_type[1,]
vec_type <- vec_type[-c(1:2),]

#Vehicle fuel type
vec_fuel <- subset(ind, select = c(2,3,87:90))
colnames(vec_fuel) = vec_fuel[1,]
vec_fuel <- vec_fuel[-c(1:2),]

#Vehicle years since manufacture
vec_manu <- subset(ind, select = c(2,3,91:93))
colnames(vec_manu) = vec_manu[1,]
vec_manu <- vec_manu[-c(1:2),]

#Income Employment and Education
iee <- read.xlsx("14100DS0006_2017-03.xlsx",
                 sheet ="Income_Educ and Emp_Health_LGA_",
                 startRow = 6)
iee[iee == "-"] <- NA

##Estimates of personal income
epi <- subset(iee, select = c(2:21), X5 != "-")
colnames(epi) = epi[1,]
epi <- epi[-c(1:2),]

##Income##
#Number, median and total employee income
employee_income <- subset(epi, select = c(1,2,3:5))
#Number, median and total self-employed income
own_income <- subset(epi, select = c(1,2,6:8))
#Number, median and total investment income
invest_income <- subset(epi, select = c(1,2,9:11))
#Number, median and total super income
super_income <- subset(epi, select = c(1,2,12:14))
#Number, median and total other income
other_income <- subset(epi, select = c(1,2,15:17))
#Number, median and total total income
total_income <- subset(epi, select = c(1,2,18:20))

##Government Pensions and Allowances
gpa <- subset(iee, select = c(2,3,22:37))
colnames(gpa) = gpa[1,]
gpa <- gpa[-c(1:2),]

##Education
edu <- subset(iee, select = c(2,3,37:42), X37 != "-")
colnames(edu) = edu[1,]
edu <- edu[-c(1:2),]

##Occupation
ocu <- subset(iee, select = c(2,3,43:51), X44 != "-")
colnames(ocu) = ocu[1,]
ocu <- ocu[-c(1:2),]

##Youth Engagement with workforce
youth <- subset(iee, select = c(2,3,52:59), X53 != "-")
colnames(youth) = youth[1,]
youth <- youth[-c(1:2),]

##Labour force
lbf <- subset(iee, select = c(2,3,60:63), X61 != "-")
colnames(lbf) = lbf[1,]
lbf <- lbf[-c(1:2),]

#Family and Community, Land and Environment
fcle <- read.xlsx("14100DS0008_2017-03.xlsx",
                  sheet = "Family_Land_LGA_1546198",
                  startRow = 6)
fcle[fcle == "-"] <- NA

#Family and Community
fc <- subset(fcle, select = c(2,3,4:53), X7 != "-")
colnames(fc) = fc[1,]
fc <- fc[-c(1:2),]

#English proficiency
eng_prof <- subset(fc, select = c(1,2,3:7))

#Citizenship
citizen <- subset(fc, select = c(1,2,8:10))

#Transport to and from work
transport <- subset(fc, select = c(1,2,11:22))

#Total employed
t_employ <- subset(fc, select = c(1,2,23))

#Household size
#Lone, Group, Family, Total, Average
house_size <- subset(fc, select = c(1,2,24:28))

#Family size
fam_size <- subset(fc, select = c(1,2,29:36))

#Voluntary work / care
vol <- subset(fc, select = c(1,2,37:41))

#Internet connect
int <- subset(fc, select = c(1,2,42:45))

#Average rental and mortgage payments
avg_rent_mort <- subset(fc, select = c(1,2,46:47))

#SEIFA Decile Ranking
seifa <- subset(fc, select = c(1,2,48:51))

#Land and environment
le <- subset(fcle, select = c(2,3,53:70), LAND.AREA != "-")
colnames(le) = fcle[1,c(2,3,53:70)]
le <- le[-c(1),]

#Land
land <- subset(le, select = c(1,2,3:18))

#Solar Panel 
solar <- subset(le, select = c(1,2,19:20))

############################################################
############################################################
############################################################

##Sample Death vs income graph
colnames(death_st)[1] <- "Area"
colnames(employee_income)[1] <- "Area"
colnames(death_st)[2] <- "Year"
colnames(death_st)[3] <- "StandardisedDeath"
colnames(employee_income)[2] <- "Year"
#Select data who's year is 2013
death_st_13 <- death_st[death_st$Year == "2013",]
#Combine dataframes together
income_death <- merge(death_st_13, employee_income)
income_death <- na.omit(income_death)
colnames(income_death)[4] <- "MedianIncome"
##income_death <- income_death[income_death$StandardisedDeath != "-",]
#Plot death rate vs income on a scatter plot
plot(income_death$MedianIncome, income_death$StandardisedDeath , main="Death per '000 versus Median Income", 
     xlab="Income", ylab="Death Rate per '000")
income_death <- transform(income_death, MedianIncome = as.numeric(MedianIncome))
income_death <- transform(income_death, StandardisedDeath = as.numeric(StandardisedDeath))
a <- with(income_death, mean(StandardisedDeath[MedianIncome > 30000 & MedianIncome < 40000]))
b <- with(income_death, mean(StandardisedDeath[MedianIncome > 40000 & MedianIncome < 50000]))
c <- with(income_death, mean(StandardisedDeath[MedianIncome > 50000 & MedianIncome < 60000]))
death_income <- c(a,b,c)
barplot(death_income, main="Death rate versus Income",
        names.arg=c("30k - 40k", "40k - 50k", "50k to 60k"))
income_death_mean <- aggregate(StandardisedDeath ~  MedianIncome, income_death, mean)
boxplot(StandardisedDeath~MedianIncome, income_death_mean, outline=FALSE)
#with(income_death, mean(StandardisedDeath[MedianIncome > 60000 & MedianIncome < 70000]))
#with(income_death, mean(StandardisedDeath[MedianIncome > 70000 & MedianIncome < 80000]))
income_death_mean$IncRange <- cut(income_death_mean$MedianIncome, breaks=seq(30000, 80000, 5000))
income_death_mean2 <- ddply(income_death_mean, .(IncRange), summarize, mean_dr=mean(StandardisedDeath))

##Education vs death
colnames(edu)[1] <- "Area"
death_st_11 <- death_st[death_st$Year == 2011,]
death_st_11 <- na.omit(death_st_11)
edu_death <- merge(edu, death_st_11, by = "Area")
edu_death[3:10] <- lapply(edu_death[3:10], as.numeric)
colnames(edu_death)[5] <- "Bachelor"
a <- with(edu_death, mean(StandardisedDeath[ Bachelor > 0 & Bachelor < 10]))
b <- with(edu_death, mean(StandardisedDeath[ Bachelor > 10 & Bachelor < 20]))
c <- with(edu_death, mean(StandardisedDeath[ Bachelor > 20 & Bachelor < 30]))
d <- with(edu_death, mean(StandardisedDeath[ Bachelor > 30 & Bachelor < 40]))
death_bach <- c(a,b,c,d)
barplot(death_bach, main="Death rate versus % Bachelor Degree",
        names.arg=c("0 - 10%", "10 - 20%", "20 - 30%", "30 - 40%"))
death_bach_mean <- aggregate(StandardisedDeath ~  Bachelor, edu_death, mean)
boxplot(StandardisedDeath~Bachelor, death_bach_mean, outline=FALSE)

##Population density
colnames(pop_den)[1] <- "Area"
colnames(pop_den)[2] <- "Year"
colnames(pop_den)[3] <- "PopDen"
pop_den_death <- merge(pop_den, death_st_11)
pop_den_death[c(2:4)] <- lapply(pop_den_death[c(2:4)], as.numeric)
pop_den_death <- na.omit(pop_den_death)
plot(pop_den_death$PopDen, pop_den_death$StandardisedDeath)
a <- with(pop_den_death, mean(StandardisedDeath[ PopDen > 0 & PopDen < 100]))
b <- with(pop_den_death, mean(StandardisedDeath[ PopDen > 100 & PopDen < 1000]))
c <- with(pop_den_death, mean(StandardisedDeath[ PopDen > 1000 & PopDen < 2000]))
d <- with(pop_den_death, mean(StandardisedDeath[ PopDen > 2000 & PopDen < 4000]))
e <- with(pop_den_death, mean(StandardisedDeath[ PopDen > 4000 & PopDen < 8000]))
barplot(c(a,b,c,d,e), main="Death rate versus Population Density",
        names.arg=c("0-100", "100-1000", "1000-2000", "2000-4000", "4000-8000"), ylab = "Average death per '000", xlab = "Person per square km")
boxplot(StandardisedDeath~PopDen, pop_den_death, col = "bisque", outline=FALSE)
means <- aggregate(StandardisedDeath ~  PopDen, pop_den_death, mean)
boxplot(StandardisedDeath~PopDen, means)

##Eng Prof
colnames(eng_prof)[c(1,2)] <- c("Area", "Year")
eng_prof_death <- merge(eng_prof, death_st_11)
eng_prof_death <- na.omit(eng_prof_death)
colnames(eng_prof_death)[6] <- "NotProf"
eng_prof_death[c(2:8)] <- lapply(eng_prof_death[c(2:8)], as.numeric)
boxplot(StandardisedDeath~NotProf, eng_prof_death, col = "bisque", outline=FALSE)

##Removing bottom 25% quartile##
#Remove NA data
test <- na.omit(death_st)
#Convert the data to numeric
test <- transform(test, StandardisedDeath = as.numeric(StandardisedDeath))
#Obtain new data frame by removing bottom 25%
test2 <- test[ test$StandardisedDeath > quantile(test$StandardisedDeath , 0.25 ) , ]