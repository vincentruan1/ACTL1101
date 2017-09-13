##Income
colnames(death_st)[c(1,2,3)] <- c("Area", "Year", "Std")
colnames(employee_income)[c(1,2,3)] <- c("Area", "Year", "MedIncome")
#Select data who's year is 2013
death_st_13 <- death_st[death_st$Year == "2013",]
#Combine dataframes together
income_death <- merge(death_st_13, employee_income)
#Remove NA data
income_death <- na.omit(income_death)
#Convert data to numeric
income_death[c(3:6)] <- lapply(income_death[c(3:6)], as.numeric)
#Group the data based on income ranges (30000, 35000, 40000, ...)
income_death$IncRange <- cut(income_death$MedIncome, breaks=seq(30000, 80000, 5000), dig.lab=5)
#Average the death rate per income group
income_death_mean <- ddply(income_death, .(IncRange), summarize, mean_dr=mean(Std))
#Plot death rate per income group
boxplot(mean_dr~IncRange,income_death_mean)

##Education vs death
colnames(edu)[c(1,2)] <- c("Area", "Year")
death_st_11 <- death_st[death_st$Year == "2011",]
death_st_11 <- na.omit(death_st_11)
edu_death <- merge(edu, death_st_11)
edu_death[3:9] <- lapply(edu_death[3:9], as.numeric)
colnames(edu_death)[5] <- "Bachelor"
death_bach_mean <- aggregate(Std ~  Bachelor, edu_death, mean)
boxplot(Std~Bachelor, death_bach_mean, outline=FALSE)

##Population density
colnames(pop_den)[c(1,2,3)] <- c("Area", "Year", "PopDen")
pop_den_death <- merge(pop_den, death_st_11)
pop_den_death <- na.omit(pop_den_death)
pop_den_death[c(2:4)] <- lapply(pop_den_death[c(2:4)], as.numeric)
pop_den_death$PopDenRange <- cut(pop_den_death$PopDen, breaks=c(0,2^(0:14)), dig.lab = 5)
pop_den_death_mean <- ddply(pop_den_death, .(PopDenRange), summarize, mean_dr=mean(Std))
boxplot(mean_dr~PopDenRange, pop_den_death_mean, main = "Death rate versus Population Density", ylab = "Death Rate per '000", xlab = "Population Density per sqkm")

##English proficiency
colnames(eng_prof)[c(1,2)] <- c("Area", "Year")
eng_prof_death <- merge(eng_prof, death_st_11)
eng_prof_death <- na.omit(eng_prof_death)
colnames(eng_prof_death)[6] <- "NotProf"
eng_prof_death[c(2:8)] <- lapply(eng_prof_death[c(2:8)], as.numeric)
eng_prof_death$NotProfRange <- cut(eng_prof_death$NotProf, breaks=seq(0, 34, 2), dig.lab = 5)
eng_prof_death_mean <- ddply(eng_prof_death, .(NotProfRange), summarize, mean_dr=mean(Std))
boxplot(mean_dr~NotProfRange, eng_prof_death_mean, main = "Death rate versus English Proficiency", ylab = "Death Rate per '000", xlab = "% English Proficiency")