library(plyr)

##Income
colnames(death_st)[c(1,2,3)] <- c("Area", "Year", "Std")
colnames(employee_income)[c(1,2,3)] <- c("Area", "Year", "MedIncome")
#Select death's in the year 2013
death_st_13 <- death_st[death_st$Year == "2013",]
#Combine death and income together
income_death <- merge(death_st_13, employee_income)
#Remove NA data
income_death <- na.omit(income_death)
#Convert data to numeric
income_death[c(3:6)] <- lapply(income_death[c(3:6)], as.numeric)
#Group the data based on income ranges (30000, 35000, 40000, ...)
income_death$IncRange <- cut(income_death$MedIncome, breaks=seq(30000, 80000, 100), dig.lab=5)
#Find the average death rate per income group
income_death_mean <- ddply(income_death, .(IncRange), summarize, mean_dr=mean(Std))
#Plot death rate of each income group
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

##Industry
colnames(bus_ind)[c(1,2)] <- c("Area", "Year")
bus_ind_death <- merge(bus_ind, death_st)
bus_ind_death <- na.omit(bus_ind_death)
bus_ind_death[c(3:24)] <- lapply(bus_ind_death[c(3:24)], as.numeric)
bus_ind_death$ManuRange <- cut(bus_ind_death$Manufacturing, breaks=c(2^(2:13)), dig.lab = 5)
bus_ind_mean <- ddply(bus_ind_death, .(ManuRange), summarize, mean_dr=mean(Std))
boxplot(mean_dr~ManuRange, bus_ind_mean)

##Occupation
colnames(ocu)[c(1,2)] <- c("Area", "Year")
ocu_death <- merge(ocu, death_st_11)
ocu_death <- na.omit(ocu_death)
ocu_death[c(3:12)] <- lapply(ocu_death[c(3:12)], as.numeric)

#Labourers
ocu_death$LabRange <- cut(ocu_death$Labourers, breaks=seq(1.6, 24.3, 0.4), dig.lab = 5)
ocu_death_mean <- ddply(ocu_death, .(LabRange), summarize, mean_dr=mean(Std))
boxplot(mean_dr~LabRange, ocu_death_mean)

#Professionals
ocu_death$ProRange <- cut(ocu_death$Professionals, breaks=seq(7.3, 47, 3), dig.lab = 5)
ocu_death_pro_mean <- ddply(ocu_death, .(ProRange), summarize, mean_dr=mean(Std))
boxplot(mean_dr~ProRange, ocu_death_pro_mean)

#Female / Male
colnames(age_dist_f)[c(1,2)] <- c("Area", "Year")
colnames(age_dist_m)[c(1,2)] <- c("Area", "Year")
f_death <- merge(age_dist_f, death_st)
m_death <- merge(age_dist_m, death_st)

#Family size
colnames(fam_size)[c(1,2)] <- c("Area", "Year")
fam_death <- merge(fam_size, death_st_11)
fam_death <- na.omit(fam_death)
colnames(fam_death)[10] <- "AvgFamSize"
fam_death[c(3:11)] <- lapply(fam_death[c(3:11)], as.numeric)
boxplot(Std~AvgFamSize, fam_death, outline = FALSE)

#Internet connection
colnames(int)[c(1,2)] <- c("Area", "Year")
int_death <- merge(int, death_st_11)
int_death <- na.omit(int_death)
colnames(int_death)[c(3,6)] <- c("Broadband", "Total") 
int_death[c(3:7)] <- lapply(int_death[c(3:7)], as.numeric)
boxplot(Std~Broadband, int_death, outline = FALSE)
boxplot(Std~Total, int_death, outline = FALSE)

#Unemployment
colnames(lbf)[c(1,2)] <- c("Area", "Year")
lbf_death <- merge(lbf, death_st_11)
lbf_death <- na.omit(lbf_death)
lbf_death[c(3:7)] <- lapply(lbf_death[c(3:7)], as.numeric)
colnames(lbf_death)[5] <- "UnemploymentR"
lbf_death$UnemployRange <- cut(lbf_death$UnemploymentR, breaks=seq(1.9, 10.9, 1), dig.lab = 5)
lbf_death_mean <- ddply(lbf_death, .(UnemployRange), summarize, mean_dr=mean(Std))
boxplot(mean_dr~UnemployRange, lbf_death_mean, outline = FALSE)