library(plyr)
library(ggplot2)
library(quantreg)

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
#Plot death rate of each income group
ggplot(income_death_mean, aes(x = MedIncome, y = Std)) + geom_boxplot(aes(group = cut_width(MedIncome, 1000))) + xlim(35000, 55000)
ggplot(income_death_mean, aes(x = MedIncome, y = Std)) + geom_boxplot(aes(group = cut_width(MedIncome, 1000))) + xlim(55000, 70000)

##Education vs death
colnames(edu)[c(1,2)] <- c("Area", "Year")
death_st_11 <- death_st[death_st$Year == "2011",]
death_st_11 <- na.omit(death_st_11)
edu_death <- merge(edu, death_st_11)
edu_death[3:9] <- lapply(edu_death[3:9], as.numeric)
colnames(edu_death)[5] <- "Bachelor"
death_bach_mean <- aggregate(Std ~  Bachelor, edu_death, mean)
boxplot(Std~Bachelor, death_bach_mean, main = "Death Rate versus % Bachelor", xlab = "% Bachelor", ylab = "Death rate per '000", outline=FALSE, ylim = c(0,10))
ggplot(death_bach_mean, aes(x = Bachelor, y = Std)) + geom_point() + geom_smooth(method = "lm")

##Population density
colnames(pop_den)[c(1,2,3)] <- c("Area", "Year", "PopDen")
pop_den_death <- merge(pop_den, death_st_11)
pop_den_death <- na.omit(pop_den_death)
pop_den_death[c(2:4)] <- lapply(pop_den_death[c(2:4)], as.numeric)
ggplot(pop_den_death, aes(x = PopDen, y = Std)) + geom_point() + scale_x_log10() + geom_smooth(method = "lm")

##Income versus population density
inc_pop_den <- merge(pop_den, employee_income)
inc_pop_den[c(3:6)] <- lapply(inc_pop_den[c(3:6)], as.numeric)
ggplot(inc_pop_den, aes(x = PopDen, y = MedIncome)) + geom_

##English proficiency
colnames(eng_prof)[c(1,2)] <- c("Area", "Year")
eng_prof_death <- merge(eng_prof, death_st_11)
eng_prof_death <- na.omit(eng_prof_death)
colnames(eng_prof_death)[6] <- "NotProf"
eng_prof_death[c(2:8)] <- lapply(eng_prof_death[c(2:8)], as.numeric)
ggplot(eng_prof_death, aes(x = NotProf, y = Std)) + geom_boxplot(aes(group = cut_width(NotProf, 1)))

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
boxplot(mean_dr~ManuRange, bus_ind_mean, main = "Death rate versus No. of manufacturing", xlab = "No. of manufacturers in an area", ylab = "Death rate per '000")

##Occupation
colnames(ocu)[c(1,2)] <- c("Area", "Year")
ocu_death <- merge(ocu, death_st_11)
ocu_death <- na.omit(ocu_death)
ocu_death[c(3:12)] <- lapply(ocu_death[c(3:12)], as.numeric)

#Labourers
ocu_death$LabRange <- cut(ocu_death$Labourers, breaks=seq(1.6, 24.3, 0.4), dig.lab = 5)
ocu_death_mean <- ddply(ocu_death, .(LabRange), summarize, mean_dr=mean(Std))
boxplot(mean_dr~LabRange, ocu_death_mean, main = "Death rate versus % Labourer population", xlab = "% Labourer in an area", ylab = "Death rate per '000 population")

#Professionals
ocu_death$ProRange <- cut(ocu_death$Professionals, breaks=seq(7.3, 47, 3), dig.lab = 5)
ocu_death_pro_mean <- ddply(ocu_death, .(ProRange), summarize, mean_dr=mean(Std))
boxplot(mean_dr~ProRange, ocu_death_pro_mean, main = "Death rate versus % Professional population", xlab = "% Professional in an area", ylab = "Death rate per '000 population")

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
ggplot(fam_death, aes(x = AvgFamSize, y = Std)) + geom_point() + geom_smooth(method = "lm")

#Internet connection
colnames(int)[c(1,2)] <- c("Area", "Year")
int_death <- merge(int, death_st_11)
int_death <- na.omit(int_death)
colnames(int_death)[c(3,6)] <- c("Broadband", "Total") 
int_death[c(3:7)] <- lapply(int_death[c(3:7)], as.numeric)
ggplot(int_death, aes(x = Total, y = Std)) + geom_point(aes(group = cut_width(Total, 5))) + geom_smooth(method = "lm")

#Unemployment
colnames(lbf)[c(1,2)] <- c("Area", "Year")
lbf_death <- merge(lbf, death_st_11)
lbf_death <- na.omit(lbf_death)
lbf_death[c(3:7)] <- lapply(lbf_death[c(3:7)], as.numeric)
colnames(lbf_death)[5] <- "UnemploymentR"
ggplot(lbf_death, aes(x = UnemploymentR, y = Std)) + geom_boxplot(aes(group = cut_width(UnemploymentR, 0.5))) + geom_quantile()

lbf_death$UnemployRange <- cut(lbf_death$UnemploymentR, breaks=seq(1.9, 10.9, 1), dig.lab = 5)
lbf_death_mean <- ddply(lbf_death, .(UnemployRange), summarize, mean_dr=mean(Std))
boxplot(mean_dr~UnemployRange, lbf_death_mean, outline = FALSE, main = "Death rate vesus unemployment rate", xlab = "Unemployment rate", ylab = "Death rate per '000 population")

#Income vs Education
employee_income_noyear <- employee_income[,-2]
edu_noyear <- edu[,-2]
inc_edu <- merge(employee_income_noyear, edu_noyear)
inc_edu[c(2:10)] <- lapply(inc_edu[c(2:10)], as.numeric)
inc_edu$TotalEdu <- rowSums(inc_edu[,c(5:9)])
colnames(inc_edu)[2] <- "MedIncome"
inc_edu <- na.omit(inc_edu)
ggplot(inc_edu, aes(x = TotalEdu, y = MedIncome)) + geom_boxplot(aes(group = cut_width(TotalEdu, 5)))

#Percentage of Aboriginal people per area
perc_ind <- subset(pop, select =c(2,3,79))
colnames(perc_ind) = perc_ind[1,]
perc_ind <- perc_ind[-c(1:3),]

#Percentage of Aboriginal people against Standardised Death Rates

#Clean up data
colnames(perc_ind)[c(1,2,3)]<- c("Area","Year","PercInd")
death_st_11<- death_st[death_st$Year == "2011",]
death_st_11<- na.omit(death_st_11)
perc_ind_death<- merge(perc_ind, death_st_11)
perc_ind_death<- na.omit(perc_ind_death)
perc_ind_death[c(3:4)]<- lapply(perc_ind_death[c(3:4)], as.numeric)

#Percentage of Aboriginal people vs Standardised Death rate
graph1<-ggplot(data=perc_ind_death, aes(x=perc_ind_death$PercInd, y=perc_ind_death$Std))+ geom_point(color=alpha("black")) + geom_smooth(method=lm, color=alpha("red")) + labs(title="Percentage of Aboriginal people vs Standardised Death Rates", x="Percentage of Aboriginal people", y="Standardised Death Rate", colour="legend")+ theme_bw()

#Line of best fit (regression line)
reg<-lm(perc_ind_death$PercInd~perc_ind_death$Std)

#Broken code ## PLEASE FIX UP! :)
coeff=coefficients(reg)
coeff(Intercept) perc_ind_death$Std

#Correlation  between Percentage of Aboriginal people and Standardised Death rate
cor(perc_ind_death$PercInd, perc_ind_death$Std)

#Annotation for the gradient of regression line
graph1 + annotate(geom="text", x=30, y=13, label="Gradient of regression line = 2.836927", colour= "blue", size=5) + coord_trans(x="log2", y="log2")

#Remove outlier by limiting axes
graph1 + coord_fixed(xlim=c(0,30), ylim=c(2,12))

#PLEASE ADD YOUR COMMENTS TO THIS!
Population <- read.xlsx("14100DS0002_2017-03.xlsx"), startRow = 9)

Population <- Population[,-c(4:74)]

Population <- Population[,-c(9:21)]

names(Population) <- c("LGA Code", "Council Name", "Year","Fertility Rate", "Deaths", "Standarised Death Rate per 1000", "delete", "Percentage of Aborignals")

Population$delete<- NULL

Population$`Percentage of Aborignals`<- as.numeric(Population$`Percentage of Aborignals`)

Population$`Fertility Rate` <- as.numeric(Population$`Fertility Rate`)

Population$`Deaths` <- as.numeric(Population$`Deaths`)

Population$`Standarised Death Rate per 1000` <- as.numeric(Population$`Standarised Death Rate per 1000`)

Population <- Population[complete.cases(Population),]

plot( log(Population$`Percentage of Aborignals`),Population$`Fertility Rate`, xlab = "Log Of Percentage of Aboriginals", ylab = "Fertility Rate", main = "Fertility Rate vs Aboriginal Percentage Rate")
