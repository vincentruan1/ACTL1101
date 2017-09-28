library(plyr)
library(ggplot2)
library(quantreg)
library(tikzDevice)
theme_set(theme_bw())
options( tikzLatexPackages = c( getOption( "tikzLatexPackages" ), "\\usepackage{libertine}", "\\usepackage{libertinust1math}"))

##Income
colnames(death_st)[c(1, 2, 3)] <- c("Area", "Year", "Std")
colnames(employee_income)[c(1, 2, 3)] <- c("Area", "Year", "MedIncome")
#Select death's in the year 2013
death_st_13 <- death_st[death_st$Year == "2013", ]
#Combine death and income together
income_death <- merge(death_st_13, employee_income)
#Remove NA data
income_death <- na.omit(income_death)
#Convert data to numeric
income_death[c(3:6)] <- lapply(income_death[c(3:6)], as.numeric)
#Plot death rate of each income group
ggplot(income_death, aes(x = MedIncome, y = Std)) + geom_boxplot(aes(group = cut_width(MedIncome, 1000))) + xlim(35000, 55000)
ggplot(income_death, aes(x = MedIncome, y = Std)) + geom_boxplot(aes(group = cut_width(MedIncome, 1000))) + xlim(55000, 70000)

##Education vs death
colnames(edu)[c(1, 2)] <- c("Area", "Year")
death_st_11 <- death_st[death_st$Year == "2011", ]
death_st_11 <- na.omit(death_st_11)
edu_death <- merge(edu, death_st_11)
edu_death[3:9] <- lapply(edu_death[3:9], as.numeric)
colnames(edu_death)[5] <- "Bachelor"
ggplot(death_bach_mean, aes(x = Bachelor, y = Std)) + geom_point() + geom_smooth(method = "lm")

##Population density vs death
colnames(pop_den)[c(1, 2, 3)] <- c("Area", "Year", "PopDen")
pop_den_death <- merge(pop_den, death_st_11)
pop_den_death[c(2:4)] <- lapply(pop_den_death[c(2:4)], as.numeric)

tikz(file = "den_death.tex", width = 3.86, height = 2.38, pointsize = 12)
ggplot(pop_den_death, aes(x = PopDen, y = Std)) + 
       geom_point(na.rm = TRUE, shape = 21) + 
       scale_x_log10() + 
       geom_smooth(method = "lm",
                   colour = "red", 
                   lwd = 0.25) +
       labs(
         title = "Population Density on Death Rate",
         x = "Population Density",
         y = "Standardised Death per 000"
       ) +
       theme(plot.title = element_text(size = rel(0.909), hjust = 0.5), 
             axis.title = element_text(size = rel(0.909)),
             axis.title.y = element_text( vjust = 0.5 ),
             axis.title.x = element_text( hjust = 0.5 ))
dev.off()

##Population density vs Education
pop_den_edu <- merge(pop_den, edu)
pop_den_edu[c(2:9)] <- lapply(pop_den_edu[c(2:9)], as.numeric)
colnames(pop_den_edu)[6] <- c("Bachelor")

tikz(file = "den_edu.tex", width = 2.57, height = 3.21, pointsize = 12)
ggplot(pop_den_edu, aes(x = PopDen, y = Bachelor)) + 
       geom_point(na.rm = TRUE, shape = 21) + 
       scale_x_log10() +
       labs(
         title = "Population Density \n against Education",
         x = "Population Density (per sq km)",
         y = "Bachelor Degree (\\%)"
       ) +
       theme(plot.title = element_text(size = rel(0.909), hjust = 0.5), 
             axis.title = element_text(size = rel(0.909)),
             axis.title.y = element_text( vjust = 0.5 ),
             axis.title.x = element_text( hjust = 0.5 ))
dev.off()

##Income versus population density
inc_pop_den <- merge(pop_den, employee_income)
colnames(employee_income)[c(1, 2, 3)] <- c("Area", "Year", "MedIncome")
inc_pop_den[c(3:6)] <- lapply(inc_pop_den[c(3:6)], as.numeric)

tikz(file = "den_inc.tex", width = 2.71, height = 3.35, pointsize = 12)
ggplot(inc_pop_den, aes(x = PopDen, y = MedIncome)) +
       geom_point(na.rm = TRUE, shape = 21) + 
       scale_x_log10() + 
       scale_y_continuous(labels = scales::comma) +
       labs(
           title = "Population Density \n against Income",
           x = "Population Density (per sq km)",
           y = "Median Income (\\$)"
           ) +
       theme(plot.title = element_text(size = rel(0.909), hjust = 0.5), 
             axis.title = element_text(size = rel(0.909)),
             axis.title.y = element_text( vjust = 0.5 ),
             axis.title.x = element_text( hjust = 0.5 ))
dev.off()

##English proficiency
colnames(eng_prof)[c(1, 2)] <- c("Area", "Year")
eng_prof_death <- merge(eng_prof, death_st_11)
eng_prof_death <- na.omit(eng_prof_death)
colnames(eng_prof_death)[6] <- "NotProf"
eng_prof_death[c(2:8)] <- lapply(eng_prof_death[c(2:8)], as.numeric)
ggplot(eng_prof_death, aes(x = NotProf, y = Std)) + geom_boxplot(aes(group = cut_width(NotProf, 1)))

eng_prof_death$NotProfRange <-
  cut(eng_prof_death$NotProf,
      breaks = seq(0, 34, 2),
      dig.lab = 5)
eng_prof_death_mean <-
  ddply(eng_prof_death, .(NotProfRange), summarize, mean_dr = mean(Std))
boxplot(
  mean_dr ~ NotProfRange,
  eng_prof_death_mean,
  main = "Death rate versus English Proficiency",
  ylab = "Death Rate per '000",
  xlab = "% English Proficiency"
)

##Industry
colnames(bus_ind)[c(1, 2)] <- c("Area", "Year")
bus_ind_death <- merge(bus_ind, death_st)
bus_ind_death <- na.omit(bus_ind_death)
bus_ind_death[c(3:24)] <- lapply(bus_ind_death[c(3:24)], as.numeric)
bus_ind_death$ManuRange <-
  cut(bus_ind_death$Manufacturing,
      breaks = c(2 ^ (2:13)),
      dig.lab = 5)
bus_ind_mean <-
  ddply(bus_ind_death, .(ManuRange), summarize, mean_dr = mean(Std))
boxplot(
  mean_dr ~ ManuRange,
  bus_ind_mean,
  main = "Death rate versus No. of manufacturing",
  xlab = "No. of manufacturers in an area",
  ylab = "Death rate per '000"
)

##Occupation
colnames(ocu)[c(1, 2)] <- c("Area", "Year")
ocu_death <- merge(ocu, death_st_11)
ocu_death <- na.omit(ocu_death)
ocu_death[c(3:12)] <- lapply(ocu_death[c(3:12)], as.numeric)

#Labourers
ocu_death$LabRange <-
  cut(ocu_death$Labourers,
      breaks = seq(1.6, 24.3, 0.4),
      dig.lab = 5)
ocu_death_mean <-
  ddply(ocu_death, .(LabRange), summarize, mean_dr = mean(Std))
boxplot(
  mean_dr ~ LabRange,
  ocu_death_mean,
  main = "Death rate versus % Labourer population",
  xlab = "% Labourer in an area",
  ylab = "Death rate per '000 population"
)

#Professionals
ocu_death$ProRange <-
  cut(ocu_death$Professionals,
      breaks = seq(7.3, 47, 3),
      dig.lab = 5)
ocu_death_pro_mean <-
  ddply(ocu_death, .(ProRange), summarize, mean_dr = mean(Std))
boxplot(
  mean_dr ~ ProRange,
  ocu_death_pro_mean,
  main = "Death rate versus % Professional population",
  xlab = "% Professional in an area",
  ylab = "Death rate per '000 population"
)

#Female / Male
colnames(age_dist_f)[c(1, 2)] <- c("Area", "Year")
colnames(age_dist_m)[c(1, 2)] <- c("Area", "Year")
f_death <- merge(age_dist_f, death_st)
m_death <- merge(age_dist_m, death_st)

#Family size
colnames(fam_size)[c(1, 2)] <- c("Area", "Year")
fam_death <- merge(fam_size, death_st_11)
fam_death <- na.omit(fam_death)
colnames(fam_death)[10] <- "AvgFamSize"
fam_death[c(3:11)] <- lapply(fam_death[c(3:11)], as.numeric)
ggplot(fam_death, aes(x = AvgFamSize, y = Std)) + geom_point() + geom_smooth(method = "lm")

#Internet connection
colnames(int)[c(1, 2)] <- c("Area", "Year")
int_death <- merge(int, death_st_11)
int_death <- na.omit(int_death)
colnames(int_death)[c(3, 6)] <- c("Broadband", "Total")
int_death[c(3:7)] <- lapply(int_death[c(3:7)], as.numeric)
ggplot(int_death, aes(x = Total, y = Std)) + geom_point(aes(group = cut_width(Total, 5))) + geom_smooth(method = "lm")

#Unemployment
colnames(lbf)[c(1, 2)] <- c("Area", "Year")
lbf_death <- merge(lbf, death_st_11)
lbf_death <- na.omit(lbf_death)
lbf_death[c(3:7)] <- lapply(lbf_death[c(3:7)], as.numeric)
colnames(lbf_death)[5] <- "UnemploymentR"
ggplot(lbf_death, aes(x = UnemploymentR, y = Std)) + geom_boxplot(aes(group = cut_width(UnemploymentR, 0.5))) + geom_quantile()

#Income vs Education
employee_income_noyear <- employee_income[, -2]
edu_noyear <- edu[, -2]
inc_edu <- merge(employee_income_noyear, edu_noyear)
inc_edu[c(2:10)] <- lapply(inc_edu[c(2:10)], as.numeric)
inc_edu$TotalEdu <- rowSums(inc_edu[, c(5:9)])
colnames(inc_edu)[2] <- "MedIncome"
inc_edu <- na.omit(inc_edu)
ggplot(inc_edu, aes(x = TotalEdu, y = MedIncome)) + geom_boxplot(aes(group = cut_width(TotalEdu, 5)))

#Percentage of Aboriginal people per area
perc_ind <- subset(pop, select = c(2, 3, 79))
colnames(perc_ind) = perc_ind[1, ]
perc_ind <- perc_ind[-c(1:3), ]