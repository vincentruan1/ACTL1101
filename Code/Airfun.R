library("ggplot2")
colnames(death_st)[c(1, 2, 3)] <- c("Area", "Year", "Std")
death_st_11 <- death_st[death_st$Year == "2011",]
#fix seifa table column names for  merging
names(seifa) <-
  c(
    "Area",
    "Year",
    "Advantage & Disadvantage",
    "Disadvantage",
    "Economic Resources",
    "Education & Occupation"
  )

# Create Seifa table with Death rates
seifa_death <- merge(seifa, death_st_11, by = "Area")
seifa_death <- seifa_death[, -c(7)]

# Convert Data types to Numeric
seifa_death[c(3:7)] <- lapply(seifa_death[c(3:7)], as.numeric)

#Leoss Seifa Disadvantage against death rates and Medians for each SEIFA decile & death rate
ggplot(na.omit(seifa_death), aes( Disadvantage, Std)) +
       geom_boxplot(aes(group = cut_width(Disadvantage, 0.5))) +
       geom_smooth(method = "loess") +
       labs(
         title = "Disadvantage Decile versus Standardised Death Rates", 
         x = "Disadvantage Decile",
         y = "Standardised Death Rate ('000 person)"
       )

#Create Table including Percentage of Aboriginals per Area
perc_ind <- na.omit(perc_ind)
colnames(perc_ind)[c(1, 2, 3)] <- c("Area", "Year", "PercInd")
seifa_ab <- merge(seifa, perc_ind)
seifa_ab[c(3:7)] <- lapply(seifa_ab[c(3:7)], as.numeric)

#Leoss Graph of Aboriginals and Seifa with Medians for Each SEIFA decile
ggplot(na.omit(seifa_ab), aes(Disadvantage, PercInd)) +
       geom_point() +
       labs(
            title = "Disadvantage Decile and Aboriginal Percentages", 
            x = "Disadvantage Decile", 
            y = "Percentage of Aborginals"
            )


#FIX UP!
cor(Seifa_Aboriginals$Disadvantage, Seifa_Aboriginals$Std.x)
cor(Seifa_Aboriginals$Disadvantage, Seifa_Aboriginals$PercInd)
cov(Seifa_Aboriginals$Disadvantage, Seifa_Aboriginals$Std.x)
cov(Seifa_Aboriginals$Disadvantage, Seifa_Aboriginals$PercInd)
cor.test(Seifa_Aboriginals$Disadvantage, Seifa_Aboriginals$Std.x)
cor.test(Seifa_Aboriginals$Disadvantage,Seifa_Aboriginals$PercInd)