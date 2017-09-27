library(ggplot2)
library(grid)
library(reshape2)

colnames(edu)[c(1, 2)] <- c("Area", "Year")
colnames(ocu)[c(1, 2)] <- c("Area", "Year")
edu_ocu <- merge(edu, ocu)
colnames(edu_ocu)[c(5,10)] <- c("Bachelor", "Professional")
edu_ocu[c(3:17)] <- lapply(edu_ocu[c(3:17)], as.numeric)

bach_pro_cor <- cor(edu_ocu$Bachelor, edu_ocu$Professional)

#Plot Bachelor Vs Professionals
ggplot(edu_ocu, aes(Professional, Bachelor)) + 
       geom_point() +
       geom_smooth(method = "lm") + 
       labs(
         y = "Percentage of Professional",
         x = "Percentage of Bachelor",
         title = "Proportional of Bachelor versus Professionals"
       ) +
       annotate(geom = "text",
                x = 10,
                y = 35,
                label = bach_pro_cor
       ) +
       theme_bw()

bach_lab_cor <- cor(edu_ocu$Bachelor, edu_ocu$Labourers)

#Plot Bachelor Vs Labourers
ggplot(edu_ocu, aes(Labourers, Bachelor)) + 
       geom_point() +
       geom_smooth(method = "loess") + 
       labs(
         y = "Percentage of Labourers",
         x = "Percentage of Bachelor",
         title = "Proportional of Bachelor versus Professionals"
       ) +
       annotate(geom = "text",
                x = 10,
                y = 35,
                label = bach_lab_cor
       ) + 
       theme_bw()

#analysis
#What is being analysed here?
#From your word doc, a box plot would be better here
#Avoid manual input of data
eduocu <- matrix(c(13, 18, 9, 55, 46, 8, 3, 3, 6, 8, 4, 3, 10, 2, 9, 3),
                 nrow = 2,
                 ncol = 8)
rownames(eduocu) <- c("VET", "University Level")
colnames(eduocu) <-
  c(
    "Managers",
    "Professionals",
    "Technicians and Trade Workers",
    "Community and Personal Service Workers",
    "Clerical and Administrative Workers",
    "Sales Workers",
    "Machinery Operators and Drivers",
    "Labourers"
  )
edu_ocu_analysis <- melt(eduocu)
ggplot(data = edu_ocu_analysis, aes(x = Var1, y = value, fill = Var2)) +
  geom_bar(stat = "identity") + geom_text(aes(label = value), position =
                                            position_stack(vjust = 0.5)) + labs(title = "Education Vs Occupation",
                                                                                fill = "Occupation",
                                                                                x = "",
                                                                                y = "%") + coord_flip() + theme(legend.position = "bottom")
