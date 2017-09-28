library(ggplot2)
library(grid)
library(reshape2)
theme_set(theme_bw())
options( tikzLatexPackages = c( getOption( "tikzLatexPackages" ), "\\usepackage{libertine}", "\\usepackage{libertinust1math}"))

colnames(edu)[c(1, 2)] <- c("Area", "Year")
colnames(ocu)[c(1, 2)] <- c("Area", "Year")
edu_ocu <- merge(edu, ocu)
colnames(edu_ocu)[c(5,10)] <- c("Bachelor", "Professional")
edu_ocu[c(3:17)] <- lapply(edu_ocu[c(3:17)], as.numeric)

bach_pro_cor <- cor(edu_ocu$Bachelor, edu_ocu$Professional)

#Plot Bachelor Vs Professionals
tikz(file = "bach_pro.tex", width = 4.85, height = 3, pointsize = 12)
ggplot(edu_ocu, aes(Professional, Bachelor)) + 
       geom_point(shape = 1) +
       geom_smooth(method = "lm", colour = "red", lwd = 0.25) + 
       labs(
         y = "Percentage of Professional",
         x = "Percentage of Bachelor",
         title = "Joint Distribution of Bachelor and Professional"
       ) +
       theme(plot.title = element_text(size = rel(0.909), hjust = 0.5), 
             axis.title = element_text(size = rel(0.909)),
             axis.title.y = element_text( vjust = 0.5 ),
             axis.title.x = element_text( hjust = 0.5 ))
dev.off()

bach_lab_cor <- cor(edu_ocu$Bachelor, edu_ocu$Labourers)

#Plot Bachelor Vs Labourers
tikz(file = "bach_lab.tex", width = 4.85, height = 3, pointsize = 12)
ggplot(edu_ocu, aes(Labourers, Bachelor)) + 
       geom_point(shape = 21) +
       geom_smooth(method = "loess", colour = "red", lwd = 0.25) + 
       labs(
         y = "Percentage of Labourers",
         x = "Percentage of Bachelor",
         title = "Joint Distribution of Labourers and Bachelor"
       ) +
       theme(plot.title = element_text(size = rel(0.909), hjust = 0.5), 
             axis.title = element_text(size = rel(0.909)),
             axis.title.y = element_text( vjust = 0.5 ),
             axis.title.x = element_text( hjust = 0.5 ))
dev.off()

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
