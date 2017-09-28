library(ggplot2)
library(gridExtra)
library(reshape2)
library(plyr)
library(tikzDevice)
theme_set(theme_bw())
options( tikzLatexPackages = c( getOption( "tikzLatexPackages" ), "\\usepackage{libertine}", "\\usepackage{libertinust1math}"))

# Merge income and occupation data frames together.
colnames(total_income)[1] <- "Area"
colnames(ocu)[1] <- "Area"
income_occupation <- merge(total_income, ocu, by = "Area")
income_occupation[c(2:15)] <- lapply(income_occupation[c(2:15)], as.numeric)

# Plot median total income against occupations. Also calculate covariance and correlation

# 1. Professionals
tikz(file = "pro_inc.tex", width = 2.71, height = 3.35, pointsize = 12)
professionals_plot <- ggplot(income_occupation, aes(Professionals, `Median Total income (excl. Government pensions)`)) + 
                             geom_point(shape = 21) + 
                             labs(
                             title = "Median Income versus \n Percentage Professionals",
                             x = "Percentage Professional \n in an LGA (\\%)",
                             y = "Median Income (\\$)"
                             ) +
                             geom_smooth(method = "lm", colour = "red", lwd = 0.25) + 
                             scale_y_continuous(labels = scales::comma) + 
                             theme(plot.title = element_text(size = rel(0.909), hjust = 0.5), 
                                   axis.title = element_text(size = rel(0.909)),
                                   axis.title.y = element_text( vjust = 0.5 ),
                                   axis.title.x = element_text( hjust = 0.5 ))
plot(professionals_plot)
dev.off()

pro_inc_cov <- cov(
  income_occupation$Professionals,
  income_occupation$`Median Total income (excl. Government pensions)`
  )
pro_inc_cor <- cor(
  income_occupation$Professionals,
  income_occupation$`Median Total income (excl. Government pensions)`
  )

# 2. Labourers
tikz(file = "lab_inc.tex", width = 2.71, height = 3.35, pointsize = 12)
labourers_plot <- ggplot(income_occupation, aes(x = Labourers, y = `Median Total income (excl. Government pensions)`)) + 
                         geom_point(shape = 21) + 
                         labs(
                           title = "Median Income versus \n Percentage Labourers",
                           x = "Percentage Labourers \n in an LGA (\\%)",
                           y = "Median Income (\\$)"
                         ) +
                         geom_smooth(method = "lm", colour = "red", lwd = 0.25) + 
                         scale_y_continuous(labels = scales::comma) + 
                         theme(plot.title = element_text(size = rel(0.909), hjust = 0.5), 
                               axis.title = element_text(size = rel(0.909)),
                               axis.title.y = element_text( vjust = 0.5 ),
                               axis.title.x = element_text( hjust = 0.5 ))
plot(labourers_plot)
dev.off()

lab_inc_cov <- cov(
  income_occupation$Labourers,
  income_occupation$`Median Total income (excl. Government pensions)`
)

lab_inc_cor <- cor(
  income_occupation$Labourers,
  income_occupation$`Median Total income (excl. Government pensions)`
)

# Display plots side by side.
grid.arrange(professionals_plot, labourers_plot, ncol = 2)

# Create new data frame with only 2 LGAs for comparison: one with high median total income
# (Peppermint Grove) and one with low median total income (Cherbourg).
occupation_comparison <-income_occupation[c(which(grepl("Peppermint Grove",income_occupation$Area)), 
                                            which(grepl("Cherbourg", income_occupation$Area))),]

# Remove unnecessary data and transpose.
rownames(occupation_comparison)[c(1, 2)] <- c("Peppermint Grove", "Cherbourg")
occupation_comparison <- occupation_comparison[, c(7:15)]
occupation_comparison <- t(occupation_comparison)
occupation_comparison <- cbind(Occupation = rownames(occupation_comparison),occupation_comparison)
occupation_comparison <- as.data.frame(occupation_comparison)
occupation_comparison[, c(2, 3)] <- lapply(occupation_comparison[, c(2, 3)], 
                                           function(x) as.numeric(as.character(x)))

# Compare occupations for these LGAs by creating pie charts.
occupation_comparison <- melt(occupation_comparison, id.vars = "Occupation")

tikz(file = "ocu_pie.tex", width = 4.85, height = 4, pointsize = 12)
ggplot(occupation_comparison, aes(x = "", y = value, fill = Occupation)) + 
       geom_bar(stat = "identity") + 
       coord_polar(theta = "y") + 
       facet_wrap( ~ variable) +
       ggtitle("Distribution of Occupation") + 
       xlab("") + 
       ylab("") +
       scale_fill_brewer(name = "",
                         guide = guide_legend(nrow = 5)) + 
       theme(plot.title = element_text(size = rel(0.909), hjust = 0.5), 
             axis.title = element_text(size = rel(0.909)),
             axis.title.y = element_text( vjust = 0.5 ),
             axis.title.x = element_text( hjust = 0.5 ),
             legend.position = "bottom")
dev.off()

# Import additional ATO income data.
ato_income <-read.xlsx("taxstats2014individual14occupationgendertaxableincome.xlsx",
                      sheet = "Individual Table 14B",
                      startRow = 3
                      )
ato_income[ato_income == "na"] <- NA
ato_income <- na.omit(ato_income)
ato_income <- ato_income[-which(grepl("Not listed", ato_income$Occupation1)),]
ato_income[, c(3, 4)] <- lapply(ato_income[, c(3, 4)], function(x) as.numeric(as.character(x)))

# Create new column with total income and then combine male and female rows to get average
# income per job.
ato_income <- cbind(ato_income,
                    `Total Income` = ato_income$Individuals.no. * ato_income$`Average.taxable.income.$`
                    )

ato_income <- ddply(ato_income, "Occupation1", numcolwise(sum))
ato_income <- cbind(ato_income,
                    `Average Income` = ato_income$`Total Income` / ato_income$Individuals.no.
                    )

# Select top 10 and bottom 10 jobs. Also, plot job against income for these.
ato_income <- ato_income[order(ato_income$`Average Income`), c(1, 5)]
income_comparison <- rbind(head(ato_income, n = 10), tail(ato_income, n = 10))
income_comparison[, 1] <- substring(income_comparison[, 1], 7)
income_comparison[3, 1] <- "Fast food cook"
income_comparison[8, 1] <- "Waiter; Waitress"
income_comparison[5, 1] <- "Crossing attendant"
options(scipen = 1)

tikz(file = "inc_comp.tex", width = 4.85, height = 3, pointsize = 12)
ggplot(income_comparison, 
       aes(x = reorder(Occupation1, `Average Income`), 
       y = `Average Income`), 
       fill = Occupation1) + 
       geom_bar(stat = "identity") + 
       coord_flip() +
       labs(
         title = "Top 10 versus Bottom 10 Incomes",
         x = "Occupation",
         y = "Average Income (\\$)"
       ) +
       scale_y_continuous(labels = scales::comma) + 
       theme(plot.title = element_text(size = rel(0.909), hjust = 0.5), 
             axis.title = element_text(size = rel(0.909)),
             axis.title.y = element_text( vjust = 0.5 ),
             axis.title.x = element_text( hjust = 0.5 ))
dev.off()

# Create plot of Professionals against Labourers. Also calculate covariance and correlation.

tikz(file = "pro_lab.tex", width = 4.85, height = 3, pointsize = 12)
ggplot(income_occupation, aes(x = Professionals, y = Labourers)) + 
       geom_point(shape = 21) +
       ylim(1, 36) + 
       labs(
         title = "Labourers and Professional Joint Distribution",
         x = "Persons Employed as Professionals (\\%)",
         y = "Persons Employed as Labourers (\\%)"
       ) +
       geom_smooth(method = "lm", colour = "red", lwd = 0.25) + 
       theme(plot.title = element_text(size = rel(0.909), hjust = 0.5), 
             axis.title = element_text(size = rel(0.909)),
             axis.title.y = element_text( vjust = 0.5 ),
             axis.title.x = element_text( hjust = 0.5 ))
dev.off()

cov(income_occupation$Professionals, income_occupation$Labourers)
cor(income_occupation$Professionals, income_occupation$Labourers)