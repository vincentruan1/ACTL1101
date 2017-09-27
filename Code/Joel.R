library(ggplot2)

# Merge income and occupation data frames together.
colnames(total_income)[1] <- "Area"
colnames(ocu)[1] <- "Area"
income_occupation <- merge(x = total_income, y = ocu, by = "Area")
income_occupation[c(2:15)] <- lapply(income_occupation[c(2:15)], as.numeric)

# Plot median total income against occupations. Also calculate covariance and correlation

# 1. Professionals
professionals_plot <-
  ggplot(
    income_occupation,
    aes(x = Professionals, y =
          `Median Total income (excl. Government pensions)`)
  ) + geom_point(size = 3) + ggtitle("Professionals") + xlab("Persons Employed (%)") + ylab("Median Total Income ($)") +
  geom_smooth(method = "loess") + scale_y_continuous(labels = scales::comma) + theme(plot.title = element_text(hjust = 0.5))
cov(
  income_occupation$Professionals,
  income_occupation$`Median Total income (excl. Government pensions)`
)
cor(
  income_occupation$Professionals,
  income_occupation$`Median Total income (excl. Government pensions)`
)

# 2. Labourers
labourers_plot <- ggplot(
  income_occupation,
  aes(x = Labourers, y =
        `Median Total income (excl. Government pensions)`)
) + geom_point(size = 3) + ggtitle("Labourers") + xlab("Persons Employed (%)") + ylab("Median Total Income ($)") +
  geom_smooth(method = "loess") + scale_y_continuous(labels = scales::comma) + theme(plot.title = element_text(hjust = 0.5))
cov(
  income_occupation$Labourers,
  income_occupation$`Median Total income (excl. Government pensions)`
)
# [1] -25512.96
cor(
  income_occupation$Labourers,
  income_occupation$`Median Total income (excl. Government pensions)`
)
# [1] -0.5259412

# Display plots side by side.
library(gridExtra)
grid.arrange(professionals_plot, labourers_plot, ncol = 2)

# Create new data frame with only 2 LGAs for comparison: one with high median total income
# (Peppermint Grove) and one with low median total income (Cherbourg).
occupation_comparison <-
  income_occupation[c(which(grepl(
    "Peppermint Grove",
    income_occupation$Area
  )), which(grepl("Cherbourg", income_occupation$Area))),]

# Remove unnecessary data and transpose.
rownames(occupation_comparison)[c(1, 2)] <-
  c("Peppermint Grove", "Cherbourg")
occupation_comparison <- occupation_comparison[, c(7:15)]
occupation_comparison <- t(occupation_comparison)
occupation_comparison <-
  cbind(Occupation = rownames(occupation_comparison),
        occupation_comparison)
occupation_comparison <- as.data.frame(occupation_comparison)
occupation_comparison[, c(2, 3)] <-
  lapply(occupation_comparison[, c(2, 3)], function(x)
    as.numeric(as.character(x)))

# Compare occupations for these LGAs by creating pie charts.
library(reshape2)
occupation_comparison <-
  melt(occupation_comparison, id.vars = "Occupation")
ggplot(occupation_comparison, aes(x = "", y = value, fill = Occupation)) + geom_bar(stat =
                                                                                      "identity") + coord_polar(theta = "y") + facet_wrap( ~ variable) +
  ggtitle("Percentage of Persons Employed by Occupation") + xlab("") + ylab("") +
  scale_fill_hue(c = 85) + theme(plot.title = element_text(hjust = 0.5))

# Import additional ATO income data.
ato_income <-
  read.xlsx(
    "taxstats2014individual14occupationgendertaxableincome.xlsx",
    sheet = "Individual Table 14B",
    startRow = 3
  )
ato_income[ato_income == "na"] <- NA
ato_income <- na.omit(ato_income)
ato_income <-
  ato_income[-which(grepl("Not listed", ato_income$Occupation1)),]
ato_income[, c(3, 4)] <-
  lapply(ato_income[, c(3, 4)], function(x)
    as.numeric(as.character(x)))

# Create new column with total income and then combine male and female rows to get average
# income per job.
ato_income <-
  cbind(
    ato_income,
    `Total Income` = ato_income$Individuals.no. *
      ato_income$`Average.taxable.income.$`
  )
library(plyr)
ato_income <- ddply(ato_income, "Occupation1", numcolwise(sum))
ato_income <-
  cbind(ato_income,
        `Average Income` = ato_income$`Total Income` / ato_income$Individuals.no.)

# Select top 10 and bottom 10 jobs. Also, plot job against income for these.
ato_income <- ato_income[order(ato_income$`Average Income`), c(1, 5)]
income_comparison <-
  rbind(head(ato_income, n = 10), tail(ato_income, n = 10))
income_comparison[, 1] <- substring(income_comparison[, 1], 7)
income_comparison[3, 1] <- "Fast food cook"
income_comparison[8, 1] <- "Waiter; Waitress"
income_comparison[5, 1] <- "Crossing attendant"
options(scipen = 1)
ggplot(income_comparison, aes(x = reorder(Occupation1, `Average Income`), y =
                                `Average Income`), fill = Occupation1) + geom_bar(stat = "identity") + coord_flip() +
  ggtitle("Comparison of Incomes") + xlab("Occupation") + ylab("Average Income ($)") +
  scale_y_continuous(labels = scales::comma)  + theme(plot.title = element_text(hjust =
                                                                                  0.5))

# Create plot of Professionals against Labourers. Also calculate covariance and correlation.
ggplot(income_occupation, aes(x = Professionals, y = Labourers)) + geom_point(size = 3) +
  ylim(1, 36) + ggtitle("Labourers and Professionals") + xlab("Persons Employed as Professionals (%)") + ylab("Persons Employed as Labourers (%)") +
  geom_smooth(method = "lm") + theme(plot.title = element_text(hjust = 0.5))

cov(income_occupation$Professionals,
    income_occupation$Labourers)
cor(income_occupation$Professionals,
    income_occupation$Labourers)