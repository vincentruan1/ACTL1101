library("ggplot2")
library("reshape2")
# Extract data for Percentage of Aboriginal people per area from data set
colnames(perc_ind)[c(1, 2, 3)] <- c("Area", "Year", "PercInd")
colnames(death_st)[c(1, 2, 3)] <- c("Area", "Year", "Std")
death_st_11 <- death_st[death_st$Year == "2011",]
perc_ind_death <- merge(perc_ind, death_st_11)
perc_ind_death[c(3:4)] <- lapply(perc_ind_death[c(3:4)], as.numeric)
# Create scatter plot for percentage of Aboriginal people and Standardised death rate
ggplot(na.omit(perc_ind_death),
       aes(x = PercInd, y = Std)) + 
       geom_point() + 
       geom_smooth(method = lm, 
                   color = alpha("blue")) + 
       labs(
         title = "Percentage of Aboriginal people vs Standardised Death Rates",
         x = "Percentage of Aboriginal People in LGA(%)",
         y = "Standardised Death Rate (per '000 person)",
         colour = "legend"
       ) +
       scale_x_log10() + 
       scale_y_log10() + 
       theme_bw()

# Input life expectancy data into new data frame.
# AVOID MANUAL DATA INPUTTING!
male_life <- data.frame(
  Group = c("non_indmale",
            "indmale"),
  '0' = c(79.7, 69.1),
  '1' = c(79.0, 68.7),
  '5' = c(75.1, 64.9),
  '25' = c(55.5, 45.7),
  '50' = c(31.7, 24.5),
  '65' = c(18.6, 13.9),
  '85' = c(4.6, 4.2),
  check.names = FALSE
)

# Melt data together and create barplot.
male_life <- melt(male_life, id.vars = "Group")

ggplot(male_life, 
       aes(variable, value, fill = Group)) +
       geom_bar(stat = "identity", 
                position = "dodge") +
       scale_fill_discrete(name = "Group",
                           labels = c("Indigenous Male", "Non-Indigenous Male")) +
       labs(
         title = "Female Life Expectancy",
         x = "Age (Year)",
         y = "Life Expectancy (Year)"
       )

# Input additional life expectancy data into new data frame.
female_life <- data.frame(
  Group = c("non_indfemale",
            "indfemale"),
  '0' = c(83.1, 73.7),
  '1' = c(82.4, 73.2),
  '5' = c(78.5, 69.3),
  '25' = c(58.7, 49.8),
  '50' = c(34.4, 27.2),
  '65' = c(20.6, 15.8),
  '85' = c(4.8, 4.4),
  check.names = FALSE
)

# Melt data together and create barplot.
female_life <- melt(female_life, id.vars =
                      "Group")

ggplot(female_life, aes(x = variable, y = value, fill = Group)) +
       geom_bar(stat = "identity", 
                position = "dodge") +
       scale_fill_discrete(name = "Group",
                           labels = c("Indigenous Female", "Non-Indigenous Female")) +
       labs(
         title = "Female Life Expectancy",
         x = "Age (Year)",
         y = "Life Expectancy (Year)"
       )