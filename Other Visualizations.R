library(vcd)
library(tidyverse)
library(reshape2)

data <- read.csv("data.csv")

data_mosaic <- as.matrix(data[, -1])
rownames(data_mosaic) <- data[, 1]

proportion_df <- data.frame(
    Test = data$Test,
    COVID = c((data$COVID[[1]] / (data$COVID[[1]] + data$COVID[[2]])),
              (data$COVID[[2]] / (data$COVID[[1]] + data$COVID[[2]]))),
    No_COVID = c((data$No_COVID[[1]] / (data$No_COVID[[1]] + 
                                        data$No_COVID[[2]])),
                 (data$No_COVID[[2]] / (data$No_COVID[[1]] + 
                                        data$No_COVID[[2]])))
)

t_data <- as.data.frame(t(data[, 2:3]))
colnames(t_data) <- c("Vaccine", "Placebo")

proportion_df2 <- data.frame(
    Test = rownames(t_data),
    Vaccine = c((t_data$Vaccine[[1]] / (t_data$Vaccine[[1]] +
                                        t_data$Vaccine[[2]])),
                (t_data$Vaccine[[2]] / (t_data$Vaccine[[1]] + 
                                        t_data$Vaccine[[2]]))),
    Placebo = c((t_data$Placebo[[1]] / (t_data$Placebo[[1]] + 
                                        t_data$Placebo[[2]])),
                (t_data$Placebo[[2]] / (t_data$Placebo[[1]] + 
                                        t_data$Placebo[[2]]))))

colnames(proportion_df2) <- c("Diagnosis", "Vaccine", "Placebo")

mosaic(data_mosaic, gp = gpar(fill = c("lightblue", "pink", "red", "blue")),
       labeling = labeling_values)

proportion_df_melted <- melt(proportion_df, id.vars = "Test", 
                      variable.name = "Result", value.name = "Proportion")

proportion_df_2_melted <- melt(proportion_df2, id.vars = "Diagnosis", 
                               variable.name = "Result", value.name = "Proportion")

ggplot(data = proportion_df_2_melted, aes(x = Result, y = Proportion, group = Diagnosis)) +
    geom_boxplot(aes(color = Diagnosis))


data_long <- data %>%
    pivot_longer(cols = c(COVID, No_COVID), names_to = "Status", values_to = "Count")
# 
# # Create the box plot with just two boxes
# ggplot(data_long, aes(x = Test, y = Count, fill = Test)) +
#     geom_boxplot() +
#     labs(title = "COVID and No_COVID Counts by Test Group",
#          x = "Test Group",
#          y = "Count") 

summary <- data_long %>%
    group_by(Test) %>%
    summarise(min = min(Count), lower = quantile(Count, 0.25), 
              median = median(Count), upper = quantile(Count, 0.75),
              max = max(Count))

ggplot(data_long, aes(x = Test, y = Count, fill = Test)) +
    geom_boxplot() +
    geom_text(data = summary, aes(x = Test, y = median, 
                                  label = paste("Median:", median)), 
              vjust = -0.5) +
    geom_text(data = summary, aes(x = Test, y = lower, 
                                  label = paste("Q1:", lower)), 
              vjust = 1.5, hjust = -0.3) +
    geom_text(data = summary, aes(x = Test, y = upper, 
                                  label = paste("Q3:", upper)), 
              vjust = -1, hjust = -0.3) +
    geom_text(data = summary, aes(x = Test, y = min, 
                                  label = paste("Min:", min)), 
              vjust = 1.5) +
    geom_text(data = summary, aes(x = Test, y = max, 
                                  label = paste("Max:", max)), 
              vjust = -1.5) +
    labs(title = "Positive and Negative COVID Test Counts by Test Group",
         x = "Test Group", y = "Number of Tests")
