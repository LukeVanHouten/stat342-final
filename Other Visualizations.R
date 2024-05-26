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
