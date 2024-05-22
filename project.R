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

mosaic(data_mosaic, gp = gpar(fill = c("lightblue", "pink", "red", "blue")),
       labeling = labeling_values)

proportion_df <- melt(proportion_df, id.vars = "Test", 
                      variable.name = "Result", value.name = "Proportion")

ggplot(data = proportion_df, aes(x = Result, y = Proportion, group = Test)) +
    geom_boxplot(aes(color = Test))
