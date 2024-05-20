library(vcd)

data <- as.matrix(read.csv("data.csv", row.names=1))

mosaic(data, gp = gpar(fill = c("lightblue", "pink", "red", "blue")),
       labeling = labeling_values,
       labeling_args = list(
           set_varnames = c(Covid = "COVID", No_Covid = "No COVID"),
           set_labels = list(Covid = rownames(data))))
