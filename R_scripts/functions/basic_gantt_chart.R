# Install necessary packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyverse)

# Create a dataframe for the Gantt chart with hypothetical months
tasks <- data.frame(
  Task = c("Data integration and preprocessing", "Exploratory data analysis", 
           "LLM fine-tuning", "Spatial optimisation development", 
           "Model testing with live data", "Code finalisation",
           "Multimodal LLM app development", "App testing and deployment", 
           "Research paper writing and submission", "Data stories and sci-comm",
           "Project documentation and review", "Datasets and model outputs"),
  Start_Month = c(1, 1, 3, 3, 5, 5, 7, 7, 9, 9, 11, 11),
  End_Month = c(4, 4, 5, 7, 7, 7, 9, 9, 11, 11, 12, 12)
)

# Plot the Gantt chart
(gantt_chart <- ggplot(tasks, aes(y = reorder(Task, Start_Month), x = Start_Month, xend = End_Month)) +
  geom_segment(aes(yend = Task), color = "dodgerblue", size = 6) +
  labs( x = "Month", y = "Tasks") +
  scale_x_continuous(breaks = 1:12) +
  theme_classic())

# Print the Gantt chart
print(gantt_chart)
