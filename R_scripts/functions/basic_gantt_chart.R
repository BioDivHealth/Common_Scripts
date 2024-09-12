# Install necessary packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyverse)

# Create a dataframe for the Gantt chart with hypothetical months and categories
tasks <- data.frame(
  Task = c("1.1: Data integration and preprocessing", "1.2: Exploratory data analysis", 
           "2.1: Agent and tool development","2.2: LLM fine-tuning",  
           "3: Model testing with live data", 
           "4.1: Multimodal LLM app development", 
           "4.2: User feedback testing and deployment", 
           "5.1: Research paper writing and submission", "5.2: Data stories and sci-comm",
           "6.1: Project documentation and review", "6.2: Datasets and model outputs"),
  Start_Month = c(1,   1, 2, 2.5, 4.5, 7,  8,  9,  9,  11, 2),
  End_Month =   c(2.5, 3, 4, 4.5, 7,   9,  9,  12, 12, 12, 12),
  Category = c("Data Processing", "Data Processing", 
               "Model Evaluation and Fine-Tuning", "Model Evaluation and Fine-Tuning", 
               "Model Evaluation and Fine-Tuning",
               "App Building and Deployment", "App Building and Deployment", 
               "Outreach", "Outreach",
               "Dataset Management", "Dataset Management")
)

tasks$Category <- as.factor(tasks$Category)

# Manually set task order to start at 1.1 and end at 6.2
task_order <- c("1.1: Data integration and preprocessing", "1.2: Exploratory data analysis", 
                "2.1: Agent and tool development","2.2: LLM fine-tuning",  
                "3: Model testing with live data", 
                "4.1: Multimodal LLM app development", 
                "4.2: User feedback testing and deployment", 
                "5.1: Research paper writing and submission", "5.2: Data stories and sci-comm",
                "6.1: Project documentation and review", "6.2: Datasets and model outputs")

tasks$Task <- factor(tasks$Task, levels = task_order)

# 2. -----
# Reorder the categories to ensure they appear correctly in the legend
tasks$Category <- factor(tasks$Category, levels = c("Data Processing", "Model Evaluation and Fine-Tuning", 
                                                    "App Building and Deployment", "Outreach", "Dataset Management"))

tasks$Task <- factor(tasks$Task, levels = rev(unique(tasks$Task)))
# Plot the Gantt chart with categories using geom_tile and reverse the y-axis
(gantt_chart <- ggplot(tasks, aes(x = Start_Month, y = Task, color = Category)) +
    geom_segment(aes(xend = End_Month, yend = Task), size = 10) +  # Thicker lines
    scale_color_manual(values = c(
      "Data Processing" = "lightblue",       
      "Model Evaluation and Fine-Tuning" = "lightgoldenrod",
      "App Building and Deployment" = "lightcoral",    
      "Outreach" = "lightgreen",            
      "Dataset Management" = "#6a6d9a"     
    )) +
    scale_y_reverse() +
    labs(x = "Month", y = "Tasks", color = "Category") +
    scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 50)) +
    theme_linedraw() +
    theme(
      axis.text.y = element_text(angle = 40, hjust = 1, margin = margin(r = 10)),  # Add margin between y-axis labels and axis
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 10),
      legend.box = "vertical" ,
      ) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)))


