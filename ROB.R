# ------------------------------------------------------------------------------
# visualization for risk-of-bias assessments
# ------------------------------------------------------------------------------
# Install & Load
install.packages("devtools")
devtools::install_github("mcguinlu/robvis")
library (robvis)
library (readxl)
library (googlesheets4)
library (dplyr)
library (ggplot2)

# clear workspace
rm (list = ls ())
# ------------------------------------------------------------------------------
# importing data
# Risk_of_Bias_Assessment <- read_excel("~/GitHub/WASH_typhoid/data/ROB.xlsx", 
#     col_names = FALSE)
Risk_of_Bias_Assessment <- read_sheet("https://docs.google.com/spreadsheets/d/1nJcbjqTqW0UW1jPiuuThie1c9kXgyrJpRPUgDDcoN4w/edit#gid=1959447890", 
                                      col_types = "cccccccccccccccccccccc",
                                      col_names = FALSE)

# data cleaning
Risk_of_Bias_Assessment <- Risk_of_Bias_Assessment[-1, c(4, 7, 9, 11, 13, 15, 17, 19, 21)]

names(Risk_of_Bias_Assessment) <- c("Author", "D1","D2", "D3", "D4", "D5", "D6", "D7", "Overall")

Risk_of_Bias_Assessment <- Risk_of_Bias_Assessment %>% 
  filter(Author != "Bruh et al." & Author != "Qamar et al.")

Risk_of_Bias_Assessment <- Risk_of_Bias_Assessment[order(Risk_of_Bias_Assessment$Author),]
# ------------------------------------------------------------------------------
png(file="figures/ROB_traffic_light.png", width=750, height=900, res=120)
rob_traffic_light(Risk_of_Bias_Assessment, tool = "ROBINS-I", psize = 5)
dev.off()

Risk_of_Bias_Assessment_sum <- Risk_of_Bias_Assessment %>% 
  mutate(Weight = 1)

png(file="figures/ROB_summary.png", width=800, height=500, res=120)
rob_summary(Risk_of_Bias_Assessment_sum, tool = "ROBINS-I")
dev.off()
