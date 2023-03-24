library(tidyverse)
library(stringi)
library(tibble)
library(dplyr)
library(ggplot2)

#import data from the csv file
df_final <- read.csv("FINAL_DATA_JOBS.csv",stringsAsFactors = FALSE)

#creating a location column which has only 3 categorical values: Hybrid, Remote, Fully Remote
df_final$location <- stri_extract_first_regex(df_final$job_details,"(Remote|Fully remote|No Remote)",case_insensitive=TRUE) %>%
  stri_replace_all(replacement = 'Hybrid',fixed = 'Remote',case_insensitive=FALSE) 

#removing location information as well as seniority information from the job name
df_final$position<- stri_replace_all(df_final$position,replacement = ""
                               ,fixed = c("(REMOTE)","(Remote)","/","Mid","Senior","Mid/Senior","Expert",",","REMOTE","Remote","- 100%","Junior")
                               ,vectorize=FALSE)%>%
  stri_trim(side = "both")

df_final$category<- stri_replace_all(df_final$category,replacement = "/",fixed = ",",vectorize=FALSE)
df_final$seniority<- stri_replace_all(df_final$seniority,replacement = "/",fixed = ",",vectorize=FALSE)

#splitting the salary column into two columns with lower and upper limit
df_final$salary <- stri_split_regex(df_final$salary, " . ", simplify = TRUE)
df_final$salary_lower_limit <- df_final$salary[,1]
df_final$salary_upper_limit <- df_final$salary[,2]

#creating a language column 
df_final$polish_requirement <- ifelse(stri_detect_fixed(df_final$posting_requirements_must, "Polish"), "yes", "no")
percent_polish <-as.numeric((sum(stringr::str_count(df_final$polish_requirement, pattern = "yes"))/nrow(df_final))*100)
percent_polish <-sprintf("%.2f",percent_polish)

#arranging columns in the data frame
df_final <- df_final[, colnames(df_final)[c(2:5,12,9,10,11,7)]]

#cleaning salaries columns and changing to numeric values
df_final$salary_upper_limit<- stri_replace(df_final$salary_upper_limit,replacement = "",fixed = "PLN",vectorize=FALSE)
df_final$salary_lower_limit<- stri_replace(df_final$salary_lower_limit,replacement = "",fixed = "PLN",vectorize=FALSE)
df_final$salary_upper_limit <- stri_replace_all_regex(df_final$salary_upper_limit, pattern = ' ',replacement = '')
df_final$salary_lower_limit <- stri_replace_all_regex(df_final$salary_lower_limit, pattern = ' ',replacement = '')
df_final$salary_lower_limit <- as.numeric(iconv(df_final$salary_lower_limit, 'utf-8', 'ascii', sub=''))
df_final$salary_upper_limit <- as.numeric(iconv(df_final$salary_upper_limit, 'utf-8', 'ascii', sub=''))
df_final$salary_upper_limit <- ifelse(is.na(df_final$salary_upper_limit), df_final$salary_lower_limit, df_final$salary_upper_limit)
df_final$position <- stri_replace_all_regex(df_final$position, pattern = '  ',replacement = '')


#cleaning salaries columns and changing to numeric values
df_final$posting_requirements_must <- stri_replace_all_regex(df_final$posting_requirements_must, "(,)*\\s*\\w+\\s*\\(\\w+\\)", "")%>%
  stri_replace_all(replacement = ",", fixed = ", ",vectorize=FALSE)%>%
  stri_trim(side = "both")%>%
  stri_replace_all_regex(pattern = "\\bPolish\\b.*", replacement = "")%>%
  stri_replace_all_regex(pattern = "\\bEnglish\\b.*", replacement = "")%>%
  stri_replace_last_fixed(pattern = ",", replacement = "")%>%
  stri_replace_all(replacement = "|",fixed = ", ",vectorize=FALSE)%>%
  stri_replace_all(replacement = "|", fixed = "| ",vectorize=FALSE)%>%
  stri_replace_all(replacement = "|", fixed = " |",vectorize=FALSE)%>%
  stri_trim(side = "both")

#separating salaries based on seniority 
junior_rows<- grep("Junior", df_final$seniority)
mid_rows<- grep("Mid", df_final$seniority)
senior_rows<- grep("Senior", df_final$seniority)

avg_lower_salary_junior = round(mean(df_final$salary_lower_limit[junior_rows]),-2)
avg_upper_salary_junior = round(mean(df_final$salary_upper_limit[junior_rows]),-2)

avg_lower_salary_mid = round(mean(df_final$salary_lower_limit[mid_rows]),-2)
avg_upper_salary_mid = round(mean(df_final$salary_upper_limit[mid_rows]),-2)

avg_lower_salary_senior = round(mean(df_final$salary_lower_limit[senior_rows]),-2)
avg_upper_salary_senior = round(mean(df_final$salary_upper_limit[senior_rows]),-2)

avg_lower_salary_total = round(mean(df_final$salary_lower_limit),-2)
avg_upper_salary_total = round(mean(df_final$salary_upper_limit),-2)


######## Analyzing All Skills *************

skills_all <- stri_extract_all(df_final$posting_requirements_must, regex =  "[^|]+")
skills_allNoMiss <- map(skills_all, stri_remove_na)
requirements_all <- data.frame(table(unlist(skills_allNoMiss)))
names(requirements_all) <- c("Must_Skills", "Frequency")
requirements_all <- requirements_all %>%
  arrange(-Frequency)


######## Analyzing Business Intelligence Skills *************

BI_rows<- grep("Business", df_final$category)
BI_requirements <- data.frame(Requirements = df_final$posting_requirements_must[BI_rows])

skills_all_BI <- stri_extract_all(BI_requirements, regex =  "[^|]+")
skills_all_BINoMiss <- map(skills_all_BI, stri_remove_na)
BI_musts <- data.frame(table(unlist(skills_all_BINoMiss)))
names(BI_musts) <- c("BI_Skills", "Frequency")
BI_musts <- BI_musts %>%
  arrange(-Frequency)

######## Analyzing Big Data Skills *************

big_data<- grep("Big Data", df_final$category)
big_data_requirements <- data.frame(Requirements = df_final$posting_requirements_must[big_data])

skills_all_BI <- stri_extract_all(big_data_requirements, regex =  "[^|]+")
skills_all_BINoMiss <- map(skills_all_BI, stri_remove_na)
big_data_musts <- data.frame(table(unlist(skills_all_BINoMiss)))
names(big_data_musts) <- c("Big_Data_Skills", "Frequency")
big_data_musts <- big_data_musts %>%
  arrange(-Frequency)


######## Horizontal Bar Chart of All Skills *************

top_5_requirements_all <- head(requirements_all, 5)
top5_skills_all_plot <- ggplot(data = top_5_requirements_all, mapping = aes(x = reorder(Must_Skills, -Frequency), y = Frequency, fill = Frequency)) +
  geom_col() +
  geom_text(aes(label = Frequency), position = position_dodge(width = 1), vjust = -0.5, hjust = -0.4) +
  scale_fill_viridis_c() +
  coord_flip() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 16),
        text = element_text(size = 12),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold")) +
  ggtitle("Top 5 Skills Required") +
  ylab("Number of Jobs") + xlab("Skills")

######## Horizontal Bar Chart of Business Intelligence Skills *************

top_5_requirements_BI <- head(BI_musts, 5)
top5_skills_BI_plot <-ggplot(data = top_5_requirements_BI, mapping = aes(x = reorder(BI_Skills, -Frequency), y = Frequency, fill = Frequency)) +
  geom_col() +
  geom_text(aes(label = Frequency), position = position_dodge(width = 1), vjust = -0.5, hjust = -0.4) +
  scale_fill_viridis_c() +
  coord_flip() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 16),
        text = element_text(size = 12),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold")) +
  ggtitle("Top 5 Skills Required for BI") +
  ylab("Number of Jobs") + xlab("Skills")


######## Horizontal Bar Chart of Big Data Skills *************

top_5_requirements_big_data <- head(big_data_musts, 5)

top5_skills_bigdata_plot <-ggplot(data = top_5_requirements_big_data, mapping = aes(x = reorder(Big_Data_Skills, -Frequency), y = Frequency, fill = Frequency)) +
  geom_col() +
  geom_text(aes(label = Frequency), position = position_dodge(width = 1), vjust = -0.5, hjust = -0.4) +
  scale_fill_viridis_c() +
  coord_flip() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 16),
        text = element_text(size = 12),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold")) +
  ggtitle("Top 5 Skills Required for Big Data") +
  ylab("Number of Jobs") + xlab("Skills")



######## Box and Whisker plot of salaries of different seniorities *************


df_salaries <- data.frame(Job = c("Junior", "Mid", "Senior"),
                          Lower_Limit = c(avg_lower_salary_junior, avg_lower_salary_mid, avg_lower_salary_senior),
                          Upper_Limit = c(avg_upper_salary_junior, avg_upper_salary_mid, avg_upper_salary_senior))
df_salaries$Mean_Salary <- (df_salaries$Lower_Limit + df_salaries$Upper_Limit) / 2


salary_plot <-ggplot(data = df_salaries, mapping = aes(x = Job, y = Mean_Salary, ymax = Upper_Limit, ymin = Lower_Limit)) +
  geom_errorbar(width = 0.3) +
  geom_point(size = 5) +
  ggtitle("Salary Range based off Seniority") +
  ylab("Salary (PLN)") + xlab("Seniority") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






