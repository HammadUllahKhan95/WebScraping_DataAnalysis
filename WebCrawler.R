library(tidyverse)
library(stringi)
library(rvest)
library(httr)
library(glue)

#definition of my user agent
my_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36"

#total numbr of pages and their links
no_page <- 1:10
url_pages <- glue("https://nofluffjobs.com/big-data?criteria=category%3Dbusiness-intelligence,business-analyst&page={no_page}")

#getting all the job listing links from every page url
df_url_jobs <- data.frame(url_jobs = NULL, position_job = NULL, company_name = NULL, scrape_time = NULL)
for (i in seq_along(url_pages)) {
  tryCatch({
    page_main <- GET(url_pages[i], user_agent(my_agent))
    page_main$status_code
    page_main$request
    page_content <- content(page_main)
    
    #scraping individual job urls
    
    url_jobs <- page_content %>%
      html_elements(xpath = "//div[@class = 'list-container ng-star-inserted']/a") %>%
      html_attr("href")
    
    #scraping the name of the job/role
    
    position_job <- page_content %>%
      html_elements(xpath = "//h3[@data-cy = 'title position on the job offer listing']") %>%
      html_text()
    
    #scraping company that is offering the job
    
    company_name <- page_content %>%
      html_elements(xpath = "//span[@data-cy = 'company name on the job offer listing']") %>%
      html_text()
    
    #today's time 
    
    scrape_time <- format(Sys.time(), "%D %X")
    df_temp <- data.frame(url_jobs,position_job,company_name, scrape_time)
    df_url_jobs <- bind_rows(df_url_jobs, df_temp)
    #print(glue("Iteration: {i} | Time: {scrape_time}"))
    Sys.sleep(2)
  }, error = function(e){
    print(paste0("Error:", e))
    df_temp <- data.frame(matrix(NA, ncol = 4, nrow = 1))
    colnames(df_temp) <- c("url_jobs", "position_job", "company_name", "scrape_time")
    df_url_jobs <- bind_rows(df_url_jobs, df_temp)
  })
}


#editing links to make sure the English language pages are shown
df_url_jobs$url_jobs <- glue("https://nofluffjobs.com{df_url_jobs$url_jobs}?lang=en")

df_main <- data.frame(Salary = NULL,Category = NULL,Seniority = NULL, Posting_requirements = NULL,Job_Details = NULL)
for (i in seq_along(df_url_jobs$url_jobs)) {
  tryCatch({
    page_job <- GET(df_url_jobs$url_jobs[i], user_agent(my_agent))
    # page_main$status_code
    # page_main$request
    
    #fetching the content of the specific job
    job_content <- content(page_job)
    
    #scraping salary
    
    salary <- job_content %>%
      html_element(xpath = "(//div[@class='salary ng-star-inserted'])[1]//h4") %>%
      html_text()
    
    #scraping category of the job BI or Big Data
    
    category <- job_content %>%
      html_elements(xpath = "//div[@class = 'tw-flex flex-wrap ng-star-inserted']") %>%
      html_text()
    
    #scraping seniority of the role
    
    seniority <- job_content %>%
      html_elements(xpath = "//li[@id = 'posting-seniority']")%>%
      html_text()
    
    #scraping job requirements
    
    posting_requirements_must <- job_content %>%
      html_elements(xpath = "//div[@id = 'posting-requirements']/section[1]//li") %>%
      html_text()%>%
      toString(collapse = ",")
  
    #scraping extra job details
    
    job_details <- job_content %>%
      html_elements(xpath = "//section[@id = 'posting-specs']/ul/li") %>%
      html_text()%>%
      toString()
    scrape_time <- format(Sys.time(), "%D %X")
    df_temp2 <- data.frame(salary,category,seniority,posting_requirements_must,job_details)
    df_main <- bind_rows(df_main, df_temp2)
    #print(glue("Iteration: {i} | Time: {scrape_time}"))
    Sys.sleep(2)
  }, error = function(e){
    print(paste0("Error:", e))
    df_temp2 <- data.frame(matrix(NA, ncol = 5, nrow = 1))
    colnames(df_temp2) <- c("salary", "category", "seniority", "posting_requirements_must", "job_details")
    df_main <- bind_rows(df_main, df_temp2)
  })
}

df_main$position <- df_url_jobs$position_job
df_main$company <- df_url_jobs$company_name
df_main <- df_main[, colnames(df_main)[c(7,6,2,3,1,4,5)]]

write.csv(df_main, "FINAL_DATA_JOBS.csv")


