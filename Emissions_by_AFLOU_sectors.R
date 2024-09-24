library(readxl)
library(dplyr)
library(tidyr)

folder_path <- "C:/Users/Mani/Documents/Task2/Middle_income_2016"

file_list <- list.files(path = folder_path, pattern = "*.xlsx", full.names = TRUE)

# for LULC ---------------------------
results <- data.frame()

process_file <- function(file_path) {
  
  country_code <- substr(basename(file_path), 1, 3)
  
  data <- read_excel(file_path, sheet = "Data_by_sector")
  
  filtered_data <- data %>% 
    filter(`GHG emissions, Gg CO2 equivalent` == "5bis. Land-Use Change and Forestry") %>%
    mutate(`1994` = as.numeric(as.character(`1994`)),
           `2016` = as.numeric(as.character(`2016`))) %>% 
    select(`GHG emissions, Gg CO2 equivalent`, `1994`, `2016`) %>% 
    drop_na() %>% 
    mutate(change = ((`2016` - `1994`) / `1994`) * 100,
           Country_Code = country_code)
  
  return(filtered_data)
}

for (file in file_list) {
  file_data <- process_file(file)
  results <- bind_rows(results, file_data)
}

summary_row <- results %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE),
            across(where(is.character), ~"Total"))

results <- bind_rows(results, summary_row)


# results <- results %>%
#   bind_rows(summarise(across(where(is.numeric), sum),
#                       across(where(is.character), ~"Total")))

write.csv(results, "C:/Users/Mani/Documents/Task2/Results/results_LULUCF_middle_income.csv", row.names = FALSE)


# FOR Enteric Fermentation and Manure management ------------------------
results <- data.frame()
process_file <- function(file_path) {
  
  country_code <- substr(basename(file_path), 1, 3)
  
  data <- read_excel(file_path, sheet = "Data_by_sector")
  
  # data <- data[29:30,]
  
  
  data <- data %>%
    filter(`GHG emissions, Gg CO2 equivalent` %in% c("4.A. Enteric Fermentation", "4.B. Manure Management")) %>%  
    mutate(`1994` = as.numeric(as.character(`1994`)),
           `2016` = as.numeric(as.character(`2016`))) %>% 
    select(`GHG emissions, Gg CO2 equivalent`, `1994`, `2016`) %>% 
    drop_na() %>% 
    mutate(change = ((`2016`-`1994`)/`1994`)*100)
  
  data<- data %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"Enteric_ferm_and_Manure_mgm")))%>% drop_na()
  
  data <- data[3,] %>% 
    mutate(Country_Code = country_code)%>% drop_na()
  
  
  return(data)
}

for (file in file_list) {
  file_data <- process_file(file)
  results <- bind_rows(results, file_data)
}

results<- results %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))

write.csv(results, "C:/Users/Mani/Documents/Task2/Results/results_Entric_manure_middle_income.csv", row.names = FALSE)

# for Rice Cultivation -------------------------------
results <- data.frame()
process_file <- function(file_path) {
  
  country_code <- substr(basename(file_path), 1, 3)
  
  data <- read_excel(file_path, sheet = "Data_by_sector")
  
  # data <- data[31,]
  
  filtered_data <- data %>%
    filter(`GHG emissions, Gg CO2 equivalent` == "4.C. Rice Cultivation") %>% 
    mutate(`1994` = as.numeric(as.character(`1994`)),
           `2016` = as.numeric(as.character(`2016`))) %>% 
    select(`GHG emissions, Gg CO2 equivalent`, `1994`, `2016`) %>%
    drop_na() %>% 
    mutate(change = ((`2016`-`1994`)/`1994`)*100) %>%
    mutate(Country_Code = country_code)
  
  
  return(filtered_data)
}

for (file in file_list) {
  file_data <- process_file(file)
  results <- bind_rows(results, file_data)
}

results<- results %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))

write.csv(results, "C:/Users/Mani/Documents/Task2/Results/results_Rice_cultivation_middle_income.csv", row.names = FALSE)

# -----------------------------

df<- read_excel("C:/Users/Mani/Documents/Task2/Middle_income_2016/BRA_ghg_profile (1).xlsx", sheet = "Data_by_sector")
df <- df[29:30,]
df <- df %>% 
filter(`GHG emissions, Gg CO2 equivalent` %in% c("4.A. Enteric Fermentation", "4.B. Manure Management")) %>%  
mutate(`1994` = as.numeric(as.character(`1994`)),
       `2016` = as.numeric(as.character(`2016`))) %>% 
  select(`GHG emissions, Gg CO2 equivalent`, `1994`, `2016`) %>% 
  drop_na() %>% 
  mutate(change = ((`2016`-`1994`)/`1994`)*100)

  df<- df %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Enteric_ferm_and_Manure_mgm")))
  
  
 

  select(`GHG emissions, Gg CO2 equivalent`, `1994`, `2016`) %>%
  drop_na()

df <- df %>%
  mutate(`1994` = as.numeric(as.character(`1994`)),
         `2016` = as.numeric(as.character(`2016`))) %>% 
  select(`GHG emissions, Gg CO2 equivalent`, `1994`, `2016`)

df<- df %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Enteric_ferm_and_Manure_mgm")))

df <- df[3,] %>% 
  mutate(change = ((`2016`-`1994`)/`1994`)*100) %>%
  mutate(Country_Code = country_code)

df <- df %>%
  mutate(`1994` = as.numeric(as.character(`1994`)),
       `2016` = as.numeric(as.character(`2016`))) %>% 
  select(`GHG emissions, kt CO2 equivalent`, `1994`, `2016`)

df<- df %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Enteric_ferm_and_Manure_mgm")))

df <- df[3,]
 
head(df)

warnings()

