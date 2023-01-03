# load packages - this script uses pacman, you may need to install pacman first.
#install.packages("pacman")

library(pacman)
p_load(tidyverse, checkmate, stringr, lubridate, gdata, readxl, gmodels, naniar, 
       openxlsx, ggpubr, janitor, skimr, REDCapR, hablar, mefa,rebus)


#load your data dictionary
#this uses the practice DD on the github folder and assumes it is saved in the working directory
redcap_dd <- PONV_Redcap_DD %>%
  clean_names() %>%
  mutate(choices_calculations_or_slider_labels = if_else(field_type == "yesno", "0, Yes|1, No", choices_calculations_or_slider_labels, choices_calculations_or_slider_labels))

#enter desired size of simulated dataset
sim_data_size <- 60



# add in required columns -------------------------------------------------

add_required_cols <- function(x) {
  
  redcap_dd <- x %>%
    mutate(dist = NA_character_,
           sd = NA_character_,
           mean = NA_character_,
           size = NA_character_,
           prob = NA_character_,
           lamda = NA_character_)
           
  redcap_dd <<- redcap_dd
  
}

add_required_cols(redcap_dd)

#identify categorical variables

cat_vars <- redcap_dd %>%
  filter(field_type %in% c("checkbox", "dropdown", "radio", "yesno")) %>% 
  mutate(to = strsplit(choices_calculations_or_slider_labels, "\\|")) %>%
  unnest(to) %>%
  separate(to, c("choice", "choice_text"), sep = ",") %>%
  select(variable_field_name, choice, choice_text) %>%
  convert(num(choice))


make_cat_vars <- function(x,y) {
  
  n = sim_data_size
  max_opts = as.numeric(x)
  min_opts = as.numeric(y)

  
  cat_out <<- sample(seq(from = min_opts, to = max_opts), size = n, replace = TRUE)
  
  
  
}

#create the simulated dataframe of categorical variables (called cat2)

cat_sim <- cat_vars %>%
  convert(num(choice)) %>%
  group_by(variable_field_name) %>%
  summarise(max_opts = max(choice),
            min_opts = min(choice)) %>%
  ungroup()

cat_out <- map2(cat_sim$max_opts, cat_sim$min_opts, make_cat_vars)
cat_out <- reduce(cat_out, rbind.data.frame)

cat_sim2 <- cat_sim %>%
  select(variable_field_name) %>%
  bind_cols(cat_out) %>%
  pivot_longer(cols = -variable_field_name, names_to = "obs", values_to = "choice") %>%
  left_join(cat_vars, by = c("variable_field_name", "choice"))%>%
  select(- choice) %>%
  pivot_wider(names_from = variable_field_name, values_from = choice_text) %>%
  select(-obs)

rm(cat_out,cat_sim, cat_vars)

#create the simulated dataframe of numerical variables (called num_sim2)

num_vars <- redcap_dd %>%
  filter((field_type == "text" & text_validation_type_or_show_slider_number == "number") |
           (field_type == "text" & text_validation_type_or_show_slider_number == "integer")) %>%
  select(variable_field_name, text_validation_min, text_validation_max)

make_num_vars <- function(x,y) {
  
  n = sim_data_size
  max_opts = ifelse(is.na(x),sample(500:1000, 1), as.numeric(x))
  min_opts = ifelse(is.na(y),sample(100:500, 1), as.numeric(y))
  
  num_out <<- sample(seq(from = min_opts, to = max_opts), size = n, replace = TRUE)
  
  
  
}

num_out <- map2(num_vars$text_validation_max, num_vars$text_validation_min, make_num_vars)
num_out <- reduce(num_out, rbind.data.frame)

num_sim2 <- num_vars %>%
  select(variable_field_name) %>%
  bind_cols(num_out) %>%
  pivot_longer(cols = -variable_field_name, names_to = "obs", values_to = "choice") %>%
  convert(num(choice)) %>%
  select(- obs) %>%
  group_by(variable_field_name) %>%
  mutate(obs_no = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from = variable_field_name, values_from = choice)  %>%
  select(-obs_no)



#create the simulated dataframe of date variables (called date_sim2)

date_vars <- redcap_dd %>%
  filter((field_type == "text" & str_detect(text_validation_type_or_show_slider_number, "date")) |
            field_type == "text" & str_detect(text_validation_type_or_show_slider_number, "time")) %>%
  select(variable_field_name,date_type = text_validation_type_or_show_slider_number, text_validation_min, text_validation_max) %>%
  mutate(variable_to_simulate = case_when(
    date_type == "date_dmy" ~ "date",
    date_type == "date_mdy" ~ "date",
    date_type == "date_ymd" ~ "date",
    date_type == "datetime_dmy" ~ "date_hrs",
    date_type == "datetime_mdy" ~ "date_hrs",
    date_type == "datetime_ymd" ~ "date_hrs",
    date_type == "datetime_seconds_dmy" ~ "date_hrs_secs",
    date_type == "datetime_seconds_mdy" ~ "date_hrs_secs",
    date_type == "datetime_seconds_ymd" ~ "date_hrs_secs",
    date_type == "time_hh_mm_ss" ~ "time_hrs_secs",
    date_type == "time" ~ "time_hrs"))

date_only <- date_vars %>% filter(variable_to_simulate == "date")
date_hours <- date_vars %>% filter(variable_to_simulate == "date_hrs")
date_hours_secs <- date_vars %>% filter(variable_to_simulate == "date_hrs_secs")
time_hrs <- date_vars %>% filter(variable_to_simulate == "time_hrs")
time_hrs_secs <- date_vars %>% filter(variable_to_simulate == "time_hrs_secs")



make_date_only <- function(x,y) {
  
  n = sim_data_size
  
  max_opts <- if_else(is.na(x), Sys.Date(), x, Sys.Date())
  min_opts <- if_else(is.na(y),(max_opts - years(2)), y)
  

dates <- sample(seq(from = min_opts, to = max_opts, by = "day"), size = n, replace = TRUE)
  
}

make_date_hours <- function(x,y) {
  
  n = sim_data_size
  
  max_opts <- ifelse(is.na(x),format.Date(Sys.time(),"%d/%m/%y %H:%M"), x)
  max_opts <- ymd_hm(max_opts)
  min_opts <- if_else(is.na(y),max_opts- dyears(2), y,max_opts- dyears(2))
  
  
  dates <- sample(seq(from = min_opts, to = max_opts, by = "day"), size = n, replace = TRUE)
  
}

make_date_time <- function(x,y) {
  
  n = sim_data_size
  
  max_opts <- ifelse(is.na(x),format.Date(Sys.time(),"%d/%m/%y %H:%M:%S"), x)
  max_opts <- ymd_hms(max_opts)
  min_opts <- if_else(is.na(y),max_opts- dyears(2), y,max_opts- dyears(2))
  
  
  dates <- sample(seq(from = min_opts, to = max_opts, by = "day"), size = n, replace = TRUE)
  
}

make_time_min <- function(x,y) {
  
  n = sim_data_size
  
  max_opts <- ifelse(is.na(x),format.Date(Sys.time(),"%d/%m/%y %H:%M"), x)
  max_opts <- ymd_hm(max_opts)
  min_opts <- if_else(is.na(y),max_opts- dyears(2), y,max_opts- dyears(2))
  
  
  dates <- sample(seq(from = min_opts, to = max_opts, by = "day"), size = n, replace = TRUE)
  
  times <- format(dates, format = "%H:%M")
  
}

make_time_secs <- function(x,y) {
  
  n = sim_data_size
  
  max_opts <- ifelse(is.na(x),format.Date(Sys.time(),"%d/%m/%y %H:%M:%S"), x)
  max_opts <- ymd_hms(max_opts)
  min_opts <- if_else(is.na(y),max_opts- dyears(2), y,max_opts- dyears(2))
  
  
  dates <- sample(seq(from = min_opts, to = max_opts, by = "day"), size = n, replace = TRUE)
  
  times <- format(dates, format = "%H:%M:%S")
  
}

date_out <- map2(date_only$text_validation_max, date_only$text_validation_min, make_date_only)
date_out <- reduce(date_out, rbind.data.frame)

date_hours_out <- map2(date_hours$text_validation_max, date_hours$text_validation_min, make_date_only)
date_hours_out <- reduce(date_hours_out, rbind.data.frame)


datetime_out <- map2(date_hours_secs$text_validation_max, date_hours_secs$text_validation_min, make_date_time)
datetime_out <- reduce(datetime_out, rbind.data.frame)

time_hrs_out <- map2(time_hrs$text_validation_max,time_hrs$text_validation_min, make_time_min)
time_hrs_out <- reduce(time_hrs_out, rbind.data.frame)

time_hrs_secs_out <- map2(time_hrs_secs$text_validation_max,time_hrs$text_validation_min, make_time_secs)
time_hrs_secs_out <- reduce(time_hrs_secs_out, rbind.data.frame)

#put data together
sample_data <- cat_sim2 %>%
  bind_cols(num_sim2)

#order columns
sample_data_cols <- as.vector(names(sample_data))
col_order <- redcap_dd %>% filter(field_name %in% sample_data_cols)
col_order <- as.vector(col_order$field_name)

sample_data <- sample_data[, col_order]


write.xlsx(sample_data, "practice_study_data.xlsx")
