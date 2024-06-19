# load packages - this script uses pacman, you may need to install pacman first.
#install.packages("pacman")

library(pacman)
p_load(tidyverse, checkmate, stringr, lubridate, gdata, readxl, naniar, 
       openxlsx, ggpubr, janitor, REDCapR, hablar, mefa,rebus)


#load your data dictionary
#this uses the practice DD on the github folder and assumes it is saved in the working directory

dd_to_simulate <- "PeriTexHonours_DataDictionary_2024-06-16.csv"
Redcap_DD <- read_csv(dd_to_simulate)

redcap_dd <- Redcap_DD %>%
  clean_names() %>%
  mutate(choices_calculations_or_slider_labels = if_else(field_type == "yesno", "0, Yes|1, No", choices_calculations_or_slider_labels, choices_calculations_or_slider_labels))


# PRE-PROCESSING ----------------------------------------------------------


# add in extra required columns -------------------------------------------------

  redcap_dd <- redcap_dd %>%
    mutate(dist = if_else(text_validation_type_or_show_slider_number == "number",NA_character_,"",""),
           sd = if_else(text_validation_type_or_show_slider_number == "number",NA_character_,"",""),
           mean = if_else(text_validation_type_or_show_slider_number == "number",NA_character_,"",""),
           size = "",
           prob = if_else(field_type == "dropdown" | field_type == "checkbox" ,NA_character_,"",""),
           lamda = "")
  

# manually correct variable classifications and option --------------------


#print variable names to screen than aren't defined
#this is then used to create the vectors below

print(redcap_dd %>%
  filter(is.na(text_validation_type_or_show_slider_number) & is.na (choices_calculations_or_slider_labels)) %>%
  select(variable_field_name), n=Inf)

classify_to_number = c("preop_egfr","preop_albumin", "pre_op_inr",
                       "pre_op_pt", "pre_op_aptt", "pre_op_fibrinogen", "txa_insion_rate_mg_kg_hr",
                       "intraop_temperature", "intraop_csl", "postoperative_nadir_platel", "postop_csl")
classify_to_multi_opt = c()
classify_to_yesno = c("mortality_30day")
classify_to_date <- c("case_date")
classify_to_date_time <- c("admit_dt_tm", "surgery_start_time", "surgery_stop_time", "extubation_time",
                           "icu_admission", "icu_discharge")

redcap_dd <- redcap_dd %>%
  mutate(text_validation_type_or_show_slider_number = case_when(
    variable_field_name %in% classify_to_number ~ "number",
    variable_field_name %in% classify_to_date ~ "date_dmy",
    variable_field_name %in% classify_to_date_time ~ "datetime_seconds_dmy",
    TRUE ~ text_validation_type_or_show_slider_number)) %>%
  mutate(choices_calculations_or_slider_labels = case_when(
    variable_field_name %in% classify_to_yesno ~ "0, Yes|1, No",
    variable_field_name %in% classify_to_multi_opt ~ "1, opt1 | 2, opt2 | 3, opt3",
    TRUE ~choices_calculations_or_slider_labels)) %>%
  mutate(field_type = case_when(
    variable_field_name %in% classify_to_yesno ~ "yesno",
    variable_field_name %in% classify_to_multi_opt ~ "checkbox",
    TRUE ~ field_type))
    

#write CSV, fill in NAs manually
fill_dist <- redcap_dd %>%
  select(variable_field_name, field_type, choices_calculations_or_slider_labels, prob, mean,sd,dist, 
          size, lamda) %>%
  filter(field_type != "text")

#. !!!!!!WARNING WILL OVERWRITE THE OLD FILE!!!!!!
write_csv(fill_dist, "fill_dist.csv")

#you need to now open excel and manually correct the spreadsheet

#bring in the updated dataset
fill_dist_updated <- read_csv("fill_dist.csv")

#add back into the master data dictionary
redcap_dd <- redcap_dd %>%
  select(-prob, -mean,-sd,-dist, -size, -lamda) %>%
  left_join(fill_dist_updated, by = c("variable_field_name", "field_type", "choices_calculations_or_slider_labels"))


# correct number variables ------------------------------------------------


num_vars <- redcap_dd %>%
  filter((field_type == "text" & text_validation_type_or_show_slider_number == "number") |
           (field_type == "text" & text_validation_type_or_show_slider_number == "integer")) %>%
  select(variable_field_name, text_validation_min, text_validation_max, mean, sd, dist)

# if you have a spreadsheet with sample number data and want to get the mean, max, sd etc then run this

numeric_example <- read.csv("numeric_ex.csv") %>%
  pivot_longer(cols = everything(), names_to = c("variable"), values_to = "value") %>%
  filter(!is.na(value)) %>%
  convert(num(value)) %>%
  group_by(variable) %>%
  # Summarise each group to calculate mean, sd, min, and max
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    sd_val = sd(value, na.rm = TRUE),
    text_validation_min = min(value, na.rm = TRUE),
    text_validation_max = max(value, na.rm = TRUE)
  )%>%
  ungroup() %>%
  mutate(distribution = "normal")

vars_already_commented <- as.vector (numeric_example$variable)

num_vars_to_comment <- num_vars %>%
  filter(!variable_field_name %in% vars_already_commented) %>%
  mutate(
    mean_val = NA_character_,
    sd_val = NA_character_,
    text_validation_min = NA_character_,
    text_validation_max = NA_character_,
    distribution = "normal") %>%
  select(-mean, -sd, -dist)

#. !!!!!!WARNING WILL OVERWRITE THE OLD FILE!!!!!!
write_csv(num_vars_to_comment, "fill_num.csv")

num_vars_to_comment_updated <- read_csv("fill_num.csv") %>%
  relocate(variable = variable_field_name, mean_val, sd_val, text_validation_min, text_validation_max, distribution)


numeric_example_complete <- numeric_example %>%
  bind_rows(num_vars_to_comment_updated) %>%
  rename(variable_field_name = variable)


redcap_dd <- redcap_dd %>%
  left_join(numeric_example_complete, by = "variable_field_name") %>%
  mutate(text_validation_min = if_else(is.na(text_validation_min.x), text_validation_min.y,text_validation_min.x, NA_real_)) %>%
  mutate(text_validation_max = if_else(is.na(text_validation_max.x), text_validation_max.y,text_validation_max.x, NA_real_)) %>%
  select(-text_validation_max.x, -text_validation_min.x,
         -text_validation_max.y, -text_validation_min.y,
         -mean, -sd, -dist, -size)







# START THE DUMMY DATA GENERATION PROCESS --------------------------------------------


#enter desired size of simulated dataset
sim_data_size <- 80


# categorical variables ---------------------------------------------------


#identify categorical variables



cat_vars <- redcap_dd %>%
  filter(field_type %in% c("checkbox", "dropdown", "radio", "yesno")) %>% 
  mutate(to = strsplit(choices_calculations_or_slider_labels, "\\|")) %>%
  unnest(to) %>%
  separate(to, c("choice", "choice_text"), sep = ",") %>%
  select(variable_field_name, choice, choice_text, prob) %>%
  convert(num(choice)) 

make_cat_vars <- function(x,y,z) {
  
  n = sim_data_size
  max_opts = as.numeric(x)
  min_opts = as.numeric(y)
  prob <- strsplit(z, ", ")[[1]]
  
  
  cat_out <<- sample(seq(from = min_opts, to = max_opts), size = n, replace = TRUE, prob = prob)

  
  
  
}

#create the simulated dataframe of categorical variables (called cat2)
prob_cols <- cat_vars %>%
  select(variable_field_name, prob) %>%
  group_by(variable_field_name) %>%
  slice_head(n = 1) %>%
  ungroup()

cat_sim <- cat_vars %>%
  convert(num(choice)) %>%
  group_by(variable_field_name) %>%
  summarise(max_opts = max(choice),
            min_opts = min(choice)) %>%
  ungroup() %>%
  left_join(prob_cols, by = "variable_field_name")

#function to fill in yes no with random selection
generate_probabilities <- function() {
  prob1 <- runif(1, 0, 1)
  prob2 <- 1 - prob1
  return(paste(round(prob1, 2), round(prob2, 2), sep = ", "))
}

# Apply the condition and generate probabilities
cat_sim <- cat_sim %>%
  mutate(prob = ifelse(is.na(prob) & max_opts == 1 & min_opts == 0, generate_probabilities(), prob))





cat_out <- pmap(list(cat_sim$max_opts, cat_sim$min_opts, cat_sim$prob), make_cat_vars)


cat_out <- reduce(cat_out, rbind.data.frame)

categorical_df <- cat_sim %>%
  select(variable_field_name) %>%
  bind_cols(cat_out) %>%
  pivot_longer(cols = -variable_field_name, names_to = "obs", values_to = "choice") %>%
  left_join(cat_vars, by = c("variable_field_name", "choice"))%>%
  select(- choice, -prob) %>%
  pivot_wider(names_from = variable_field_name, values_from = choice_text) %>%
  select(-obs)

rm(cat_out,cat_sim, cat_vars, prob_cols)


# numeric variables -------------------------------------------------------


#create the simulated dataframe of numerical variables (called num_sim2)

num_vars <- redcap_dd %>%
  filter((field_type == "text" & text_validation_type_or_show_slider_number == "number") |
           (field_type == "text" & text_validation_type_or_show_slider_number == "integer")) %>%
  select(variable_field_name, text_validation_min, text_validation_max, mean_val, sd_val, distribution)



generate_normal_sequence <- function(n, min_val, max_val, mean_val, sd_val) {
  seq <- rnorm(n, mean = mean_val, sd = sd_val)
  scaled_seq <- (seq - min(seq)) / (max(seq) - min(seq)) * (max_val - min_val) + min_val
  return(scaled_seq)
}

generate_uniform_sequence <- function(n, min_val, max_val) {
  seq <- runif(n, min = min_val, max = max_val)
  return(seq)
}

generate_sequence <- function(n, min_val, max_val, mean_val, sd_val, distribution) {
  if (!is.na(distribution) && distribution == "normal") {
    return(generate_normal_sequence(n, min_val, max_val, mean_val, sd_val))
  } else {
    return(generate_uniform_sequence(n, min_val, max_val))
  }
}


# Apply the function to each row of the DataFrame
sequences <- pmap(list(sim_data_size, numeric_example_complete$text_validation_min, numeric_example_complete$text_validation_max,
                       numeric_example_complete$mean_val, numeric_example_complete$sd_val, numeric_example_complete$distribution), generate_sequence)

# Combine sequences into a single data frame
numeric_df <- bind_cols(sequences)
colnames(numeric_df) <- numeric_example_complete$variable_field_name

rm(num_vars, num_vars_to_comment, num_vars_to_comment_updated, numeric_example, numeric_example_complete,
   fill_dist, fill_dist_updated, Redcap_DD, sequences)
# Dates -------------------------------------------------------------------
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
  
  n <- sim_data_size
  
  max_opts <- ifelse(is.na(x), Sys.Date(), as.POSIXct(x))
  min_opts <- ifelse(is.na(y), (Sys.Date() - years(2)), as.POSIXct(y))
  
  # Adjust range if min_opts and max_opts are the same or very close
  if (min_opts >= max_opts) {
    max_opts <- min_opts + years(2)
  }
  
  dates <- sample(seq(from = as.Date(min_opts), to = as.Date(max_opts), by = "day"), size = n, replace = TRUE)
  
  # Sort the dates in ascending order
  dates <- sort(dates)
  
  return(as.data.frame(dates))
}

make_date_hours <- function(x,z) {
  
  n <- sim_data_size
  
  max_opts <- ifelse(is.na(x), Sys.Date(), as.POSIXct(x))
  min_opts <- ifelse(is.na(y), (Sys.Date() - years(2)), as.POSIXct(y))
  
  # Adjust range if min_opts and max_opts are the same or very close
  if (min_opts >= max_opts) {
    max_opts <- min_opts + years(2)
  }
  
  # Generate date-time values
  dates <- sample(seq(from = as.POSIXct(min_opts), to = as.POSIXct(max_opts), by = "min"), size = n, replace = TRUE)
  
  # Sort the date-time values in ascending order
  dates <- sort(dates)
  
  # Format the date-time values as strings in the desired format
  dates_formatted <- format(dates, format = "%d/%m/%Y %H:%M")
  
  return(as.data.frame(dates_formatted))
}

make_date_time <- function(x,y) {
  
  n <- sim_data_size
  
  max_opts <- ifelse(is.na(x), Sys.Date(), as.POSIXct(x))
  min_opts <- ifelse(is.na(y), (Sys.Date() - years(2)), as.POSIXct(y))
  
  # Adjust range if min_opts and max_opts are the same or very close
  if (min_opts >= max_opts) {
    max_opts <- min_opts + years(2)
  }
  
  # Generate date-time values
  dates <- sample(seq(from = as.POSIXct(min_opts), to = as.POSIXct(max_opts), by = "sec"), size = n, replace = TRUE)
  
  # Sort the date-time values in ascending order
  dates <- sort(dates)
  
  # Format the date-time values as strings in the desired format
  dates_formatted <- format(dates, format = "%d/%m/%y %H:%M:%S")
  
  return(as.data.frame(dates_formatted))
  
}

make_time_min <- function(x,y) {
  
  n <- sim_data_size
  
  max_opts <- ifelse(is.na(x), Sys.Date(), as.POSIXct(x))
  min_opts <- ifelse(is.na(y), (Sys.Date() - years(2)), as.POSIXct(y))
  
  # Adjust range if min_opts and max_opts are the same or very close
  if (min_opts >= max_opts) {
    max_opts <- min_opts + years(2)
  }
  
  # Generate date-time values
  dates <- sample(seq(from = as.POSIXct(min_opts), to = as.POSIXct(max_opts), by = "sec"), size = n, replace = TRUE)
  
  # Sort the date-time values in ascending order
  dates <- sort(dates)
  
  # Format the date-time values as strings in the desired format
  dates_formatted <- format(dates, format = "%H:%M")
  
  return(as.data.frame(dates_formatted))
  
  
}

make_time_secs <- function(x,y) {
  n <- sim_data_size
  
  max_opts <- ifelse(is.na(x), Sys.Date(), as.POSIXct(x))
  min_opts <- ifelse(is.na(y), (Sys.Date() - years(2)), as.POSIXct(y))
  
  # Adjust range if min_opts and max_opts are the same or very close
  if (min_opts >= max_opts) {
    max_opts <- min_opts + years(2)
  }
  
  # Generate date-time values
  dates <- sample(seq(from = as.POSIXct(min_opts), to = as.POSIXct(max_opts), by = "sec"), size = n, replace = TRUE)
  
  # Sort the date-time values in ascending order
  dates <- sort(dates)
  
  # Format the date-time values as strings in the desired format
  dates_formatted <- format(dates, format = "%H:%M:%S")
  
  return(as.data.frame(dates_formatted))
  
}

date_out <- map2(date_only$text_validation_max, date_only$text_validation_min, make_date_only)
date_out <- do.call(cbind.data.frame, date_out)
column_names <- date_only$variable_field_name
colnames(date_out) <- column_names


date_hours_out <- map2(date_hours$text_validation_max, date_hours$text_validation_min, make_date_hours)
date_hours_out <- do.call(cbind.data.frame, date_hours_out)
column_names <- date_hours$variable_field_name
colnames(date_hours_out) <- column_names

datetime_out <- map2(date_hours_secs$text_validation_max, date_hours_secs$text_validation_min, make_date_time)
datetime_out <- do.call(cbind.data.frame, datetime_out)
column_names <- date_hours_secs$variable_field_name
colnames(datetime_out) <- column_names

time_hrs_out <- map2(time_hrs$text_validation_max,time_hrs$text_validation_min, make_time_min)
time_hrs_out <- do.call(cbind.data.frame, time_hrs_out)
column_names <- time_hrs$variable_field_name
colnames(time_hrs_out) <- column_names

time_hrs_secs_out <- map2(time_hrs_secs$text_validation_max,time_hrs$text_validation_min, make_time_secs)
time_hrs_secs_out <- do.call(cbind.data.frame, time_hrs_secs_out)
column_names <- time_hrs_secs$variable_field_name
colnames(time_hrs_secs_out) <- column_names

rm(date_only,
   date_hours,
   date_hours_secs,
   time_hrs ,
   time_hrs_secs,date_vars)
# combine all the dataframes ----------------------------------------------
#hash out the lines you dont need


sample_data <- categorical_df %>%
  bind_cols(numeric_df) %>%
  bind_cols(date_out) %>%
  bind_cols(datetime_out)# %>%
  #bind_cols(time_hrs_out) %>%
 # bind_cols(time_hrs_secs_out) %>%
 #bind_cols(date_hours_out)

#order columns
sample_data_cols <- as.vector(names(sample_data))
col_order <- redcap_dd %>% filter(variable_field_name %in% sample_data_cols)
col_order <- as.vector(col_order$variable_field_name)

sample_data <- sample_data[, col_order]


# post processing ---------------------------------------------------------

#what isn't included?
original_variables <- as.vector(redcap_dd$variable_field_name)
sample_variables <- as.vector(names(sample_data))

#use this to check if anything is missing
#easiest way to add it is to manually correct the fill_dist and fill_num files
#then repeat the scripts but dont write new fill_dist and fill_num files!
missing_cols <- setdiff(original_variables, sample_variables)
missing_cols

#add in the missing columns as empty fields
for (col in missing_cols) {
  sample_data[[col]] <- NA  # You can set any default value here if needed
}

#give a unique record_id

sample_data <- sample_data %>%
  mutate(record_id = floor(runif(n = n(), min = 1000000, max = 9999999)))


# Define your mapping string
cat_vars <- redcap_dd %>%
  filter(field_type %in% c("checkbox", "dropdown", "radio", "yesno"))

cat_vars_vector <- as.vector(cat_vars$variable_field_name)

chk_vars <- redcap_dd %>%
  filter(field_type %in% c("checkbox"))

chk_vars_vector <- as.vector(chk_vars$variable_field_name)


catergorical_to_convert <- sample_data %>%
  select(record_id, all_of(cat_vars_vector)) %>%
  pivot_longer(-record_id, names_to = "variable_field_name", values_to = "choice_text")

cat_vars <- redcap_dd %>%
  filter(field_type %in% c("checkbox", "dropdown", "radio", "yesno")) %>% 
  mutate(to = strsplit(choices_calculations_or_slider_labels, "\\|")) %>%
  unnest(to) %>%
  separate(to, c("choice", "choice_text"), sep = ",") %>%
  select(variable_field_name, choice, choice_text, prob) %>%
  convert(num(choice))%>%
  select(-prob)

catergorical_to_convert <- catergorical_to_convert %>%
  left_join(cat_vars, by = c("variable_field_name", "choice_text")) %>%
  select(-choice_text) %>%
  pivot_wider(id_cols = record_id, names_from = variable_field_name, values_from = choice)


sample_data <- sample_data %>%
  select(-all_of(cat_vars_vector)) %>%
  left_join(catergorical_to_convert, by = "record_id") %>% 
  relocate(record_id)

check_boxes <- sample_data %>%
  select(record_id, all_of(chk_vars_vector))%>%
  pivot_longer(-record_id, names_to = "variable_field_name", values_to = "choice_text") %>%
  mutate(new_name = str_c(variable_field_name, "___", choice_text, sep=""),
         new_choice = "1") %>%
  select(-variable_field_name, -choice_text)%>%
  pivot_wider(id_cols = record_id, names_from = new_name, values_from = new_choice)%>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))

sample_data <- sample_data %>%
  select(-all_of(chk_vars_vector)) %>%
  left_join(check_boxes, by = "record_id") %>% 
  relocate(record_id)

write_csv(sample_data, "practice_study_data.csv", na = "")




