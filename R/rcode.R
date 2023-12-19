source("R/packages.R")

# import data

accidents <- read.csv("data/accidents.csv", header=TRUE)
bikers <- read.csv("data/bikers.csv", header=TRUE)

# ----------------------

# summaries 

accidents <- accidents %>% clean_names()
accidents %>% summary()


bikers <- bikers %>% clean_names()
bikers %>% summary()

# -----------------------
# -------------------------------------------------------------

# A. PROCESS PHASE

# ACCIDENTS CLEANING AND FORMATTING

# 1. correct formatting: date and time

# check formatting:

# date
class(accidents$date)
#time
class(accidents$time)


# they are both character, convert them
# ymd(), parse_date_time(), floor_date from lubridate
# mutate function and pipe from dplyr

accidents <- accidents %>%  
  mutate(date = ymd(date),
         time = parse_date_time(time, orders = c("%H:%M")),
         time = floor_date(time, "hour"))

# check if conversion succeeded
# date -> Date object 
# time -> POSIXct date-time object
class(accidents$time)
class(accidents$date)

# ------------------------

# 2. filter year = 2018

accidents_2018 <- accidents %>%
  filter(year(date) == 2018)

# 3. missing values and duplicates

# replace values like “Unknown” and “Missing Data” with NA

accidents_2018 <-
  accidents_2018 %>% 
  mutate(across(where(is.character), str_to_title)) %>% 
  mutate(across(where(is.character), ~na_if(., c("Missing Data")))) 

# check NA, yes NA
#  %>% (pipe) operator from the dplyr package
accidents_2018 %>% anyNA()
# how many
sum(is.na(accidents_2018))

# check duplicates, no duplicates
sum(duplicated(accidents_2018))

# remove NA
accidents_2018 <- accidents_2018 %>% drop_na()

# check if removal succeeded, yes
accidents_2018 %>% anyNA()
#-------------------------------

# 4. outlier detection and remotion

# naive way
accidents_2018 %>% summary()
# is working, easy to check from summary, keep like that

#-------------------------------


# BIKERS CLEANING AND FORMATTING

# filter bikers by 'accident_index' 
# retain only bikers related to year 2018
bikers_2018 <- bikers %>%
  filter(accident_index %in% accidents_2018$accident_index)


# character variables converted to title case
# levels of age group 

bikers_2018 <-
  bikers_2018 %>% mutate(across(where(is.character), str_to_title)) %>%
  mutate(
    age_grp = str_replace_all(age_grp, pattern = " To ", replacement = "-"),
    age_grp = factor(
      age_grp,
      levels = c(
        "6-10",
        "11-15",
        "16-20",
        "21-25",
        "26-35",
        "36-45",
        "46-55",
        "56-65",
        "66-75"
      )
    )
  )

# replace values like “Unknown” and “Missing Data” with NA

bikers_2018 <-
  bikers_2018 %>%  
  mutate(across(where(is.character), ~na_if(., c("Missing Data")))) 

# check NA, yes NA
#  %>% (pipe) operator from the dplyr package
bikers_2018 %>% anyNA()

# check duplicates, no duplicates
sum(duplicated(bikers_2018))

# get unique values
for (col in setdiff(names(bikers_2018), "accident_index")) {
  cat("Unique values for column", col, ":", "\n")
  print(unique(bikers_2018[[col]]))
  cat("\n")
}

# -------------------------

# MERGING DATA

bike_accidents_18 <- accidents_2018 %>% left_join(bikers_2018, by = "accident_index")

# -----------------------

# check NA in merged data:

bike_accidents_18 %>% anyNA()

# ------------------------

head(bike_accidents_18, 3)
head(bike_accidents_18, 3)
head(bike_accidents_18, 3)



#-------------------------------------------------------------


# B. SHORT ANALYSIS PHASE

# 1. who are the injured?

# gender plot exploting patchwork package

male <- bikers_2018 %>%
  count(gender,
        sort = T,
        name = "Counts") %>% 
  mutate(Percent = Counts/sum(Counts)*100) %>% 
  filter(gender ==  "Male")

male_plt <- ggplot(male, aes(x = gender, y = Counts, color = gender, size = Counts)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(vars(gender), scales = "free") +
  annotate(geom = "text", x = "Male", y = male$Counts, label = glue("Male\n",round(male$Percent,1), "%"), size = 8, color = "white")+
  scale_color_manual(values = c(
    Male = "steelblue"
  )) +
  scale_size(range = c(100,80)) +
  theme_void() +
  theme(axis.text = element_blank(),
        strip.text = element_blank()) +
  labs(title = NULL,
       x = NULL,
       y = NULL)


female <- bikers_2018 %>%
  count(gender,
        sort = T,
        name = "Counts") %>% 
  mutate(Percent = Counts/sum(Counts)*100) %>% 
  filter(gender ==  "Female")

female_plt <- ggplot(female, aes(x = gender, y = Counts, color = gender, size = Counts)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(vars(gender), scales = "free") +
  annotate(geom = "text", x = "Female", y = female$Counts, label = glue("Female\n",round(female$Percent,1), "%"), size = 4, color = "white")+
  scale_color_manual(values = c(
    Female = "darkgreen"
  )) +
  scale_size(range = c(80,20)) +
  theme_void() +
  theme(axis.text = element_blank(),
        strip.text = element_blank()) +
  labs(title = NULL,
       x = NULL,
       y = NULL)

female_plt | male_plt + plot_layout(widths = 0.5)



# casualties by age and group

bikers_2018 %>% select(age_grp, gender) %>%
  filter(gender != "Other") %>%
  count(age_grp, gender,
        sort = T,
        name = "Counts") %>%
  mutate(Percent = Counts / sum(Counts) * 100) %>%
  arrange(age_grp) %>%
  ggplot(aes(
    x = age_grp,
    y = Percent,
    fill = gender,
    label = str_c(round(Percent, 1), "%")
  )) +
  geom_col(position = position_dodge()) +
  geom_text(position = position_dodge(width = 1),
            vjust = -0.3,
            size = 3.5) +
  scale_fill_manual(values = c(Female = "#343D46", Male = "#66A498")) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(title = "The percentage casualties by gender and age group",
       x = "Age Group",
       y = "Percent",
       fill = NULL)


# ------------------------------

# 2. which are the factors leading to injuries?

# severity by speed limit

severity_speed_plot <- ggplot(bike_accidents_18, aes(x = factor(speed_limit), fill = severity)) +
  geom_bar(position = "stack", stat = "count") +
  scale_fill_manual(values = c("Fatal" = "red", "Serious" = "orange", "Slight" = "yellow")) +
  labs(title = "Distribution of Accidents by Speed Limit and Severity",
       x = "Speed Limit",
       y = "Count") +
  theme_minimal()

print(severity_speed_plot)


# ----------------------------------

# severity by road conditions

road_condition_severity_plot <- ggplot(bike_accidents_18, aes(x = road_conditions, fill = severity)) +
  geom_bar(position = "stack", stat = "count") +
  scale_fill_manual(values = c("Fatal" = "red", "Serious" = "orange", "Slight" = "yellow")) +
  labs(title = "Distribution of Accidents by Road Conditions and Severity",
       x = "Road Conditions",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

print(road_condition_severity_plot)

# ----------------------------------

# by weather condition

weather_condition_severity_plot <- ggplot(bike_accidents_18, aes(x = weather_conditions, fill = severity)) +
  geom_bar(position = "stack", stat = "count") +
  scale_fill_manual(values = c("Fatal" = "red", "Serious" = "orange", "Slight" = "yellow")) +
  labs(title = "Distribution of Accidents by Weather Conditions and Severity",
       x = "Weather Conditions",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

print(weather_condition_severity_plot)

# -------------------------------

# by road type

road_type_severity_plot <- ggplot(bike_accidents_18, aes(x = road_type, fill = severity)) +
  geom_bar(position = "stack", stat = "count") +
  scale_fill_manual(values = c("Fatal" = "red", "Serious" = "orange", "Slight" = "yellow")) +
  labs(title = "Distribution of Accidents by Road Type and Severity",
       x = "Road Type",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

print(road_type_severity_plot)

# ---------------------------------

# by light conditions

light_condition_severity_plot <- ggplot(bike_accidents_18, aes(x = light_conditions, fill = severity)) +
  geom_bar(position = "stack", stat = "count") +
  scale_fill_manual(values = c("Fatal" = "red", "Serious" = "orange", "Slight" = "yellow")) +
  labs(title = "Distribution of Accidents by Light Conditions and Severity",
       x = "Light Conditions",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

print(light_condition_severity_plot)




