#####
# This Project analyzes All space missions from 1957 to August 2022, including details on the location, date, and result of the launch,
# the company responsible, and the name, price, and status of the rocket used for the mission.
# Source: https://www.mavenanalytics.io/data-playground
# Done with ♥ by Alberto Frison - August 2022



#####
# 0. Initializing
rm (list = ls())
library (tidyverse)
# devtools::install_github("hrbrmstr/ggalt")
library (ggalt)


#####
# 1. Data Reading & Wrangling
data <- read.csv("./data/space_missions.csv", sep = ",")
data$Year <- format.Date (data$Date, format = "%Y")
data <- data %>%
  mutate (Location_New = str_sub(Location, start = Location %>% str_locate_all(",") %>% sapply(., function(x) x[[nrow(x)]]) + 2, end = str_length(Location)))


# Number of missions
data %>%
  group_by (Year) %>%
  summarize(n()) %>%
  plot(type ="h")

# Successful missions %
data %>%
  group_by (Year) %>%
  summarize(Success_Rate = sum(MissionStatus=="Success")/n()) %>%
  plot()

# Missions by Rocket
data %>%
  group_by (Rocket) %>%
  summarize(suc = sum(MissionStatus=="Success")/n(), dt = n()) %>%
  filter (dt > 15) %>%
  ggplot () +
  geom_bar (aes(x = Rocket , y = dt, fill = suc), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
# Missions by company
data %>%
  group_by (Company) %>%
  summarize(suc = sum(MissionStatus=="Success")/n(), dt = n()) %>%
  filter (dt > 5) %>%
  ggplot () +
  geom_bar (aes(x = Company , y = dt, fill = suc), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 10 longest serving Rockets 
data %>%
  group_by (Rocket) %>%
  mutate (min_y = min (Year), max_y = max (Year), duration_y = as.numeric(max_y) - as.numeric(min_y)) %>%
  select(Rocket, min_y, max_y, duration_y) %>%
  distinct() %>%
  filter (min_y != max_y) %>%
  arrange (desc(as.numeric(duration_y))) %>%
  mutate (Rocket = factor (Rocket, levels = Rocket)) %>%
  head (n=10) %>%
  ggplot (aes (y= Rocket, x = min_y, xend = max_y)) +
  geom_dumbbell (color="blue", 
                 size=1, 
                 colour_x ="black",
                 colour_xend = "black") +
  labs(x=NULL, y=NULL, title="Longest Serving Rockets") +
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05))

# Missions by Location & Company
data %>%
  group_by (Location_New, Company) %>%
  summarize(missions = n()) %>%
  filter (missions > 15) %>%
  ggplot () +
  geom_bar (aes(x = Location_New , y = missions, fill = Company), color = "Black", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

head (data)
    
##############################  code to extract the LOCATION

# x <- data[131,]$Location
# y <- str_locate_all (x,",")[[1]]
# y[length(y)/2]
# 
# length(y)
# 
# data <- data %>%
#   mutate (Location_New = str_sub(Location, 
#                                  start = str_locate_all (Location,",")[[1]][length(str_locate_all (Location,",")[[1]])/2] + 2, 
#                                  end = str_length(Location)))
# 
# data <- data %>%
#   mutate (Location_New = Location,
#           length = length(lapply(x = Location, FUN = str_locate_all((",")[[1]])/2)),
#           start = str_locate_all (Location,",")[[1]][length(str_locate_all (Location,",")[[1]])/2] + 2,
#           end = str_length(Location))
# 
# data <- data %>%
#   mutate (Location_New = Location,
#           start = Location %>%
#             str_locate_all(",") %>% 
#             sapply(., function(x) x[[nrow(x)]]),
#           end = str_length(Location))
# data$Location %>%
#   str_locate_all(",") %>% 
#     sapply(., function(x) x[[nrow(x)]]) 
# 
# tail(data,50)
# 
# str_sub(x, 
#         start = str_locate_all (x,",")[[1]][length(str_locate_all (x,",")[[1]])/2] + 2, 
#         end = str_length(x))
# 
# 
# gregexpr(pattern = "\,(?=[^,]*$)",)
# 
# 
# 
# 
# str_sub(x,tail(str_locate_all (x,",")[[2]]$start,1), 5)
# 
# x <- head(data$Location,1)
# y <- regexpr("\,(?=[^,]*$)", x)
# 
# x <- "http://stat.umn.edu:80/xyz"
# m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
# m
# regmatches(x, m)
# 
# x <- "Site 1/5, Baikonur Cosmodrome, Kazakhstan"
# tail(unlist(gregexpr(',', x)), n=1)




