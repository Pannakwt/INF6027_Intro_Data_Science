library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)

########################## import data ##########################
match_player_stats <- read.csv("Coursework/match_player_stats.csv")
match_line_ups <- read.csv("Coursework/match_line_ups.csv")



########################## cleaning and joining data ##########################
######### match_player_stats #########
# drop unused column for pivot data
match_player_stats <- match_player_stats [-c(9, 12)]

# pivot the data
match_player_stats <- match_player_stats %>%
  pivot_wider(names_from = StatsName, values_from = Value)

# create full name by joining first name and last name
match_player_stats$full_name <- paste(match_player_stats$PlayerName, match_player_stats$PlayerSurname)


######### match_line_ups #########
# create full name by joining first name and last name
match_line_ups$full_name <- paste(match_line_ups$OfficialName, match_line_ups$OfficialSurname)

# check unique value
duplicates <- duplicated(match_line_ups[, c("MatchID", "full_name")])
match_line_ups[duplicates, ]

# drop full name that are “None None” value (staff)
match_line_ups <- match_line_ups %>%
  filter(IsStaff != "True")


######### joining #########
# check for duplicate column name
intersect(names(match_player_stats), names(match_line_ups))

# test join to see that each pair of duplicate columns have all same values
left_join_result <- left_join(match_player_stats, match_line_ups, by = c("MatchID", "full_name"))
all(left_join_result$HomeTeamName.x == left_join_result$HomeTeamName.y) # TRUE
all(left_join_result$AwayTeamName.x == left_join_result$AwayTeamName.y) # TRUE
all(left_join_result$IsGoalkeeper.x == left_join_result$IsGoalkeeper.y) # FALSE

# found that IsGoalkeeper.x is all false, so we decided to drop duplicate column in match_player_stats
match_player_stats <- match_player_stats %>%
  select(-HomeTeamName, -AwayTeamName, -IsGoalkeeper)

# left join match_player_stats with match_line_ups by MatchID and full_name
left_join_result <- left_join(match_player_stats, match_line_ups, by = c("MatchID", "full_name"))

# check unique roles
unique(left_join_result$Role)

# filter goalkeeper and defender out
left_join_result <- left_join_result %>%
  filter(Role %in% c("forwards", "midfielders"))

# trim white space before and after full name
left_join_result$full_name <- str_trim(left_join_result$full_name)

# check number of samples
nrow(left_join_result) # 953



########################## pre-processing data for analysis ##########################
# select the features
selected_features <- left_join_result %>%
  select(full_name,
         Goals,
         Assists,
         `PlayedTime`,
         `Time spent in attacking third`,
         `Time spent in penalty area`,
         `Total Attempts`,
         `Attempts Accuracy`,
         `Passes completed`,
         `Passes accuracy`,
         `Passes accuracy short`,
         `Passes accuracy medium`,
         `Passes accuracy long`,
         `Passes received`,
         `Crosses attempted`,
         `Delivery into attacking third`,
         `Delivery into key play area`,
         `Delivery into penalty area`,
         `Instance of possession `,
         `Distance covered (m)`,
         `Distance covered in possession (m)`,
         `Solo run into attacking third`,
         `Solo run into key play area`,
         `Solo run into penalty area`,
         `Sprints`,
         `Player average speed`)

# convert type all column that are number from charater to numeric
selected_features <- selected_features %>%
  mutate(across(c(Goals,
                  Assists,
                  `Total Attempts`, 
                  `Attempts Accuracy`, 
                  `Passes completed`, 
                  `Passes accuracy`, 
                  `Distance covered (m)`, 
                  `Distance covered in possession (m)`, 
                  `Instance of possession `, 
                  `Crosses attempted`, 
                  `Passes received`,
                  `Passes accuracy short`, 
                  `Passes accuracy medium`, 
                  `Passes accuracy long`, 
                  `Delivery into attacking third`, 
                  `Delivery into key play area`, 
                  `Delivery into penalty area`, 
                  `Solo run into attacking third`, 
                  `Solo run into key play area`, 
                  `Solo run into penalty area`,
                  `Sprints`, 
                  `Player average speed`), as.numeric))

# convert time to numeric (`Time spent in attacking third`, `Time spent in penalty area`)
# create function to handle all time patterns
time_to_seconds <- function(time_str) {
  if (time_str == "") return(0) 
  parts <- unlist(strsplit(time_str, "[:.]")) 
  h <- as.numeric(parts[1])
  m <- as.numeric(parts[2])
  s <- as.numeric(parts[3])
  ms <- ifelse(length(parts) > 3, as.numeric(parts[4]), 0)
  total_seconds <- h * 3600 + m * 60 + s + ms / 10000000
  return(total_seconds)
}

selected_features$`Time spent in attacking third` <- sapply(selected_features$`Time spent in attacking third`, time_to_seconds)
selected_features$`Time spent in penalty area` <- sapply(selected_features$`Time spent in penalty area`, time_to_seconds)

# check missing value
colSums(is.na(selected_features))

selected_features %>%
  filter(if_any(everything(), is.na))

# drop missing values
selected_features <- selected_features %>%
  filter(if_all(everything(), ~ !is.na(.)))

# create goal contribution column - sum the number of goal and assist
final_df <- selected_features %>%
  mutate(GACombined = Goals + Assists)



########################## exploratory data analysis ##########################
######### analysing data using time span per match or across the tournament? #########
# create a copy of cleaned dataframe
eda_df <- final_df

# convert to character for display all labels in x-axis
eda_df$GACombined <- as.character(eda_df$GACombined)

# plot bar chart - sample count of goal contribution per match
ggplot(eda_df, aes(x = GACombined)) +
  geom_bar(fill = "#0072B2") +
  labs(
    title = "Sample Count of Goal Contribution per Match",
    subtitle = "Amount of goal contributions per match range from 0 to 3",
    x = "Amount of goal contribution (goals+assists)",
    y = "Count",
    caption = "Data source: UEFA Euro 2020 Dataset by Mikhail Zhilkin\nhttps://data.world/cervus/uefa-euro-2020"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "#505050")
  )

# convert to numeric for aggregation
eda_df$GACombined <- as.numeric(eda_df$GACombined)

# aggregate player data from per match to across the tournament
eda_df <- eda_df %>%
  group_by(full_name) %>%
  summarise(
    ga_combined = sum(GACombined)
  )

# plot bar chart - sample count of goal contribution across the tournament
ggplot(eda_df, aes(x = ga_combined)) +
  geom_bar(fill = "#0072B2") +
  labs(
    title = "Sample Count of Goal Contributions across the Tournament",
    subtitle = "Amount of goal contributions across the Tournament range from 0 to 6",
    x = "Amount of goal contribution (goals+assists)",
    y = "Count",
    caption = "Data source: UEFA Euro 2020 Dataset by Mikhail Zhilkin\nhttps://data.world/cervus/uefa-euro-2020"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "#505050")
  )


######### is the amount of goal contribution equal or have a little different when calculating per match? #########
# create a copy of cleaned dataframe
eda_df <- final_df

# aggregate player data from per match to across the tournament
eda_df <- eda_df %>%
  group_by(full_name) %>%
  summarise(
    ga_combined = sum(GACombined),
    played_time = sum(PlayedTime)
  )

# filter only player who scored 3 goals in tournament
eda_df <- eda_df %>%
  filter(ga_combined == 3)

# convert amount of actual goal contribution to per match
eda_df <- eda_df %>%
  mutate(across(ga_combined, ~ . * 90 * 60 / played_time))

# highlight color for better communication
eda_df <- eda_df %>% 
  mutate(Color = "#B0B0B0") %>% 
  mutate(Color = ifelse(full_name == "Kasper Dolberg", "#E69F00", Color)) %>% 
  mutate(Color = ifelse(full_name == "Pierre-Emile Højbjerg", "#0072B2", Color))

# plot bar chart - amount of goal contributions per match from player who scored 3 actual goals across the tournament
ggplot(eda_df, aes(x = full_name, y = ga_combined, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("#E69F00" = "#E69F00", "#0072B2" = "#0072B2"),
    name = "Indicators",
    labels = c("Lowest", "Highest")
  ) +
  labs(
    title = "Player who have 3 goal contributions across the tournament",
    subtitle = "Different amount of goal contributions per match among player who scored the same amount of goals across the tournament",
    x = "Player Name",
    y = "Amount of goal contribution per 90 minutes",
    caption = "Data source: UEFA Euro 2020 Dataset by Mikhail Zhilkin\nhttps://data.world/cervus/uefa-euro-2020"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "#505050"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = 'top', 
    legend.justification = 'left',
    legend.direction = 'horizontal'
  )


######### relationship of total attempts and goal contributions per match #########
# create a copy of cleaned dataframe
eda_df <- final_df

# aggregate player data from per match to across the tournament
eda_df <- eda_df %>%
  group_by(full_name) %>%
  summarise(
    ga_combined = sum(GACombined),
    played_time = sum(PlayedTime),
    total_attempts = sum(`Total Attempts`)
  )

# convert amount of actual goal contribution to per match
eda_df <- eda_df %>%
  mutate(across(c(ga_combined,
                  total_attempts), ~ . * 90 * 60 / played_time))

# filter only player that have goal contribution (more than 0)
eda_df <- eda_df %>%
  filter(ga_combined != 0)

# plot scatter plot - total attempts vs goal contributions
ggplot(eda_df, aes(x = ga_combined, y = total_attempts)) +
  geom_point(color = "#0072B2", size = 3) +
  labs(
    title = "Scatter plot between total attempts and goal contributions per match",
    subtitle = "As the amount of total attempts increases, amount of goal contribution also go up",
    x = "Amount of goal contribution per 90 minutes",
    y = "Amount of total attempt per 90 minutes",
    caption = "Data source: UEFA Euro 2020 Dataset by Mikhail Zhilkin\nhttps://data.world/cervus/uefa-euro-2020"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "#505050")
  )



########################## data analysis ##########################
######### correlation matrix #########
# create a copy of cleaned dataframe
eda_df <- final_df

# aggegrate stats for each player - actual value (sum) / avg or ratio (avg)
eda_df <- eda_df %>%
  group_by(full_name) %>%
  summarise(
    matches_played = n(),
    ga_combined = sum(GACombined),
    goals = sum(Goals),
    assists = sum(Assists),
    played_time = sum(PlayedTime),
    time_atkthird = sum(`Time spent in attacking third`),
    time_penarea = sum(`Time spent in penalty area`),
    total_attempts = sum(`Total Attempts`),
    attempt_acc = mean(`Attempts Accuracy`),
    pass_completed = sum(`Passes completed`),
    pass_acc = mean(`Passes accuracy`),
    pass_acc_short = mean(`Passes accuracy short`),
    pass_acc_medium = mean(`Passes accuracy medium`),
    pass_acc_long = mean(`Passes accuracy long`),
    pass_received = sum(`Passes received`),
    cross_attempted = sum(`Crosses attempted`),
    delivery_atkthird = sum(`Delivery into attacking third`),
    delivery_keyarea = sum(`Delivery into key play area`),
    delivery_penarea = sum(`Delivery into penalty area`),
    instance_possession = sum(`Instance of possession `),
    distance_covered = sum(`Distance covered (m)`),
    distance_covered_posession = sum(`Distance covered in possession (m)`),
    solorun_atkthird = sum(`Solo run into attacking third`),
    solorun_keyarea = sum(`Solo run into key play area`),
    solorun_penarea = sum(`Solo run into penalty area`),
    sprints = sum(`Sprints`),
    avg_speed = mean(`Player average speed`)
  )

# convert metrics into per match (90 mins) (based on eda result)
eda_df <- eda_df %>%
  mutate(across(c(ga_combined, 
                  time_atkthird,
                  time_penarea,
                  total_attempts,
                  pass_completed,
                  pass_received,
                  cross_attempted,
                  delivery_atkthird,
                  delivery_keyarea,
                  delivery_penarea,
                  instance_possession,
                  distance_covered,
                  distance_covered_posession,
                  solorun_atkthird,
                  solorun_keyarea,
                  solorun_penarea,
                  sprints), ~ . * 90 * 60 / played_time))

# filter only player that have goal contribution (more than 0)
eda_df <- eda_df %>%
  filter(ga_combined != 0)

# get only all features
eda_df <- eda_df %>%
  select(-full_name, -matches_played)

# reverse column order for better visualization
eda_df <- eda_df[, rev(seq_along(eda_df))]

# calculate correlation between each variables
cor_data <- cor(eda_df)

# transform data structure for plotting
cor_data_melted <- melt(cor_data, id.vars = "V")

# plot heatmap - strength of relationship between variables
ggplot(cor_data_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#0072B2", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation"
  ) +
  labs(
    title = "Correlation Matrix",
    subtitle = "Degree of relationship between selected variables. Dependent variable is on the top.",
    x = "Variable 1",
    y = "Variable 2",
    caption = "Data source: UEFA Euro 2020 Dataset by Mikhail Zhilkin\nhttps://data.world/cervus/uefa-euro-2020"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "#505050"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
  )