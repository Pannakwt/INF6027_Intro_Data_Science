library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

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

# convert type all column that are number from character to numeric
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
selected_features <- selected_features %>%
  mutate(GACombined = Goals + Assists)

# aggregate stats for each player - actual value (sum) / avg or ratio (avg)
selected_features <- selected_features %>%
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
selected_features <- selected_features %>%
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

# duplicate dataframe for use in analysis
final_df <- selected_features



########################## conducting analyses ##########################
# scatter plot to see relationship
ggplot(
  data=final_df,
  aes(x=ga_combined, y=time_atkthird)
) + geom_point()

# test correlation
cor.test(
  final_df$ga_combined,
  final_df$time_atkthird
)

# split data (80-20)
df_train <- final_df[1:245,]
df_test <- final_df[246:306,]


######### model 1 #########
# train model
model_lr <- lm(
  formula=ga_combined~total_attempts,
  data=df_train
)

# model statistics
summary(model_lr) # R2 = 0.2582 (0.2552)
coef(model_lr) # intercept = -0.0091 / total_attempts = 0.1824

# plot the model
coefs_model_lr <- coef(model_lr)

ggplot(
  data=df_train,
  aes(x=total_attempts, y=ga_combined)
) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_model_lr["total_attempts"],
    intercept=coefs_model_lr["(Intercept)"]
  ), color='red')

# plot model with residual
df_resid <- df_train
df_resid$predicted <- predict(model_lr)
df_resid$residuals <- residuals(model_lr)

ggplot(
  data=df_resid,
  aes(x=total_attempts, y=ga_combined)
) +
  geom_point(size=3) + 
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=total_attempts, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_model_lr["total_attempts"],
    intercept=coefs_model_lr["(Intercept)"]
  ), color='gray')

# diagnostic plots
plot(model_lr, which=1)

# make a prediction
df_test_temp <- df_test
df_test_temp$predicted <- predict(model_lr, newdata=df_test_temp)
df_test_temp$residuals <- df_test_temp$predicted - df_test_temp$ga_combined
df_test_temp

# test set with residual
ggplot(
  data=df_test_temp,
  aes(x=total_attempts,y=ga_combined)
) +
  geom_point(size=3) +
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=total_attempts, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_model_lr["total_attempts"],
    intercept=coefs_model_lr["(Intercept)"]
  ), color='gray')

# error
sse_dist <- sum(df_test_temp$residuals**2)
sse_dist # 6.2480


######### model 2 #########
# train model
model_mlr <- lm(
  formula=ga_combined~total_attempts+solorun_penarea+delivery_penarea+time_atkthird,
  data=df_train
)

# model statistics
summary(model_mlr) # R2 = 0.2754 (0.2633)
coef(model_mlr) # intercept = 0.0328 / total_attempts = 0.1866 / solorun_penarea = 0.0312 / delivery_penarea = 0.0514 / time_atkthird = -0.0001

# diagnostic plots
plot(model_mlr, which=1)

# make a prediction
df_test_temp <- df_test
df_test_temp$predicted <- predict(model_mlr, newdata=df_test_temp)
df_test_temp$residuals <- df_test_temp$predicted - df_test_temp$ga_combined
df_test_temp

# error
sse_dist <- sum(df_test_temp$residuals**2)
sse_dist # 6.5610



########################## clustering ##########################
######### cleaning data #########
# select the features that can distinguish attack and defense player
clustering_features <- left_join_result %>%
  select(full_name,
         Role,
         PlayedTime,
         Goals,
         Assists,
         `Total Attempts`,
         `Offsides`,
         `Tackles`,
         `Recovered balls`,
         `Fouls committed`,
         `Yellow cards`)

# convert type all column that are number from character to numeric
clustering_features <- clustering_features %>%
  mutate(across(c(Goals,
                  Assists,
                  `Total Attempts`, 
                  `Offsides`, 
                  `Tackles`, 
                  `Recovered balls`, 
                  `Fouls committed`,
                  `Yellow cards`), as.numeric))

# check missing value
colSums(is.na(clustering_features))

clustering_features %>%
  filter(if_any(everything(), is.na))

# drop missing values
clustering_features <- clustering_features %>%
  filter(if_all(everything(), ~ !is.na(.)))

# create goal contribution column - sum the number of goal and assist
clustering_features <- clustering_features %>%
  mutate(GACombined = Goals + Assists)

# aggregate stats for each player - actual value (sum) / avg or ratio (avg)
clustering_features <- clustering_features %>%
  group_by(full_name, Role) %>%
  summarise(
    played_time = sum(PlayedTime),
    ga_combined = sum(GACombined),
    total_attempts = sum(`Total Attempts`),
    offsides = sum(`Offsides`),
    tackles = sum(`Tackles`),
    recovered_balls = sum(`Recovered balls`),
    fouls_committed = sum(`Fouls committed`),
    yellow_cards = sum(`Yellow cards`)
  )

# convert metrics into per match (90 mins) (based on eda result)
clustering_features <- clustering_features %>%
  mutate(across(c(played_time, 
                  ga_combined,
                  total_attempts,
                  offsides,
                  tackles,
                  recovered_balls,
                  yellow_cards), ~ . * 90 * 60 / played_time))


######### building clustering model #########
# duplicate dataframe for storing clustering results
df_clustering_results <- clustering_features

# select only features
df_clustering <- clustering_features[, 4:10]

# normalize data
df_clustering <- scale(df_clustering)

# build model
set.seed(123)
kmeans_result <- kmeans(df_clustering, centers = 2, nstart = 25)
print(kmeans_result)

# results
print(kmeans_result$centers)
print(kmeans_result$cluster)
print(kmeans_result$size)

# add cluster number to dataset
df_clustering_results$Cluster <- as.factor(kmeans_result$cluster)


######### visualisation comparing metric between clusters #########
ggplot(df_clustering_results, aes(x = Cluster, y = recovered_balls, fill = Cluster)) +
  geom_boxplot() +
  labs(
    title = "Comparison of Recovered Balls Distribution Between Clusters",
    subtitle = "Finding of characteristics between two specific roles of midfielders",
    x = "Cluster",
    y = "Recovered Balls",
    fill = "Cluster",
    caption = "Data source: UEFA Euro 2020 Dataset by Mikhail Zhilkin\nhttps://data.world/cervus/uefa-euro-2020"
  ) +
  ylim(0, 10) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "#505050"),
    legend.position = 'top',
    legend.justification = 'left',
    legend.direction = 'horizontal'
  )


######### filtering player #########
# get only attacking player
df_attacking_player <- df_clustering_results %>%
  filter(!(Role == "midfielders" & Cluster == 1))

# filter player with defense instruction out of original dataframe
df_attacking_player <- semi_join(selected_features, df_attacking_player, by = "full_name")

# filter only player that have goal contribution (more than 0)
final_df <- df_attacking_player %>%
  filter(ga_combined != 0)



########################## conducting analyses ##########################
# split data (80-20)
df_train <- final_df[1:53,]
df_test <- final_df[54:66,]


######### model 1 #########
# train model
model_lr <- lm(
  formula=ga_combined~total_attempts,
  data=df_train
)

# model statistics
summary(model_lr) # R2 = 0.4690
coef(model_lr) # intercept = -0.1047 / total_attempts = 0.3764

# plot the model
coefs_model_lr <- coef(model_lr)

ggplot(
  data=df_train,
  aes(x=total_attempts, y=ga_combined)
) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_model_lr["total_attempts"],
    intercept=coefs_model_lr["(Intercept)"]
  ), color='red')

# plot model with residual
df_resid <- df_train
df_resid$predicted <- predict(model_lr)
df_resid$residuals <- residuals(model_lr)

ggplot(
  data=df_resid,
  aes(x=total_attempts, y=ga_combined)
) +
  geom_point(size=3) + 
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=total_attempts, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_model_lr["total_attempts"],
    intercept=coefs_model_lr["(Intercept)"]
  ), color='gray')

# diagnostic plots
plot(model_lr, which=1)

# make a prediction
df_test_temp <- df_test
df_test_temp$predicted <- predict(model_lr, newdata=df_test_temp)
df_test_temp$residuals <- df_test_temp$predicted - df_test_temp$ga_combined
df_test_temp

# test set with residual
ggplot(
  data=df_test_temp,
  aes(x=total_attempts,y=ga_combined)
) +
  geom_point(size=3) +
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=total_attempts, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_model_lr["total_attempts"],
    intercept=coefs_model_lr["(Intercept)"]
  ), color='gray')

# error
sse_dist <- sum(df_test_temp$residuals**2)
sse_dist # 1.9968


######### model 2 #########
# train model
model_mlr <- lm(
  formula=ga_combined~total_attempts+solorun_penarea+delivery_penarea+time_atkthird,
  data=df_train
)

# model statistics
summary(model_mlr) # R2 = 0.4907
coef(model_mlr) # intercept = 0.5764 / total_attempts = 0.3806 / solorun_penarea = 0.1272 / delivery_penarea = 0.0560 / time_atkthird = -0.0005

# diagnostic plots
plot(model_mlr, which=1)

# make a prediction
df_test_temp <- df_test
df_test_temp$predicted <- predict(model_mlr, newdata=df_test_temp)
df_test_temp$residuals <- df_test_temp$predicted - df_test_temp$ga_combined
df_test_temp

# error
sse_dist <- sum(df_test_temp$residuals**2)
sse_dist # 2.1199