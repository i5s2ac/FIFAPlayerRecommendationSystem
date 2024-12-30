#CYO Project
#Isaac Cyrman Casafont
#3/22/2021

#Install all the required packages:
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

#load all the required packages:
library(dslabs)
library(broom)
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(caret)

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#########################################
# DOWNLOAD, UNZIP AND READ DATA PROCESS #
#########################################

temp <- tempfile()
download.file("https://github.com/Isaacj59/CYO-project/raw/main/FIFA_21.zip",temp)
FIFA2021 <- read_csv(unz(temp, "players_21.csv"))
unlink(temp)

#Show initial data set
show(FIFA2021)

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#################################  #In this process, we are going to create three data sets (PlayerInfo, PlayerCLubInfo and PlayerStats) with 
# DATA CLEANING, FILTERING AND  #   specific selected columns for future data analysis and modeling work. Also, for this process, we will have
#       SELECTING PROCESS       #   to filter the data by player positions in order to only have field players in the data sets and clean them               
#################################   omitting and replacing NA values.

#We are going to create a data set with columns that have to do with the player's information and filter it in order to only have field players.
PlayerInfo<- FIFA2021 %>% filter(!player_positions=="GK") %>% select(sofifa_id, short_name, age, dob, nationality, height_cm, weight_kg,player_positions,wage_eur,value_eur,overall)

#Show the data set.
show(PlayerInfo)

#See if there's any NA value in the data set.
NA_Values_PlayerInfo<-sum(is.na(PlayerInfo==TRUE))   
NA_Values_PlayerInfo  %>% knitr::kable()

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a data set with columns that have to do with the club's information and filter it in order to only have field players.
PlayerCLubInfo<- FIFA2021 %>% filter(!player_positions=="GK") %>% select(sofifa_id,short_name, club_name, league_name, league_rank, team_position, value_eur, wage_eur, release_clause_eur, player_positions,overall) 

#Change the class of the variables that at first were "dbl".
PlayerCLubInfo %>% mutate(club_name = as.character(PlayerCLubInfo$club_name),
                          long_name = as.character(PlayerCLubInfo$short_name),
                          league_name = as.character(PlayerCLubInfo$league_name),
                          sofifa_id= as.numeric(PlayerCLubInfo$sofifa_id),
                          league_rank= as.numeric(PlayerCLubInfo$league_rank),
                          value_eur= as.numeric(PlayerCLubInfo$value_eur),
                          wage_eur= as.numeric(PlayerCLubInfo$wage_eur),
                          release_clause_eur= as.numeric(PlayerCLubInfo$release_clause_eur))

#Omit all the NA values in the data set.
PlayerCLubInfo<-na.omit(PlayerCLubInfo)

#Check if there's any NA value in the data set.
NA_Values_PlayerClubInfo<-sum(is.na(PlayerCLubInfo==TRUE))   
NA_Values_PlayerClubInfo  %>% knitr::kable()

#Show the data set.
show(PlayerCLubInfo)

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a data set with columns that have to do with the player's stats information and filter it in order to only have field players.
PlayerStats<- FIFA2021 %>% filter(!player_positions=="GK") %>%  select(sofifa_id,short_name,potential,work_rate,international_reputation, overall, age, weak_foot, skill_moves, pace, shooting, passing, dribbling,
                                  defending, physic, attacking_crossing, attacking_finishing, attacking_heading_accuracy, attacking_short_passing, attacking_volleys, movement_acceleration,
                                  movement_sprint_speed, movement_agility, movement_reactions, movement_balance, power_shot_power, power_jumping, power_stamina, power_strength, power_long_shots,
                                  mentality_aggression, mentality_interceptions, mentality_positioning, mentality_composure, mentality_vision, mentality_penalties, defending_standing_tackle,
                                  defending_sliding_tackle,skill_curve,skill_moves,skill_dribbling,skill_fk_accuracy,skill_long_passing,skill_ball_control)

#Replace all the NA values for 0 in the data set. 
PlayerStats<-replace(PlayerStats, is.na(PlayerStats==TRUE), 0)

#See if there's any NA value in the data set.
NA_Values_PlayerStats<-sum(is.na(PlayerStats==TRUE))   
NA_Values_PlayerStats %>% knitr::kable()

#show the data set.
show(PlayerStats)

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#########################   #In this process, we are going to apply our management and visualization skills in the clean data sets that were created in
# DATA ANALYSIS PROCESS #   the *DATA CLEANING, FILTERING AND SELECTING PROCESS*. In addition, we will elaborate bar plots and tables in order to achieve a deeper 
#########################   comprehension of the data. (This process can be considered crucial for the modeling process).

#We are going to create a new data set, add the player ID and count them.
#This will show us the quantity of players that are in the data set.
Quantity_Players<- PlayerInfo %>% summarise(PlayerId=sofifa_id) %>% count()

#Show the data.
(Quantity_Players) %>% knitr::kable()

#We are going to create a new data set, group by player nationality and count them.
#This will show us the quantity of players nationalities that are in the data set.
Quantity_Nationality<- PlayerInfo %>% group_by(nationality) %>% count()

#We are going to create a new data set and select the first column of the data set above.
#This will show us the nationalities that we can find in the data set.
Nationalities_list<-Quantity_Nationality[1]

#We are going to create a new data set, order the values and select the first ten values.
#This will show us the top ten nationalities based on quantity.
top10_nationality_quantity<-Quantity_Nationality[order(-Quantity_Nationality$n),][1:10,]

#show the data.
(top10_nationality_quantity) %>% knitr::kable()

#We are going to create a bar plot for the top 10 nationalities.
ggplot(data=top10_nationality_quantity, aes(x=nationality, y=n)) +
  geom_bar(stat="identity", fill="light blue",color="black") +
  theme_minimal() + theme(panel.grid.major = element_blank(), 
                          panel.background = element_rect(fill = "light pink",
                          colour = "black", size = 0.5, linetype = "solid"), axis.line = element_line(colour = "black")) +  
                          geom_text(aes(label = signif(n, digits = 3)), nudge_y = -50,nudge_x = -0) + 
                                theme(axis.text.x = element_text(angle = 60, hjust = 1),
                                      axis.text.y = element_blank()) + 
                                ggtitle("Top 10 nationalities based on quantity") + xlab("") + ylab("Quantity of players")

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by age and count them.
#This will show us the quantity of players that have the same age in the data set.
Quantity_Age<- PlayerInfo %>% group_by(age) %>% count()
Quantity_Age %>% knitr::kable()

#We are going to create a bar plot to visualize the distribution of players ages in the data set.
ggplot(Quantity_Age, aes(age, n)) +
  geom_col(aes(fill = factor(age))) +theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "light gray",
  colour = "black",size = 0.2, linetype = "solid"), axis.line = element_line(colour = "black")) + 
  ggtitle("Age distribution") + xlab("Age") + ylab("Quantity")
#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by age and the players name added with summarise function.
#This will show us the age and name of every player in the data set.
Ages_all_players<-PlayerInfo %>% group_by(age) %>% summarise(Name=short_name)

#We are going to create a new data set, order the values and select the first ten values. 
#This will show us the top ten with more age players in the data set.
Top10_Players_Older<-Ages_all_players[order(-Ages_all_players$age),][1:10,]
#Show the data.
(Top10_Players_Older) %>% knitr::kable()

#We are going to create a new data set, order the values and select the first ten values. 
#This will show us the top ten with less age players in the data set.
Top10_Player_Youngest<-Ages_all_players[order(Ages_all_players$age),][1:10,]
#Show the data.
(Top10_Player_Youngest) %>% knitr::kable()

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set and select the players name, wage and value.
Players_wage_value<- PlayerInfo %>% select(short_name,wage_eur,value_eur)

#We are going to create a new data set, group by players wage and add the player name with summarize function.
#This will show us the wage and name of every player in the data set.
Wage_Players<- Players_wage_value %>% group_by(wage_eur) %>% summarize(Name=short_name)

#We are going to create a new data set, order the values and select the first ten values.
#This will show us the top ten players with best wage in the data set.
Top10_best_paid<-Wage_Players[order(-Wage_Players$wage_eur),][1:10,]
#show the data.
(Top10_best_paid) %>% knitr::kable()

#We are going to create a bar plot to visualize the top ten players with best wage in the data set.
ggplot(Top10_best_paid, aes(x=wage_eur, y=Name)) +
  geom_bar(stat="identity", fill="turquoise",colour="black") + 
  geom_text(aes(label = Name,wage_eur), nudge_y = 0,nudge_x = -55000) +
  theme_minimal() + theme(panel.grid.major = element_blank(), 
                          panel.background = element_rect(fill = "black",
                                                          colour = "black", size = 0.5, linetype = "solid"), 
                                                          axis.line = element_line(colour = "black")) + 
                                                          theme(axis.text.y = element_blank()) + 
                                                                ggtitle("Top 10 players with best wage") + xlab("Wage in euros") + ylab("Player name")

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by players value and add the players name with summarize function. 
#This will show us the name and value in euros of every player in the data set.
Value_Players<- Players_wage_value %>% group_by(value_eur) %>% summarize(Name=short_name)

#We are going to create a new data set, order values and select the ten first values.
#This will show us the top ten more valuable players in the data set.
Top10_more_valuable<-Value_Players[order(-Value_Players$value_eur),][1:10,]
#show the data.
Top10_more_valuable %>% knitr::kable()

#We are going to create a bar plot to visualize the top ten more valuable players in the data set.
ggplot(Top10_more_valuable, aes(x=value_eur, y=Name)) +
  geom_bar(stat="identity", fill="maroon",colour="black") + 
  geom_text(aes(label = Name), nudge_y = 0,nudge_x = -12220000,color="white") +
  theme_minimal() + theme(panel.grid.major = element_blank(), 
                          panel.background = element_rect(fill = "black",
                                                          colour = "black", size = 0.5, linetype = "solid"), 
                                                          axis.line = element_line(colour = "black")) + 
                                                            theme(axis.text.y = element_blank()) + 
                                                                  ggtitle("Top 10 more valuable players") + xlab("Value in euros") + ylab("Player name")

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by players height and add players name with summarize function.
#This will show us the height and name of every player in the data set.
Height_Players<-PlayerInfo %>% group_by(height_cm) %>% summarise(Name=short_name)

#We are going to create a new data set, order the values and select the first ten values.
#This will show us the top ten tallest players in the data set.
Top10_Tallest<-Height_Players[order(-Height_Players$height_cm),][1:10,]

#show the data.
Top10_Tallest %>% knitr::kable()

#We are going to create a new data set, group by players weight and add players name with summarize function. 
#This will show us the weight and name of every player in the data set.
Weight_Players<-PlayerInfo %>% group_by(weight_kg) %>% summarise(Name=short_name)

#We are going to create a new data set, order the values and select the first ten values.
#This will show us the top ten heaviest players in the data set.
Top10_Heavier<-Weight_Players[order(-Weight_Players$weight_kg),][1:10,]

#show the data.
Top10_Heavier %>% knitr::kable()

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We going to create a new data set and select the weight and height columns.
#This will show us the weight and height of every player in the data set.
CorWH<-PlayerInfo %>% select(weight_kg,height_cm)
#Here we are going to create, evaluated and plot a linear regression model for the following two variables: Height_cm and weight_kg. 
lm(height_cm~weight_kg,data=PlayerInfo)
ggplot(CorWH, aes(x = weight_kg, y = height_cm)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                   panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
                                                   ggtitle("Linear regression model: height and weight") + xlab("Weights in kg") + ylab("Height in cm")

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by club name, add the mean of the club wage with summarize function and extract the top ten values.
#This will show us the club name, mean wage of every club and the Top 10 clubs with the highest salaries.
Mean_Top10_club_wage<- PlayerCLubInfo %>% group_by(club_name) %>% summarise(Mean=mean(wage_eur)) %>% top_n(10)
#Show the data.
(Mean_Top10_club_wage) %>% knitr::kable()

#We are going to create a bar plot to visualize the top 10 best paying clubs in the data set.
ggplot(Mean_Top10_club_wage, aes(x=Mean, y=club_name)) +
  geom_col(aes(fill = club_name)) + geom_text(aes(label = signif(Mean)), nudge_y = 0,nudge_x = -14000,color="white") + 
                                        theme_minimal() + theme(panel.grid.major = element_blank(), 
                                        panel.background = element_rect(fill = "black",
                                        colour = "black", size = 0.5, linetype = "solid")) + 
                                        theme(axis.text.x = element_blank(),
                                              axis.text.y = element_blank()) +
                                            ggtitle("Top 10 clubs with the highest salaries") + xlab("Club wague in euros based on players wages") + ylab("")

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by club name, add the mean of the club value with summarize function and extract the top ten values.
#This will show us the club name, mean value of every club and the top ten more valuable clubs.
Mean_Top10_club_value<- PlayerCLubInfo %>% group_by(club_name) %>% summarise(Mean=mean(value_eur)) %>% top_n(10)
#show the data.
(Mean_Top10_club_value) %>% knitr::kable()

#We are going to create a bar plot to visualize the top 10 more valuable clubs in the data set.
ggplot(Mean_Top10_club_value, aes(x=Mean, y=club_name)) +
  geom_col(aes(fill = club_name)) + geom_text(aes(label = signif(Mean)), nudge_y = 0,nudge_x = -4000000, color="white") + 
                                          theme_minimal() + theme(panel.grid.major = element_blank(), 
                                                                  panel.background = element_rect(fill = "black",
                                                                                                  colour = "black", size = 0.5, linetype = "solid")) +
                                                                                                  theme(axis.text.x = element_blank(),
                                                                                                        axis.text.y = element_blank()) +
                                                                                                        ggtitle("Top 10 more valuable clubs") + xlab("Club value in euros based on players values") + ylab("")
#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by players overall and count them.
#This will show us the quantity of players that have the same overall in the data set.
Quantity_Overalls<- PlayerStats %>% group_by(overall) %>% count()
#show the data.
(Quantity_Overalls) %>% knitr::kable()

#We are going to create a bar plot to visualize the distribution of players overalls in the data set.
ggplot(Quantity_Overalls, aes(overall, n)) +
  geom_col(aes(fill = factor(overall))) +
  theme_minimal() + theme(panel.grid.major = element_blank(), 
                          panel.background = element_rect(fill = "light grey",
                                                          colour = "black", size = 0.5, linetype = "solid")) +
                                                                  ggtitle("Overall distribution") + xlab("Overall") + ylab("Quantity")
#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by players work rate and count them
#This will show us the quantity of players that have the same work rate in the data set.
Quantity_WorkRate<- PlayerStats %>% group_by(work_rate) %>% count()
#show the data.
Quantity_WorkRate %>% knitr::kable()

#We are going to create a bar plot to visualize the distribution of players work rate in the data set.
ggplot(Quantity_WorkRate, aes(x=work_rate, y=n)) +
  geom_bar(stat="identity", fill="steel blue",colour="black") + 
  geom_text(aes(label = signif(n)), nudge_y = 300,nudge_x = 0,color="black") +
  theme_minimal() + theme(panel.grid.major = element_blank(), 
                          panel.background = element_rect(fill = "light yellow",
                                                          colour = "black", size = 0.5, linetype = "solid"), 
                                                          axis.line = element_line(colour = "black"))+ 
                                                          theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text.y = element_blank())+
                                                          ggtitle("Distribution of players work rate") + xlab("Work Rate") + ylab("Quantity of players")

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by players overall and add players name with summarize function.
#This will show us the name and overall of every player in the data set.
Players_overall_name<- PlayerStats %>% group_by(overall) %>% summarise(Name=short_name)

#We are going to create a new data set, order the values and select the first ten values.
#This will show us the top ten players in overall terms.
Top10_Players<- Players_overall_name[order(-Players_overall_name$overall),][1:10,]
#Show data.
(Top10_Players) %>% knitr::kable()

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#Here we are going to create, evaluated and plot a linear regression model for the following two variables: overall and age. 
lm(overall ~ age, data= FIFA2021)
ggplot(PlayerStats, aes(x = age, y = overall, colour=factor(age))) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  ggtitle("Linear regression: players age and players overall") + xlab("Age") + ylab("Overall")

#---------------------------------------------------------------------------------------------------------------------------------------------------#
#We are going to create a new data set, group by players potential and add players name with summarize function. 
#This will show us the potential of every player in the data set.
stats_potential<-PlayerStats %>% group_by(potential) %>% summarise(Name=short_name)

#We are going to create a new data set, order the values and select the first ten values.
#This will show us the top ten players with more potential in the data set.
Top10_potential<-stats_potential[order(-stats_potential$potential),][1:10,]

#show the data.
Top10_potential %>% knitr::kable()

#---------------------------------------------------------------------------------------------------------------------------------------------------#
#We are going to create a new data set, group by club name and count them.
#This will show us the quantity of players that are joined in a same soccer club in the data set.
Quantity_PLayers_Clubs<- PlayerCLubInfo %>% group_by(club_name) %>% count()
#show an example of how data looks.
(Quantity_PLayers_Clubs)[1:10,] %>% knitr::kable()

#We are going to create a new data set and select the first column of the data set above.
#This will show us a list of all the clubs that are in the data set.
Clubs_names<-Quantity_PLayers_Clubs[1]

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by league rank and count them.
#This will show us the quantity of players that are part of a same soccer league rank in the data set.
Quantity_LeagueRanks<-PlayerCLubInfo %>% group_by(league_rank) %>% count()
#show the data.
Quantity_LeagueRanks %>% knitr::kable()

#We are going to create a bar plot to visualize the distribution of players in every league rank.
ggplot(Quantity_LeagueRanks,aes(league_rank,n)) + geom_col(aes(fill=factor(league_rank))) + 
  geom_text(aes(label = signif(n)), nudge_y = 350,nudge_x = 0) + 
  theme_minimal() + theme(panel.grid.major = element_blank(), 
                          panel.background = element_rect(fill = "white",
                                                          colour = "black", size = 0.5, linetype = "solid")) + 
                                                          theme(axis.text.y = element_blank(),) +
                                                                ggtitle("League rank distribution") + xlab("League rank") + ylab("Quantity of players")

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by team position and count them.
#This will show us the quantity of players that have the same team position in the data set.
Quantity_TeamPosition<- PlayerCLubInfo %>% group_by(team_position) %>% count()
#show the data.
(Quantity_TeamPosition) %>% knitr::kable()

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a data set, add the leagues names with summarize function and count them.
#This will show us the quantity of leagues that are in the data set.
Quantity_leagues<- PlayerCLubInfo %>% summarise(LeagueName=league_name) %>% count()

#We are going to create a new data set, group by league name and count them.
#This will show us the quantity of players that are joined in the same league.
Quantity_Players_Leagues<- PlayerCLubInfo %>% group_by(league_name) %>% count()

#We are going to create a new data set and select the first column of the data set above. 
#This will show us a list of all the league that are in the data set.
Leagues_name<- Quantity_Players_Leagues[1]

#We are going to create a new data set, order the values and select the ten first values.
#This will show us the top ten leagues in terms of players quantity.
Top10_Leagues<- Quantity_Players_Leagues[order(-Quantity_Players_Leagues$n),][1:10,]
#show the data.
Top10_Leagues %>% knitr::kable()

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by club name, add the mean of the players overall with summarize function and extract the top ten values.
#This will show us the top ten clubs in terms of overall mean.
Mean_Top10_clubs<- PlayerCLubInfo %>% group_by(club_name) %>% summarise(Mean=mean(overall)) %>% top_n(10)
#show the data.
Mean_Top10_clubs %>% knitr::kable()

#We are going to create a bar plot to visualize the top ten clubs based on overall in the data set.
ggplot(Mean_Top10_clubs, aes(x=Mean, y=club_name)) +
  geom_bar(stat="identity", fill="purple",colour="black")  +  
  theme_minimal() + theme(panel.grid.major = element_blank(), 
  panel.background = element_rect(fill = "maroon",
  colour = "black", size = 0.5, linetype = "solid")) + 
  geom_text(aes(y = club_name,label = club_name), nudge_y = 0,nudge_x = -10, color = "white") +
  theme(axis.text.y = element_blank()) +
        ggtitle("Top 10 clubs based on overall") + xlab("Overall mean") + ylab("Club Name")

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by league name, add the mean of the league wage with summarize function and extract the top ten values.
#This will show us the Top ten leagues with the highest salaries in euros.
Mean_Top10_leagues_wage<- PlayerCLubInfo %>% group_by(league_name) %>% summarise(Mean=mean(wage_eur)) %>% top_n(10)
#show the data.
Mean_Top10_leagues_wage %>% knitr::kable()

#We are going to create a bar plot to visualize the top 10 best paying leagues in the data set.
ggplot(Mean_Top10_leagues_wage, aes(x=Mean, y=league_name)) +
  geom_col(aes(fill = league_name))  +  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
  panel.background = element_rect(fill = "black",
  colour = "black", size = 0.5, linetype = "solid")) + 
  geom_text(aes(label = signif(Mean, digits = 3)), nudge_y = 0,nudge_x = -5000, color = "white") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
        ggtitle("Top 10 leagues with the highest salaries") + xlab("Wage based on the mean of the players wages in euros") + ylab("")

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, group by league name, add the mean of the league value with summarize function and extract the top ten values.
#This will show us the top ten leagues in terms of value in euros.
Mean_Top10_leagues_value<- PlayerCLubInfo %>% group_by(league_name) %>% summarise(Mean=mean(value_eur)) %>% top_n(10)
#Show the data.
Mean_Top10_leagues_value %>% knitr::kable()

#We are going to create a bar plot to visualize the top 10 more valuable leagues in the data set.
ggplot(Mean_Top10_leagues_value, aes(x=Mean, y=league_name)) +
  geom_col(aes(fill = league_name))  +  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "black",
                                        colour = "black", size = 0.5, linetype = "solid")) + 
                                        geom_text(aes(label = signif(Mean, digits = 3)), nudge_y = 0,nudge_x = -1300000, color = "white") +
                                        theme(axis.text.x = element_blank(),
                                              axis.text.y = element_blank(),) +
                                              ggtitle("Top 10 more valuable leagues") + xlab("Value based on the mean of the players values in euros") + ylab("")


#---------------------------------------------------------------------------------------------------------------------------------------------------#

######################################
# CREATE FIFA SET (TRAINING SET) AND #
#     VALIDATION SET (TEST SET)      #
######################################

#Validation set will be 40% of FIFA2021 data.
set.seed(123, sample.kind="Rounding") 
test_index <- createDataPartition(y = PlayerStats$overall, times = 1, p = 0.40, list = FALSE)
FIFA <- PlayerStats[-test_index,]
validation_set <- PlayerStats[test_index,]

#Here we have to make sure sofifa_id in validation set are also in FIFA set.
validation <- validation_set %>% 
  semi_join(validation_set, by = "sofifa_id")

#Add rows removed from validation set back into FIFA set
removed <- anti_join(validation_set, validation)
FIFA <- rbind(FIFA, removed)

#Remove non-wanted data sets
rm(test_index, validation_set, removed)

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#Check if there's any NA value in the train set.
NA_Values_FIFA<-colSums(is.na(FIFA==TRUE))   
NA_Values_FIFA %>% knitr::kable()

#Show the train set.
FIFA

#Check if there's any NA value in the test set.
NA_Values_Validation<-colSums(is.na(validation==TRUE))   
NA_Values_Validation %>% knitr::kable()

#Show the validation set.
validation

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, do a function and make correlations for all the variables in the FIFA data set.
Correlation <- function(data=FIFA,sig=0.1){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  FIFA_Correlation <- FIFA %>% mutate_if(is.character, as.factor)
  FIFA_Correlation <- FIFA_Correlation %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(FIFA_Correlation)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  matrix_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  #plot correlations visually
  corrplot(matrix_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}

Correlation()

#---------------------------------------------------------------------------------------------------------------------------------------------------#

######################   #In this process, we are going to train and build different types of models in order to create predictions for the FIFA 2022
#  MODELING PROCESS  #   players overall. The purpose of this process is to achieve the most accurate predictions using the RMSE as our evaluation
######################   metric and demonstrate data visualization, analytics, training and precision skills. All models will be retrieved from the caret package.

#---------------------------------------------------------------------------------------------------------------------------------------------------#

###########################
# LINEAR REGRESSION MODEL #
###########################

#We are going to create a linear regression model with the function lm().
#Use overall as the variable that will be predicted and use the rest of the variables as contributors for the prediction.
#Add our train data for the model.
LinearRegression_Model<-lm(overall~  age + international_reputation+ weak_foot + skill_moves + pace + shooting + passing + dribbling +
                        defending + physic + attacking_crossing + attacking_finishing + attacking_heading_accuracy + attacking_short_passing + attacking_volleys + movement_acceleration +
                        movement_sprint_speed + movement_agility + movement_reactions + movement_balance + power_shot_power + power_jumping + power_stamina +
                        power_strength + power_long_shots + mentality_aggression + mentality_interceptions + mentality_positioning + mentality_composure + mentality_vision +
                        mentality_penalties + defending_standing_tackle + defending_sliding_tackle+ skill_curve + skill_moves + skill_dribbling + skill_fk_accuracy + skill_long_passing
                        + skill_ball_control,
                        #We are going to use FIFA data set for training the data.
                        data=FIFA)

#Show an executive summary of the model.
Summary<-summary(LinearRegression_Model) 

#We are going to create a new data set, use predict() function, add our model and evaluate in the test set and set a confidence interval.
#This will create predictions for our linear regression model. 
Prediction_Linear_Regression<-predict(LinearRegression_Model, validation,interval="confidence")
#Plot the prediction results.

#We are going to create a new data set, use cbind() function and generate an outcome with test set and the prediction of the linear regression model.
Outcome_linear_regression<-cbind(validation, Prediction_Linear_Regression)

#Plot the predictions - this one will show us the distribution of predicted overalls.
ggplot(Outcome_linear_regression,aes(x=1:6746,y=fit)) +geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Distribution of predicted overalls: Linear Regression") + ylab("Predited overall")+ xlab("Players index")  

#We are going to use confint() function in order to see the confidence intervals.
Confint_values<-confint(LinearRegression_Model)

#Here we are going to create, evaluated and plot a linear regression model for the following two variables: 
#overall(actual values) and fit(predicted values). 
lm(fit~overall,data=Outcome_linear_regression) 
ggplot(Outcome_linear_regression, aes(x = overall, y =fit, xmin = lwr, xmax = upr,color = factor(overall))) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") + geom_ribbon(alpha = 0.2, color = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
        ggtitle("Linear Regression (lm): Overall (Predicted) vs. Overall (Actual)") + xlab("Overall") + ylab("Predicted overall")

#We are going to evaluate our linear regression model with the RMSE as our metric.
Linear_regression_RMSE<-RMSE(validation$overall,Prediction_Linear_Regression)
#Show the RMSE.
Linear_regression_RMSE %>% knitr::kable()

#We are going to show the main four graphs of our model.
par(mfrow=c(2,2))
plot(LinearRegression_Model, which=1) #Residuals vs. Fitted
plot(LinearRegression_Model, which=2) #Normal Q-Q
plot(LinearRegression_Model, which=3) #Scale-Location
plot(LinearRegression_Model, which=5) #Residuals vs. Leverage

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#######################
# RANDOM FOREST MODEL #
#######################

#We are going create a trControl with the best parameters for the Random Forest model.
trControlRF <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

#We are going to create a Random Forest model with the function train().
#Use overall as the variable that will be predicted and use the rest of the variables as contributors for the prediction.
#Add our train data, train control parameter define above, our metric and method for the model.
RandomForest_Model <- train(overall~  age + international_reputation+ weak_foot + skill_moves + pace + shooting + passing + dribbling +
                        defending + physic + attacking_crossing + attacking_finishing + attacking_heading_accuracy + attacking_short_passing + attacking_volleys + movement_acceleration +
                        movement_sprint_speed + movement_agility + movement_reactions + movement_balance + power_shot_power + power_jumping + power_stamina +
                        power_strength + power_long_shots + mentality_aggression + mentality_interceptions + mentality_positioning + mentality_composure + mentality_vision +
                        mentality_penalties + defending_standing_tackle + defending_sliding_tackle+ skill_curve + skill_moves + skill_dribbling + skill_fk_accuracy + skill_long_passing
                        + skill_ball_control,
                        data = FIFA,
                        method = "rf",
                        metric="RMSE",
                        trControl = trControlRF)
#show the results.
show(RandomForest_Model)
#Plot the results.
plot(RandomForest_Model,xlab="Randomly Selected Predictors (mtry Values)", main="Random Forest Results")

#We are going to create a new data set and store the best tune obtained in the model.
Best_mtry<-RandomForest_Model$bestTune$mtry
#We are going to create a new data set and store the best RMSE obtained in the model.
Best_RMSE<-min(RandomForest_Model$results$RMSE)
#show the best RMSE result.
(Best_RMSE) %>% knitr::kable()

#We are going to create a new data set, use predict() function, add our model and evaluate in the test set and set a prediction interval.
#This will create predictions for our random forest model. 
Prediction_RandomForest<-predict(RandomForest_Model, validation,interval="prediction")

#We are going to create a new data set, use cbind() function and generate an outcome with test set and the prediction of the random forest model.
Outcome_RandomForest<-cbind(validation, Prediction_RandomForest)

#Plot the predictions - this one will show us the distribution of predicted overalls.
ggplot(Outcome_RandomForest,aes(x=1:6746,y=Prediction_RandomForest)) +geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Distribution of predicted overalls: Random Forest") + ylab("Predited overall")+ xlab("Players index")  

#Here we are going to create, evaluated and plot a linear regression model (RF) for the following two variables: 
#overall(actual values) and fit(predicted values). 
lm(Prediction_RandomForest~overall,data=Outcome_RandomForest) 
ggplot(Outcome_RandomForest, aes(x = overall, y = Prediction_RandomForest,color = factor(overall))) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
        ggtitle("Linear Regression (RF): Overall (Predicted) vs. Overall (Actual)") + xlab("Overall") + ylab("Predicted overall")

#We are going to evaluate our Random Forest model with the RMSE as our metric.
RF_RMSE<-RMSE(validation$overall,Prediction_RandomForest)
#Show the RMSE.
RF_RMSE %>% knitr::kable()

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#############
# KNN MODEL #
#############

#We are going create a trControl with the best parameters for the Knn model.
trControlKnn <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

#We are going to create a Knn model with the function train().
#Use overall as the variable that will be predicted and use the rest of the variables as contributors for the prediction.
#Add our train data, train control parameter define above, tuneLength parameter, our metric and method for the model.
Knn_Model <- train(overall~  age + international_reputation+ weak_foot + skill_moves + pace + shooting + passing + dribbling +
                        defending + physic + attacking_crossing + attacking_finishing + attacking_heading_accuracy + attacking_short_passing + attacking_volleys + movement_acceleration +
                        movement_sprint_speed + movement_agility + movement_reactions + movement_balance + power_shot_power + power_jumping + power_stamina +
                        power_strength + power_long_shots + mentality_aggression + mentality_interceptions + mentality_positioning + mentality_composure + mentality_vision +
                        mentality_penalties + defending_standing_tackle + defending_sliding_tackle+ skill_curve + skill_moves + skill_dribbling + skill_fk_accuracy + skill_long_passing
                        + skill_ball_control,
                        data = FIFA,
                        method = "knn",
                        metric="RMSE",
                        tuneLength = 20,
                        trControl = trControlKnn)

#show the results.
show(Knn_Model)
#Plot the results - RMSE vs. K-values.
ggplot(Knn_Model,aes(x=k,y=Knn_Model$results$RMSE)) + geom_point(size=2) +
                      theme(axis.line = element_line(colour = "black")) +
                      ggtitle("Knn Model:")  + xlab("K-Values")

#We are going to create a new data set, use predict() function, add our model and evaluate in the test set and set a prediction interval.
#This will create predictions for our Knn model. 
Prediction_Knn<-predict(Knn_Model, validation,interval="prediction") 

#We are going to create a new data set, use cbind() function and generate an outcome with test set and the prediction of the Knn model.
Outcome_Knn<-cbind(validation, Prediction_Knn)

#Plot the predictions - this one will show us the distribution of predicted overalls.
ggplot(Outcome_Knn,aes(x=1:6746,y=Prediction_Knn)) +geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Distribution of predicted overalls: Knn") + ylab("Predited overall")+ xlab("Players index")  

#Here we are going to create, evaluated and plot a linear regression model (Knn) for the following two variables: 
#overall(actual values) and fit (predicted values). 
lm(Prediction_Knn~overall,data=Outcome_Knn) 
ggplot(Outcome_Knn, aes(x = overall, y = Prediction_Knn,color = factor(overall))) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
        ggtitle("Linear Regression (Knn): Overall (Predicted) vs. Overall (Actual)") + xlab("Overall") + ylab("Predicted overall")

#We are going to evaluate our Knn model with the RMSE as our metric.
Knn_RMSE<-RMSE(validation$overall,Prediction_Knn)
#Show the RMSE.
(Knn_RMSE) %>% knitr::kable()

#---------------------------------------------------------------------------------------------------------------------------------------------------#

##########################
# RIDGE REGRESSION MODEL #
##########################

#We are going create a trControl with the best parameters for the Ridge Regression model.
trControlRigde <- trainControl("cv", number = 10)

#We are going to create a lambdas with the best parameters for the Ridge Regression model.
lambda <- 10^seq(-3, 3, length = 100)

#We are going to create a Ridge Regression model with the function train().
#Use overall as the variable that will be predicted and use the rest of the variables as contributors for the prediction.
#Add our train data, train control parameter define above, tuneGrid parameter, our metric and method for the model.
Ridge_Model<-train(overall~  age + international_reputation+ weak_foot + skill_moves + pace + shooting + passing + dribbling +
                             defending + physic + attacking_crossing + attacking_finishing + attacking_heading_accuracy + attacking_short_passing + attacking_volleys + movement_acceleration +
                             movement_sprint_speed + movement_agility + movement_reactions + movement_balance + power_shot_power + power_jumping + power_stamina +
                             power_strength + power_long_shots + mentality_aggression + mentality_interceptions + mentality_positioning + mentality_composure + mentality_vision +
                             mentality_penalties + defending_standing_tackle + defending_sliding_tackle+ skill_curve + skill_moves + skill_dribbling + skill_fk_accuracy + skill_long_passing
                             + skill_ball_control,
                             data=FIFA,
                             method="glmnet",
                             trControl=trControlRigde,
                             tuneGrid = expand.grid(alpha = 0, lambda = lambda))

#Show the results.
show(Ridge_Model)
#Plot the results - RMSE vs. Lambdas.
ggplot(Ridge_Model,aes(x=lambda,y=Ridge_Model$results$RMSE)) + geom_point(size=2) +
       ggtitle("Ridge regression Model:") + xlab("Lambdas")


#We are going to create a new data set, use predict() function, add our model and evaluate in the test set and set a prediction interval.
#This will create predictions for our Ridge Regression model. 
Prediction_Ridge<-predict(Ridge_Model, validation,interval="prediction")

#We are going to create a new data set, use cbind() function and generate an outcome with test set and the prediction of the Ridge Regression model.
Outcome_Ridge<-cbind(validation, Prediction_Ridge)

#Plot the predictions - this one will show us the distribution of predicted overalls.
ggplot(Outcome_Ridge,aes(x=1:6746,y=Prediction_Ridge)) +geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Distribution of predicted overalls: Ridge Regression") + ylab("Predited overall")+ xlab("Players index")  


#Here we are going to create, evaluated and plot a linear regression model (Ridge) for the following two variables: 
#overall(actual values) and fit (predicted values). 
Ridge_corr<-lm(Prediction_Ridge~overall,data=Outcome_Ridge) 
ggplot(Outcome_Ridge, aes(x = overall, y = Prediction_Ridge,color = factor(overall))) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
        ggtitle("Linear Regression (Ridge): Overall (Predicted) vs. Overall (Actual)") + xlab("Overall") + ylab("Predicted overall")

#We are going to evaluate our Ridge Regression model with the RMSE as our metric.
Ridge_RMSE<-RMSE(validation$overall,Prediction_Ridge)
#Show the RMSE.
(Ridge_RMSE) %>% knitr::kable()

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#############
# GBM MODEL #
#############

#We are going create a trControl with the best parameters for the Gbm model.
trControlGBM<- trainControl(method = "repeatedcv",
                               number = 10,
                               repeats=10)

#We are going create a Grid with the best parameters for the Gbm model.
GridGBM <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

#We are going to set seed in 825 in order to generate a sequence of random numbers.
set.seed(825)
#We are going to create a Gbm model with the function train().
#Use overall as the variable that will be predicted and use the rest of the variables as contributors for the prediction.
#Add our train data, train control parameter define above, grid parameter, our metric and method for the model.
GBM_Model <- train(overall~  age + international_reputation+ weak_foot + skill_moves + pace + shooting + passing + dribbling +
                   defending + physic + attacking_crossing + attacking_finishing + attacking_heading_accuracy + attacking_short_passing + attacking_volleys + movement_acceleration +
                   movement_sprint_speed + movement_agility + movement_reactions + movement_balance + power_shot_power + power_jumping + power_stamina +
                   power_strength + power_long_shots + mentality_aggression + mentality_interceptions + mentality_positioning + mentality_composure + mentality_vision +
                   mentality_penalties + defending_standing_tackle + defending_sliding_tackle+ skill_curve + skill_moves + skill_dribbling + skill_fk_accuracy + skill_long_passing
                  + skill_ball_control, data = FIFA, 
                   method = "gbm", 
                   trControl = trControlGBM,
                   ## This last option is actually one
                   ## for gbm() that passes through
                   verbose = FALSE,
                   tuneGrid = GridGBM)

#Show the results.
GBM_Model 
#Plot the results - RMSE vs. n trees.
ggplot(GBM_Model,aes(n.trees,GBM_Model$results$RMSE)) + geom_point(size=2) +
  theme(axis.line = element_line(colour = "black"), 
        panel.background = element_rect(fill = "white")) +
        ggtitle("Gbm Model Results") + xlab("n trees")


#We are going to create a new data set, use predict() function, add our model and evaluate in the test set.
#This will create predictions for our Gbm model. 
Prediction_gbm<-predict(GBM_Model, validation)

#We are going to create a new data set, use cbind() function and generate an outcome with test set and the prediction of the Gbm model.
Outcome_gbm<-cbind(validation, Prediction_gbm)

#Plot the predictions - this one will show us the distribution of predicted overalls.
ggplot(Outcome_gbm,aes(x=1:6746,y=Prediction_gbm)) +geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Distribution of predicted overalls: Gbm") + ylab("Predited overall")+ xlab("Players index")  


#Here we are going to create, evaluated and plot a linear regression model (Gbm) for the following two variables: 
#overall(actual values) and fit (predicted values). 
Gbm_corr<-lm(Prediction_gbm~overall,data=Outcome_gbm) 
ggplot(Outcome_gbm, aes(x = overall, y = Prediction_gbm,color = factor(overall))) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
        ggtitle("Linear Regression (Gbm): Overall (Predicted) vs. Overall (Actual)") + xlab("Overall") + ylab("Predicted overall")

#We are going to evaluate our Gbm model with the RMSE as our metric.
GBM_RMSE<-RMSE(validation$overall,Prediction_gbm)
#Show the RMSE.
(GBM_RMSE) %>% knitr::kable()

#---------------------------------------------------------------------------------------------------------------------------------------------------#

#We are going to create a new data set, add models name and RMSE results of all the models into a data frame.
RESULTS<- data.frame(
  Model = c("Linear Regression","Random Forest","Knn", "Ridge Regression","Gbm"),
  RMSE= c( Linear_regression_RMSE, RF_RMSE, Knn_RMSE, Ridge_RMSE, GBM_RMSE))

RESULTS %>% knitr::kable() 
  
  
  

