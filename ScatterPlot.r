library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(ggmap)

df <- read.csv('BIG FIVE 1995-2019.csv')
df$winningTeam = NA
for (row in 1:nrow(df)) {
    Team1pts <- df[row, "Team.1..pts."]
    Team2pts  <- df[row, "Team.2..pts."]
    Team1 <- as.character(df[row, "Team.1"])
    Team2 <- as.character(df[row, "Team.2"])
    if(Team1pts == 3) {
        df[row, "winningTeam"] = Team1
    } else if(Team1pts == 1) {
        df[row, "winningTeam"] = "Draw"
    } else {
        df[row, "winningTeam"] = Team2
    }
}

df$losingTeam = NA
for (row in 1:nrow(df)) {
    Team1pts <- df[row, "Team.1..pts."]
    Team2pts  <- df[row, "Team.2..pts."]
    Team1 <- as.character(df[row, "Team.1"])
    Team2 <- as.character(df[row, "Team.2"])
    if(Team1pts == 0) {
        df[row, "losingTeam"] = Team1
    } else if(Team1pts == 1) {
        df[row, "losingTeam"] = "Draw"
    } else {
        df[row, "losingTeam"] = Team2
    }
}

average_win <- df %>% filter(winningTeam != "Draw") %>% group_by(Country, Year, winningTeam) %>% summarize(Average_Game_GD_Win = mean(GGD))
average_loss <- df %>% filter(winningTeam != "Draw") %>% group_by(Country, Year, losingTeam) %>% summarize(Average_Game_GD_Loss = mean(GGD))
names(average_win)[names(average_win) == "winningTeam"] <- "Team"
names(average_loss)[names(average_loss) == "losingTeam"] <- "Team"
combinedWinLoss <- merge(average_win,average_loss,by=c("Team", "Year", "Country"))
combinedWinLoss$totalGoalDifferential <- combinedWinLoss$Average_Game_GD_Win - combinedWinLoss$Average_Game_GD_Loss
top10season <- combinedWinLoss %>% group_by(Country, Year) %>% arrange(totalGoalDifferential) %>% tail(10)

#png(filename="BarcelonaTop.png")
ggplot(top10season, aes(x=Average_Game_GD_Loss, y=Average_Game_GD_Win, size=10)) +

geom_point(aes(colour=Team)) +

theme(legend.position="none") + geom_text(size=3, aes(label=Team)) + geom_text(size=3, aes(label=Year), vjust=2) +

ggtitle("Teams with the best average goal differential in a season since 1995") +

xlim(0.75,1.5) +

xlab("Avg Goal Differential in Losses") + ylab("Avg Goal Differential in Wins") +
theme(panel.background = element_rect(fill = 'gray90')) +
theme(panel.border = element_blank(),
panel.grid.major = element_blank(),
panel.grid.minor = element_blank())
