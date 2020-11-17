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

#combinedWinLoss %>% group_by(Year, Country) %>% arrange(Year, Country, totalGoalDifferential)

yearCountryMax = combinedWinLoss %>% group_by(Year, Country) %>% summarize(MaxGGD = max(totalGoalDifferential))
yearCountryMin = combinedWinLoss %>% group_by(Year, Country) %>% summarize(MinGGD = min(totalGoalDifferential))

maxMinByYear <- merge(yearCountryMax,yearCountryMin,by=c("Year", "Country"))

maxMinByYear$totalDiff = maxMinByYear$MaxGGD - maxMinByYear$MinGGD

# maxMinByYear %>%
#   ggplot( aes(x=Year, y=totalDiff, group=Year, color=Country)) +
#     geom_line()

png(filename="line-graph.png")
ggplot(data = maxMinByYear, aes(x = Year, y = totalDiff, color=Country))+
    geom_line(size=2) +
    scale_x_continuous(breaks = round(seq(1995, 2020, by = 2),2020)) +
    scale_y_continuous(breaks = round(seq(0, 5, by = 0.2),5)) +
    theme(panel.background = element_rect(fill = 'white')) +
    scale_colour_brewer(palette = "Paired") + theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ggtitle("Difference Between Best Team and Worst Team from 1995-2020") +
    xlab("Season") + ylab("Total Goal Difference")
dev.off()
