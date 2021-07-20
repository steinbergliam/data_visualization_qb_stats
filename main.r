##### Load data #####
library(readr)
library(tidyverse)

passer <- read.csv("<QBdata>")
# View(passer)

##### Omit missing values / Group data #####

passer <- na.omit(passer)
passer$deepPass <- c(passer$passLength > 20)

    # To use this dataset we need to do a lot of cleanup, as each observation
    # is only one pass and the data spans decades. Therefore first I am
    # grouping the data by playerId, then combining all summary stats (plus
    # some new variables) in a new data frame, "qbstats", then I filter by
    # pass attempts to exclude low outliers.

qbstats <- passer %>% 
  group_by(playerId) %>% 
  summarize(att = sum(passAtt, na.rm = T), 
            comp = sum(passComp, na.rm = T),
            compPercent = round(comp/att*100,2),
            tds = sum(passTd, na.rm = T),
            ints = sum(passInt, na.rm = T),
            tdIntRatio = round(tds/ints,2),
            sacks = sum(passSack, na.rm = T),
            aveLength = round(mean(passLength, na.rm = T),2),
            longPasses = sum(deepPass, na.rm = T),
            ) %>% 
    filter(att >= 500)

  # From here on out I will exclusively use the "qbstats" data frame 
  # instead of "passer".

# View(qbstats)


##### Visualizations #####

# 1. Touchdowns to Interceptions

qbstats %>% 
  ggplot(aes(x = tds, y = ints)) + 
  geom_point() + ggtitle("QB TDs vs Interceptions") + 
  scale_x_log10() + scale_y_log10()
  # Each dot on this scatterplot represents one of the 207 qualifying QBs 
  # in relation to how many touchdowns and interceptions they've thrown in
  # the last 16 years. The graph supports the common theory that throwing 
  # more touchdown passes is correlated to throwing more interceptions.


# 2. Average Air-yards per Pass to Completion %

qbstats %>% 
  ggplot(aes(x = aveLength, y = compPercent)) + 
  geom_smooth()
  # This line graph shows the relationship between depth of target and 
  # completion percentage. While I expected longer passes to result in a 
  # lower completion percentage, I did not expect the data to portray a bell
  # curve, specifically for short passes. One possible explanation for this
  # is passes knocked down at the line of scrimmage or thrown away by the QB.


# 3. Are QBs that are above average in TD-Int ratio typically above average
#    in completion % and/or average pass length as well? 

qbstats$goodTdInt <- c(qbstats$tdIntRatio > mean(qbstats$tdIntRatio))
qbstats$goodCompPer <- c(qbstats$compPercent > mean(qbstats$compPercent))

qbstats %>% 
  ggplot(aes(x = aveLength, y = compPercent, color = goodTdInt)) +
  geom_point() + facet_wrap(~goodTdInt)
  # Most QBs that have good TD-Int ratios are above average in both of 
  # the other two stats.


# 4. Do sacks affect TD-Int ratio?

qbstats %>% 
  ggplot(aes(x = sacks, y = tdIntRatio, color = goodTdInt)) + 
  geom_point() + geom_smooth(method = "lm", color = "black")
  # The graph shows a positive correlation between sacks and TD-Int ratio.
  # This leads me to the conclusion that good QBs (high TD-Int ratio) get 
  # more playing time, and therefore more sacks.


# 5. How do QBs with good completion %'s relate in terms of pass
#    attempts and touchdown passes?

qbstats %>% 
  ggplot(aes(x = att, y = tds, color = goodCompPer)) +
  geom_point() + scale_x_log10() + scale_y_log10() +
  geom_smooth(color = "black")
  # Players with good completion %'s typically have more TDs and 
  # pass attempts as compared with players with bad (below ave) 
  # completion %'s, though the difference isn't as striking as some 
  # of the other graphs. The correlation between the 3 variables
  # is obvious, but since we are comparing totals instead of averages,
  # the numbers will obviously favor good QBs who have played a lot of 
  # games.
