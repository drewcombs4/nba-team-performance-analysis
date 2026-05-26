###################################################
### NBA Team Performance & Shooting Efficiency  ###
### Fixed Effects Regression Analysis           ###
### Author: Drew Combs                          ###
###################################################

####################
# PACKAGE SETUP
####################

setwd("~/Desktop/nba-analytics-project")

required_packages <- c(
  "car",
  "readxl",
  "writexl",
  "ggplot2",
  "ggimage",
  "dplyr"
)

installed_packages <- rownames(installed.packages())

for(pkg in required_packages){
  if(!(pkg %in% installed_packages)){
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

####################
# LOAD DATA
####################

NBA_Data <- read_excel("data/NBA_DATA_2014-2024.xlsx")

####################
# DATA PREPARATION
####################

NBA_Data <- NBA_Data %>%
  mutate(
    
    # Weighted Efficiency Variables
    Weighted3PEff = `3P%` * `3PA`,
    Weighted2PEff = `2P%` * `2PA`,
    WeightedFTEff = `FT%` * FTA,
    
    # Demeaned Variables by Year
    Weighted3PEff_Demeaned =
      Weighted3PEff - ave(Weighted3PEff, Year, FUN = mean),
    
    TwoPointEff_Demeaned =
      Weighted2PEff - ave(Weighted2PEff, Year, FUN = mean),
    
    FTEff_Demeaned =
      WeightedFTEff - ave(WeightedFTEff, Year, FUN = mean),
    
    AST_Demeaned =
      AST - ave(AST, Year, FUN = mean),
    
    TOV_Demeaned =
      TOV - ave(TOV, Year, FUN = mean),
    
    ORB_Demeaned =
      ORB - ave(ORB, Year, FUN = mean),
    
    STL_Demeaned =
      STL - ave(STL, Year, FUN = mean),
    
    BLK_Demeaned =
      BLK - ave(BLK, Year, FUN = mean),
    
    DRB_Demeaned =
      DRB - ave(DRB, Year, FUN = mean),
    
    PF_Demeaned =
      PF - ave(PF, Year, FUN = mean)
    
  )

####################
# FIXED EFFECTS MODEL
####################

nba_fixed_effects_model <- lm(
  
  W ~
    Weighted3PEff_Demeaned +
    TwoPointEff_Demeaned +
    FTEff_Demeaned +
    TOV_Demeaned +
    ORB_Demeaned +
    AST_Demeaned +
    STL_Demeaned +
    BLK_Demeaned +
    DRB_Demeaned +
    PF_Demeaned +
    factor(Team),
  
  data = NBA_Data
  
)

####################
# MODEL SUMMARY
####################

summary(nba_fixed_effects_model)

####################
# EXPORT REGRESSION TABLE
####################

regression_output <- as.data.frame(
  summary(nba_fixed_effects_model)$coefficients
)

write.csv(
  regression_output,
  "outputs/regression_coefficients.csv"
)

####################
# MULTICOLLINEARITY TEST
####################

vif_results <- vif(nba_fixed_effects_model)

write.csv(
  vif_results,
  "outputs/vif_results.csv"
)

####################
# RESIDUAL DIAGNOSTICS
####################

pdf("outputs/residual_diagnostics.pdf")

par(mfrow = c(2,2))
plot(nba_fixed_effects_model)

dev.off()

####################
# TEAM LOGOS
####################

NBA_Data$Year <- as.factor(NBA_Data$Year)

team_logo_urls <- c(
  
  "Atlanta Hawks" = "https://a.espncdn.com/i/teamlogos/nba/500/atl.png",
  "Boston Celtics" = "https://a.espncdn.com/i/teamlogos/nba/500/bos.png",
  "Brooklyn Nets" = "https://a.espncdn.com/i/teamlogos/nba/500/bkn.png",
  "Charlotte Hornets" = "https://a.espncdn.com/i/teamlogos/nba/500/cha.png",
  "Charlotte Bobcats" = "assets/Charlotte_Bobcats.png",
  "Chicago Bulls" = "https://a.espncdn.com/i/teamlogos/nba/500/chi.png",
  "Cleveland Cavaliers" = "https://a.espncdn.com/i/teamlogos/nba/500/cle.png",
  "Dallas Mavericks" = "https://a.espncdn.com/i/teamlogos/nba/500/dal.png",
  "Denver Nuggets" = "https://a.espncdn.com/i/teamlogos/nba/500/den.png",
  "Detroit Pistons" = "https://a.espncdn.com/i/teamlogos/nba/500/det.png",
  "Golden State Warriors" = "https://a.espncdn.com/i/teamlogos/nba/500/gs.png",
  "Houston Rockets" = "https://a.espncdn.com/i/teamlogos/nba/500/hou.png",
  "Indiana Pacers" = "https://a.espncdn.com/i/teamlogos/nba/500/ind.png",
  "Los Angeles Clippers" = "https://a.espncdn.com/i/teamlogos/nba/500/lac.png",
  "Los Angeles Lakers" = "https://a.espncdn.com/i/teamlogos/nba/500/lal.png",
  "Memphis Grizzlies" = "https://a.espncdn.com/i/teamlogos/nba/500/mem.png",
  "Miami Heat" = "https://a.espncdn.com/i/teamlogos/nba/500/mia.png",
  "Milwaukee Bucks" = "https://a.espncdn.com/i/teamlogos/nba/500/mil.png",
  "Minnesota Timberwolves" = "https://a.espncdn.com/i/teamlogos/nba/500/min.png",
  "New Orleans Pelicans" = "https://a.espncdn.com/i/teamlogos/nba/500/no.png",
  "New York Knicks" = "https://a.espncdn.com/i/teamlogos/nba/500/ny.png",
  "Oklahoma City Thunder" = "https://a.espncdn.com/i/teamlogos/nba/500/okc.png",
  "Orlando Magic" = "https://a.espncdn.com/i/teamlogos/nba/500/orl.png",
  "Philadelphia 76ers" = "https://a.espncdn.com/i/teamlogos/nba/500/phi.png",
  "Phoenix Suns" = "https://a.espncdn.com/i/teamlogos/nba/500/phx.png",
  "Portland Trail Blazers" = "https://a.espncdn.com/i/teamlogos/nba/500/por.png",
  "Sacramento Kings" = "https://a.espncdn.com/i/teamlogos/nba/500/sac.png",
  "San Antonio Spurs" = "https://a.espncdn.com/i/teamlogos/nba/500/sa.png",
  "Toronto Raptors" = "https://a.espncdn.com/i/teamlogos/nba/500/tor.png",
  "Utah Jazz" = "https://a.espncdn.com/i/teamlogos/nba/500/utah.png",
  "Washington Wizards" = "https://a.espncdn.com/i/teamlogos/nba/500/wsh.png"
  
)

NBA_Data$Logo <- team_logo_urls[NBA_Data$Team]

####################
# OVERALL SCATTER PLOT
####################

three_point_pct_plot <- ggplot(
  NBA_Data,
  aes(x = `3P%`, y = W)
) +
  
  geom_image(aes(image = Logo), size = 0.04) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  
  labs(
    title = "Three Point Percentage vs Wins (2014–2024)",
    x = "Three Point Percentage",
    y = "Wins"
  ) +
  
  theme_minimal()

print(three_point_pct_plot)

ggsave(
  "outputs/three_point_pct_plot.pdf",
  plot = three_point_pct_plot,
  width = 8,
  height = 6
)

####################
# YEARLY SCATTER PLOT
####################

three_point_pct_plot_per_year <- ggplot(
  NBA_Data,
  aes(x = `3P%`, y = W)
) +
  
  geom_image(aes(image = Logo), size = 0.05) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  
  facet_wrap(~Year) +
  
  labs(
    title = "Three Point Percentage vs Wins by Year",
    x = "Three Point Percentage",
    y = "Wins"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10)
  )

print(three_point_pct_plot_per_year)

ggsave(
  "outputs/three_point_pct_plot_per_year.pdf",
  plot = three_point_pct_plot_per_year,
  width = 12,
  height = 9
)

####################
# TEAM SCATTER PLOT
####################

three_point_pct_plot_per_team <- ggplot(
  NBA_Data,
  aes(x = `3P%`, y = W)
) +
  
  geom_image(aes(image = Logo), size = 0.08) +
  
  geom_text(
    aes(label = Year),
    vjust = -2,
    size = 1.5
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  
  facet_wrap(~Team) +
  
  labs(
    title = "Three Point Percentage vs Wins by Team",
    x = "Three Point Percentage",
    y = "Wins"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    strip.text = element_text(size = 9)
  )

print(three_point_pct_plot_per_team)

ggsave(
  "outputs/three_point_pct_plot_per_team.pdf",
  plot = three_point_pct_plot_per_team,
  width = 14,
  height = 12
)

###################################################
### END OF SCRIPT                               ###
###################################################
