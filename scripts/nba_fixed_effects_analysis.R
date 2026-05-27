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
  "dplyr",
  "ggpubr",
  "ggrepel"
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
    color = "blue",
    linewidth = 1.2
  ) +
  
  # CORRELATION DISPLAY
  stat_cor(
    method = "pearson",
    label.x = 0.323,
    label.y = 73,
    size = 4.5,
    color = "black"
  ) +
  
  # LABEL NBA CHAMPIONS (2014–2024)
  geom_text_repel(
    
    data = subset(
      NBA_Data,
      
      (Team == "San Antonio Spurs" & Year == "2014") |
        
        (Team == "Golden State Warriors" & Year %in% c("2015", "2017", "2018", "2022")) |
        
        (Team == "Cleveland Cavaliers" & Year == "2016") |
        
        (Team == "Toronto Raptors" & Year == "2019") |
        
        (Team == "Los Angeles Lakers" & Year == "2020") |
        
        (Team == "Milwaukee Bucks" & Year == "2021") |
        
        (Team == "Denver Nuggets" & Year == "2023") |
        
        (Team == "Boston Celtics" & Year == "2024")
      
    ),
    
    aes(
      label = paste("🏆", Team, Year)
    ),
    
    size = 3.2,
    
    max.overlaps = Inf,
    
    force = 3,
    
    force_pull = 0.5,
    
    box.padding = 1,
    
    point.padding = 1.2,
    
    segment.alpha = 0.5,
    
    segment.color = "black",
    
    bg.color = "white",
    
    bg.r = 0.15,
    
    min.segment.length = 0
  ) +
  
  labs(
    title = "Three Point Percentage vs Wins (2014–2024)",
    x = "Three Point Percentage",
    y = "Wins"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 16
    ),
    
    axis.text = element_text(
      size = 12,
      color = "black"
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(15, 15, 15, 15)
  )

print(three_point_pct_plot)

ggsave(
  "outputs/main_scatterplot.png",
  plot = three_point_pct_plot,
  width = 8,
  height = 6,
  bg = "white"
)

####################
# YEARLY SCATTER PLOT
####################

three_point_pct_plot_per_year <- ggplot(
  NBA_Data,
  aes(x = `3P%`, y = W)
) +
  
  geom_image(aes(image = Logo), size = 0.06) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue",
    linewidth = 1
  ) +
  
  # CORRELATION DISPLAY
  stat_cor(
    method = "pearson",
    label.x = 0.323,
    label.y = 71,
    size = 2.8,
    color = "black"
  ) +
  
  # LABEL NBA CHAMPION EACH SEASON
  geom_text_repel(
    
    data = subset(
      NBA_Data,
      
      (Team == "San Antonio Spurs" & Year == "2014") |
        
        (Team == "Golden State Warriors" & Year %in% c("2015", "2017", "2018", "2022")) |
        
        (Team == "Cleveland Cavaliers" & Year == "2016") |
        
        (Team == "Toronto Raptors" & Year == "2019") |
        
        (Team == "Los Angeles Lakers" & Year == "2020") |
        
        (Team == "Milwaukee Bucks" & Year == "2021") |
        
        (Team == "Denver Nuggets" & Year == "2023") |
        
        (Team == "Boston Celtics" & Year == "2024")
      
    ),
    
    aes(
      label = paste("🏆", Team)
    ),
    
    size = 2.8,
    
    max.overlaps = Inf,
    
    force = 2,
    
    box.padding = 0.5,
    
    point.padding = 0.7,
    
    segment.alpha = 0.5,
    
    segment.color = "black",
    
    bg.color = "white",
    
    bg.r = 0.12,
    
    min.segment.length = 0
  ) +
  
  facet_wrap(~Year) +
  
  labs(
    title = "Three Point Percentage vs Wins by Year",
    x = "Three Point Percentage",
    y = "Wins"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    legend.position = "none",
    
    strip.text = element_text(
      size = 12,
      face = "bold"
    ),
    
    plot.title = element_text(
      size = 22,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 15
    ),
    
    axis.text = element_text(
      size = 10,
      color = "black"
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(15, 15, 15, 15)
  )

print(three_point_pct_plot_per_year)

ggsave(
  "outputs/yearly_relationship_plot.png",
  plot = three_point_pct_plot_per_year,
  width = 12,
  height = 9,
  bg = "white"
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
    size = 1.5,
    color = "black"
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue",
    linewidth = 0.8
  ) +
  
  facet_wrap(~Team) +
  
  labs(
    title = "Three Point Percentage vs Wins by Team",
    x = "Three Point Percentage",
    y = "Wins"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    legend.position = "none",
    
    strip.text = element_text(
      size = 10,
      face = "bold",
      color = "black"
    ),
    
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 13
    ),
    
    axis.text = element_text(
      size = 8,
      color = "black"
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    strip.background = element_rect(
      fill = "white",
      color = "gray80"
    ),
    
    plot.margin = margin(15, 15, 15, 15)
  )

print(three_point_pct_plot_per_team)

ggsave(
  "outputs/team_level_relationships.png",
  plot = three_point_pct_plot_per_team,
  width = 14,
  height = 12,
  bg = "white"
)

###################################################
### END OF SCRIPT                               ###
###################################################