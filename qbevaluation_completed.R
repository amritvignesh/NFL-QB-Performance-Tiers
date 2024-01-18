# R U Ready for Some Football? (Computationally Modeling the NFL)
# Princeton Wintersession 2024
# January 17, 2024; 1-4 PM EST
# Day Yi, Sean Holland, Amrit Vignesh, Momin Ghaffar, Ronny Qian

# This is the completed code with all brackets [] filled in!

# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("nflfastR")
# install.packages("cluster")
# install.packages("factoextra")
# install.packages("gt")
# install.packages("gtExtras")

library(dplyr)
library(tidyverse)
library(nflfastR)
library(cluster)
library(factoextra)
library(gt)
library(gtExtras)

# Load play-by-play data from the 2023 NFL Regular Season

pbp <- load_pbp(2023) %>%
  filter(season_type == "REG")

# Look at all columns names from the play-by-play data

colnames(pbp)

# Pull total stats using the play-by-play data

qb_stats <- pbp %>%
  
  # Stats for QBs should only come from pass or rush plays
  
  filter(pass == 1 | rush == 1) %>% 
  group_by(id) %>% 
  mutate(pass_yards = ifelse(pass == 1, yards_gained, 0), 
         sack_yards = ifelse(sack == 1, -yards_gained, 0), 
         success = ifelse(epa > 0, 1, 0)) %>%
  summarize(name = first(name), 
            team = last(posteam), 
            plays = n(), 
            dropbacks = sum(qb_dropback, na.rm = TRUE), 
            pass_attempts = sum(pass_attempt, na.rm = TRUE), 
            epa_per_play = mean(epa, na.rm = TRUE), 
            cpoe = mean(cpoe, na.rm = TRUE), 
            pass_yards = sum(pass_yards, na.rm = TRUE), 
            pass_touchdowns = sum(pass_touchdown, na.rm = TRUE), 
            sacks = sum(sack, na.rm = TRUE), 
            interceptions = sum(interception, na.rm = TRUE), 
            sack_yards = sum(sack_yards, na.rm = TRUE), 
            
            # ANY/A is adjusted net yards per attempt and is based on a simple formula as shown below
            
            any_a = (pass_yards + 20 * pass_touchdowns - 45 * interceptions - sack_yards)/(pass_attempts + sacks), 
            success_rate = mean(success, na.rm = TRUE) * 100, 
            pass_touchdown_rate = pass_touchdowns/dropbacks * 100, 
            interception_rate = interceptions/dropbacks * 100) %>%
  
  # Filtered to quarterbacks with at least 200 dropbacks considering rate statistics need a decent sample size
  
  filter(dropbacks >= 200) %>%
  
  # Selecting six different statistics to use to evaluate quarterbacks
  
  select(name, team, epa_per_play, cpoe, any_a, success_rate, pass_touchdown_rate, interception_rate) 

# Standardizing all statistical values with easier interpretation when analyzing the average values per cluster

qb_stats_scaled <- scale(qb_stats[,c(3:8)])

# Using silhouette method to determine optimal amount of clusters to use in k-means based on the scaled data

fviz_nbclust(qb_stats_scaled, kmeans, method = "silhouette", k.max=15)

# 2 clusters won't be enough to communicate enough performance tier groups so instead we use the second most optimal amount - 4

set.seed(0)
kmeans_qbs <- kmeans(qb_stats_scaled, centers = 4, nstart = 25, iter.max = 20)
kmeans_centers <- as.data.frame(kmeans_qbs$centers)

kmeans_centers$cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4')

kmeans_centers <- kmeans_centers %>%
  rename(c('EPA Per Play'='epa_per_play', 'CPOE'='cpoe', 'ANY/A'='any_a', 'Success %'='success_rate', 'Pass TD%'='pass_touchdown_rate', 'INT%'='interception_rate')) %>%
  pivot_longer(!cluster, names_to = 'statname', values_to = 'statvalue')

# Outputs the scaled statistical centers for each cluster, helps us evaluate which clusters represent elite performance tiers and which display the opposite

kmeans_centers %>%
  ggplot(aes(x=statname, y=statvalue, color=cluster)) +
  geom_point() +
  facet_wrap(~ cluster, ncol = 2) +
  labs(x = "Statistic Predictor", y = "Scaled Statistical Value Center Per Cluster",
       title = "Cluster Compositions for 2023 NFL QBs (250+ Dropbacks)") +
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), 
        panel.grid.minor = element_blank(), plot.title = element_text(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"))

# Using Principal Component Analysis (PCA) To Create a Visual Of the Distinction In Cultures

pca_qbs <- prcomp(qb_stats_scaled)
pca_summary <- summary(pca_qbs)

twopcas <- as.data.frame(pca_qbs$x[,1:2])
twopcas$cluster <- as.factor(kmeans_qbs$cluster)
variance_1 <- 100 *round(pca_summary$importance[2,1], 4) 
variance_2 <- 100 *round(pca_summary$importance[2,2], 4) 

twopcas %>%
  ggplot(aes(x=PC1, y=PC2, color= cluster)) + 
  geom_point(alpha=0.3) + 
  stat_ellipse(level=(2/3)) + 
  labs(x = paste0('PC1 (Accounts for ', variance_1, '% of Variance)'), 
       y = paste0('PC2 (Accounts for ', variance_2, '% of Variance)'), 
       title = 'K-Means Cluster Differences for NFL QBs in the 2023 Season (250+ Dropbacks)',
       subtitle = '(Cluster Numbers are Before the Manual Change Based on Performance)') +
  theme(plot.title = element_text(size=14, hjust=0.5, face="bold"), plot.subtitle = element_text(size=10, hjust=0.5), axis.title = element_text(face="bold"))

# The clusters are numbered by PCA value which is a combination of all metrics, however, cluster 3 seems to be in a worse performance tier than cluster 4 as interception percentage is included which should be inversely correlated with performance

name_cluster <- data.frame(name=qb_stats$name, team = qb_stats$team, epa_per_play = qb_stats$epa_per_play, cluster=kmeans_qbs$cluster) %>%
  mutate(cluster = ifelse(cluster == 3, 4, ifelse(cluster == 4, 3, cluster)))

# Adding team logos to create nice tables

logos <- teams_colors_logos %>%
  select(team = team_abbr, team_logo_espn)

final_qb_data <- left_join(name_cluster, logos, by = "team")

# Generating the tables for each cluster or now "tier" and arranging by EPA per play which is recognized as the top indicator of QB performance compared to other metrics

for (clust in unique(final_qb_data$cluster)) {
  cluster_stats <- final_qb_data %>%
    filter(cluster == clust) %>%
    arrange(-epa_per_play) %>%
    mutate(epa_per_play = round(epa_per_play, 2)) %>%
    select(name, team_logo_espn, epa_per_play)
  table <- cluster_stats %>% gt() %>%
    gt_img_rows(columns = team_logo_espn) %>%
    gt_theme_538() %>%
    cols_align(
      align = "center",
      columns = c(name, team_logo_espn, epa_per_play)
    ) %>%
    gt_hulk_col_numeric(epa_per_play) %>%
    cols_label(
      name = md("**QB**"),
      team_logo_espn = md("**Team**"),
      epa_per_play = md("**EPA Per Play**")
    ) %>%
    tab_header(
      title = md(paste("**Tier", clust, "Quarterbacks**")),
      subtitle = "Based on Rate Statistics From the 2023 NFL Regular Season"
    ) 
  gtsave(table, paste0("tier", clust, ".png"))
}
