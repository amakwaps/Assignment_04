library(tidyverse)
library(ggplot2)
library(reshape2)
library(forcats)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)
library(scales)

library(httr)
set_config(config(ssl_verifypeer = 0L))

library(readxl)
project <- read_excel("~/Desktop/Assignment_04/data/AIU All Women Dataset.xlsx")

graph1 <- project %>%
  group_by(region) %>%
  summarise(weighted = weighted.mean(rate_matdeaths))

#Creates a line plot which shows the rate of maternal deaths in each continent(region)
graph1 %>%  
  ggplot(aes(x=region, y=weighted, group = 1)) +
  geom_point(size=3, colour = "red") +
  geom_line(colour = "blue") +
  labs(title = "Maternal deaths per 100,000 live births by region",
       caption = "Source: The Guttmacher Institute",
       x = "Region", y = "Maternal deaths per 100,000 live births") +
  theme_minimal()

#Graph 2 - percentage of maternal deaths caused by each cause by region 
graph2 <- project %>%
  mutate_if(is.numeric, round, 2) %>%
  select(continent = region, cause_safeabortion = pct_matdeaths_safeabs, cause_unsafeabortion = pct_matdeaths_unsafeabs, cause_miscarriage = pct_matdeaths_miscar, cause_ectopic = pct_matdeaths_ectopic, cause_hemor = pct_matdeaths_hem, cause_hypertension = pct_matdeaths_hyptn, cause_otherdirect = pct_matdeaths_other, cause_inf = pct_matdeaths_inf, cause_indirect = pct_matdeaths_indirect) %>% 
  group_by(continent) %>% 
  summarize(weighted.mean(cause_safeabortion), 
            weighted.mean(cause_unsafeabortion), 
            weighted.mean(cause_miscarriage), 
            weighted.mean(cause_ectopic), 
            weighted.mean(cause_hemor), 
            weighted.mean(cause_hypertension),
            weighted.mean(cause_otherdirect),
            weighted.mean(cause_inf),
            weighted.mean(cause_indirect))  %>%
  mutate_if(is.numeric, round, 2)
graph2 <- melt(graph2,  id.vars = 'continent', variable.name = 'series')

#Creates a bar graph showing the rates of the various causes of deaths in each region
ggplot(graph2,aes(x = factor(continent), y = value, fill = series) ) +
  geom_bar(stat="identity", position = 'dodge') +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.01) +
  labs(
    title = "Causes of Maternal Deaths In World Regions",
    caption = "Source: The Guttmacher Institute", 
    x = "Maternal Death Causes By Regions", y = "Percentage of Death by Each Cause",
  ) +
  scale_fill_discrete(name = "Causes of Maternal Deaths", 
                      labels= c("weighted.mean(cause_unsafeabortion)" = "Unsafe Abortion", 
                                "weighted.mean(cause_safeabortion)" = "Safe Abortion",
                                "weighted.mean(cause_miscarriage)" = "Miscarriage",
                                "weighted.mean(cause_ectopic)" = "Ectopic Pregnancy", 
                                "weighted.mean(cause_hemor)" = "Hemmorrhage", 
                                "weighted.mean(cause_hypertension)" = "Hypertension",
                                "weighted.mean(cause_otherdirect)" = "Other Direct Causes",
                                "weighted.mean(cause_inf)" = "Maternal Infection",
                                "weighted.mean(cause_indirect)" = "Indirect Causes")) +
  facet_wrap ( ~continent,
               ncol = 1,
               scales = "free",
               drop = TRUE,
               shrink = TRUE,
               as.table = TRUE,
               strip.position = "top")

#Graph 3
#Groups per capita cost when all needs are met and current cost per capita by number of women in reproductive age and region
graph3 <- project %>%
  select(country = country, continent = region, wra = wra, cc_cost_per_capita = curr_costs_percap, cost_to_meet_all_needs_percapita = all_costs_percap) %>%
  group_by(continent) %>%
  mutate_if(is.numeric, round, 2)
#Creates a bar graph plotting per capita cost when all needs are met and current cost per capita by number of women in reproductive age and region
graph3 %>%
  ggplot(aes(x=cc_cost_per_capita, y=cost_to_meet_all_needs_percapita, size = wra, color = continent)) +
  geom_point(alpha=0.99) +
  labs(
    color ="Continents",
    title = "Total Current Cost Per Capita & Cost to Meet All Needs",
    subtitle = "This shows the cost structure in all five regions",
    caption = "Source: The Guttmacher Institute", 
    x = " Per Capita Cost to Meet All Needs", y = "Per Capita Current Cost",
  ) +
  scale_size_continuous(name = "Number of Women in Reproductive Age",
                        limits = c(1000, 100000),
                        range = c(0, 5)) 


#Graph 4
#Creating a new variable - percentage of women in need/wanting to avoid pregnancy by region
graph4 <- project %>%
  mutate(percent_avoid= inneed/wra) %>%
  group_by(region) %>%
  summarize(percent_avoid = weighted.mean(percent_avoid))
#Creates a lollipop graph showing percentage of women in need/wanting to avoid pregnancy by region
ggplot(graph4, aes (x=region, y=percent_avoid, fill=region)) +
  geom_segment(aes(x=region,xend=region,y=0,yend=percent_avoid)) +
  geom_point(aes(colour=region), size=4) +
  ylim(0, 1) +
  theme_minimal() +
  scale_x_discrete(guide = guide_axis (n.dodge =2)) +
  labs(title = "Percentage of Women(15-49 Age Group) in Need/Wanting to Avoid Pregnancy by Region",
       caption ="Source: The Guttmacher Institute",
       x = "Region", y = "Percentage of Women") 

