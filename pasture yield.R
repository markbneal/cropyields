#crop yields code modified for pasture


#### Get the Data ####

library(tidyverse)

db <- read.csv("file:///C:/Users/nealm/OneDrive - DairyNZ Limited/Desktop/dairynz/econ2020/dairybaseto1819_April2020.csv")
head(db)


# DairyBase data largely "tidy" already 

# Create numeric (continuous) year ending variable for the production season (and make it a tibble, optional?)
# i.e. Season 2012-13 makes year_ending 2013
db <- db %>% 
  as_tibble() %>% 
  mutate(year_ending = as.numeric(str_sub(Season,1,4))+1)

#filter out NA for pasture harvest and Map region number to names
db <- db %>% 
  filter(!is.na(Pasture.and.Crop.harvested.t.DM.ha) )   %>% 
  mutate( Region = case_when(
                            Region == 1 ~ "Northland",
                            Region == 2 ~ "Waikato",
                            Region == 3 ~ "Bay of Plenty",
                            Region == 4 ~ "Taranaki",
                            Region == 5 ~ "Lower North Island",
                            Region == 6 ~ "West Coast-Tasman",
                            Region == 7 ~ "Marlborough-Canterbury",
                            Region == 8 ~ "Otago-Southland"
                            ))


#This data structure is just right for plotting crop yield over time!

db %>%
  ggplot(aes(year_ending, Pasture.and.Crop.harvested.t.DM.ha, colour = Region)) +
  geom_smooth(alpha = 0.7, size = 1.5) +
  geom_point() +
  facet_wrap(~Region, ncol = 4) +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  labs(x = NULL, y = "Yield (tDM per hectare)")

ggsave("pasture by year, facet by region.png", width = 8, height=6) #saves last plot

#### Many models ####
# Now let’s fit a linear model to each country-crop combination.
library(tidymodels)

class(db)

tidy_lm_db <- db %>%
  select(c("Region", "year_ending", "Pasture.and.Crop.harvested.t.DM.ha")) %>%  #select only essential columns. Data to nest within region, because multiple observations for each region at each year
  nest(yields = c(year_ending, Pasture.and.Crop.harvested.t.DM.ha)) %>%
  mutate(model = map(yields, ~ lm(Pasture.and.Crop.harvested.t.DM.ha ~ year_ending, data = .x)))

#rlang::last_error()
tidy_lm_db


# Next, let’s tidy() those models to get out the coefficients, 
# and adjust the p-values for multiple comparisons while we’re at it.

slopes_db <- tidy_lm_db %>%
  mutate(coefs = map(model, tidy)) %>%
  unnest(coefs) %>%
  filter(term == "year_ending") %>%
  mutate(p.value = p.adjust(p.value))

slopes_db


#### Explore results ####
# Now we can visualize the results of this modeling, 
# which is estimating how crop yields are changing around the world.

library(ggrepel)
slopes_db %>%
  ggplot(aes(estimate, p.value, label = Region, colour = Region )) +
  geom_vline(
    xintercept = 0, lty = 2,
    size = 1.5, alpha = 0.7, color = "gray50"
  ) +
  geom_point( alpha = 0.8, size = 2.5, show.legend = FALSE) +
  scale_y_log10() +
  #facet_wrap(~Region) +
  geom_text_repel(size = 3) +
  theme_light() +
  theme(strip.text = element_text(size = 12)) +
  labs(x = "increase in t DM per hectare per year")+
  theme(legend.position = "none")

ggsave("linear trend and p value, pasture by year, facet by region.png", width = 8, height=6) #saves last plot


