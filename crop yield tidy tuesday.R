# tidy tuesday crop yields
# https://www.r-bloggers.com/train-and-analyze-many-models-for-tidytuesday-crop-yields/
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-09-01/readme.md


library(tidyverse)

#### Get the Data ####

# Read in with tidytuesdayR package 
# Install from CRAN via: 
#install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2020-09-01')
#tuesdata <- tidytuesdayR::tt_load(2020, week = 36)

key_crop_yields <- tuesdata$key_crop_yields

long_crops <- key_crop_yields %>% 
  pivot_longer(cols = 4:last_col(),
               names_to = "crop", 
               values_to = "crop_production") %>% 
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>% 
  set_names(nm = names(.) %>% tolower())
long_crops

land_use <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv")

#### Explore the data ####
# I’m going to use the land_use dataset only to find the top population countries.
top_countries <- land_use %>%
  janitor::clean_names() %>%
  filter(!is.na(code), entity != "World") %>%
  group_by(entity) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  slice_max(total_population_gapminder, n = 30) %>%
  pull(entity)

top_countries

#Now let’s create a tidy version of the crop yields data, for the countries and crops I am interested in.
tidy_yields <- key_crop_yields %>%
  janitor::clean_names() %>%
  pivot_longer(wheat_tonnes_per_hectare:bananas_tonnes_per_hectare,
               names_to = "crop", values_to = "yield"
  ) %>%
  mutate(crop = str_remove(crop, "_tonnes_per_hectare")) %>%
  filter(
    crop %in% c("wheat", "rice", "maize", "barley"),
    entity %in% top_countries,
    !is.na(yield)
  )

tidy_yields

#This data structure is just right for plotting crop yield over time!

tidy_yields %>%
  ggplot(aes(year, yield, color = crop)) +
  geom_line(alpha = 0.7, size = 1.5) +
  geom_point() +
  facet_wrap(~entity, ncol = 5) +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  labs(x = NULL, y = "yield (tons per hectare)")

#### Many models ####
# Now let’s fit a linear model to each country-crop combination.
library(tidymodels)

class(tidy_yields)

tidy_lm <- tidy_yields %>%
  nest(yields = c(year, yield)) %>%
  mutate(model = map(yields, ~ lm(yield ~ year, data = .x)))

tidy_lm


# Next, let’s tidy() those models to get out the coefficients, 
# and adjust the p-values for multiple comparisons while we’re at it.

slopes <- tidy_lm %>%
  mutate(coefs = map(model, tidy)) %>%
  unnest(coefs) %>%
  filter(term == "year") %>%
  mutate(p.value = p.adjust(p.value))

slopes

#### Explore results ####
# Now we can visualize the results of this modeling, 
# which is estimating how crop yields are changing around the world.

library(ggrepel)
slopes %>%
  ggplot(aes(estimate, p.value, label = entity)) +
  geom_vline(
    xintercept = 0, lty = 2,
    size = 1.5, alpha = 0.7, color = "gray50"
  ) +
  geom_point(aes(color = crop), alpha = 0.8, size = 2.5, show.legend = FALSE) +
  scale_y_log10() +
  facet_wrap(~crop) +
  geom_text_repel(size = 3, family = "IBMPlexSans") +
  theme_light(base_family = "IBMPlexSans") +
  theme(strip.text = element_text(family = "IBMPlexSans-Bold", size = 12)) +
  labs(x = "increase in tons per hectare per year")

