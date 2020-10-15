#crop yields code modified for pasture


#### Get the Data ####

library(tidyverse)

db <- read.csv("file:///C:/Users/nealm/OneDrive - DairyNZ Limited/Desktop/dairynz/econ2020/dairybaseto1819_April2020.csv")
head(db)
# unique(db$Season)
# unique(db$ï..Farm.N)
         
# DairyBase data largely "tidy" already 

# Create numeric (continuous) year ending variable for the production season (and make it a tibble, optional?)
# i.e. Season 2012-13 makes year_ending 2013
db <- db %>% 
  as_tibble() %>% 
  mutate(year_ending = as.numeric(str_sub(Season,1,4))+1) #%>% 
  #filter( year_ending != 2019)# filtering out last year of data sends Canterbury trend to less significance.

#filter out NA for pasture harvest and Map region number to names
db <- db %>% 
  filter(!is.na(Pasture.and.Crop.harvested.t.DM.ha) )   %>% 
  mutate( Region_name = case_when(
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
  ggplot(aes(year_ending, Pasture.and.Crop.harvested.t.DM.ha, colour = Region_name)) +
  geom_smooth(alpha = 0.7, size = 1.5) +
  geom_point() +
  facet_wrap(~Region_name, ncol = 4) +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  labs(x = NULL, y = "Yield (tDM per hectare)")+
  theme(legend.position = "none")

ggsave("pasture by year, facet by region.png", width = 8, height=6) #saves last plot

#### Many models - Region Level ####
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

ggsave("linear trend and p value, pasture by year, by region.png", width = 8, height=6) #saves last plot


#plot trends on chloropeth map
library(sf)
library(ggrepel)
library(ggsflabel)

NZ_TLA <- readRDS("C:/Users/nealm/OneDrive - DairyNZ Limited/Desktop/dairynz/NZmaps/NZ_TLA.RDS")

#Aggregate from TLA
NZ_TLA <- NZ_TLA %>% group_by(DairyBase_Region_Number) %>% summarize()

#join trends (slopes)
NZ_TLA <- left_join(NZ_TLA, slopes_db, by = c("DairyBase_Region_Number" = "Region"))
colnames(NZ_TLA)
max(NZ_TLA$estimate, na.rm = TRUE)
min(NZ_TLA$estimate, na.rm = TRUE)
summary(NZ_TLA$estimate, na.rm = TRUE)

# scale_fill_gradient2(
#   low = muted("red"),
#   mid = "white",
#   high = muted("blue"),
#   midpoint = 0
# )

ggplot()+
  geom_sf(data = NZ_TLA, aes(fill=estimate))+
  scale_fill_gradient2(
    low = ("red"),
    mid = "white",
    high = ("blue"),
    midpoint = 0
  )+
  geom_sf_label_repel(data = NZ_TLA, aes(label = DairyBase_Region_Number, colour = DairyBase_Region_Number ),
                      force = 1, seed = 10, size = 2)+
  scale_y_continuous(expand = expansion(mult = 0.5))+
  scale_x_continuous(expand = expansion(mult = 0.5))+
  theme_void()#+
  #theme(legend.position = "none")

ggsave("NZ Region pasture yield trend.png", width = 10, height = 10)

#### Now repeat for district level ####

# read and join district data for district names

library(readxl)
District_key <- read_xlsx("List of TLAs against Dairy Statistics and Economic Survey Regions v3 MN.xlsx")

colnames(db)
unique(db$District.council)
colnames(District_key)
unique(District_key$DairyBase_District_Number)

db <- left_join(db, District_key, by = c("District.council" = "DairyBase_District_Number"))


#### Many models - District Level ####
# Now let’s fit a linear model to each country-crop combination.
library(tidymodels)

class(db)
District_key$DistrictDairyStatistics_2018
tidy_lm_db <- db %>%
  select(c("District.council", "year_ending", "Pasture.and.Crop.harvested.t.DM.ha")) %>%  #select only essential columns. Data to nest within region, because multiple observations for each region at each year
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
  ggplot(aes(estimate, p.value, label = District.council, colour = District.council )) +
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

ggsave("linear trend and p value, pasture by year, facet by district.png", width = 8, height=6) #saves last plot

#plot trends on chloropeth map
library(sf)
library(ggrepel)
library(ggsflabel)

NZ_TLA <- readRDS("C:/Users/nealm/OneDrive - DairyNZ Limited/Desktop/dairynz/NZmaps/NZ_TLA.RDS")
colnames(NZ_TLA)
#join trends

NZ_TLA <- left_join(NZ_TLA, slopes_db, by = c("TA2020_V1_" = "District.council"))

max(slopes_db$estimate, na.rm = TRUE)
min(slopes_db$estimate, na.rm = TRUE)
summary(slopes_db$estimate, na.rm = TRUE)
#alpha()
# scale_fill_gradient2(
#   low = muted("red"),
#   mid = "white",
#   high = muted("blue"),
#   midpoint = 0
# )
ggplot()+
  geom_sf(data = NZ_TLA, aes(fill=estimate))+
  scale_fill_gradient2(
    low = ("red"),
    mid = "white",
    high = ("blue"),
    midpoint = 0
  )+
  geom_sf_label_repel(data = NZ_TLA, aes(label = TLA_1995, colour = TLA_1995 ),
                      force = 1, seed = 10, size = 2)+
  scale_y_continuous(expand = expansion(mult = 0.5))+
  scale_x_continuous(expand = expansion(mult = 0.5))+
  theme_void()+
  theme(legend.position = "none")

ggsave("NZ TLA pasture yield trend.png", width = 10, height = 10)



