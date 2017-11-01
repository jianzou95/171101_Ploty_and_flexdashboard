library(tidyverse)
library(janitor)
library(stringr)
library(forcats)
library(viridis)

library(plotly)

airbnb_data = read_csv("../data/nyc_airbnb.zip") %>%
  clean_names() %>%
  mutate(rating = review_scores_location / 2) %>%
  select(boro = neighbourhood_group, neighbourhood, rating, price, room_type,
         latitude, longitude) %>%
  filter(!is.na(rating),
         boro == "Manhattan",
         room_type == "Entire home/apt",
         price %in% 100:400)  %>% 
  sample_n(5000)


airbnb_data %>%
  mutate(text_label = str_c("Price: $", price, '\nRating: ', rating)) %>% 
  plot_ly(x = ~longitude, y = ~latitude, type = "scatter", mode = "markers",
          alpha = 0.5, 
          color = ~price,
          text = ~text_label)

common_neighborhoods =
  airbnb_data %>% 
  count(neighbourhood, sort = TRUE) %>% 
  top_n(8) %>% 
  select(neighbourhood)


inner_join(airbnb_data, common_neighborhoods,
           by = "neighbourhood") %>% 
  mutate(neighbourhood = fct_reorder(neighbourhood, price)) %>% 
  plot_ly(y = ~price, color = ~neighbourhood, type = "box",
          colors = "Set2")

airbnb_data %>% 
  count(neighbourhood) %>% 
  mutate(neighbourhood = fct_reorder(neighbourhood, n)) %>% 
  plot_ly(x = ~neighbourhood, y = ~n, color = ~neighbourhood, type = "bar")


scatter_ggplot = airbnb_data %>%
  ggplot(aes(x = longitude, y = latitude, color = price)) +
  geom_point(alpha = 0.25) +
  scale_color_viridis() +
  coord_cartesian() +
  theme_classic()

ggplotly(scatter_ggplot)

box_ggplot = 
  inner_join(airbnb_data, common_neighborhoods,
             by = "neighbourhood") %>% 
  mutate(neighbourhood = fct_reorder(neighbourhood, price)) %>% 
  ggplot(aes(x = neighbourhood, y = price, fill = neighbourhood)) +
  geom_boxplot() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplotly(box_ggplot)

