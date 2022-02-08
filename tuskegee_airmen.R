library(tidyverse)
library(ggimage)
library(maps)



draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  grid::rectGrob(width = grid::unit(0.6, "npc"), height = grid::unit(0.6, "npc"),
                 gp = grid::gpar(col = data$colour, fill = alpha(data$fill, data$alpha), lty = data$linetype, lwd = lwd * .pt, linejoin = "mitre"))}
# Load data 
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

airmen %>% 
  group_by(state) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n))

cities <- get('us.cities') %>% 
  ungroup() %>% 
  select(name,lat,long,country.etc) %>% 
  mutate_at("name", str_replace, "Saint", "St.") %>% 
  mutate_at("name", str_replace, "WASHINGTON DC", "Washington DC") %>% 
  distinct()


airmen_cities<- airmen %>% 
  mutate_at("military_hometown_of_record", str_replace, "Pittsburg", "Pittsburgh") %>% 
  mutate(cat_city = paste0(military_hometown_of_record," ",state)) %>% 
  left_join(cities, by =c("cat_city" = "name")) %>% 
  select(cat_city,country.etc,state,lat,long,name) %>% 
  group_by(cat_city) %>% 
  summarize(latitude = max(lat),longitude = max(long), n = n_distinct(name)) %>% 
  mutate(num_airmen = ifelse(n >= 0 & n <= 10,"1-10 AIRMEN",
                             ifelse(n >= 11 & n <=20,"11-20 AIRMEN",
                                    ifelse(n >= 21 & n <=50,"21-50 AIRMEN",
                                           ifelse(n > 50,"50+ AIRMEN","--")))))

state_plot<-airmen %>% 
  full_join(data.frame(state.abb, state.name), by = c("state" = "state.abb")) %>%
  mutate(region = tolower(state.name)) %>% 
  left_join(map_data("state")) %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "#654321",lwd = 0.2, key_glyph = "polygon3") +
  theme_void() 



airmen_cities$num_airmen<-factor(airmen_cities$num_airmen,levels = c("1-10 AIRMEN","11-20 AIRMEN","21-50 AIRMEN","50+ AIRMEN"))

final_plot<-state_plot +
  geom_point(data= airmen_cities,aes(x=longitude, y=latitude,group = cat_city ,color =num_airmen, size = n) ,alpha = 0.9) +
  scale_color_manual(name="TOTAL AIRMEN",values = c("#000000","#dc143c","#00aa00","#ffd700")) +
  theme(text = element_text(family = "sans"), 
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.caption = element_text(size=8, hjust=0.5),
        legend.position = "bottom",
        plot.background = element_rect(fill = "#d2b48c"),
        panel.background = element_rect(fill = "#d2b48c",
                                        colour = "#d2b48c"),
        legend.text = element_text(color = "#000000"))+
  guides(size = "none") +
  labs(title = "HOMETOWNS OF TUSKEGEE AIRMEN ACROSS THE \nUNITED STATES", 
       caption = "Source - #DuBoisChallenge2022 & #TidyTuesday | plot by: @__sierradavis")

ggsave("tuskegee_airmen.png", final_plot,dpi = 300, height = 7, width = 11)




