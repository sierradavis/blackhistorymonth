library(tidyverse)
library(extrafont)
library(ggimage)
library(rvest)
library(showtext)
font_add_google(name = "Rowdies")

asp_ratio <- 1.618 

first_url <- "https://en.wikipedia.org/wiki/List_of_African-American_firsts"

raw_first <- read_html(first_url)

get_year <- function(id_num) {
  raw_first %>% 
    html_nodes(glue::glue("#mw-content-text > div > h4:nth-child({id_num}) > span")) %>% 
    html_attr("id") %>% 
    .[!is.na(.)]
}

get_first <- function(id_num){
  raw_first %>% 
    html_nodes(glue::glue("#mw-content-text > div > ul:nth-child({id_num})")) %>% 
    html_text() %>% 
    str_split("\n")
}

tidyr::crossing(id_num = 9:400, count = 1:5)

raw_first_df <- tibble(id_num = 9:450) %>% 
  mutate(year = map(id_num, get_year),
         text = map(id_num, get_first))

clean_first <- raw_first_df %>% 
  mutate(year = as.integer(year)) %>% 
  fill(year) %>% 
  unnest(text) %>% 
  unnest(text) %>% 
  separate(text, into = c("role", "person"), sep = ": ") %>% 
  mutate(person = str_remove_all(person, "\\\\"),
         person = str_trim(person),
         role = str_replace(role, "African American", "African-American")) %>% 
  select(year, role, person)

clean_first %>% 
  group_by(year) %>% 
  summarize(n =n())

first_role <- function(category){
  str_detect(tolower(role), category)
}

edu <- paste0(c(
  "practice", "graduate", "learning", "college", "university", "medicine",
  "earn", "ph.d.", "professor", "teacher", "school", "nobel", "invent", "patent",
  "medicine", "degree", "doctor", "medical", "nurse", "physician", "m.d.", "b.a.", "b.s.", "m.b.a",
  "principal", "space", "astronaut"
), collapse = "|")

religion <- c("bishop", "rabbi", "minister", "church", "priest", "pastor", "missionary",
              "denomination", "jesus", "jesuits", "diocese", "buddhis") %>%
  paste0(collapse = "|")

politics <- c(
  "diplomat", "elected", "nominee", "supreme court", "legislature", "mayor", "governor",
  "vice President", "president", "representatives", "political", "department", "peace prize",
  "ambassador", "government", "white house", "postal", "federal", "union", "trade",
  "delegate", "alder", "solicitor", "senator", "intelligience", "combat", "commissioner",
  "state", "first lady", "cabinet", "advisor", "guard", "coast", "secretary", "senate",
  "house", "agency", "staff", "national committee"
) %>%
  paste0(collapse = "|")

sports <- c(
  "baseball", "football", "basketball", "hockey", "golf", "tennis",
  "championship", "boxing", "games", "medal", "game", "sport", "olympic", "nascar",
  "coach", "trophy", "nba", "nhl", "nfl", "mlb", "stanley cup", "jockey", "pga",
  "race", "driver", "ufc", "champion"
) %>%
  paste0(collapse = "|")

military <- c(
  "serve", "military", "enlist", "officer", "army", "marine", "naval",
  "officer", "captain", "command", "admiral", "prison", "navy", "general",
  "force"
) %>%
  paste0(collapse = "|")

law <- c("american bar", "lawyer", "police", "judge", "attorney", "law", 
         "agent", "fbi") %>%
  paste0(collapse = "|")

arts <- c(
  "opera", "sing", "perform", "music", "billboard", "oscar", "television",
  "movie", "network", "tony award", "paint", "author", "book", "academy award", "curator",
  "director", "publish", "novel", "grammy", "emmy", "smithsonian",
  "conduct", "picture", "pulitzer", "channel", "villain", "cartoon", "tv", "golden globe",
  "comic", "magazine", "superhero", "pulitzer", "dancer", "opry", "rock and roll", "radio",
  "record") %>%
  paste0(collapse = "|")

social <- c("community", "freemasons", "vote", "voting", "rights", "signature", 
            "royal", "ceo", "community", "movement", "invited", "greek", "million",
            "billion", "attendant", "chess", "pilot", "playboy", "own", "daughter",
            "coin", "dollar", "stamp", "niagara",
            "stock", "north pole", "reporter", "sail around the world", "press", "miss ",
            "everest")  %>%
  paste0(collapse = "|")

first_df <- clean_first %>% 
  mutate(gender = if_else(str_detect(role, "woman|Woman|her|she|female"), 
                          "Female African American Firsts", "African-American Firsts"),
         role = str_remove_all(role, "\""),
         person = str_remove_all(person, "\""),
         category = case_when(
           str_detect(tolower(role), military) ~ "Military",
           str_detect(tolower(role), law) ~ "Law",
           str_detect(tolower(role), arts) ~ "Arts & Entertainment",
           str_detect(tolower(role), social) ~ "Social & Jobs",
           str_detect(tolower(role), religion) ~ "Religion",
           str_detect(tolower(role), edu) ~ "Education & Science",
           str_detect(tolower(role), politics) ~ "Politics",
           str_detect(tolower(role), sports) ~ "Sports",
           TRUE ~ NA_character_
         )) %>% 
  rename(accomplishment = role)

final_df <- first_df[-c(543:561),] 

firsts <- final_df %>% 
  mutate_at(c("gender", "category"), as.factor) %>% 
  mutate(decade = year - year %% 10)

write.csv(firsts, "first_aa_achievements.csv")

first_plot<-firsts %>% 
  filter(category != "NA") %>% 
  group_by(category) %>% 
  summarize(n = n_distinct(accomplishment)) %>% 
  ggplot(aes(x=reorder(category,n),y=n)) +
  geom_col(fill= "#222119",width = 0.97)+
  geom_image(image = "bg.png",size = 0.05, by = "width", asp = asp_ratio) +
  coord_flip()+
  labs(title = "First Achievements by Black Americans", 
       subtitle = "The total number of accomplishments in each category from 1670 - 2022",
       x = "", y = "",
       caption = "Source: wikipedia.org/wiki/List_of_African-American_firsts | Plot by: sierradavis.xyz") +
  scale_y_continuous(expand = c(0, 2.5)) +
  #scale_x_continuous(expand = c(0, 0)) +
  theme( plot.title = element_text(size = 40, family="Rowdies", color = "#222119"),
         plot.subtitle = element_text(size = 30,  color = "#222119"),
         plot.caption = element_text(size = 20,  color = "#222119"),
         plot.background = element_rect(fill = "#f4efe6"),
         panel.background = element_rect(fill = "#f4efe6",
                                         colour = "#f4efe6"),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         axis.text.x=element_text(size = 35, family="Rowdies",color = "#222119"),
         axis.ticks.x=element_blank(),
         axis.ticks.y=element_blank(),
         axis.text.y=element_text(size = 30, family="Rowdies",color = "#222119"))

ggsave("first_aa_achievements.png", first_plot,dpi = 300)

