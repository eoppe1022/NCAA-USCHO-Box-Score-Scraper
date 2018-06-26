library(tidyverse)
library(splashr)
library(rvest)

# docker commands in docker quickstart terminal
docker pull scrapinghub/splash:3.0
docker run -p 5023:5023 -p 8050:8050 -p 8051:8051 scrapinghub/splash:3.0

docker rm -f $(docker ps -a -q)



sp <- start_splash()

url <- "http://www.uscho.com/scoreboard/division-i-men/20172018/composite-schedule/"
url <- "https://www.uscho.com/scoreboard/michigan/mens-hockey/"  
url <- "https://www.uscho.com/scoreboard/connecticut/mens-hockey/2016-2017/"

get_schedule <- function(myurl) {
  
  link_data <- myurl %>%
    read_html() %>%
    html_nodes("td:nth-child(13) a") %>%
    html_attr("href") %>%
    str_c("https://www.uscho.com", .) %>%
    as_tibble() %>%
    set_names("url")
  
  game_type <- myurl %>%
    read_html() %>%
    html_nodes("td:nth-child(12)") %>%
    html_text() %>%
    as_tibble() %>%
    set_names("game_type") %>%
    filter(game_type != "Type") %>%
    filter(!c(between(row_number(), 648, 648) & game_type == "EX"))

  as_tibble(data.frame(link_data, game_type))
  
}

link_list <- get_schedule(url)


urls <- link_list %>%
  filter(game_type != "EX") %>%
  pull(url)


get_box_score <- function(my_url) {
  
  #progress_bar$tick()$print()
  
  splash_container <- start_splash()
  on.exit(stop_splash(splash_container))
  
  Sys.sleep(runif(1, 20, 35))
  
  mydata <- splash_local %>%
    splash_response_body(TRUE) %>%
    splash_enable_javascript(TRUE) %>%
    splash_plugins(TRUE) %>%
    splash_user_agent(ua_win10_chrome) %>%
    splash_go(my_url) %>%
    splash_wait(runif(1, 10, 15)) %>%
    splash_html() %>%
    html_node("#boxgoals") %>%
    html_table(fill = TRUE) %>%
    as_tibble() %>%
    mutate_all(as.character)

  return(mydata)
}

persistently_get_box_score <- warrenr::persistently(get_box_score, max_attempts = 10, wait_seconds = 0.0001)

try_get_box_score <- function(my_url) {
  
  attempt::try_catch(persistently_get_box_score(my_url), 
            
            .e = function(e){
              
              print(e)
              
              time <- Sys.time()
              a <- paste("+ At", Sys.time(), ", \nError:",e)
              write(a, "log.txt", append = TRUE)
              print(paste("log saved on log.txt at", time))

              data_frame()
            
          })

}

progress_bar <- link_list %>%
  filter(game_type != "EX") %>%
  tally() %>%
  progress_estimated(min_time = 0)


mydata_201718 <- pmap_df(list(urls), try_get_box_score)

stop_splash(sp)
sp <- start_splash()

progress_bar <- link_list %>%
  filter(game_type != "EX") %>%
  slice(25:n()) %>%
  tally() %>%
  progress_estimated(min_time = 0)

mydata_2 <- pmap_df(list(urls[25:length(urls)]), get_box_score)

stop_splash(sp)

mydata <- bind_rows(mydata_1, mydata_2)

new_data <- mydata %>% 
  as_tibble() %>%
  rename(period = Per, team = Team, goal = Scorer, primary_assist = `Assist 1`, secondary_assist = `Assist 2`, game_strength = `Goal Type`, time = Time) %>%
  filter(period != "No data available in table") %>%
  mutate(season = "2016-17") %>%
  mutate(primary_assist = ifelse(primary_assist == "", NA, primary_assist)) %>%
  mutate(secondary_assist = ifelse(secondary_assist == "", NA, secondary_assist)) %>%
  mutate(team = str_replace(team, "\\-.*", "")) %>%
  mutate(game_strength = case_when(is.na(game_strength) ~ "5v5",
                                   game_strength == "5x4" ~ "5v4",
                                   game_strength == "" ~ "5v5",
                                   game_strength == "GWG" ~ "5v5",
                                   game_strength == "PPG 5x4" ~ "5v4",
                                   game_strength == "PPG 5x3" ~ "5v3",
                                   game_strength == "4x5" ~ "4v5",
                                   game_strength == "4x4" ~ "4v4",
                                   game_strength == "GWG PPG 5x4" ~ "5v4",
                                   game_strength == "GWGSHG  5x4" ~ "4v5",
                                   game_strength == "GWG 4x4" ~ "4v4",
                                   game_strength == "SHG 5x4" ~ "4v5",
                                   game_strength == "SHG  5x4" ~ "4v5",
                                   game_strength == "GWG  4x4" ~ "4v4",
                                   game_strength == "PPG 4x3" ~ "4v3",
                                   game_strength == "5x3" ~ "5v3",
                                   game_strength == "3x4" ~ "3v4",
                                   game_strength == "4x3" ~ "4v3",
                                   game_strength == "3x5" ~ "3v5",
                                   TRUE ~ as.character(game_strength)))

goal_data <- new_data %>%
  select(assist = primary_assist, goal = goal, team = team, game_strength = game_strength, season = season) %>%
  mutate(assist = ifelse(is.na(assist), goal, assist))


get_data <- function(my_season, my_team) {
  

  get_betweenness <- function(mydata) {
    mydata %>%
      filter(season == my_season) %>%
      filter(team == my_team) %>%
      select(-c(team, season, game_strength)) %>%
      as.matrix() %>%
      graph.edgelist(directed = TRUE) %>%
      betweenness(directed = TRUE, normalized = TRUE) %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      rename(name = "rowname", betweenness = ".") %>%
      mutate(team = my_team, season = my_season)
    
  }
  
  data_all_situations <- goal_data %>%
    get_betweenness()
  
  data_five_on_five <- goal_data %>%
    filter(game_strength == "5v5") %>%
    get_betweenness()
  
  data_even_strength <- goal_data %>%
    filter(game_strength == "5v5" | game_strength == "4v4" | game_strength == "3v3") %>%
    get_betweenness()  
  
  data_power_play <- goal_data %>%
    filter(game_strength == "5v4" | game_strength == "5v3" | game_strength == "4v3") %>%
    get_betweenness()
  
  data_penalty_kill <- goal_data %>%
    filter(game_strength == "4v5" | game_strength == "3v5" | game_strength == "3v4") %>%
    get_betweenness()
  
  full_data <- data_all_situations %>%
    left_join(data_five_on_five, by = c("name", "team", "season")) %>%
    left_join(data_even_strength, by = c("name", "team", "season")) %>%
    left_join(data_power_play, by = c("name", "team", "season")) %>%
    left_join(data_penalty_kill, by = c("name", "team", "season")) %>%
    select(name, team, season, betweenness_all = betweenness.x, betweenness_5v5 = betweenness.y, betweenness_ev = betweenness.x.x, betweenness_pp = betweenness.y.y, betweenness_pk = betweenness) %>%
    as_tibble() %>%
    arrange(desc(betweenness_5v5))
  
  return(full_data)
  
}  


try_get_data <- function(my_season, my_team) {
  tryCatch(get_data(my_season, my_team), error = function(e) {data_frame()})}  

distinct_teams <- goal_data %>%
  select(season, team) %>%
  distinct()


between_data <- pmap_df(list(distinct_teams[["season"]], distinct_teams[["team"]]), try_get_data)







number <- link_list %>% 
  select(parameter) %>%
  mutate(parameter = str_split(parameter, "_", n = 3)) %>% 
  flatten() %>% 
  pluck(2) %>%
  unlist() 

season <- link_list %>% 
  select(parameter) %>%
  mutate(parameter = str_split(parameter, "_", n = 3)) %>% 
  flatten() %>% 
  pluck(3) %>%
  unlist()

mydata <- data_frame(number, season) %>%
  mutate(game_type = link_list[["value"]])


  mutate(season_short = str_c(str_sub(season, 3, 4), str_sub(season, 7, 8))) %>%
  mutate(url = str_c("uscho.com/recaps/?p=", season_short, ""))




pg <- render_html(url = 'http://www.uscho.com/recaplink.php?gid=1_970_20172018')

lua_ex <- '

function main(splash)
  splash:go("http://www.uscho.com/recaplink.php?gid=1_970_20172018")
  splash:wait(0.5)
  local url = splash:url()
  return {url=url}
end

'

  
  
library(tidyverse)
library(magrittr)

mylist <- list("this", "is", "pretty", "cool")

mylist[[3]]
mylist %>% magrittr::extract2(3)

# looping variable can be i
for (i in 1:10) {
  print(i)
}

# or it can be another single character
for (x in 1:10) {
  print(x)
}

# or any combo of characters (assuming it begins with a letter)
for (look_at_all_these_characters_woah in 1:10) {
  print(look_at_all_these_characters_woah)
}
