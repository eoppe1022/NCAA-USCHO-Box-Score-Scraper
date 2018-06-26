library(tidyverse)
library(splashr)
library(rvest)
library(attempt)
library(warrenr)

url <- "http://www.uscho.com/scoreboard/division-i-men/20172018/composite-schedule/"

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
    filter(!c(between(row_number(), 648, 648) & game_type == "EX"))  #row brings errors so I removed it

  as_tibble(data.frame(link_data, game_type))
}

link_list <- get_schedule(url)

urls <- link_list %>%
  filter(game_type != "EX") %>%
  pull(url)

get_box_score <- function(my_url) {
    
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
  
  attempt::try_catch(persistently_get_box_score(my_url), .e = function(e){
    print(e)
    time <- Sys.time()
    a <- paste("+ At", time, ", \nError:",e)
    write(a, "log.txt", append = TRUE)
    print(paste("log saved on log.txt at", time))

    data_frame()
  })
}

mydata_201718 <- pmap_df(list(urls), try_get_box_score)
