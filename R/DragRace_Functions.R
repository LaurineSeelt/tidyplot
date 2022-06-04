library(tidyverse)
library(dragracer)

rpdr_contestants %>% filter(season == "S01" & contestant == "Nina Flowers") %>% select(age)

rpdr_contestants %>% filter(contestant == "Ongina") %>% select(season, age, hometown)

rpdr_contep %>% filter(finale == "1" & season == "S05") %>%
  filter(outcome == "WIN" | outcome == "Winner") %>% select(contestant)

general_info <- function(character){
  general <- rpdr_contestants %>% filter(contestant == character) %>%
    select(season, age, hometown) %>% unlist()
  return(general)
}

# general_info("Jinkx Monsoon")

episode_name <- function(character, numeric) {
  name <- rpdr_ep %>% filter(season == character & episode == numeric) %>%
    select(nickname) %>% unlist()
  return(name)
}

# episode_name("S08", 7)

special_ep <- function(character, numeric) {
special <- rpdr_ep %>% filter(season == character & episode == numeric) %>%
  select(special)
if (special == 1) {
  return("It's a special episode hunty!")
} else {
  return("It's not a special episode")
  }
}

# special_ep("S05", 3)

winner <- function(season) {
  if (season == "S01") {
    return("The queen who won this season was Bebe Zahara Benet")
  } else if (season == "S02") {
    return("The queen who won this season was Tyra Sanchez")
  } else if (season == "S03") {
    return("The queen who won this season was Raja")
  } else if (season == "S04") {
    return("The queen who won this season was Sharon Needles")
  } else if (season == "S05") {
    return("The queen who won this season was Jinkx Monsoon, the GOAT")
  } else if (season == "S06") {
    return("The queen who won this season was Bianca Del Rio")
  } else if (season == "S07") {
    return("The queen who won this season was Violot Chachki")
  } else if (season == "S08") {
    return("The queen who won this season was Bob the Drag Queen")
  } else if (season == "S09") {
    return("The queen who won was Sasha Velour")
  } else if (season == "S10") {
    return("The queen who won this season was Aquaria")
  } else if (season == "S11") {
    return("The queen who won this season was Yvie Oddly")
  } else if (season == "S12") {
    return("The queen who won this season was Jaida Essence Hall")
  } else if (season == "S13") {
    return("The queen who won this season was Symone")
  } else if (season == "S14") {
    return("The queen who won this season was Willow Pill")
  }
}

# winner("S11")
