library(tidyverse)
library(data.table)
library(randomNames)

shuffle_deck <- function(n_cards = 52, random_seed = Sys.time()) {
  if(n_cards > 52 | n_cards < 1) {
    stop('Number of cards cannot be greater than 52 or less than 1')
  }
  
  set.seed(random_seed)
  
  suites <- factor(c('Spade', 'Club', 'Heart', 'Diamond'))
  card_values <- c('2','3','4','5','6','7','8','9','10','Jack','Queen','King','Ace')
  card_values <- ordered(card_values,levels = card_values)
  
  deck_df <- expand.grid(suites, card_values) %>%
    transmute(suite = Var1,
              value = Var2)
  
  return(deck_df[sample(1:52, n_cards),] %>%
           mutate(order = row_number()))
}


shuffled_deck_df <- shuffle_deck(n_cards = 52, random_seed = 123)


hands_df <- data.frame(hand = c('Royal Flush',
                                'Straight Flush',
                                'Four of a Kind',
                                'Full House',
                                'Flush',
                                'Straight',
                                'Three of a Kind',
                                'Two Pair',
                                'Pair',
                                'High Card'
))

deal_new_round <- function(n_players, player_name, player_position = NULL, random_seed = Sys.time()) {
  if(n_players > 10 | n_players < 2) {
    stop('Number of players must be between 2 and 10')
  }
  if(is.null(player_position)) {
    player_position <- sample(1:n_players, 1)
  } else if(player_position > n_players | player_position < 1) {
    stop(paste0('Player position, if specified, must be between 1 and n_players (', n_players, ')'))
  }
  
  players_df <- data.frame(player_position_id = 1:n_players) %>%
    mutate(player_name = randomNames(n = n_players,
                              which.names = "both",
                              name.order = "first.last",
                              name.sep = " ",
                              sample.with.replacement = FALSE),
           main_player_flag = as.logical(rep(FALSE, n_players)))
  
  players_df$player_name[player_position] <- player_name
  players_df$main_player_flag[player_position] <- TRUE
  
  shuffled_deck_df <- shuffle_deck(n_cards = 52, random_seed = random_seed) %>%
    mutate(player_position_id = c(rep(1:n_players, 2), rep(NA, 52-(n_players*2))),
           status = c(rep('Dealt', n_players*2), rep(NA, 52-(n_players*2)))) %>%
    left_join(players_df)
  
  return(shuffled_deck_df)
}




flop <- function(deck_df) {
  n_cards_used <- nrow(deck_df %>% filter(!is.na(status)))
  
  deck_df$status[n_cards_used + 1] <- 'Burnt'
  deck_df$status[(n_cards_used + 2):(n_cards_used + 4)] <- 'Flop'
  
  return(deck_df)
}

turn <- function(deck_df) {
  n_cards_used <- nrow(deck_df %>% filter(!is.na(status)))
  
  deck_df$status[n_cards_used + 1] <- 'Burnt'
  deck_df$status[(n_cards_used + 2)] <- 'Turn'
  
  return(deck_df)
} 

river <- function(deck_df) {
  n_cards_used <- nrow(deck_df %>% filter(!is.na(status)))
  
  deck_df$status[n_cards_used + 1] <- 'Burnt'
  deck_df$status[(n_cards_used + 2)] <- 'River'
  
  return(deck_df)
} 

knowns <- function(deck_df) {
  return(deck_df %>% filter(status %in% c('Flop','Turn','River') | main_player_flag))
}

poker_deck <- deal_new_round(n_players = 5, player_name = 'Nakai') 
poker_deck
knowns(poker_deck)

poker_deck <- flop(poker_deck)
poker_deck
knowns(poker_deck)

poker_deck <- turn(poker_deck)
poker_deck
knowns(poker_deck)

poker_deck <- river(poker_deck)
poker_deck
knowns(poker_deck)



# Hypergeometric Distribution Functions

# probability density function
dhyper(x = )

# distribution function
# 30 balls in an urn, 10 black, 20 white, sample of 5

phyper(q = 2-1, m = 20, n = 10, k = 5)

# quantile function
qhyper(p = c(.3,.5,.7,.2,.3,.4), prob = 0.2)

# random generation function
geo_rand <- rhyper(nn = 10000, prob = 0.5)

ggplot() +
  geom_histogram(aes(x = geo_rand), binwidth = 1)
