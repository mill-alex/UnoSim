###Welcome to the source file for the UNO simulation! :)
# 
# Order/index of code:
#   Color Enum
#   Create Cards and Deck
#   Find matching color / number / find wild methods
#   The 6 strategies based on the color/number/wild reorderings
#   Functions for drawing a card from the deck
#   The player class/initialization
#   Game initialization and use
#   Loop for running many games and collecting data

#CARD NUMBERs: 0:9 is that int, 10 is reverse, 11 is skip, 12 is draw 2, -1 is wild, -4 is wild4


ColorsEnum <- list(BLUE = 1, RED = 2, YELLOW = 3, GREEN = 4)

#Create Deck
BASE_DECK <- vector("list", length = 108)
for(i in 1:8) {
  for(j in 1:9) {
    BASE_DECK[[12 * (i - 1) + j]] <- list(isSpecial = FALSE, number = j, color = ColorsEnum[[i %% 4 + 1]])
  }
  for(j in 10:12) {
    BASE_DECK[[12 * (i - 1) + j]] <- list(isSpecial = TRUE, number = j, color = ColorsEnum[[i %% 4 + 1]])
  }
}
for(i in 1:4) {
  BASE_DECK[[96 + i]] <- list(isSpecial = TRUE, number = -1, color = "NA") #WILD
  BASE_DECK[[100 + i]] <- list(isSpecial = TRUE, number = -4, color = "NA") #WILD 4
  BASE_DECK[[104 + i]] <- list(isSpecial = FALSE, number = 0, color = ColorsEnum[[i %% 4 + 1]]) #ZEROs
}

#Get a copy of the base deck
newDeck <- function() {
  BASE_DECK
}

#STRATEGIES

color_match <- function(hand, topcard) {
  for(i in 1:length(hand)) {
    if(identical(hand[[i]]$color, topcard$color)) {
      card = hand[i]
      hand = hand[-i]
      return(list(1, card, hand))
    }
  }
  return(list(0))
}

number_match <- function(hand, topcard) {
  if(topcard$number < 0) { return(list(0)) }
  
  for(i in 1:length(hand)) {
    if(hand[[i]]$number == topcard$number) {
      card = hand[i]
      hand = hand[-i]
      return(list(1, card, hand))
    }
  }
  return(list(0))
}

has_wild <- function(hand, topcard) {
  for(i in 1:length(hand)) {
    if(identical(hand[[i]]$color, "NA")) {
      card = hand[i]
      hand = hand[-i]
      card[[1]]$color = ColorsEnum[[sample(1:4, 1)]]
      return(list(1, card, hand))
    }
  }
  return(list(0))
}

##Color match, then number match, then wildcard
color_number_wild <- function(hand, topcard) {
  result <- color_match(hand, topcard)
  if(result[[1]] == 1) {
    return(result)
  }
  
  result <- number_match(hand, topcard)
  if(result[[1]] == 1) {
    return(result)
  }
  
  result <- has_wild(hand, topcard)
  if(result[[1]] == 1) { 
    return(result)
  }
  
  #No playable cards
  return(list(0, "draw"))
}

color_wild_number <- function(hand, topcard) {
  result <- color_match(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  result <- has_wild(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  result <- number_match(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  #No playable cards
  return(list(0, "draw"))
}

wild_number_color <- function(hand, topcard) {
  result <- has_wild(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  result <- number_match(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  result <- color_match(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  #No playable cards
  return(list(0, "draw"))
}

wild_color_number <- function(hand, topcard) {
  result <- has_wild(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  result <- number_match(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  result <- color_match(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  #No playable cards
  return(list(0, "draw"))
}

number_color_wild <- function(hand, topcard) {
  result <- number_match(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  result <- color_match(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  result <- has_wild(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  #No playable cards
  return(list(0, "draw"))
}

number_wild_color <- function(hand, topcard) {
  result <- number_match(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  result <- has_wild(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  result <- color_match(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  #No playable cards
  return(list(0, "draw"))
}

drawcard <- function(deck, pile) {
  if(length(deck) == 0) {
    if(length(pile) == 0) { stop("Attempt to draw card when pile and deck are empty.")}
    deck <- sample(pile)
    pile = list()
  }
  card <- deck[[length(deck)]]
  deck = deck[-length(deck)]
  list(card, deck, pile)
}

setGameAfterDrawCard <- function(game) {
  #Player draws from deck
  carddeck = drawcard(game$deck, game$pile)
  game$deck = carddeck[[2]]
  game$pile = carddeck[[3]]
  game$players[[game$nextPlayer * 2]] = append(game$players[[game$nextPlayer * 2]], list(carddeck[[1]]))
  game
}

#Player class
initializePlayer <- function(strategy) {
  newPlayer <- list(playCard = strategy, hand = vector("list", length = 7))
  class(newPlayer) <- "Player"
  newPlayer
}

#Game class, setup for a game of Uno
createGame <- function(playerlist) {
  
  newGame <- list(players = playerlist, topcard = "notyet", nextPlayer = 0, winner = NULL, turn = 0, isReverseOrder = FALSE
                  ,deck = sample(newDeck()), pile = list())
  
  #set each player's hand
  for(i in 1:length(newGame$players)) {
    if(i %% 2 == 0) {
      startlen = length(newGame$deck)
      for(j in startlen:(startlen - 6)) {
        newGame$players[[i]][[j + 7 - startlen]] <- newGame$deck[[j]]
        newGame$deck = newGame$deck[-j]
      }
    }
  }
  
  #SET TOP CARD - pull non-special card from deck
  tc <- newGame$deck[[length(newGame$deck)]]
  while(tc$isSpecial) {
    index = sample(1:(length(newGame$deck) - 5), 1)
    newGame$deck[[length(newGame$deck)]] <- newGame$deck[[index]]
    newGame$deck[[index]] <- tc
    tc <- newGame$deck[[length(newGame$deck)]]
  }
  newGame$deck = newGame$deck[-length(newGame$deck)]
  newGame$topcard = tc
  
  class(newGame) <- "Game"
  newGame
}

#Identify next player in the game
nextPlayer <- function(game) {
  expected = game$nextPlayer
  if(game$isReverseOrder) {
    expected = expected - 1
  }
  else {
    expected = expected + 1
  }
  
  if(expected < 1) {
    expected = length(game$players) / 2
  }
  else if(expected > length(game$players) / 2) {
    expected = 1
  }
  expected
}

#Declare odds that a player forgets to say Uno
#This is implemented so that there is a 1/n chance that the player is beaten to saying Uno
#when they're supposed to, resulting in them drawing 2 cards. For example, if n is 4, there
#is a 25% chance they will have to draw 2 cards, and a 75% chance they will draw no cards.
forgot_to_say_uno = 4

#Run one whole game of Uno
simulateGame <- function(playerfunctionlist, nameoffunctionslist) {
  playerlist = list()
  for(f in playerfunctionlist) {
    playerlist = append(playerlist, initializePlayer(f))
  }
  game <- createGame(playerlist)
  
  while(is.null(game$winner)) {
    game$turn = game$turn + 1
    game$nextPlayer = nextPlayer(game)
    result <-  game$players[[game$nextPlayer * 2 - 1]](game$players[[game$nextPlayer * 2]], game$topcard) #Calls playCard()

    #If no card is played
    if(result[[1]] == 0) {
      game = setGameAfterDrawCard(game)
    }
    else{
      if(length(result[[3]]) == 0) {
        game$winner = playerfunctionlist[[game$nextPlayer]]
      }
      if(game$topcard$number < 0) {
        #Reset color of now-unused wild card
        game$topcard$color = "NA"
      }
     
      #Set pile, top card, and the player's hand
      game$pile = append(game$pile, list(game$topcard))
      game$topcard = result[[2]][[1]]
      game$players[[game$nextPlayer * 2]] = result[[3]]
      num = game$topcard$number
      
      #Check for UNO!
      if(length(game$players[[game$nextPlayer * 2]]) == 1) {
        if(sample(1:forgot_to_say_uno, 1) == forgot_to_say_uno) {
          #The player with uno didn't say it first & draws 2 cards
          game = setGameAfterDrawCard(game)
          game = setGameAfterDrawCard(game)
        }
      }
      
      #Perform any special actions from topcard:
      
      #Reverse
      if(num == 10) {
        game$isReverseOrder = ! game$isReverseOrder
      }
      #Skip
      if(num == 11) {
        game$nextPlayer = nextPlayer(game)
      }
      #Draw 2 (also skips)
      if(num == 12) {
        game$nextPlayer = nextPlayer(game)
        game = setGameAfterDrawCard(game)
        game = setGameAfterDrawCard(game)
      }
      #Draw 4 (also skips)
      if(num == -4) {
        game$nextPlayer = nextPlayer(game)
        for(i in 1:4) {
          game = setGameAfterDrawCard(game)
        }
      }
    }
    
  }

    return(list(nameoffunctionslist[[game$nextPlayer]], game$turn)) #add any data here
}

## RUN MANY SIMULATIONS PITTING DIFFERING STRATEGIES

results = as.data.frame(matrix(NA, nrow = 66000, ncol = 5))
colnames(results) = c("P1 Strat", "P2 Strat", "Winner", "Turns", "WinnerWentFirst")

Total_Strategies = list(color_number_wild, color_wild_number, number_color_wild, number_wild_color, wild_color_number, wild_number_color)
Strategy_names = list("color_number_wild", "color_wild_number", "number_color_wild", "number_wild_color", "wild_color_number", "wild_number_color")
counter = 0

#TODO remove
tot_turns1 = 0

for(i in 1:5){
  for(j in (i+1):6){
    for(k in 1:100){
      counter = counter + 1
      winner_ij = simulateGame(list(Total_Strategies[[i]], Total_Strategies[[j]]), list(Strategy_names[[i]], Strategy_names[[j]])) 
      results[counter,] = c(Strategy_names[[i]], Strategy_names[[j]], winner_ij, (Strategy_names[[i]] == winner_ij[[1]]))
      counter = counter + 1
      winner_ji = simulateGame(list(Total_Strategies[[j]], Total_Strategies[[i]]), list(Strategy_names[[j]], Strategy_names[[i]]))
      results[counter,] = c(Strategy_names[[j]], Strategy_names[[i]], winner_ji, (Strategy_names[[j]] == winner_ji[[1]]))
      
      #TODO remove
      tot_turns1 = tot_turns1 + winner_ij[[2]] + winner_ji[[2]]
    }
  }
}

# for(i in 1:6){
#   for(j in 1:1000){
#     counter = counter + 1
#     winner = simulateGame(Total_Strategies[[i]], Total_Strategies[[i]])
#     results[counter,] = c(Total_Strategies[[i]], Strategy_names[[i]], winner)
#   }
#}