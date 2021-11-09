###Welcome to the source file for the UNO simulation! :)
# 
# Notes:
#
# OUTDATED order/index of code:
#   Color Enum
#   Card class/initialization
#   Hand initialization
#   Find matching color / number / find wild methods
#   The 6 strategies based on the color/number/wild reorderings
#   The player class/initialization


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

#Card class has elements: isSpecial, number, color, specialType
generateCard <- function() {
  newCard <- list(isSpecial = FALSE, number = "NA", color = "NA", specialType = "NA")
  class(newCard) <- "Card"
  
  #TODO - determine if card should be special
  if(!newCard$isSpecial) {
    newCard$number = sample(0:9, 1)
    newCard$color = sample(ColorsEnum, 1)
  }
  
  newCard
}

#length 7 list of Cards
generatehand <- function() {
  list(generateCard(), generateCard(), generateCard(), generateCard(), generateCard(), generateCard(), generateCard())
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
  for(i in 1:length(hand)) {
    if(hand[[i]]$number == topcard$number) {
      card = hand[i]
      hand = hand[-i]
      return(list(1, card, hand))
    }
  }
  return(list(0))
}

#TODO - CHOOSE COLOR OF WILD
has_wild <- function(hand, topcard) {
  for(i in 1:length(hand)) {
    if(identical(hand[[i]]$color, "NA")) {
      card = hand[i]
      hand = hand[-i]
      return(list(1, card, hand))
    }
  }
  return(list(0))
}

##Color match, then number match, then wildcard
color_number_wild <- function(hand, topcard) {
  result <- color_match(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  result <- number_match(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
  result <- has_wild(hand, topcard)
  if(result[[1]] == 1) { return(result)}
  
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

gethand <- function(shuffled_deck) {
  list()
}

drawcard <- function(deck) {
  card <- deck[[length(deck)]]
  deck = deck[[-length(deck)]]
  list(card, deck)
}

#Player class
initializePlayer <- function(strategy) {
  newPlayer <- list(playCard = strategy, hand = list())
  class(newPlayer) <- "Player"
  newPlayer
}

#Game class
createGame <- function(playerlist) {
  # tc <- generateCard()
  # while(tc$isSpecial) {
  #   tc <- generateCard()
  # }
  
  newGame <- list(players = playerlist, topcard = "notyet", nextPlayer = 0, winner = NULL, turn = 0, isReverseOrder = FALSE
                  ,deck = sample(newDeck()), pile = list())
  
  for(i in length(newGame$players)) {
    if(i %% 2 == 0) {
      for(j in length(newGame$deck):(length(newGame$deck) - 7)) {
        newGame$players[[i]] <- append(newGame$players[[i]], newGame$deck[[j]])
        newGame$deck = newGame$deck[[-j]]
      }
    }
  }
  
  tc <- newGame$deck[[length(newGame$deck)]]
  while(tc$isSpecial) {
    index = sample(1:(length(newGame$deck) - 5), 1)
    newGame$deck[[length(newGame$deck)]] <- newGame$deck[[index]]
    newGame$deck[[index]] <- tx
    tc <- newGame$deck[[length(newGame$deck)]]
  }
  newGame$deck = newGame$deck[[-length(newGame$deck)]]
  newGame$topcard = tc
  
  
  class(newGame) <- "Game"
  newGame
}

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
    
    if(result[[1]] == 0) {
      #Player draws from deck
      carddeck = drawcard(game$deck, game$pile)
      game$deck = carddeck[[2]]
      game$players[[game$nextPlayer * 2]] = append(game$players[[game$nextPlayer * 2]], carddeck[[1]])
    }
    else{
      if(length(result[[3]]) == 0) {
        game$winner = playerfunctionlist[[game$nextPlayer]]
      }
      game$pile = append(game$pile, game$topcard) # TODO CHECK LINE FOR CORRECTNESS
      game$topcard = result[[2]][[1]]
      game$players[[game$nextPlayer * 2]] = result[[3]]
      
      #TODO Check For Wild Card & do results
      #TODO Check for skip, +2, reverse & do results
    }
    
  }
  cat(nameoffunctionslist[[game$nextPlayer]])
  cat(game$turn)
  #  return(nameoffunctionslist[[game$nextPlayer]], game$turn) #add any data here
}

###   EXAMPLE GAME
#simulateGame(list(color_number_wild, number_color_wild), list("cnw", "ncw"))
