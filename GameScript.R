###Welcome to the source file for the UNO simulation! :)
# 
# Current order/index of code:
#   Color Enum
#   Card class/initialization
#   Hand initialization
#   Find matching color / number / find wild methods
#   The 6 strategies based on the color/number/wild reorderings
#   The player class/initialization


ColorsEnum <- list(BLUE = 1, RED = 2, YELLOW = 3, GREEN = 4)

#Card class has elements: isSpecial, number, color, specialType
initializeCard <- function() {
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
makehand <- function() {
  list(initializeCard(), initializeCard(), initializeCard(), initializeCard(), initializeCard(), initializeCard(), initializeCard())
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

#Player class
initializePlayer <- function(strategy) {
  newPlayer <- list(playCard = strategy, hand = makehand())
  class(newPlayer) <- "Player"
  newPlayer
}

#Game class
createGame <- function(playerlist) {
  tc <- initializeCard()
  while(tc$isSpecial) {
    tc <- initializeCard()
  }
  
  newGame <- list(players = playerlist, topcard = tc, nextPlayer = 0, winner = NULL, turn = 0, isReverseOrder = FALSE)
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
    expected = length(game$players)
  }
  else if(expected > length(game$players)) {
    expected = 0
  }
  expected
}

simulateGame <- function(playerfunctionlist) {
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
      #Player draws from pile
      game$players[[game$nextPlayer]]$hand = append(game$players[[game$nextPlayer]]$hand, initializeCard())
    }
    else{
      if(length(result[[2]]) == 0) {
        game$winner = playerlist[[nextplayer]]
      }
      game$topcard = result[[2]]
      game$players[[game$nextPlayer * 2]]$hand = result[[3]]
      
      #TODO Check For Wild Card & do results
      #TODO Check for skip, +2, reverse & do results
    }
    
  }
  
  cat(list(game$winner$playCard, game$turn)) #add any data here
  #  return(list(game$winner$playCard, game$turn)) #add any data here
}

