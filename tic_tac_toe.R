library(dplyr)

tic_tac_toe <- function(print_matrices = F){
  
  print_matrix <- function(x){
    if(print_matrices){
      print(paste0("This is matrix '", deparse(substitute(x)),"'"))
      print(x)
    }
  }
  
  get_coords <- function(player = whose_turn){
    where_next <- readline(paste0(player, " to play, which row and column (input as 'row, column'): "))
    coords_next <- str_split(where_next, ",") %>% unlist() %>% as.numeric()
    coords_next
  }
  
  check_for_win <- function(player = whose_turn){
    score_matrix <- get(player)
    row_win <- rowSums(score_matrix) == 3
    col_win <- colSums(score_matrix) == 3
    diag1_win <- sum(diag(score_matrix)) == 3
    diag2_win <- sum(diag(score_matrix[, 3:1])) == 3
    if(any(row_win, col_win, diag1_win, diag2_win)){
      print(paste0(player, " is the WINNER!"))
      plot(1:3, 1:3, type = "n", axes = F, xlim = c(0.5, 3.5), ylim = c(0.5, 3.5), xlab = "", ylab = "", cex.lab = 2)
      text(2, 2.5, player, cex = 8)
      text(2, 1.5, "is the WINNER!", cex = 3)
      break # need to change this so that turn >= 9
    }
  }
  
  turn <- 0
  par(mar = c(5.1, 5.1, 4.1, 3.1))
  plot(1:3, 1:3, type = "n", axes = F, xlim = c(0.5, 3.5), ylim = c(0.5, 3.5), xlab = "Column", ylab = "Row", cex.lab = 2)
  abline(h = c(1.5, 2.5), lwd = 5)
  abline(v = c(1.5, 2.5), lwd = 5)
  mtext(side = 1, line = 1, at = 1:3, text = 1:3, col = "darkgrey", cex = 3)
  mtext(side = 2, line = 1, at = 1:3, text = 1:3, las = 1, col = "darkgrey", cex = 3)
  mtext(side = 3, line = 1, at = 1:3, text = 1:3, col = "darkgrey", cex = 3)
  mtext(side = 4, line = 1, at = 1:3, text = 1:3, las = 1, col = "darkgrey", cex = 3)
  o <- x <- matrix(rep(0, 9), ncol = 3)
  places_taken <- matrix(rep(F, 9), ncol = 3)
  xo_seq <- rep(c("x", "o"), length.out = 10)
  
  while(turn != 9){
    if(turn == 0){
      which_start <- readline("'x' or 'o' to start? ")
      if(which_start == "x"){
        xo_seq <- xo_seq[1:9]
      } else if(which_start == "o"){
        xo_seq <- xo_seq[2:10]
      } else {
        print("Well, I asked a simple question, and if you can't do that simple thing I'm going to have a tantrum.")
        break
      }
    }
    
    turn <- turn + 1
    whose_turn <- xo_seq[turn]
    
    where_next <- get_coords()
    
    while(places_taken[where_next[1], where_next[2]]){
      print("Sorry, that place is already occupied, please select another.")
      where_next <- get_coords()
    }
    
    places_taken[where_next[1], where_next[2]] <- T
    print_matrix(places_taken)
    
    if(whose_turn == "x"){
      x[where_next[1], where_next[2]] <- 1
      print_matrix(x)
    } else {
      o[where_next[1], where_next[2]] <- 1
      print_matrix(o)
    }
    
    points(where_next[2], where_next[1], pch = whose_turn, cex = 8)
    
    check_for_win()
    
    if(turn >= 9){
      print("Oh dear, nobody won... :-(")
      plot(1:3, 1:3, type = "n", axes = F, xlim = c(0.5, 3.5), ylim = c(0.5, 3.5), xlab = "", ylab = "", cex.lab = 2)
      text(2, 2.5, "Oh dear!", cex = 3)
      text(2, 1.5, "Nobody won... :-(", cex = 3)
    }
  }
}

tic_tac_toe(print_matrices = F)
