dice_roll <- function(x){  # takes a vector of integers
  stopifnot(is.numeric(x), x %in% c(4, 6, 8, 10, 12, 20))
  data.frame(die = sapply(x, function(x){paste0(x, "-sided")}),
             result = sapply(x, sample, 1))
}

dice_roll(6)
dice_roll(c(4, 8, 20, 20))
