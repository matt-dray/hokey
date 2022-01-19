type <- function(n = 5) {
  
  if (!keypress::has_keypress_support()) {
    stop("{keypress} isn't supported in this terminal.\n")
  }
  
  for (i in 3:1) {
    cat(paste0(i, "... "))
    Sys.sleep(1)
  }
  
  cat("Go!\n")
  
  q <- 0  # question counter
  s <- 0  # score counter
  t0 <- Sys.time()  # start time
  
  while (q < n) {
    
    q <- q + 1
    
    l <- sample(letters, 1)
    cat(paste0("Press '", l, "'!"))
    
    kp <- keypress::keypress()
    kp <- tolower(kp)
    
    if (kp == l) {
      cat(" Hit!\n")
      s <- s + 1
    }
    
    if (kp != l) {
      cat(paste0(" '", kp, "'? Miss!\n"))
    }
    
  }
  
  t1 <- Sys.time()
  dur <- round(t1 - t0, 3)
  score <- paste0(s, "/", n)
  
  cat("End!", score, "in", dur, "seconds.\n")
  
  if (score == s) {
    cat("Perfect!\n")
  }
  
}

adventure <- function(len = 10) {
  
  if (!keypress::has_keypress_support()) {
    stop("{keypress} isn't supported in this terminal.\n")
  }
  
  cat("Press left/right arrow keys\n")
  
  sprite <- "."
  space  <- "-"
  
  l <- as.list(rep(space, len))
  loc <- floor(len / 3)
  
  l[loc] <- sprite
  
  cat(paste(unlist(l), collapse = ""), "\n")
  
  win <- FALSE
  
  while (win == FALSE) {
    
    kp <- keypress::keypress()
    
    if (l[[1]] == sprite) {
      kp <- "right"
    }
    
    l[loc] <- space
    
    if (kp == "left") {
      loc <- loc - 1
      msg <- "<"
    }
    
    if (kp == "right") {
      loc <- loc + 1
      msg <- ">"
    }
    
    l[loc] <- sprite
    
    if (loc == 1) {
      msg <- "x"
    }
    
    if (loc == len) {
      msg <- "!"
      win <- TRUE
    }
    
    cat(paste(unlist(l), collapse = ""), msg, "\n")
    
  }
  
}

.make_sprite <- function() {
  
  eyes_list  <- list(".", "'", "-", "O")
  mouth_list <- list("o", "v", "_", "o", "=", "u")
  face_list  <- list(
    c("[", "]"), c("{", "}"), c("(", ")"),
    c("|", "|"), c("<", ">")
  )
  
  eyes  <- sample(eyes_list, 1)
  mouth <- sample(mouth_list, 1)
  face  <- sample(face_list, 1)[[1]]
  
  paste(face[1], eyes, mouth, eyes, face[2])
  
}

.battle <- function(hp, sprite) {
  
  cat(paste0("NEW FOE! ", sprite, " ", hp, " HP ", "\n"))
  
  hits <- 0
  
  while (hits < hp) {
    hits <- hits + 1
    keypress::keypress()
    cat(".")
  }
  
  substr(sprite, 3, 3) <- "x"
  substr(sprite, 7, 7) <- "x"
  
  cat(paste0("\nVICTORY! ", sprite, "  0 HP\n\n"))
  
}

battle <- function(n = 3) {
  
  if (!keypress::has_keypress_support()) {
    stop("{keypress} isn't supported in this terminal.\n")
  }
  
  sprites <- c()
  for (i in 1:n) {
    sprites[i] <- .make_sprite()
  }
  
  hp <- c()
  for (i in 1:n) {
    hp[i] <- i * 10
  }
  
  monsters <- setNames(as.list(hp), sprites)
  
  for (i in seq_along(monsters)) {
    x <- monsters[i]
    .battle(x, names(x))
  }
  
}
