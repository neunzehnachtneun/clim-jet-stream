## source('~/01-Master-Thesis/02-code-git/a-help-functions.r')
## 
## KLEINE HILFSFUNKTIONEN
####
####


## hilfsfunktion zur bestimmung der länge eines vektors ohne berücksichtigung von NAs
len_na <- function(x) {
  len <- sum(!is.na(x))
  return(len)
}

## hilfsfunktion zur bestimmung der zwei größten werte eines vektors
diff_max <- function(x) {
  xx <- x
  # auffinden der zwei größten werte
  xx <- if (length(which(!is.na(xx)))) xx[!is.na(xx)] else xx
  y_max_1 <- max(xx)
  xx <- if (length(xx) == 1) NA else if (length(which(!is.na(xx)))) xx[which(xx < max(xx))] else xx
  y_max_2 <- max(xx)
  # positionen der zwei größten werte
  x_max_1 <- if (!is.na(y_max_1)) which(x == y_max_1) else NA
  x_max_2 <- if (!is.na(y_max_2)) which(x == y_max_2) else NA
  return(list(max.x = c(x_max_1, x_max_2), max.y = c(y_max_1, y_max_2)))
}

# Hilfsfunktion zum Auffüllen von Vektoren
fun_fill_vec <- function(x, n) {
  while (length(x) < n) {
    x <- c(x, NA)
  }
  return(x)
}

####
## FUNKTION ZUR NORMIERUNG VON VEKTOREN ####
## SCALE() LIEFERT KEIN VERGLEICHBARES ERGEBNIS.
####
normalize_vec <- function(vec) {
  normalized_vec <- vec / sqrt( sum(vec ** 2))
  return(normalized_vec)
}


####
## FUNKTION ZUM SPEICHERN VON PLOTS IM PDF- UND TIKZ-FORMAT ####
####
save_plot <- function(plt, 
                     width = 135, height, pointsize = 9, 
                     filepath, filename, 
                     format_pdf = TRUE, format_tikz = TRUE) {
  library(ggplot2)
  library(tikzDevice)
  
  # save as pdf
  if (format_pdf) {
    ggsave(plot = plt, device = pdf, 
           path = filepath, 
           filename = paste0(filename, ".pdf"),
           dpi = 600, width = width, height = height, units = "mm")
  }
  
  # save as tex
  if (format_tikz) {
    # tikz(file = paste0(filepath, filename, ".tex"), 
    #      width = width/2.51, height = height/2.51)
    # print(plt)
    # dev.off()
    ggsave(plot = plt, device = tikz,
           path = filepath,
           filename = paste0(filename, ".tex"),
           dpi = 600, width = width, height = height, units = "mm")
  }
}




