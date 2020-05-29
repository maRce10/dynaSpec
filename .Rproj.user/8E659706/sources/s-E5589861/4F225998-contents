#internal dynaSpec function called by scrolling_spectro. Fades in and out annotated text. Not to be called by users.
# x is a vector with alpha (transparesence values)
# start and end are the position of the fading in and out
# fading is the length of the fading period, text is the annotation text
fading_text_dynaspec_int <- function(x, start, end, alpha = 1, fading, labels){
  
  if (fading == 0) fading <- 1
  
  #fading-in part
  fi <- seq(0, alpha, length.out = fading)
  
  # fade out 
  fo <- rev(fi)

  # modify x
  x[start:end] <- alpha
  x[start:(start + fading -1)] <- fi
  x[(end - fading + 1):(end)] <- fo
  
  names(x)[start:end] <- labels
  
  return(x)
}
