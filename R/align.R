# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}


library(Biostrings)
library(ggplot2)

getColor <- function(c) {
  if(c == "A")
  {
    return ("red");
  }

  if(c == "G")
  {
    return ("green");
  }

  if(c == "C")
  {
    return ("blue");
  }

  if(c == "T")
  {
    return ("#608899");
  }

  if(c== "U")
  {
    return ("#287c8eff")
  }

  return ("black");
}

draw_compare <- function (x, y)
{
  n = nchar(x);
  df <- data.frame()
  sp = ggplot(df) + geom_point() + xlim(0, n+1) + ylim(-1, 1)

  #t = -1

  eq = c()
  for(i in 1:n)
  {
    chrx = substr(x, i, i);
    chry = substr(y, i, i);

    sp <- sp +  annotate(geom="text", x=i, y=0.2, label=chrx, color=getColor(chrx), size = 10) +

      annotate(geom="text", x=i, y=-0.2, label=chry, color=getColor(chry), size = 10)

    if(chrx == chry)
    {
      # print("eq")
      # t = i
      # print(t)
      # sp <- sp +  geom_segment(x = 1, y = -0.15, xend = 1, yend = 0.15)

      eq = c(eq, i)
    }

  }

  if(length(eq) != 0)
  {
    sp <- sp +  geom_segment(aes(x = eq, y = -0.14, xend = eq, yend = 0.14))
  }

  return (sp)
}

align <- function(x, y)
{
  res = pairwiseAlignment(pattern = c(x), subject = y)
  resx = pattern(res)
  resy = subject(res)
  resx = as.character(resx)
  resy = as.character(resy)
  g = draw_compare(resx, resy)
  return (g)
}

