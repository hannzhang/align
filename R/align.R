

#' Get color of the character
#'
#' @param c A character
#' @return the color string of the character
#' @examples
#' getColor("A")
#' getColor("G")
#' getColor("C")
#' @export
getColor <- function(c) {
  if (c == "A")
  {
    return ("red")

  }

  if (c == "G")
  {
    return ("green")

  }

  if (c == "C")
  {
    return ("blue")

  }

  if (c == "T")
  {
    return ("#608899")

  }

  if (c == "U")
  {
    return ("#287c8eff")
  }

  return ("black")

}

#' Draw the plot of the two mathed string
#'
#' @param x A string
#' @param y A string
#' @return ggplot of the matched string
#' @examples
#' draw_compare("AG-T", "AGCT")
#' draw_compare("GAGCGT", "GA-C-T")
#' @export
#' @importFrom ggplot2 ggplot geom_point xlim ylim annotate geom_segment aes
draw_compare <- function (x, y)
{
  n = nchar(x)

  df <- data.frame()
  sp = ggplot(df) + geom_point() + xlim(0, n + 1) + ylim(-1, 1)

  #t = -1

  eq = c()
  for (i in 1:n)
  {
    chrx = substr(x, i, i)

    chry = substr(y, i, i)


    sp <-
      sp +  annotate(
        geom = "text",
        x = i,
        y = 0.2,
        label = chrx,
        color = getColor(chrx),
        size = 10
      ) +

      annotate(
        geom = "text",
        x = i,
        y = -0.2,
        label = chry,
        color = getColor(chry),
        size = 10
      )

    if (chrx == chry)
    {
      eq = c(eq, i)
    }

  }

  if (length(eq) != 0)
  {
    sp <-
      sp +  geom_segment(aes(
        x = eq,
        y = -0.14,
        xend = eq,
        yend = 0.14
      ))
  }

  return (sp)
}


#' Align the two string and draw the plot of the alignment
#'
#' @param x A string
#' @param y A string
#' @return ggplot of the alignment of x and y
#' @examples
#' align("ACT", "AGCT")
#' align("GAGCGT", "GACT")
#' align("ACTCGCAATATGVTAGGVVAG", "ACTTTATGCTATGCGC")
#' @export
#' @importFrom Biostrings pairwiseAlignment pattern subject
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
