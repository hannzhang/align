##################################################
## Purpose: sequence alignment visualization R shiny
## Author:  Helen Zhang
## Date:    2018/12/04
## Version: 0.1.0
## Bugs:    not known yet
##################################################
library(Biostrings)
library(ggplot2)
library('shiny')

#' Get color of the character
#'
#' @param c A character
#' @return the color string of the character
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

  # black for others
  return ("black")
}

#' Draw the plot of the two matched string
#'
#' @param x A string
#' @param y A string
#' @return ggplot of the matched string
#' @importFrom ggplot2 ggplot geom_point xlim ylim annotate geom_segment aes labs
draw_compare <- function (x, y)
{
  n <- nchar(x)
  df <- data.frame()
  # create a empty plot
  sp <- ggplot(df) + geom_point() + xlim(0, n + 1) + ylim(-1, 1) +
    labs(x = "position of nucleotide", y = "")

  # eq record positions matched
  eq <- c()
  for (i in seq(1, n))
  {
    chrx <- substr(x, i, i)
    chry <- substr(y, i, i)

    sp <-
      # draw the character in first string
      sp +  annotate(
        geom = "text",
        x = i,
        y = 0.2,
        label = chrx,
        color = getColor(chrx),
        size = 10
      ) +

      # draw the character in second string
      annotate(
        geom = "text",
        x = i,
        y = -0.2,
        label = chry,
        color = getColor(chry),
        size = 10
      )

    # add the matched position
    if (chrx == chry)
    {
      eq <- c(eq, i)
    }
  }

  # if has matched position
  # draw vertical lines to connect them
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
  if(!is.character(x))
  {
    stop("the first argument is not string");
  }

  if(!is.character(y))
  {
    stop("the second argument is not string");
  }
  # align the two string
  res <- pairwiseAlignment(pattern = c(x), subject = y)
  # get characters of the matched result
  resx <- pattern(res)
  resy <- subject(res)
  resx <- as.character(resx)
  resy <- as.character(resy)
  # draw the matched alignment
  g <- draw_compare(resx, resy)
  return (g)
}

# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Your Title"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "first",
                label = "First:",
                value = "GACT"),

      textInput(inputId = "second",
                label = "Second:",
                value = "GAGCGT"),

      actionButton("update", "Update View")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),

      plotOutput('plot')

    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  # @import
  output$caption <- renderText({
    "Result:"
  })

  datasetInput <- eventReactive(input$update, {
    align(input$first, input$second)

  }, ignoreNULL = FALSE)

  output$plot <- renderPlot({

    p <- datasetInput()

    print(p)

  }, height=500)

}

# Create Shiny app ----
shinyApp(ui, server)

# [END]
