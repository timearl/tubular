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
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' TubeColours
#'
#' @param extended (FALSE) additionally return non-tube lines on the underground map
#'
#' @return A dataframe of colour values and line names
#'
#' @examples TubeColours()
TubeColours <- function(extended=FALSE) {
  tubular <-rgb(c(178, 220, 255,   0, 245, 155,   0,   0, 131,   3, 118),
                c( 99,  36, 200, 125, 137,   0,   0,  25, 141, 151, 208),
                c(  0,  31,  10,  50, 166,  88,   0, 168, 147, 229, 189),
                maxColorValue = 255)

  names = c("Bakerloo", "Central", "Circle", "District",
            "Hammersmith & City", "Metropolitan", "Northern", "Picadilly",
            "Jubilee", "Victoria", "Waterloo & City")

   if(extended){
     tubular <- c(tubular, "#9364CD", "#00A4A7", "#EE7C0E","#84B817","#E21836")
     names <- c(names, "Elizabeth", "Docklands Light Railway", "London Overground", "London Trams", "Cable Car")
   }

  data.frame(x=seq_along(tubular), col=tubular, names=names)

}




#' PlotTubecolours
#'
#' @param extended (FALSE) additionally include non-tube lines on the underground map
#'
#' @return A ggplot of the colours of TubeColours()
#'
#' @examples PlotTubecolours(extended=TRUE)
PlotTubecolours <- function(extended=FALSE){
  ggplot2::ggplot(TubeColours(extended), ggplot2::aes(x=0, xend=1, y=-x, yend=-x, col=col, label=names)) +
    ggplot2::geom_segment( linewidth=5) +
    ggplot2::scale_colour_identity() +
    ggplot2::geom_label(x=0.5) +
    ggplot2::theme_void()
}