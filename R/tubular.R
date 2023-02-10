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
#' @param extended (FALSE) additionally return non-tube lines on the underground map taken from
#' https://blog.tfl.gov.uk/2022/12/22/digital-colour-standard/
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
     tubular <- c(tubular, "#60399E", "#00AFAD", "#FA7B05","#5FB526","#DC241F")
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


##ggplot(diamonds[1:1000,], aes(x=carat, y=price, col=cut)) + geom_point() + scale_colour_tube()

scale_colour_tube <- function(..., alpha = 1, aesthetics = "colour"){
  discrete_scale(aesthetics, "tube", tube_pal(alpha), ...)
}

tube_pal <- function(alpha=1){
  cols <- alpha(TubeColours()$col)
  function(n) {if (n>length(cols)) stop("Too many colours requested for tube palette"); cols[seq_len(n)]}
}


scale_fill_tube <- function(..., alpha = 1, aesthetics = "fill"){
  discrete_scale(aesthetics, "tube", tube_pal(alpha), ...)
}

