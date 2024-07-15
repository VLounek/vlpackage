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

#' Funkce 111
#'
#'

#' @title Govlab barvy
#' @description
#' @param
#' @return
#' @export
# Govlab barvy ===================
govlab_color <- c(
  `blue`        = "#00415f",
  `cyan`      = "#237E83",
  `green`       = "#94BBBA",
  `grey`     = "#DDD3CA",
  `darkpeach`     = "#E47D5C",
  `orange` = "#E6A85F",
  `yellow`  = "#F1D363")

govlab_color_new <- c(
  `blue`        = "#00415F",
  `cyan`      = "#217E83",
  `green`       = "#95BABA",
  `grey`     = "#DBD3CB",
  `darkpeach`     = "#D68263",
  `orange` = "#DDAA6B",
  `yellow`  = "#ECD474",
  `white` = "#F4F2EF")


#' @title String Wrap
#' @description
#' @param
#' @return
#' @export
string_wrap <- function(s, width) {
  unlist(lapply(s, function(s) paste(strwrap(s,width), collapse="\n")))
}

