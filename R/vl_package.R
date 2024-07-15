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
library(ggplot2)
library(magrittr)
library(plyr)
library(dplyr)
library(plotwidgets)
library(viridisLite)

#' Fahrenheit conversion
#'
#' Convert degrees Fahrenheit temperatures to degrees Celsius
#'


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

# Vraci zadany text s odradkovany po dosazeni sirky width
string_wrap <- function(s, width) {
  unlist(lapply(s, function(s) paste(strwrap(s,width), collapse="\n")))
}

# Rozdeli odpovedi na multiple choice otazku (var) na jednotlive promenne. Vyzaduje seznam (values) moznych odpovedi. Vraci cely datovy soubor
split_multiple <- function(data, var, values) {
  for (i in 1:length(values)) {
    choice <- values[i]
    data[choice] <- ifelse(grepl(choice, data[, var]) == T, 1, 0)
  }
  return(data)
}

# Fce splitdata pripravi vybrane promenne do formatu pro zobrazeni pomoci ggplot2.
# column_numbers: hlavni promenne(a); cisla jejich sloupcu se ziskaji pomoci fce cisla_sloupcu()
# split_variables: dle kterych promennych delit hlavni promenne (definovana jmeny, ne cisly sloupcu)
# Priklad:  splitdata(c(43, 1312), c("e_sex", "work_3cat"), eu8)
my_group_by <- function(data, cols) {
  group_by(data, pick({{ cols }}))
}

splitdata <- function(column_numbers, split_variables = NA, weightvar = 1, df) {
  # if(!is.numeric(column_numbers) | !is.character(split_variables)) {
  #   stop(("Insert column numbers of variables and names of splitting variables"))
  # }
  plotData <- NULL
  if (!is.na(split_variables)) {
    for (j in split_variables) {
      df <- df %>% filter (!is.na(.data[[j]]))
    }
  }

  for (i in colnames(df[column_numbers])) {
    if(!any(is.na(split_variables))) {

      x <- as.data.frame(df %>%
                           my_group_by(all_of(c(split_variables, i))) %>%
                           filter(!is.na(.data[[i]])) %>%
                           dplyr::summarise(n = n(),
                                            vaha = mean({{weightvar}}, na.rm = T),
                                            wn = round(n*vaha))) %>%
        ungroup() %>%
        my_group_by(all_of(c(split_variables))) %>%
        mutate(prop = prop.table(wn),
               vari = i,
               group_sum = sum(wn))
      colnames(x)[which(colnames(x) == i)] <- "value"
      plotData <- rbind(plotData, x)
    }
    else {
      x <- as.data.frame(df %>%
                           my_group_by(all_of(i)) %>%
                           filter(!is.na(.data[[i]])) %>%
                           dplyr::summarise(n = n(),
                                            vaha = mean({{weightvar}}, na.rm = T),
                                            wn = round(n*vaha))) %>%
        mutate(prop = prop.table(wn),
               vari = i,
               group_sum = sum(wn))
      colnames(x)[which(colnames(x) == i)] <- "value"
      plotData <- rbind(plotData, x)
    }
  }
  plotData
}




# Prikaz cliff_data vytvori novy datovy soubor z plotdata s vazenymi pocty pozorovani
cliff_data <- function(plot_data) {
  n_grouping_vars <- which(colnames(plotdata) == "value") - 1
  grouping_vars <- colnames(plotdata[c(1:n_grouping_vars)])

  grouped_weight_df <- plot_data %>%
    my_group_by(all_of(c("vari", grouping_vars))) %>%
    summarise(new_df = list(uncount(across(), wn)))
  print(grouped_weight_df)

  extracted_dfs <- lapply(grouped_weight_df$new_df, identity)
  extracted_dfs
}

# Vypocet vazeneho Cliffova delta
# Hodnoty cisla group1 a group2 zavisi na tom, ktere dva podsoubory extracted_dfs chceme porovnat
w_cliff <- function(extracted_df, group1, group2) {
  cliff.delta(as.numeric(unlist(extracted_df[[group1]][[1]])),as.numeric(unlist(extracted_df[[group2]][[1]])))
}

# Fce slouzici u boxplotu jako parametr pro stat_summary
upper_limit <- NA
get_box_stats <- function(y) {
  return(data.frame(
    y = upper_limit,
    label = paste(" n =", length(y), "\n",
                  " Prům.: ", round(mean(y), 1), "\n",
                  " Med.: ", round(median(y), 1), "\n"
    )
  ))
}

# Fce vraci cisla sloupcu, jejich jmena jsou zadana jako parametry
cisla_sloupcu <- function(data, sloupce) {
  x <- c()
  for (i in 1:length(sloupce)) {
    a <- which(colnames(data) == sloupce[i])
    x <- c(x, a)
  }
  x
}

# Fce pro vytvareni split violin
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])

                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

# zkopiruje x do clipboardu (uzitecne napr. pro presouvani do excelu)
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names)
}

# Zaokrouhlovani 0.5 nahoru
round.off <- function (x, digits=0)
{
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  return(z)
}

# Desaturovana barevna skala
desaturatedViridis <- function(n) {
  unlist(lapply(viridis(n = n), modCol, darken = -0.02, saturate = -0.08))
}

desaturatedViridis1 <- unlist(lapply(viridis(n = 5), modCol, darken = -0.02, saturate = -0.08))[2]

