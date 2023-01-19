# Libraries

# HELP -> https://r-graph-gallery.com/stacked-area-graph.html

library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)

prepare_inflation_data <- function(df, period) {
  df <- df[-1,][-1,]
  df <- filter_by_time(df, period)
  months <- df[,1, drop=FALSE]
  df <- as.data.frame(sapply(df, as.double))
  df %>% select(-1) -> df

  #df %>% select(-1) %>% mutate(across(everything(), ~lead(.x) - (.x))) %>% na.omit() -> tmp
  #tmp <- as.data.frame(sapply(tmp, as.numeric))
  #tmp %>% mutate(across(everything(), ~ . * -1)) -> tmp

  df <- rowMeans(df)
  df  <- data.frame(matrix(unlist(df), nrow=length(df), byrow = TRUE))

  colnames(df) <- c("InflationMonthly")
  colnames(months) <- c("Period")

  df <- cbind(months, df)
}

filter_by_time <- function(data, period) {
  data %>% filter(
    X >= period[1],
    X <= period[2]
  )
}

prepare_plot <- function(euro, nonEuro) {
  A <- euro
  B <- nonEuro
  colnames(A) <- c("Period","Eurozone")
  colnames(B) <- c("Period2", "NoEurozone")

  A_B <- cbind(A, B)
  A_B$Period <- fct_rev(factor(A_B$Period, levels = A_B$Period))
  A_B$Period <- rev(A_B$Period)
  A_B$Eurozone <- rev(A_B$Eurozone)
  A_B$NoEurozone <- rev(A_B$NoEurozone)
  A_B[order(A_B$Period, decreasing=TRUE),]
  (g <- ggplot(data = A_B, aes(x = Period)) +
      scale_x_discrete(limits = A_B$Period) +
      scale_x_discrete(breaks = A_B$Period[seq(1, length(A_B$Period), by = 6)])+
      ylab('Zmiana [p.p.]') +
      xlab('Miesiąc')  +
      scale_color_manual(name = "Strefy", values = c("Euro" = "blue", "NoEuro" = "red")) +
      ggtitle(paste("Inflacja w ujęciu miesięcznym")) +
    geom_line(aes(y = Eurozone, color="Euro", group = 1)) +
    geom_line(aes(y = NoEurozone, color="NoEuro", group = 1))
    )
}
