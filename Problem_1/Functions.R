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

draw_stacking_data <- function(no_euro_zone_data, euro_zone_data, title) {
  merged_df <- merge(no_euro_zone_data, euro_zone_data, by = "Period")
  colnames(merged_df)[2] <- "Strefa euro"
  colnames(merged_df)[3] <- "Pozostałe państwa UE"

  data_frame <- pivot_longer(merged_df, -Period, names_to = "Grupa", values_to = "InflationMonthly")

  ggplot(data_frame, aes(x = Period, y = InflationMonthly, group = Grupa, color = Grupa)) +
    # scale_color_viridis(discrete = TRUE, option = "H") +
    geom_line(aes(color = factor(Grupa)), size = 2) +
    geom_point(shape = 21, color = "black", fill = "black", size = 1.5) +
    scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
    guides(fill = guide_legend(title = NULL)) +
    xlab("Miesiąc") +
    ylab("Wartość inflacji [%]") +
    ggtitle(title) +
    theme_minimal() +
    theme(
      plot.title=element_text(size = 20, hjust=0.5, vjust=0.5, margin = margin(20, 0, 20, 0)),
      axis.title = element_text(size=14),
      axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
      axis.title.y = element_text(margin = margin(0, 20, 0, 20)),
      axis.text = element_text(
        size=15,
        face=3
      ),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
    )
}

draw_histogram <- function(df, title, column_name, mean, sd, color) {
  cat(column_name, ": ", "mean - ", mean, ', standard deviation = ', sd, '\n')
  ggplot(df) +
    geom_histogram(binwidth = 0.1, aes(x = get(column_name), y = after_stat(count)), fill = paste(color, '1'), color = color) +
    xlab("Wartość inflacji") +
    ylab("Ilość wystąpień") +
    theme_light() +
    ggtitle(title) +
    theme(
      plot.title=element_text(size = 20, hjust=0.5, vjust=0.5, margin = margin(20, 0, 20, 0)),
      axis.title = element_text(size = 14),
      axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
      axis.title.y = element_text(margin = margin(0, 20, 0, 20)),
      axis.text = element_text(
        size = 15,
        face = 3
      )
    )
}