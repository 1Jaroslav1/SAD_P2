library(dplyr)
library(ggplot2)
library(forcats)
inflation_data <- read.csv("~/SAD_2/EU_Inflation_HICP_data.csv", header = TRUE, skip = 2)

euro_zone <- c("X", "Austria", "Belgium", "Cyprus", "Estonia", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Portugal", "Slovakia", "Slovenia", "Spain")
no_euro_zone <- c("X", "Bulgaria", "Croatia", "Czech.Republic", "Denmark", "Hungary", "Poland", "Romania", "Sweden")

select(inflation_data, euro_zone) -> euro_zone_inflation
select(inflation_data, no_euro_zone) -> no_euro_zone_inflation


prepare_inflation_data <- function(df) {
  df <- df[-1,][-1,]
  df <- df[47:82,]
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

mean_euro_zone_inflation_monthly <- prepare_inflation_data(euro_zone_inflation)
mean_no_euro_zone_inflation_monthly <- prepare_inflation_data(no_euro_zone_inflation)

prepare_plot(mean_euro_zone_inflation_monthly, mean_no_euro_zone_inflation_monthly)

shapiro.test(mean_euro_zone_inflation_monthly$InflationMonthly)
shapiro.test(mean_no_euro_zone_inflation_monthly$InflationMonthly)

wilcox.test(mean_euro_zone_inflation_monthly$InflationMonthly, 
       y = mean_no_euro_zone_inflation_monthly$InflationMonthly,
       paired=TRUE,
       alternative="greater")
