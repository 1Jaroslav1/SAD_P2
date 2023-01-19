source("./Problem_1/Functions.R")

mean_euro_zone_inflation_monthly <- prepare_inflation_data(euro_zone_inflation, c("2015-11-01", "2018-10-01"))
mean_no_euro_zone_inflation_monthly <- prepare_inflation_data(no_euro_zone_inflation, c("2015-11-01", "2018-10-01"))

prepare_plot(mean_euro_zone_inflation_monthly, mean_no_euro_zone_inflation_monthly)

shapiro.test(mean_euro_zone_inflation_monthly$InflationMonthly)
shapiro.test(mean_no_euro_zone_inflation_monthly$InflationMonthly)

wilcox.test(mean_euro_zone_inflation_monthly$InflationMonthly, 
       y = mean_no_euro_zone_inflation_monthly$InflationMonthly,
       paired=TRUE,
       alternative="greater")
