source("./Problem_1/Functions.R")
source("./Problem_1/InflationDataPreparing.R")

mean_euro_zone_inflation_monthly <- prepare_inflation_data(euro_zone_inflation, c("2015-11-01", "2018-10-01"))
mean_no_euro_zone_inflation_monthly <- prepare_inflation_data(no_euro_zone_inflation, c("2015-11-01", "2018-10-01"))

draw_stacking_data(mean_no_euro_zone_inflation_monthly, mean_euro_zone_inflation_monthly, "Inflation")

draw_histogram(mean_no_euro_zone_inflation_monthly,
               "Monthly inflation delta for countries not in the euro zone",
                    "InflationMonthly",
               mean(mean_no_euro_zone_inflation_monthly$InflationMonthly),
               sd(mean_no_euro_zone_inflation_monthly$InflationMonthly),
               "cadetblue")

draw_histogram(mean_euro_zone_inflation_monthly,
               "Monthly inflation delta for countries not in the euro zone",
                    "InflationMonthly",
               mean(mean_euro_zone_inflation_monthly$InflationMonthly),
               sd(mean_euro_zone_inflation_monthly$InflationMonthly),
               "darkgoldenrod")

# prepare_plot(mean_euro_zone_inflation_monthly, mean_no_euro_zone_inflation_monthly)

shapiro.test(mean_euro_zone_inflation_monthly$InflationMonthly)
shapiro.test(mean_no_euro_zone_inflation_monthly$InflationMonthly)

wilcox.test(mean_euro_zone_inflation_monthly$InflationMonthly, 
       y = mean_no_euro_zone_inflation_monthly$InflationMonthly,
       paired=TRUE,
       alternative="greater")

