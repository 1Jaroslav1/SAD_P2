source("./Problem_1/Functions.R")
source("./Problem_1/InflationDataPreparing.R")

mean_euro_zone_inflation_monthly <- prepare_inflation_data(euro_zone_inflation, c("2016-01-01", "2018-12-31"))
mean_no_euro_zone_inflation_monthly <- prepare_inflation_data(no_euro_zone_inflation, c("2016-01-01", "2018-12-31"))

draw_stacking_data(mean_no_euro_zone_inflation_monthly, mean_euro_zone_inflation_monthly, "Inflacja w latach 2016-2018")

draw_histogram(mean_no_euro_zone_inflation_monthly,
               "Histogram wartości inflacji w krajach UE z własną walutą",
                    "InflationMonthly",
               mean(mean_no_euro_zone_inflation_monthly$InflationMonthly),
               sd(mean_no_euro_zone_inflation_monthly$InflationMonthly),
               "cadetblue")

draw_histogram(mean_euro_zone_inflation_monthly,
               "Histogram wartości inflacji w krajach strefy euro",
                    "InflationMonthly",
               mean(mean_euro_zone_inflation_monthly$InflationMonthly),
               sd(mean_euro_zone_inflation_monthly$InflationMonthly),
               "darkgoldenrod")

shapiro.test(mean_euro_zone_inflation_monthly$InflationMonthly)
shapiro.test(mean_no_euro_zone_inflation_monthly$InflationMonthly)

cor(mean_euro_zone_inflation_monthly$InflationMonthly, mean_no_euro_zone_inflation_monthly$InflationMonthly)
cor.test(mean_euro_zone_inflation_monthly$InflationMonthly, mean_no_euro_zone_inflation_monthly$InflationMonthly,
         method = 'p', alternative='g')

diff <- mean_euro_zone_inflation_monthly$InflationMonthly - mean_no_euro_zone_inflation_monthly$InflationMonthly

shapiro.test(diff)

t.test(mean_euro_zone_inflation_monthly$InflationMonthly, mean_no_euro_zone_inflation_monthly$InflationMonthly,
       paired=TRUE, alternative='g')

wilcox.test(mean_euro_zone_inflation_monthly$InflationMonthly, 
       y = mean_no_euro_zone_inflation_monthly$InflationMonthly,
       paired=TRUE,
       alternative="greater")

