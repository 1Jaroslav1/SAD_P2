library(anytime)

inflation_data <- read.csv("./Problem_1/data/EU_Inflation_HICP_data.csv", header = TRUE, skip = 2)
inflation_data <- tail(inflation_data, -2)
inflation_data["X"] <- anytime::anydate(paste(inflation_data[, "X"], 1))

euro_zone <- c("X", "Austria", "Belgium", "Cyprus", "Estonia", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Portugal", "Slovakia", "Slovenia", "Spain")
no_euro_zone <- c("X", "Bulgaria", "Croatia", "Czech.Republic", "Denmark", "Hungary", "Poland", "Romania", "Sweden")

select(inflation_data, euro_zone) -> euro_zone_inflation
select(inflation_data, no_euro_zone) -> no_euro_zone_inflation

