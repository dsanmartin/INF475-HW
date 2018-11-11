library(forecast)

data = read.csv("data/river_flows.csv", header=FALSE, col.names = c("date", "flow"))

flows <- data$flow

ts_flows <- ts(flows, frequency=12)

decomp_flows <- decompose(ts_flows, "additive")

y <- flows - decomp_flows$seasonal 

fit <- auto.arima(ts_flows)
fit