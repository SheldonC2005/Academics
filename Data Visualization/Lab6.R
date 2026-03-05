url <- "https://api.open-meteo.com/v1/forecast?latitude=13.08&longitude=80.27&current_weather=true"
response <- GET(url)
data <- fromJSON(content(response, "text"))
data
weather <- data$current_weather
temp <- weather$temperature
windspeed <- weather$windspeed
time <- weather$time


social_data <- data.frame(
  Likes = c(120,300,250,400,350),
  Comments = c(30,45,40,60,55),
  Shares = c(20,50,35,70,65),
  Views = c(1000,2500,1800,3200,2900)
)
rownames(social_data) <- c("Instagram", "Facebook", "Twitter", "YouTube", "LinkedIn")
social_data
data_long <- melt(as.matrix(social_data))
colnames(data_long) <- c("Platform", "Metric", "Value")
data_long

heatmap_plot <- ggplot(data_long , aes(x=Metric,y=Platform,fill=Value))+
  geom_tile(color="white")+
  scale_fill_gradient(low="yellow", high="red")+
  theme_minimal()
print(heatmap_plot)


# Time Series Data Visualization
time <- 1:12
sales <- c(120,135,150,170,160,180,200,210,190,220,240,260)
ts_data <- data.frame(time, sales)
ts_data
sales_ts <- ts(sales, start = 1, frequency = 12)
sales_ts
plot(sales_ts,col = "blue",main = "Monthly Sales Time Series",
     xlab = "Time (Months)", ylab = "Sales")
moving_avg <- ma(sales_ts, order =3)
moving_avg
plot(sales_ts,col="black",main = "Monthly Sales with Moving Average",
     xlab = "Time (Months)", ylab = "Sales")