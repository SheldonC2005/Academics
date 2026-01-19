library(tidyr)
library(dplyr)
mtcars
Vehicle <- cars
car <- as.data.frame(Vehicle)
car

car%>% 
  filter(car_name=="Mazda 787B" | car_name=="Lexus LFA")
car%>% 
  filter(fuel_type=="Petrol")

car%>%
  summarise(avg_selling_price=mean(selling_price),
            sd_selling_price=sd(selling_price),
            max_selling_price=max(selling_price),
            min_selling_price=min(selling_price),
            sum_selling_price=sum(selling_price),
            median_selling_price=median(selling_price),
            total=n())

library(arules)
data(package="arules")
data("Groceries")
summary(Groceries)
Groceries
inspect(Groceries[1:3])
inspect(Groceries[9000:9010])
itemFrequency(Groceries[, 1:5])