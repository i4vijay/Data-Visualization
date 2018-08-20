############### Time series plot ####################################################################

# Data Set used: Economics data set

# Source of data set: R-Inbuild data set

# Problem statement: Plot the unemployment rate over the year.

library(ggplot2)

data1 = economics

chart1 = ggplot(data1, aes(x=date, y = unemploy)) + geom_line()

chart1 

# Problem statement 2: Want to change the line thickness based on unemployment % over the population

data1$rate = round((data1$unemploy/data1$pop)*100, digits = 2)

chart2 = ggplot(data1, aes(x = date, y = unemploy)) + geom_line(aes(size = rate))

chart2

# Problem statement 3: Plotting multiple line charts

chart4 = ggplot(data1, aes(x = date)) + 
  geom_line(aes(y = unemploy), col = "Red") +
  geom_line(aes(y = pce), col = "Green")

chart4 

# Or

chart4 = ggplot(data1, aes(x = date)) + 
  geom_line(aes(y = unemploy, color = "Unemployment")) + 
  geom_line(aes(y = pce, color = "Price"))

chart4 

# Melting the data frame on date to plot all the variables

library(reshape2)
library(dplyr)

data1 = data1[,1:6]
data2 = melt(data1, id = "date")

data2.1 = filter(data2, variable == "pce" | variable == "unemploy")

chart5 = ggplot(data2, aes(x = date, y = value, col = variable)) + geom_line() 

chart5
chart5 + scale_color_manual(labels = c("pce", "unemploy"), 
                            values = c("pce"="Red", "unemploy"="Green"))

# My chart is showing data over a period of 10 years. I want to show for each year

library(lubridate)

brks <- data1$date[seq(1, length(data1$date), 12)]
lbls <- lubridate::year(brks)

chart4 + scale_x_date(labels = lbls, breaks = brks) + theme(axis.text.x = element_text(angle = 90))

###################################### Candle stick chart #####################################

# Data set: Stock market data from Yahoo 

# Data Source: Yahoo

# Problem statement: Analyise the stock price of last 30 days

# How to get the data from yahoo

library(quantmod)

getSymbols("AAPL",src='yahoo')

# basic example of ohlc charts
df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)


library(plotly)

df = read.csv("E://Training data//DV for Batch 3//Data Set//Stockmarket.csv")

df = tail(df, 30)

p = plot_ly(data = df, x = ~Date, type="candlestick",
            open = ~AAPL.Open, close = ~AAPL.Close,
            high = ~AAPL.High, low = ~AAPL.Low) %>% layout(title = "Basic Candlestick Chart") 

p

## Custom color

i = list(line = list(color = 'Green'))
d = list(line = list(color = 'Red'))

p = plot_ly(data = df, x = ~Date, type="candlestick",
            open = ~AAPL.Open, close = ~AAPL.Close,
            high = ~AAPL.High, low = ~AAPL.Low, increasing = i, decreasing = d) %>% layout(title = "Basic Candlestick Chart") 

p


########### Pie chart ####################################################################

# Data Set used: Cost per event and cost per athlete in the Olympics.

# Source of Data : data.world

# Problem statement : To identify the cost per event in the olympics category wise.

library(plotly)
library(dplyr)

data1 = read.csv("E://Training data//DV for Batch 3//Data Set//Cost.csv")

data_final = data1 %>% group_by(Type) %>% summarise(Total_Cost = sum(Cost.per.event..mio..USD))

pie = plot_ly(data_final, labels = ~Type, values = ~Total_Cost, type = 'pie', 
              textposition = 'inside', textinfo = 'label+percent', showlegend = FALSE, 
              hoverinfo = 'text', text = ~paste('$', Total_Cost, ' millions')) %>%
  layout(title = 'Expense on Olympic')

pie

################################# Tree Map ##############################################

# Data Set used: ODI

# Source of Data : data.world

# Problem statement :Plot the average score rate for top 50 indian player.

library(treemapify)
library(readxl)

odi = read_excel("E://Training data//DV for Batch 3//Data Set//odi-batting-analysis.xlsx")

indian_players_summary = odi %>% filter(Country=='India') %>% group_by(Player) %>% summarise(Total_Runs = sum(Runs, na.rm=T), Avg_SR=mean(ScoreRate, na.rm=T)) %>% arrange(-Total_Runs) %>% 
  head(50) 

indian_players_summary

g = ggplot(indian_players_summary, aes(area=Total_Runs, label=Player, fill=-Avg_SR)) + geom_treemap()

g = g + geom_treemap_text()

g


############ Stacked Area chart ############################################
# Time Series Plot From a Data Frame

# Data Set used: Economics data set

# Source of data set: R-Inbuild data set

# Problem statement: To draw stacked area chart for Unemployment and Price

data1 = economics
library(ggplot2)
chart6 = ggplot(data1, aes(x=date)) + 
  geom_area(aes(y=unemploy, fill="Unemployment")) + 
  geom_area(aes(y=pce, fill="Price"))

chart6

