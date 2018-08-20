######### Adding motion of Bubble chart
#PS: Plot the relation between Rape and kidnapping of women and girls using crime data set

library(ggplot2)

library(plotly)

File = read.csv("E://Training data//DV for Batch 3//Data Set//rajanand-crime-in-india//01_District_wise_crimes_committed_IPC_2001_2012.csv") 

chart = ggplot(File, aes(x = RAPE, y = KIDNAPPING.AND.ABDUCTION.OF.WOMEN.AND.GIRLS)) + 
  geom_point(aes(col = STATE.UT, frame = YEAR))


chart

chart1 = ggplotly(chart)
chart1

########### Stacked bar chart #######################################################

# Data set used: mpg

# Data Source: R-Inbuild data set

# Problem Statement: manufacturer wise, class wise count of cars

data1 = mpg %>% group_by(manufacturer, class) %>% summarise(Count = n())

stack_bar = ggplot(data1, aes(x = manufacturer, y = Count, fill = class)) + geom_bar(stat = "Identity", width = .6) 

stack_bar + geom_text(data=data1, aes(x=manufacturer, y = Count, label = Count),position = position_stack(vjust=0.5)) +
  
  theme(axis.text.x = element_text(angle = 90))


##  stacked bar chart with % contribution
stack_fill = ggplot(mpg, aes(x = manufacturer)) + 
  geom_bar(aes(fill = class),position = "fill", width = .6) 

stack_fill
stack_fill + theme(axis.text.x = element_text(angle = 90))


######################################## Heat Map ################################################

# Data set used: Crime in India

# Data Source: data.world

# Problem Statement: Major reason people being kidnapped in each state

crime = read.csv("E://Training data//DV for Batch 3//Data Set//rajanand-crime-in-india//39_Specific_purpose_of_kidnapping_and_abduction.csv")

library(dplyr)



cime_file = crime %>% select(ï..Area_Name, Group_Name,Year, Sub_Group_Name, K_A_Grand_Total) %>%
  group_by(ï..Area_Name, Sub_Group_Name) %>% summarise(Total = sum(as.numeric(K_A_Grand_Total)))


chart1.1 = ggplot(cime_file, aes(x = Sub_Group_Name, y = ï..Area_Name)) + 
 geom_tile(aes(fill = Total), color = "white") +
  theme(axis.text.x = element_text(angle = 90)) 

chart1.1

# Plotting for each state

library(dplyr)
library(ggplot2)

# Creating a subset of only area name and group name
data_for_plotting = Kidnapped_data %>% select(area_name, group_name) %>% group_by(area_name, group_name)%>% summarise(Count_of = n())

# Removing the rows containing "Kidnap - Total"
data_for_plotting = data_for_plotting[!(data_for_plotting$group_name=="Kidnap - Total"),]

# Plotting a heat map using ggplot

ggplot(data = cime_file, aes(x = group_na, y = area_name)) +
  
  geom_tile(aes(fill = Count_of), color = "white") + theme_bw() + theme(axis.text.x = element_text(angle = 90)) +
  
  
  scale_fill_gradient(low = "green", high = "red") + ylab("States") +
  xlab("Reason of kidnapping")


######### Histogram on a continuous variable ##################################################
## We will try to use the option of bin() and binwidth()

# Data Set used: mpg

# Source of Data : R- Inbuild data set

# Problem statement : histogram of displacement of various classes of cars.

#                     histogram of manufacturer of various classes of cars.


# Iam not giving the number of bins in this case.

chart11 = ggplot(mpg, aes(displ)) +
  geom_histogram(aes(fill = class), binwidth = .4, color = "black")  


chart11


chart11.1 = ggplot(mpg, aes(displ)) +
  geom_histogram(aes(fill = class), binwidth = .8, color = "black")  



chart11.1

library(gridExtra)

P = grid.arrange(chart11, chart11.1)

P








# Now I will define the number of bins

chart12 = ggplot(mpg, aes(displ)) + 
  geom_histogram(aes(fill = class), bins = 3, color = "black") + labs(title = "Histogram with Numbers of Bins")


chart12


######## # Histogram on a Categorical variable ###################################

# Data Set used: mpg

# Source of Data : R- Inbuild data set

# Problem statement : To create a histogram of manufacturer(which is a categorical variable) of various car's class.

chart13 = ggplot(data1, aes(manufacturer)) + 
  geom_histogram(aes(fill = class)) + theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Histogram on Categorical Variable")

chart13  #### Does it giving an Error????

########## Box Plot ################################################################


# Data Set used: mpg

# Source of Data : R- Inbuild data set

# Problem statement : Analysis the city mileage for each class of car with different cylender


chart14= ggplot(mpg, aes(class, cty)) + geom_boxplot(varwidth = TRUE)

chart14



# Now I want to plot the case for each cylender car

chart14 = ggplot(mpg, aes(class, cty)) + geom_boxplot(aes(fill = factor(cyl)))

chart14


############### Time series plot ####################################################################

# Data Set used: Economics data set

# Source of data set: R-Inbuild data set

# Problem statement: Plot the unemployment rate over the year.

library(ggplot2)

data1 = economics

chart1 = ggplot(data1, aes(x=date, y = unemploy)) + geom_line()

chart1 


# Adding points to line

chart1 + geom_point()

# Problem statement 2: Want to change the line thickness based on unemployment % over the population

View(data1)

data1$rate = round((data1$unemploy/data1$pop)*100, digits = 2)

chart2 = ggplot(data1, aes(x = date, y = unemploy)) + geom_line(aes(size = rate))

chart2

# My chart is showing data over a period of 10 years. I want to show for each year

library(lubridate)

brks <- data1$date[seq(1, length(data1$date), 12)]
lbls <- lubridate::year(brks)

chart2 + scale_x_date(labels = lbls, breaks = brks) + theme(axis.text.x = element_text(angle = 90))


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


