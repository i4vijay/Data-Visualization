############# Lollipop Chart ########################################################

# Data Set used: Crime in Atlanta 2009-2017

# Source of Data : data.world

crime = read.csv("E://Training data//DV for Batch 3//Data Set//Crime in Atlanta 2009-2017.csv") 

#### Objective is to identified which month had highest count of crime and ploting them on lollipop chart

# Extracting Month from Date column
str(crime)

# Converting date column from factor to date 
crime$date = as.Date(crime$date, format = "%m/%d/%Y")
str(crime$date)

# Creating a month column and I need month in abbreviation
crime$Month = format(crime$date, "%b")

# Using dplyr summarise the crime month wise
library(dplyr)

crime_month_wise = crime %>% group_by(Month) %>% summarise(Count = n()) 

# Ploting lollipop chart
library(ggplot2)

chart = ggplot(crime_month_wise, aes(x= Month, y=Count))+ geom_point(size = 5) +  
      geom_segment(aes(x=Month, xend=Month, y=0, yend=Count)) + 
    labs(title="Lollipop Chart", subtitle="Month Wise Chart", 
       caption="source: Data.World") + scale_x_discrete(limits = month.abb)
chart

############# Diverging Bars #######################################################################
# Data Set used: Crime in Atlanta 2009-2017

# Source of Data : data.world

# Problem Statement: Objective is to identifiy the months with below average 
# and above average crime
# Find the average value of crime
Average = round(mean(crime_month_wise$Count))

# Creating column marked with below and above based on the average value
crime_month_wise$Avg_type1 = ifelse(crime_month_wise$Count < Average, "below", "above") 

# Converting all the count of crimes below average to negative values - this will make average as the middle axis
crime_month_wise$Count = ifelse(crime_month_wise$Avg_type1 == "below", -1*crime_month_wise$Count, crime_month_wise$Count)

chart3 = ggplot(crime_month_wise, aes(x= Month, y= Count)) + 
  geom_bar(stat='identity', aes(fill=Avg_type1), width=.5) + 
  scale_fill_manual(name="Crime", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="Red", "below"="Green")) + 
  labs(subtitle="Avg. Crime Month Wise", 
       title= "Diverging Bars", x = "Month", y = "Crime Count") + scale_x_discrete(limits = month.abb)

chart3


## Correct method:-

# The Crime count from crime_month_wise dataset is normalised by computing the z score. 

# Those Months with Crime Count above Average are marked green and those below are marked red.

# Lets calculate the z score
crime_month_wise$Count_z = round((crime_month_wise$Count - mean(crime_month_wise$Count))/sd(crime_month_wise$Count), 2)  

# Above/Below average flag
crime_month_wise$Avg_type = ifelse(crime_month_wise$Count_z < 0, "below", "above")  

# Sorting the data based on Normilized value column
crime_month_wise = arrange(crime_month_wise, Count_z)  

# Ploting
chart2 = ggplot(crime_month_wise, aes(x= Month, y= Count_z, label= Count_z)) + 
  geom_bar(stat='identity', aes(fill=Avg_type), width=.5) + 
  scale_fill_manual(name="Crime", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="Red", "below"="Green")) + 
  labs(subtitle="Avg. Crime Month Wise", 
       title= "Diverging Bars", x = "Month", y = "Normilized Crime Rate") + scale_x_discrete(limits = month.abb)

chart2
chart2 + coord_flip()

library(gridExtra)
A = grid.arrange(chart2,chart3)
A
######### Diverging Lollipop Chart ##############################################################

# Problem Statement: Create Diverging Lollipop chart using same data set. 

# Solution: 

ggplot(crime_month_wise, aes(x= Month,  y= Count_z, label=Count_z)) + geom_point(aes(col = Avg_type), size=6)  + 
  geom_segment(aes(x = Month, xend = Month, y = 0, yend = Count_z)) +
  labs(title="Diverging Lollipop Chart", subtitle="Normalized Crime Rate: Lollipop") + 
  scale_x_discrete(limits = month.abb) + geom_text(color="white", size=2) + coord_flip()


######## Scatter Plot ##########################################################################
# Data Set used: mpg

# Source of Data : R - Inbuild data set

# Problem statement : Objective is to plot the relation between highway mileage and City 
                      # mileage of each class. 
data1= mpg

# Lets plot a simple scatter Plot
chart3 = ggplot(data1, aes(x=cty, y=hwy)) + geom_point(aes(col=class)) +
  labs(subtitle="City Mileage v/s Highway Mileage", y="Highway Mileage", x="City Mileage", title="Scatterplot" ,
       caption = "Source: mpg data set")

chart3

############ Jitter Plot ###################################################################
# Data Set used: mpg

# Source of Data : R - Inbuild data set

# Problem statement : Find the corelation between highway mileage and City mileage of each class without having the problem of data points over lapping

chart4 = ggplot(mpg, aes(x = cty, y = hwy)) + geom_jitter(aes(col = class), width = .5, size=1) +
  labs(subtitle="City Mileage v/s Highway mileage", y="Highway Mileage", x="City Mileage", 
       title="Jittered Points")

chart4


library(gridExtra)
Plot = grid.arrange(chart3, chart4)
Plot


semi_mpg = filter(mpg, cty>= 15 & cty <= 20) 

chart4.1 = ggplot(semi_mpg, aes(x = cty, y = hwy)) + geom_jitter(aes(col = class), width = .5, size=5) +
  labs(subtitle="City Mileage v/s Highway mileage", y="Highway Mileage", x="City Mileage", 
       title="Jittered Points")

chart4.1


############ Count Plot ####################################################################

# Data Set used: mpg

# Source of Data : R - Inbuild data set

# Problem statement : Find the corelation between highway mileage and City mileage of each class without having the problem of data points over lapping

chart5 = ggplot(mpg, aes(x = cty, y = hwy)) + geom_count(aes(col= class)) +
  labs(subtitle="City Mileage v/s Highway mileage", y="Highway Mileage", x="City Mileage", 
       title="Count Plot")

chart5

############ Bubble chart #################################################################
# Data Set used: mpg

# Source of Data : R - Inbuild data set

# Problem Statement: Plot relation between displacement and mileage (Both city and highway mileage) 
                     # of various class.

chart6 = ggplot(mpg, aes(x = cty, y = hwy)) + geom_jitter(aes(col = class, size = displ)) + 
  labs(subtitle="Displacement v/s Mileage", y="Highway Mileage", x="City Milage", 
       title="Bubble Chart")

chart6

chart6.1 = ggplot(mpg, aes(x = cty, y = hwy)) + geom_jitter(aes(col = displ, shape = class, size = class)) + 
  labs(subtitle="Displacement v/s Mileage", y="City Mileage", x="Displacement", 
       title="Bubble Chart")

chart6.1

library(gridExtra)

grid.arrange(chart6, chart6.1)

library(dplyr)

final_data = filter(mpg, cty>=15 & cty<=20)

chart6.2 = ggplot(final_data, aes(x = cty, y = hwy)) + geom_jitter(aes(col = class, size = displ)) + 
  labs(subtitle="Displacement v/s Mileage", y="highway Mileage", x="city milage", 
       title="Bubble Chart")

chart6.2


########### Correlogram ###########################################################################
# We will create correlogram in ggplot as well as corrplot() also.
# lets first try with corrplot()

# Data Set used: mtcars

# Source of Data : R - Inbuild data set

# Problem statement : To find the correlation between various variable 
                      # and dividing them into to hierarchical clustering.


library(corrplot)
data_cor = mtcars

# First think to draw a correlogram is to create a correlation matrix between various variables.

str(data_cor)

Matrix = cor(data_cor)

# Ploting the correlation plot

chart9 = corrplot(Matrix, method = "square", type = "upper")

# Reordering the correlation matrix
# The correlation matrix can be reordered according to the correlation coefficient. 
# This is important to identify the hidden structure and pattern in the matrix. 
# "hclust" for hierarchical clustering order is used for this purpose.

chart9.1 = corrplot(Matrix, method = "color", type = "upper" , order = "hclust")

chart9.1

##### Using ggcorrplot
install.packages("ggcorrplot")
library(ggcorrplot)

Matrix1 = cor(data_cor)

Chart10 = ggcorrplot(Matrix1, hc.order = TRUE, type = "upper", lab = TRUE, lab_size = 3, method="circle")

Chart10

