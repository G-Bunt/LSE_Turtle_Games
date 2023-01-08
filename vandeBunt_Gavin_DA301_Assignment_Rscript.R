## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact
# Author: Gavin van de Bunt
# Date: 09 January 2023

###############################################################################

# Context

## This is a continuation of the analysis for Turtle Games, a game manufacturer 
## and retailer. They manufacture and sell their own products, along with 
## sourcing and selling products manufactured by other companies. Their product 
## range includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
## performance by utilising customer trends. 

## In particular, this R-script contains analysis that builds on the work done
## in Python, which can be found in the GitHub repository. The previous Python 
## analysis helps Turtle games understand: 
## - how customers accumulate loyalty points.
## - how useful remuneration and spending scores data are.
## - how social data (e.g. customer reviews) can be used in marketing campaigns.

## The analysis in R below aims to answer the remaining business questions for
## Turtle games:
## - what is the impact on sales per product?
## - what is the reliability of the data (e.g. normal dist., Skew, Kurtosis)?
## - is there any possible relationship(s) in sales between North America, 
##    Europe, and global sales?

## In addition to this Python notebook and the above mentioned R script, 
## you can find a detailed report and accompanying presentation in the 
## parent GitHub repository.

## PLEASE NOTE: 
## Chapter numbering is a continuation from the Python script,
## allowing for easier documentation through the accompanying PDF document. 

## For any questions, please reach out to GavinvdBunt@Gmail.com

################################################################################

# 8.Exploratory data analysis using R

## The sales department of Turtle games prefers R to Python. Therefore we will
## explore and prepare the data set for analysis by utilising basic statistics 
## and plots. 

## Import all the required libraries for the analysis and view the data. 
# install.packages("tidyverse")
library(tidyverse)
library(skimr)
library(DataExplorer)
library(moments)
library(psych)
library(dplyr)

## Load and explore the data.
getwd()
turtle_sales_raw <- read.csv(file.choose(),header = TRUE)
as.tibble(turtle_sales_raw)

## Remove redundant columns (Ranking, Year, Genre, Publisher) 
## by creating a subset of the data frame and create a summary.
turtle_clean <- select(turtle_sales_raw, -Ranking,-Year,-Genre,-Publisher)

# View the data frame to sense-check the data set.
View(turtle_clean)

# Product refers to a product code, so change data type to factor for analysis.
turtle_clean$Product <- factor(turtle_clean$Product)
dim(turtle_clean)

## Other statistics: no missing values, 175 products across 17 platforms.
skim(turtle_clean)

# Data Explorer (comment out to avoid rerunning)
## Graphs show interesting preliminary insights into the sales data:
## - Data is clearly skewed to the right across all regions. More on this later.
## - Strong correlation between the different sales regions. More on this later.
DataExplorer::create_report(turtle_clean)

## Scatterplots, histograms and boxplots to gain insights into the sales data.
# Pivot data to allow easy comparison of Sales categories.
turtle_long <- turtle_clean %>% 
  pivot_longer(c("NA_Sales","EU_Sales","Global_Sales"), 
               names_to="Region", values_to="Sales")

ggplot(turtle_long, aes(x =Region, y=Sales)) +
  geom_boxplot()

ggplot(data=turtle_long, aes(x=Sales)) +
  geom_histogram()  +
  facet_wrap(~Region)

qplot(data=turtle_clean,x=EU_Sales,y=Global_Sales,geom='point')
qplot(data=turtle_clean,x=NA_Sales,y=Global_Sales,geom='point')
qplot(data=turtle_clean,x=EU_Sales,y=NA_Sales,geom='point')

# 9.Understand data structure and popular products.
## This section explores the normality of the dataset based on plots, Skewness, 
## Kurtosis, and a Shapiro-Wilk test. In addition, the Sales data is explored
## further and popular Products and Platforms are identified for further
## analysis.

# Determine the min, max and mean values of all the sales data.
## Both the mean and the max of the NA Sales are higher than the EU Sales. It
## therefore seems likely that NA Sales have the greater impact on Global sales.
## This will be confirmed in the upcoming section through the correlations.
turtle_sales_only <- select(turtle_sales_raw,NA_Sales,EU_Sales,Global_Sales)
apply(turtle_sales_only,2,min)
apply(turtle_sales_only,2,max)
apply(turtle_sales_only,2,mean)

# Determine the impact on sales per product_id.
product_sales_sum <- turtle_clean %>%
  group_by(Product) %>%
  summarize(Total_NA_Sales=sum(NA_Sales),
            Total_EU_Sales=sum(EU_Sales),
            Total_Global_Sales = sum(Global_Sales))
summary(product_sales_sum)
str(product_sales_sum)

# Plot the top 20 Products.
product_sales_sum %>% 
  arrange(desc(Total_Global_Sales)) %>%
  slice(1:20) %>%
  ggplot(., aes(x=reorder(Product,-Total_Global_Sales), y=Total_Global_Sales))+
  geom_bar(stat='identity') +
  labs(title="Global best selling Products",
       x="Products ordered by Global Sales",
       y="Global Sales")

# Determine the impact on sales per platform.
platform_sales_sum <- turtle_clean %>%
  group_by(Platform) %>%
  summarize(Total_NA_Sales=sum(NA_Sales),
            Total_EU_Sales=sum(EU_Sales),
            Total_Global_Sales = sum(Global_Sales))
summary(platform_sales_sum)
str(platform_sales_sum)
head(platform_sales_sum)

# Plot the top 20 Platforms.
platform_sales_sum %>% 
  arrange(desc(Total_Global_Sales)) %>%
  slice(1:20) %>%
  ggplot(., aes(x=reorder(Platform,-Total_Global_Sales), y=Total_Global_Sales))+
  geom_bar(stat='identity') +
  labs(title="Global best selling Platforms",
     x="Platforms ordered by Global Sales",
     y="Global Sales")

# Investigate Global top selling products by platform
## The below barchart shows that out of the 15 top selling products globally,
## a significant amount (33%, 5 products) are Wii exclusives.

# Create a merged table to allow sorting on Global Sales value. 
## NOTE: this table is not ideal for future analysis  as Total Sales are
## double counted. It is created for the barchart only.
turtle_prod_plat <- merge(x=turtle_clean,y=product_sales_sum,by='Product')
head(turtle_prod_plat)

turtle_prod_plat %>% 
  arrange(desc(Total_Global_Sales)) %>%
  slice(1:35) %>%
  ggplot(., aes(x=reorder(Product,-Total_Global_Sales), 
                y=Global_Sales, 
                fill=Platform)) +
  geom_bar(stat='identity', position="stack")  +
  labs(title="Global best selling Products by Platform",
       x="Product by Platform",
       y="Global Sales")

# Determine the normality of the data set (sales data).
# Total Global Sales per Product.
## Conclusions - data is not normally distributed:
## - Shapiro-Wilk p<0.05: data is not normally distributed.
## - Skewness much larger than 1: data is skewed (right)
## - Kurtosis much larger than 3: heavy tails.
qqnorm(product_sales_sum$Total_Global_Sales)
qqline(product_sales_sum$Total_Global_Sales)     
shapiro.test(product_sales_sum$Total_Global_Sales)
skewness(product_sales_sum$Total_Global_Sales)
kurtosis(product_sales_sum$Total_Global_Sales)

# Total NA Sales per Product.
## Conclusions - data is not normally distributed:
## - Shapiro-Wilk p<0.05: data is not normally distributed.
## - Skewness much larger than 1: data is skewed (right)
## - Kurtosis much larger than 3: heavy tails.
qqnorm(product_sales_sum$Total_NA_Sales)
qqline(product_sales_sum$Total_NA_Sales)     
shapiro.test(product_sales_sum$Total_NA_Sales)
skewness(product_sales_sum$Total_NA_Sales)
kurtosis(product_sales_sum$Total_NA_Sales)

# Total EU Sales per Product.
## Conclusions - data is not normally distributed:
## - Shapiro-Wilk p<0.05: data is not normally distributed.
## - Skewness much larger than 1: data is skewed (right)
## - Kurtosis much larger than 3: heavy tails.
qqnorm(product_sales_sum$Total_EU_Sales)
qqline(product_sales_sum$Total_EU_Sales)     
shapiro.test(product_sales_sum$Total_EU_Sales)
skewness(product_sales_sum$Total_EU_Sales)
kurtosis(product_sales_sum$Total_EU_Sales)

# Determine if there is any correlation between the sales data columns.
## As expected, there is heavy correlation between all the sales variables:
## - Global vs. NA: 0.92
## - Global vs. EU: 0.85
## - NA vs. EU: 0.62
cor(select(product_sales_sum,-Product))
corPlot(select(product_sales_sum,-Product),cex=1.0)


# 10: Making recommendations based on Sales figures for Turtle Games

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and Global sales. Therefore, below analysis
## investigates any possible relationship(s) in the sales data by creating both 
## simple and multiple linear regression models. 

## Based on these models and previous analysis recommendations can be made to 
## Turtle Games including:
##   - Confidence in the models based on goodness of fit and
##        accuracy of predictions.
##   - Suggestions and recommendations to the business.
##   - Potential improvements the model(s).
## These full recommendations can be found in the accompanying PDF document 
## and video presentation, stored in the GitHub repository.

# First two simple linear regression models are created with EU and NA sales 
# as independent variable respectively. Both models are very significant with 
# p close to 0 and have a high predictive value with R-squared being 0.72 and 
# 0.84 respectively. As found before through the correlation plots, NA sales 
# explain the Global sales slightly better. However, the coefficient does show
# that for each increase in EU sales, the Global sales shows a stronger
# increase (coefficient of 2.24 vs. 1.63). 

# Create plots to view the linear regression for EU sales and NA sales.
EU_model = lm(Total_Global_Sales~Total_EU_Sales,
           data=product_sales_sum)
summary(EU_model)

NA_model = lm(Total_Global_Sales~Total_NA_Sales,
              data=product_sales_sum)
summary(NA_model)

# Visualize both models in the same output.
# Arrange plot with the par(mfrow) function.
par(mfrow=c(2, 1))

# Compare both graphs (model1 and model2).
plot(product_sales_sum$Total_EU_Sales,product_sales_sum$Total_Global_Sales)
abline(coefficients(EU_model), col='red')

plot(product_sales_sum$Total_NA_Sales,product_sales_sum$Total_Global_Sales)
abline(coefficients(NA_model), col='blue')

# The linear model shows a strong predictive value with an R-squared of .97. 
# As expected, both predictor variables are strongly significant. However,
# we have to be careful with interpretation of the model as many assumptions of
# linear regression have been proven to be broken in the previous sectors
# (e.g. not normally distributed, heavy correlation).

# Nevertheless, it does make conceptual sense that global sales will rise when
# sales in an individual region (NA and/or EU) go up.

# Create a multiple linear regression model.
model = lm(Total_Global_Sales~Total_EU_Sales+Total_NA_Sales,
           data=product_sales_sum)
summary(model)

## For business recommendations and further context, please refer to the PDF 
## file and video presentation located in the GitHub repository.
