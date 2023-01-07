## Import all the required libraries for the analysis and view the data. 
# install.packages("tidyverse")
library(tidyverse)
library(skimr)
library(DataExplorer)
library(moments)
library(psych)

## Load and explore the data.
getwd()
turtle_sales_raw <- read.csv(file.choose(),header = TRUE)
as.tibble(turtle_sales_raw)

## Remove redundant columns (Ranking, Year, Genre, Publisher) 
## by creating a subset of the data frame and create a summary.
turtle_clean <- select(turtle_sales_raw, -Ranking,-Year,-Genre,-Publisher)

# View the data frame to sense-check the data set.
View(turtle_clean)
turtle_clean$Product <- factor(turtle_clean$Product)
dim(turtle_clean)

## Other statistics
skim(turtle_clean)
# Data Explorer (commented out to avoid rerunning)
DataExplorer::create_report(turtle_clean)

## Create scatterplots, histograms and boxplots 
## to gain insights into the sales data.
qplot(data=turtle_clean,x=Platform,y=Global_Sales,geom='boxplot')
qplot(data=turtle_clean,x=Global_Sales,geom='histogram')
qplot(data=turtle_clean,x=Platform,y=Global_Sales,geom='point',colour = Product)
qplot(data=turtle_clean,x=Platform,y=Global_Sales,geom='col')
qplot(data=turtle_clean,x=Platform,geom='bar')

# Determine the min, max and mean values of all the sales data.
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

# To add: ranking of top 10 products - check how many unique Plat/Prod.
# To add: extra group by(s) on the platform
# To add: barcharts to represent above top n. 
# Can use dodge for NA vs. EU vs. Global bar comparison.

# Determine the normality of the data set (sales data).
# Total Global Sales per Product.
qqnorm(product_sales_sum$Total_Global_Sales)
qqline(product_sales_sum$Total_Global_Sales)     
shapiro.test(product_sales_sum$Total_Global_Sales)
skewness(product_sales_sum$Total_Global_Sales)
kurtosis(product_sales_sum$Total_Global_Sales)

# Total NA Sales per Product.
qqnorm(product_sales_sum$Total_NA_Sales)
qqline(product_sales_sum$Total_NA_Sales)     
shapiro.test(product_sales_sum$Total_NA_Sales)
skewness(product_sales_sum$Total_NA_Sales)
kurtosis(product_sales_sum$Total_NA_Sales)

# Total EU Sales per Product.
qqnorm(product_sales_sum$Total_EU_Sales)
qqline(product_sales_sum$Total_EU_Sales)     
shapiro.test(product_sales_sum$Total_EU_Sales)
skewness(product_sales_sum$Total_EU_Sales)
kurtosis(product_sales_sum$Total_EU_Sales)

# Determine if there is any correlation between the sales data columns.
cor(select(product_sales_sum,-Product))
corPlot(select(product_sales_sum,-Product),cex=1.0)

# Create plots to review and determine insights into the data set.
product_sales_sum_pivot = melt(product_sales_sum, id= c("Product"))
summary(test_df$variable)

ggplot(product_sales_sum_pivot, aes(x = value)) +
  geom_histogram() +
  facet_grid(~variable)

ggplot(product_sales_sum_pivot, aes(x = value)) +
  geom_boxplot() +
  facet_grid(~variable)

ggplot(product_sales_sum_pivot, aes(x = value,y=variable)) +
  geom_point() 

plot(select(product_sales_sum,-Product))

# Create plots to gain insights into the sales data. 
# Choose the type of plot you think best suits the data set 
# and what you want to investigate. Explain your answer in your report.
# Compare all the sales data (columns) for any correlation(s).
# Add a trend line to the plots for ease of interpretation.
# Idea if time: top selling games per region, platform, product.
# Idea if time: do the top games sell equally good across platform and region?

# Create a simple linear regression model.
# Create plots to view the linear regression.

# Create a multiple linear regression model.
model = lm(Total_Global_Sales~Total_EU_Sales+Total_NA_Sales,
           data=product_sales_sum)
summary(model)

