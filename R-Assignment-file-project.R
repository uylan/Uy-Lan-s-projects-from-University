# load and read the datasets
orders <- read.csv("project_orders.csv")
product <-read.csv("project_product_info.csv")

### TASK 1
library(tidyverse) #load tidyverse before using Join functions

## Test Join
# Assumption 1: Using Join function to merge the datasets
full <- left_join(orders, product, by = "StockCode")
View(full) #Check the merged the dataset

## I spotted that I should exclude Invoices starts with "C" in revenue calculation
# => Assumption 2: Remove Invoices starts with "C" in revenue calculation
full <- full %>% filter(!str_starts(InvoiceNo, "C"))

## Assumption 3: Create total revenue for each product 
full <- full %>%
  mutate(total_revenue = Quantity * UnitPrice) # Update data frame full

# Create a data frame to make them easier to view 
# Revenue is aggregated by Description to compute total revenue per product.
df<-data.frame(Description = full$Description, total_revenue = full$total_revenue) %>% arrange(desc(total_revenue))

# Merge the duplicate invoice number
df_unique <- aggregate(total_revenue ~ Description, data = df, sum)

## To calculate top 10 highest and bottom 10 lowest-selling product ~ total revenue,
# I have to sort df descending
df_sorted <- df_unique %>% arrange(desc(total_revenue))

## I noticed there are 0 revenue and negative revenue while testing Bottom 10 products 
# => Assumption 4: remove 0 and negative revenue
any(df_unique$total_revenue < 0) # Check any 0 or negative revenue
df_unique <- df_unique %>% filter(total_revenue>0)
any(df_unique$total_revenue < 0) # Re-check the Assumption. If it is false, then the filter is correct
df_sorted <- df_unique %>% arrange(desc(total_revenue)) #re-run the code to update df_sorted

# Top 10 highest-selling product
Top10_products <- head(df_sorted,10)

# Bottom 10 lowest-selling product
Bottom10_products <-tail(df_sorted, 10)

# Top 3 highest-selling product
Top3_products <- head(df_sorted,3)
# Top 3 highest-selling product in percentage
Top3_products_perc <- sum(Top3_products$total_revenue) / sum(df_unique$total_revenue)*100
cat( "Top 3 highest-selling product in percentage is", Top3_products_perc, "%.")

# Present my results in tables
library(kableExtra)
kable(Top10_products, caption = "Top 10 Highest-Selling Products")
kable(Bottom10_products, caption = "Bottom 10 Lowest-Selling Products")
kable(Top3_products, caption = "Top 3 Products by Revenue")

# Using bar plots to visualize the revenue concentration
library(ggplot2)
Top10_bar <- ggplot(data= Top10_products, aes(x= reorder(Description, -total_revenue), y= total_revenue, fill = total_revenue))+ geom_col() + labs( title =" Top 10 highest-selling products", x= "product", y="Total Revenue") + scale_fill_gradient(low="light green", high="dark green") +theme(axis.text.x= element_text(angle=90, hjust=1)) +theme(plot.title= element_text(hjust = 0.5))
Bottom10_bar <- ggplot(data= Bottom10_products, aes(x= reorder(Description, -total_revenue), y= total_revenue, fill = total_revenue))+ geom_col() + labs( title =" Bottom 10 lowest-selling products", x= "product", y="Total Revenue") + scale_fill_gradient(low="pink", high="red") +theme(axis.text.x= element_text(angle=90, hjust=1))+theme(plot.title= element_text(hjust = 0.5))

### Task 2:High-value Customer Analysis
any(is.na(full$CustomerID)) #Check missing value in Customer ID
full[is.na(full$CustomerID), ] #Identify where the missing values are
# Assumption 5: remove all the missing values in Customer ID
full <- full %>% filter(!is.na(CustomerID))
any(is.na(full$CustomerID)) # Re-check the missing values.

# Calculate customer spending
customer_spending <- full %>% group_by(CustomerID) %>% summarise(total_spending= sum(total_revenue)) %>% arrange(desc(total_spending))
#Identify the TOp 5 highest spending customers
Top5_customer <- head(customer_spending, 5)

#Create Top 5 customer spending with more details from full table for more information
Top5_data <- full %>% filter(CustomerID %in% Top5_customer$CustomerID) %>% 
select(InvoiceNo, StockCode, Quantity, InvoiceDate, UnitPrice, CustomerID, Country, Description, total_revenue) %>% arrange(desc(Quantity))
kable(Top5_data) # Check the result

## Create Top 5 Customer Behavior table based on Top5_data 
# to analyse their purchasing behavior to identify 
# which products they purchased the most (based on total quantity)
Top5_behavior <- Top5_data %>% group_by(CustomerID,Description) %>% 
  summarise(total_quantity=sum(Quantity), total_revenue = sum(total_revenue), .groups="drop") %>% 
  arrange(CustomerID, desc(total_quantity), desc(total_revenue))


# Visualize the Top 5 customer and Top 5 behavior by bar chart
options(scipen=9999) # To make value of total_spending measurable
# Top 5 customer spending plot
Top5_customer_bar <- ggplot(Top5_customer, mapping=aes(x= reorder(CustomerID, -total_spending), y= total_spending, fill= total_spending)) +geom_col() + labs(title = "Total Spending by Top 5 Customers", x="CustomerID", y="Total Spending") + scale_fill_viridis_c() +theme_minimal()+theme(plot.title= element_text(hjust = 0.5))
print(Top5_customer_bar)
## Assumption 6: Select only Top 5 products purchased by each Top customers
# Because dataset Top5_behaviour have 2753 observations of products and total_quantity,
# which results in overcrowded and unreable chart.
# Therefore, Assumption 6 helps clearly highlight customers' purchasing behaviour.
Top5_products <- Top5_behavior %>% group_by(CustomerID) %>% slice_max(order_by = total_quantity,n=5)

# Top 5 customer behavior bar chart
Top5_behavior_plot <- ggplot(Top5_products, aes(x= reorder(Description, -total_quantity), y=total_quantity, fill= factor(CustomerID))) +geom_col() +facet_grid(~CustomerID, scale="free_x") + scale_fill_brewer(palette="Dark2") + labs(title = "Top 5 Most Purchased Products by Top Customers", x="Products", y="Total Quantity")+theme(axis.text.x= element_text(angle=90, hjust=1),plot.title= element_text(hjust = 0.5)) 
print(Top5_behavior_plot)

### Task 3: Global Market Analysis
# Revenue for each country
Country_Revenue <- full %>% group_by(Country) %>% summarise(total_revenue=sum(total_revenue)) %>% arrange(desc(total_revenue))

# Identify single product with the highest quantity sold per country
country_product <- full %>% group_by(Country, Description) %>% summarise(total_quantity=sum(Quantity), .groups = "drop") %>% slice_max(order_by = total_quantity, n=1, with_ties = FALSE) %>% arrange(desc(total_quantity))

## Assumption 7: Select only top 22 countries by total revenue to avoid overcrowded and improve readability.
# Moreover, the excluded countries have revenue less than 7000 pounds.
Country_Revenue_filtered <- Country_Revenue %>%
  slice_max(order_by = total_revenue, n = 22)

# Visualization to compare revenue across regions
Country_revenue_plot <- ggplot(Country_Revenue_filtered , mapping=aes(x= reorder(Country, -total_revenue), y= total_revenue, fill= total_revenue)) +geom_col() + labs(title = "Total Revenue by Country", x="Country", y="Total Revenue") + scale_fill_viridis_c(option = "C", breaks = c(0, 7000, 1000000, 2000000, 3000000, 7000000)) +scale_y_continuous(breaks = c(0, 7000, 1000000, 2000000, 3000000, 7000000)) +theme(axis.text.x= element_text(angle=90, hjust=1))+theme(plot.title= element_text(hjust = 0.5))


### Task 4
## Assumption 8: Convert InvoiceDate to POSIXct to ensure it is recognized as a datetime object
# so that year and month can be extracted correctly from "full" table
full$InvoiceDate <- as.POSIXct(full$InvoiceDate, format = "%d/%m/%Y %H:%M")
# Extract months, years for each transaction from "full"
full <- full %>% mutate( year = as.numeric(format(InvoiceDate, "%Y")), month = as.numeric(format(InvoiceDate, "%m")))

# Calculate total revenue for each month
monthly_revenue <- full %>% group_by(year, month) %>% summarise(total_revenue=sum(total_revenue))

# Identify months with highest sales
Highest_sales_month <- monthly_revenue %>% slice_max(order_by = total_revenue, n=1)

# Analyse product performance over time
monthly_product_revenue <- full %>% group_by(year, month, Description) %>% summarise(product_revenue=sum(total_revenue))
full %>% filter(Description == "" | is.na(Description)) # Check as if any Description is empty product

## Assumption 9: Remove Description rows is missing or have empty product descriptions
full <- full %>% filter(!is.na(Description) & Description != "")
monthly_product_revenue <- full %>% group_by(year, month, Description) %>% 
  summarise(product_revenue=sum(total_revenue)) %>% arrange(year, month, desc(product_revenue)) # Re-run the code


# Identify products consistently among the lowest-selling across multiple months
Lowest_product <- monthly_product_revenue %>% group_by(year, month) %>% slice_min(order_by=product_revenue, n=1)

## Create a year_month variable so that R recognises the time variable as a Date object.
# This allows ggplot to correctly interpret the data as a time series and automatically
# plot the x-axis in chronological order.
monthly_revenue <- monthly_revenue %>% mutate( year_month = as.Date(paste(year,month,"01", sep="-"))) # add year_month for more detail

# Visualize the monthly sales trend using a time-series line chart
Monthly_sales_plot <- ggplot(monthly_revenue, mapping=aes(x= year_month, y= total_revenue)) +geom_line(color= 'steelblue') +scale_x_date(date_breaks= "1 month", date_labels = "%b %Y") + labs(title = "Monthly Revenue Trend", x="Months", y="Total Revenue") +theme(axis.text.x= element_text(angle = 45, hjust=1)) + theme(plot.title= element_text(hjust = 0.5))
print(Monthly_sales_plot)
