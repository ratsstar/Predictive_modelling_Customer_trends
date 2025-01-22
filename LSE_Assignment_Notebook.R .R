## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################

## Assignment 5 objective
## Load and wrangle the data. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

################################################################################

# Module 5 assignment: Load, clean and wrangle data using R

## It is strongly advised that you use the cleaned version of the data set that you created and 
##  saved in the Python section of the course. Should you choose to redo the data cleaning in R, 
##  make sure to apply the same transformations as you will have to potentially compare the results.
##  (Note: Manual steps included dropping and renaming the columns as per the instructions in module 1.
##  Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

## 1. Open your RStudio and start setting up your R environment. 
## 2. Open a new R script and import the turtle_review.csv data file, which you can download from 
##      Assignment: Predicting future outcomes. (Note: You can use the clean version of the data 
##      you saved as csv in module 1, or, can manually drop and rename the columns as per the instructions 
##      in module 1. Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

# Import the customers & reviews data sets cleaned in Python (customers_full.csv) using readr package.
customers <- read.csv('customers_full.csv', header=T)

## 3. Import all the required libraries for the analysis and view the data. 


# Import the necessary libraries.
library(tidyverse)
library(skimr)
library(DataExplorer)
library(moments)
library(psych)

## 4. Load and explore the data.
##    - View the head the data.
##    - Create a summary of the new data frame.

## We can sense-check whether the data set was imported correctly with the View(), 
## as_tibble(), head(), tail(), dim(), str(), and glimpse() functions.
## Missing values can be determined with the is.na() and sum(is.na()) functions. 
## We can view column names, which tell us what data the set contains with the names() or colnames() functions.

## customers
head(customers)
tail(customers)
summary(customers)
str(customers)
glimpse(customers)
as_tibble(customers)
dim(customers)
View(customers)
colnames(customers)
names(customers)
# he skim() function from the skimr package
skim(customers)

# Search for missing values
sum(is.na(customers))
sum(is.na(customers$gender))


# Ensure categorical variables are factors
customers$customer_ID <- as.factor(customers$customer_ID)
customers$cluster <- as.factor(customers$cluster)
customers$spending_score_cat <- as.factor(customers$spending_score_cat)
customers$remuneration_cat <- as.factor(customers$remuneration_cat)

# Creates an HTML report with summary stats of the data set.
DataExplorer::create_report(customers)


## add gender_viz columns with male = -1 and female = 1
customers$gender_viz[which(customers$gender == 'Male')] <- -1
customers$gender_viz[which(customers$gender == 'Female')] <- 1

str(customers)

# Return a frequency table for the 'cluster' column.
table(customers$cluster)

## the dataset is imported as expected

## 5. Perform exploratory data analysis by creating tables and visualisations to better understand 
##      groupings and different perspectives into customer behaviour and specifically how loyalty 
##      points are accumulated. Example questions could include:
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate
## 6. Create
##    - Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.


#################################################################################
################################# CUST ALL ####################################
#################################################################################

names(customers)

# view distribution: loyalty points, age, remun, count review
boxplot(customers$loyalty_points,
        main = "Boxplot of Customer Loyalty Points",
        cex.main = 2)
boxplot(customers$age,
        main = "Boxplot of Age",
        cex.main = 2)
boxplot(customers$remuneration,
        main = "Boxplot of Remuneration",
        cex.main = 2)
boxplot(customers$spending_score,
        main = "Boxplot of Spending Score",
        cex.main = 2)
boxplot(customers$count_review)

#### analyse remuneration vs. categorical features

names(customers)

# Create a boxplot of remuneration by education
boxplot(remuneration ~ education, 
        data = customers, 
        main = "Boxplot of Remuneration by Education", 
        cex.main = 2,
        xlab = "Education Level", 
        ylab = "Remuneration")

# Create a boxplot of remuneration by gender
boxplot(remuneration ~ gender, 
        data = customers, 
        main = "Boxplot of Remuneration by Gender", 
        cex.main = 2,
        xlab = "Gender", 
        ylab = "Remuneration")

# Create a boxplot of remuneration by age_group
boxplot(remuneration ~ age_group, 
        data = customers, 
        main = "Boxplot of Remuneration by age_group", 
        cex.main = 2,
        xlab = "age_group", 
        ylab = "Remuneration")



# grouped barplot count by age group 
ggplot(customers, aes(x = age_group, fill = cluster)) +
  geom_bar(position='dodge') +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +
  scale_y_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(title = "Number of customers per age group and cluster", 
       x = "Age group") +
  theme_classic()

# grouped barplot count by loyalty points 
ggplot(customers, aes(x = loyalty_points, fill = cluster)) +
  geom_bar() +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +
  scale_y_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(title = "Number of customers per loyalty points and cluster", 
       x = "Loyalty points balance") +
  theme_classic()

# density loyalty points 
ggplot(customers, aes(x = loyalty_points, col=cluster)) + 
  geom_density(bw = 10) +
  scale_color_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +
  scale_x_continuous(breaks = seq(0, 350, 10), "Loyalty points balance, count") +
  labs(x = "Loyalty pionts",
       y = "Customers, count",
       title = "Loyalty points by cluster") +
  theme_classic()

####################
## cluster 4 & 0 are extreme; others are more or less in the same range 
#####################

# barplot count by age group side-by-side
ggplot(customers, aes(x = age_group, fill = cluster)) +
  geom_bar(position='dodge') +
  facet_wrap(~cluster) +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +
  scale_y_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(title = "Number of customers per age group and cluster", x = "Age group") +
  theme_classic()

# barplot count by education side-by-side
# Set the custom order for the education factor
customers$education <- factor(customers$education, levels = c("Pre-school", "High-school", 
                                                              "Graduate", "Postgraduate", "PhD"))
# plot
ggplot(customers, aes(x = education, fill = cluster)) +
  geom_bar(position='dodge') +
  facet_wrap(~cluster) +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +
  scale_y_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(title = "Number of customers per education level and cluster", x = "Education") +
  theme_classic()


# density
ggplot(customers, aes(x = age, col=cluster)) + 
  geom_density(bw = 4) +
  scale_color_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +
  scale_x_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(x = "Age, years",
       y = "Customers, count",
       title = "Customers by age & cluster") +
  theme_classic()


# Aggregate the data by age group and gender, summing the gender_viz values
agg_data <- aggregate(gender_viz ~ age_group + gender + cluster, data = customers, sum)


# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = age_group, y = gender_viz, fill = cluster)) + 
  # Add background colours first (behind the bars)
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -50, ymax = 0), fill = "lightblue", alpha = 0.03) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 50), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
  coord_flip() +  # Flip the x-axis and y-axis
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  # Set y-axis limits from -40 to 40
  scale_y_continuous(limits = c(-50, 50)) + 
  labs(x = "Age group",
       y = "Customers, count",
       title = "Customers by gender and age group") +
  theme_classic()


######################
## in clusters 0 and 3 women prevail. 1&2&4 are balanced by gender
## 30-40 prevail in all clusters, but in cluster 4 the 30-40 age group share is extremely high
#####################


# Aggregate the data by age group and cluster, calculating average for the count_review values
agg_data <- aggregate(count_review ~ age_group + cluster, data = customers, mean)


# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = age_group, y = count_review, fill = cluster)) + 
  # Add background colours first (behind the bars)
  #  geom_rect(data = subset(agg_data, cluster == 1), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#FDAF2A', alpha = 0.1) +
  #  geom_rect(data = subset(agg_data, cluster == 4), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#D95769', alpha = 0.1) +
  #  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
  #  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  labs(x = "Age group",
       y = "Average number of reviews, count",
       title = "Average number of reviews by cluster and age group") +
  theme_classic()

######################
## some age groups are more active in clusters 2&4 than others. 
## this might be not representative because of the count of customers per group. 
## Small average number of review per customer in most of the cases corresponds 
## to demographic groups with small count

######################

# Aggregate the data by age group and cluster, calculating average for the loyalty_points values
agg_data <- aggregate(loyalty_points ~ age_group + cluster, data = customers, mean)

# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = age_group, y = loyalty_points, fill = cluster)) + 
  # Add background colours first (behind the bars)
  #  geom_rect(data = subset(agg_data, cluster == 1), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#FDAF2A', alpha = 0.1) +
  #  geom_rect(data = subset(agg_data, cluster == 4), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#D95769', alpha = 0.1) +
  #  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
  #  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  labs(x = "Age group",
       y = "Average balance of loyalty points",
       title = "Average balance of loyalty points by cluster and age group") +
  theme_classic()

##################
## The most significant features affecting loyalty points balance are remuneration level &  
## spending score (i.e. cluster)
## there is some variability by age groups within clusters, particularly in cluster 4, but
## in cluster 4 all the age groups except for 30-40 are insignificant in terms of customer count and 
## for this reason most likely are not representative
#################


# Aggregate the data by education and cluster, calculating average for the loyalty_points values
agg_data <- aggregate(loyalty_points ~ education + cluster, data = customers, mean)


# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = education, y = loyalty_points, fill = cluster)) + 
  # Add background colours first (behind the bars)
  #  geom_rect(data = subset(agg_data, cluster == 1), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#FDAF2A', alpha = 0.1) +
  #  geom_rect(data = subset(agg_data, cluster == 4), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#D95769', alpha = 0.1) +
  #  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
  #  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  labs(x = "Age group",
       y = "Average balance of loyalty points",
       title = "Average balance of loyalty points by cluster and education") +
  theme_classic()

######################
#### loyalty points do not depend on education level (rather clusters)
###################

# Aggregate the data by education and cluster, calculating average for the income values
agg_data <- aggregate(remuneration ~ education + cluster, data = customers, mean)


# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = education, y = remuneration, fill = cluster)) + 
  # Add background colours first (behind the bars)
  #  geom_rect(data = subset(agg_data, cluster == 1), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#FDAF2A', alpha = 0.1) +
  #  geom_rect(data = subset(agg_data, cluster == 4), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#D95769', alpha = 0.1) +
  #  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
  #  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  labs(x = "Age group",
       y = "Average balance of loyalty points",
       title = "Average remuneration by cluster and education") +
  theme_classic()

#######################
## there is no significant variability in average remuneration by education level within clusters 
#######################

# barplot count per cluster by education side-by-side
# Set the custom order for the education factor
customers$education <- factor(customers$education, levels = c("Pre-school", "High-school", "Graduate", "Postgraduate", "PhD"))

# plot
ggplot(customers, aes(x = cluster, fill = cluster)) +
  geom_bar(position='dodge') +
  facet_wrap(~education) +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +
  scale_y_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(title = "Number of customers per education level and cluster", x = "Education") +
  theme_classic()

##############
## most of the customers are graduates & above
#############


# Scatterplot: loyalty points vs  remuneration by cluster
ggplot(customers, aes(x=remuneration, y=loyalty_points, col=cluster)) + 
  scale_color_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +
  geom_point() + 
  geom_smooth(method=lm) +
  labs(x = "Remuneration, GBPk", 
       y = "Loyalty points balance",
       title = "Loyalty points balance vs. remuneration by cluster") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold"), # Increase title font size
    axis.title.x = element_text(size = 16),             # Increase x-axis label font size
    axis.title.y = element_text(size = 16)              # Increase y-axis label font size
  )

######################################
### The slope of the lines of the best fit suggests that a unit increase in income
### leads to a proportionately greater rise in loyalty points for customers 
### categorised in higher spending score clusters (specifically clusters 2 and 4) 
### than for those in lower spending score clusters (clusters 0 and 1), i.e. loyalty
### points exhibit greater variability in response to income changes for customers with 
### higher spending scores. Recognising this we fit three spending score specific 
## linear regression models to achieve better predictive power 
## (see the linear regression section of the technical report). 
######################################


# scatterplot:  Remuneration by cluster and education level
ggplot(customers, aes(x = education, y = remuneration, fill=cluster)) +  
  # Specify the geom_violin function and fill.  
  scale_fill_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +
  geom_violin(fill = 'white') +    
  facet_wrap(~cluster) +
  # Specify the geom_boxplot.  
  geom_boxplot(width = 0.25, outlier.color = 'red', outlier.size = 1, outlier.shape = 'square') +  
  labs(x = "Education level",
       y = "Remunaration, GBPk",
       title = "Remuneration by cluster and education level") +
  theme_classic()


# Scatterplot: age by educ levelby cluster
ggplot(customers, aes(x=age, y=remuneration, col=education)) + 
  #  scale_color_manual(values = c('#FDAF2A', '#D95769')) +
  geom_rect(data = subset(customers, cluster == 0), aes(xmin = -Inf, xmax = Inf, 
                                                        ymin = -Inf, ymax = Inf), fill = '#BFBFBF', alpha = 0.01) +
  geom_rect(data = subset(customers, cluster == 1), aes(xmin = -Inf, xmax = Inf, 
                                                        ymin = -Inf, ymax = Inf), fill = '#F6E825', alpha = 0.01) +
  geom_rect(data = subset(customers, cluster == 2), aes(xmin = -Inf, xmax = Inf, 
                                                        ymin = -Inf, ymax = Inf), fill = '#FDAF2A', alpha = 0.01) +
  geom_rect(data = subset(customers, cluster == 3), aes(xmin = -Inf, xmax = Inf,
                                                        ymin = -Inf, ymax = Inf), fill = '#EF7E50', alpha = 0.01) +
  geom_rect(data = subset(customers, cluster == 4), aes(xmin = -Inf, xmax = Inf, 
                                                        ymin = -Inf, ymax = Inf), fill = '#D95769', alpha = 0.01) +
  facet_wrap(~cluster) +
  geom_point() + 
  geom_smooth(method=lm) +
  labs(x = "Age, years",
       y = "Remunaration, GBPk",
       title = "Age by cluster and education level") +
  theme_classic()

# no clear strong patterns - confidence interval is too wide


#################################################################################
################################# CUST 1 & 4 ####################################
#################################################################################

# Filter for clusters 1 and 4
customers_1_4 <- customers %>% 
  filter(cluster %in% c(1, 4))


# grouped barplot count by age group 
ggplot(customers_1_4, aes(x = age_group, fill = cluster)) +
  geom_bar(position='dodge') +
  scale_fill_manual(values = c('#FDAF2A','#D95769')) +
  scale_y_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(title = "Number of customers per age group and cluster", 
       x = "Age group", subtitle = "Clusters 1 & 4") +
  theme_classic()


# barplot count by age group side-by-side
ggplot(customers_1_4, aes(x = age_group, fill = cluster)) +
  geom_bar(position='dodge') +
  facet_wrap(~cluster) +
  scale_fill_manual(values = c('#FDAF2A','#D95769')) +
  scale_y_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(title = "Number of customers per age group and cluster", x = "Age group", 
       subtitle = "Clusters 1 & 4") +
  theme_classic()

# barplot count by education side-by-side
# Set the custom order for the education factor
customers_1_4$education <- factor(customers_1_4$education, levels = c("Pre-school", "High-school", "Graduate", "Postgraduate", "PhD"))
# plot
ggplot(customers_1_4, aes(x = education, fill = cluster)) +
  geom_bar(position='dodge') +
  facet_wrap(~cluster) +
  scale_fill_manual(values = c('#FDAF2A','#D95769')) +
  scale_y_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(title = "Number of customers per education level and cluster", x = "Education", 
       subtitle = "Clusters 1 & 4") +
  theme_classic()


# density
ggplot(customers_1_4, aes(x = age, col=cluster)) + 
  geom_density(bw = 4) +
  scale_color_manual(values = c('#FDAF2A','#D95769')) +
  scale_x_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(x = "Age, years",
       y = "Customers, count",
       title = "Customers by age group & cluster",
       subtitle = "Clusters 1 & 4") +
  theme_classic()


# Aggregate the data by age group and gender, summing the gender_viz values
agg_data <- aggregate(gender_viz ~ age_group + gender + cluster, data = customers_1_4, sum)


# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = age_group, y = gender_viz, fill = cluster)) + 
  # Add background colours first (behind the bars)
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -40, ymax = 0), fill = "lightblue", alpha = 0.03) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#FDAF2A', '#D95769')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
  coord_flip() +  # Flip the x-axis and y-axis
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  # Set y-axis limits from -40 to 40
  scale_y_continuous(limits = c(-40, 40)) + 
  labs(x = "Age group",
     y = "Customers, count",
     title = "Customers by gender and age group",
     subtitle = "Clusters 1 and 4") +
  theme_classic()


# Aggregate the data by age group and cluster, calculating average for the count_review values
agg_data <- aggregate(count_review ~ age_group + cluster, data = customers_1_4, mean)


# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = age_group, y = count_review, fill = cluster)) + 
  # Add background colours first (behind the bars)
#  geom_rect(data = subset(agg_data, cluster == 1), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#FDAF2A', alpha = 0.1) +
#  geom_rect(data = subset(agg_data, cluster == 4), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#D95769', alpha = 0.1) +
#  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#FDAF2A', '#D95769')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
#  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  labs(x = "Age group",
       y = "Average number of reviews, count",
       title = "Average number of reviews by cluster and age group",
       subtitle = "Clusters 1 and 4") +
  theme_classic()

# Aggregate the data by age group and cluster, calculating average for the loyalty_points values
agg_data <- aggregate(loyalty_points ~ age_group + cluster, data = customers_1_4, mean)

# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = age_group, y = loyalty_points, fill = cluster)) + 
  # Add background colours first (behind the bars)
  #  geom_rect(data = subset(agg_data, cluster == 1), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#FDAF2A', alpha = 0.1) +
  #  geom_rect(data = subset(agg_data, cluster == 4), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#D95769', alpha = 0.1) +
  #  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#FDAF2A', '#D95769')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
  #  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  labs(x = "Age group",
       y = "Average balance of loyalty points",
       title = "Average balance of loyalty points by cluster and age group",
       subtitle = "Clusters 1 and 4") +
  theme_classic()

# Aggregate the data by education and cluster, calculating average for the loyalty_points values
agg_data <- aggregate(loyalty_points ~ education + cluster, data = customers_1_4, mean)


# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = education, y = loyalty_points, fill = cluster)) + 
  # Add background colours first (behind the bars)
  #  geom_rect(data = subset(agg_data, cluster == 1), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#FDAF2A', alpha = 0.1) +
  #  geom_rect(data = subset(agg_data, cluster == 4), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#D95769', alpha = 0.1) +
  #  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#FDAF2A', '#D95769')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
  #  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  labs(x = "Age group",
       y = "Average balance of loyalty points",
       title = "Average balance of loyalty points by cluster and age group",
       subtitle = "Clusters 1 and 4") +
  theme_classic()


# Scatterplot: loyalty points vs  renumeration by cluster
ggplot(customers_1_4, aes(x=remuneration, y=loyalty_points, col=cluster)) + 
  scale_color_manual(values = c('#FDAF2A', '#D95769')) +
  geom_point() + 
  geom_smooth(method=lm) +
  labs(x = "Remunaration, GBPk",
       y = "Loyalty points balance",
       title = "Loyalty points balance by cluster and age group",
       subtitle = "Clusters 1 and 4") +
  theme_classic()


# scatterplot:  Remuneration by cluster and education level
ggplot(customers_1_4, aes(x = education, y = remuneration, fill=cluster)) +  
  # Specify the geom_violin function and fill.  
  scale_fill_manual(values = c('#FDAF2A', '#D95769')) +
  geom_violin(fill = 'white') +    
  facet_wrap(~cluster) +
  # Specify the geom_boxplot.  
  geom_boxplot(width = 0.25, outlier.color = 'red', outlier.size = 1, outlier.shape = 'square') +  
  labs(x = "Education level",
     y = "Remunaration, GBPk",
     title = "Remuneration by cluster and education level",
     subtitle = "Clusters 1 and 4") +
  theme_classic()


# Scatterplot: loyalty points vs  renumeration by cluster
ggplot(customers_1_4, aes(x=age, y=remuneration, col=education)) + 
#  scale_color_manual(values = c('#FDAF2A', '#D95769')) +
  geom_rect(data = subset(customers_1_4, cluster == 1), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#FDAF2A', alpha = 0.01) +
  geom_rect(data = subset(customers_1_4, cluster == 4), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#D95769', alpha = 0.01) +
  facet_wrap(~cluster) +
  geom_point() + 
  geom_smooth(method=lm) +
  labs(x = "Age, years",
       y = "Remunaration, GBPk",
       title = "Remuneration by cluster and education level",
       subtitle = "Clusters 1 and 4") +
  theme_classic()

# no clear patterns - confidence interval is too wide


#################################################################################
################################# CUST 0 & 2 ####################################
#################################################################################

# Filter for clusters 0 and 2
customers_0_2 <- customers %>% 
  filter(cluster %in% c(0, 2))


# grouped barplot count by age group 
ggplot(customers_0_2, aes(x = age_group, fill = cluster)) +
  geom_bar(position='dodge') +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825')) +
  scale_y_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(title = "Number of customers per age group and cluster", 
       x = "Age group", subtitle = "Clusters 0 & 2") +
  theme_classic()


# barplot count by age group side-by-side
ggplot(customers_0_2, aes(x = age_group, fill = cluster)) +
  geom_bar(position='dodge') +
  facet_wrap(~cluster) +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825')) +
  scale_y_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(title = "Number of customers per age group and cluster", x = "Age group", 
       subtitle = "Clusters 0 & 2") +
  theme_classic()

# barplot count by education side-by-side
# Set the custom order for the education factor
customers_0_2$education <- factor(customers_0_2$education, levels = c("Pre-school", "High-school", "Graduate", 
                                                                      "Postgraduate", "PhD"))
# plot
ggplot(customers_0_2, aes(x = education, fill = cluster)) +
  geom_bar(position='dodge') +
  facet_wrap(~cluster) +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825')) +
  scale_y_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(title = "Number of customers per education level and cluster", x = "Education", 
       subtitle = "Clusters 0 & 2") +
  theme_classic()


# density
ggplot(customers_0_2, aes(x = age, col=cluster)) + 
  geom_density(bw = 4) +
  scale_color_manual(values = c('#BFBFBF', '#F6E825')) +
  scale_x_continuous(breaks = seq(0, 350, 10), "Customers, count") +
  labs(x = "Age, years",
       y = "Customers, count",
       title = "Customers by age group & cluster",
       subtitle = "Clusters 0 & 2") +
  theme_classic()


# Aggregate the data by age group and gender, summing the gender_viz values
agg_data <- aggregate(gender_viz ~ age_group + gender + cluster, data = customers_0_2, sum)


# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = age_group, y = gender_viz, fill = cluster)) + 
  # Add background colours first (behind the bars)
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -40, ymax = 0), fill = "lightblue", alpha = 0.03) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
  coord_flip() +  # Flip the x-axis and y-axis
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  # Set y-axis limits from -40 to 40
  scale_y_continuous(limits = c(-40, 40)) + 
  labs(x = "Age group",
       y = "Customers, count",
       title = "Customers by gender and age group",
       subtitle = "Clusters 0 and 2") +
  theme_classic()


# Aggregate the data by age group and cluster, calculating average for the count_review values
agg_data <- aggregate(count_review ~ age_group + cluster, data = customers_0_2, mean)


# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = age_group, y = count_review, fill = cluster)) + 
  # Add background colours first (behind the bars)
  #  geom_rect(data = subset(agg_data, cluster == 1), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#FDAF2A', alpha = 0.1) +
  #  geom_rect(data = subset(agg_data, cluster == 4), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#D95769', alpha = 0.1) +
  #  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
  #  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  labs(x = "Age group",
       y = "Average number of reviews, count",
       title = "Average number of reviews by cluster and age group",
       subtitle = "Clusters 0 and 2") +
  theme_classic()

# Aggregate the data by age group and cluster, calculating average for the loyalty_points values
agg_data <- aggregate(loyalty_points ~ age_group + cluster, data = customers_0_2, mean)

# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = age_group, y = loyalty_points, fill = cluster)) + 
  # Add background colours first (behind the bars)
  #  geom_rect(data = subset(agg_data, cluster == 1), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#FDAF2A', alpha = 0.1) +
  #  geom_rect(data = subset(agg_data, cluster == 4), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#D95769', alpha = 0.1) +
  #  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
  #  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  labs(x = "Age group",
       y = "Average balance of loyalty points",
       title = "Average balance of loyalty points by cluster and age group",
       subtitle = "Clusters 0 and 2") +
  theme_classic()

# Aggregate the data by education and cluster, calculating average for the loyalty_points values
agg_data <- aggregate(loyalty_points ~ education + cluster, data = customers_0_2, mean)


# Create the plot with aggregated data and custom background colours
ggplot(agg_data, aes(x = education, y = loyalty_points, fill = cluster)) + 
  # Add background colours first (behind the bars)
  #  geom_rect(data = subset(agg_data, cluster == 1), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#FDAF2A', alpha = 0.1) +
  #  geom_rect(data = subset(agg_data, cluster == 4), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#D95769', alpha = 0.1) +
  #  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40), fill = "lightpink", alpha = 0.03) +
  # Add the bars on top of the background
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('#BFBFBF', '#F6E825')) +  # Custom fill colours for cluster
  facet_wrap(~cluster) +
  labs(title = "Sum of Gender Viz by Age Group", 
       x = "Age Group", 
       y = "Sum of Gender Viz") +
  #  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 1) +
  labs(x = "Age group",
       y = "Average balance of loyalty points",
       title = "Average balance of loyalty points by cluster and age group",
       subtitle = "Clusters 0 and 2") +
  theme_classic()


# Scatterplot: loyalty points vs  renumeration by cluster
ggplot(customers_0_2, aes(x=remuneration, y=loyalty_points, col=cluster)) + 
  scale_color_manual(values = c('#BFBFBF', '#F6E825')) +
  geom_point() + 
  geom_smooth(method=lm) +
  labs(x = "Remunaration, GBPk",
       y = "Loyalty points balance",
       title = "Loyalty points balance by cluster and age group",
       subtitle = "Clusters 0 and 2") +
  theme_classic()


# scatterplot:  Remuneration by cluster and education level
ggplot(customers_0_2, aes(x = education, y = remuneration, fill=cluster)) +  
  # Specify the geom_violin function and fill.  
  scale_fill_manual(values = c('#BFBFBF', '#F6E825')) +
  geom_violin(fill = 'white') +    
  facet_wrap(~cluster) +
  # Specify the geom_boxplot.  
  geom_boxplot(width = 0.25, outlier.color = 'red', outlier.size = 1, outlier.shape = 'square') +  
  labs(x = "Education level",
       y = "Remunaration, GBPk",
       title = "Remuneration by cluster and education level",
       subtitle = "Clusters 1 and 4") +
  theme_classic()


# Scatterplot: loyalty points vs  renumeration by cluster
ggplot(customers_0_2, aes(x=age, y=remuneration, col=education)) + 
  #  scale_color_manual(values = c('#FDAF2A', '#D95769')) +
  geom_rect(data = subset(customers_0_2, cluster == 0), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#BFBFBF', alpha = 0.01) +
  geom_rect(data = subset(customers_0_2, cluster == 2), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = '#F6E825', alpha = 0.01) +
  facet_wrap(~cluster) +
  geom_point() + 
  geom_smooth(method=lm) +
  labs(x = "Age, years",
       y = "Remunaration, GBPk",
       title = "Remuneration by cluster and education level",
       subtitle = "Clusters 0 and 2") +
  theme_classic()

# no clear patterns - confidence interval is too wide


#####################################################################

##    - Select appropriate visualisations to communicate relevant findings and insights to the business.

names(customers)

#  scale_color_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +
ggplot(customers, aes(x=remuneration, y=loyalty_points, col=spending_score_cat)) + 
  geom_point() + 
  geom_smooth(method=lm, se = FALSE) +
  scale_x_continuous(
    limits = c(15, 75), 
    breaks = seq(20, 70, by = 10)
  ) +
  labs(x = "Annual income, £k",
       y = "Loyalty points balance",
       title = "Loyalty points balance by age and spending score categoryp") +
  theme_classic()


#  scale_color_manual(values = c('#BFBFBF', '#F6E825', '#FDAF2A', '#EF7E50', '#D95769')) +
ggplot(customers, aes(x=remuneration, y=loyalty_points, col=cluster)) + 
  geom_point() + 
  geom_smooth(method=lm, se = FALSE) +
  scale_x_continuous(
    limits = c(15, 75), 
    breaks = seq(20, 70, by = 10)
  ) +
  labs(x = "Annual income, £k",
       y = "Loyalty points balance",
       title = "Loyalty points balance by age and spending score category cluster") +
  theme_classic()



###
    labs(x = "Age, years",
       y = "Customers, count",
       title = "Customers by age & cluster") +
  theme_classic()

## 7. Note your observations and recommendations to the technical and business users.

# pls see above, in the tech report and presentation  



###############################################################################
###############################################################################

# Assignment 6 scenario
#############################################################
## In Module 5, you were requested to redo components of the analysis using Turtle Games’s preferred 
## language, R, in order to make it easier for them to implement your analysis internally. As a final 
## task the team asked you to perform a statistical analysis and create a multiple linear regression 
## model using R to predict loyalty points using the available features in a multiple linear model. 
## They did not prescribe which features to use and you can therefore use insights from previous modules 
## as well as your statistical analysis to make recommendations regarding suitability of this model type,
## the specifics of the model you created and alternative solutions. As a final task they also requested 
## your observations and recommendations regarding the current loyalty programme and how this could be 
## improved. 

################################################################################

## Assignment 6 objective
## You need to investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the work completed in modules 1-5 as well as the statistical analysis and modelling efforts of module 6.
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

################################################################################

## Assignment 6 assignment: Making recommendations to the business.
#############################################################
## 1. Continue with your R script in RStudio from Assignment Activity 5: Cleaning, manipulating, and 
##     visualising the data.
## 2. Load and explore the data, and continue to use the data frame you prepared in Module 5.
##############################################################
## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.

###########################
#### summary statistics

# calculate min, mean, max, sd & var for loyalty points by cluster and education
customers_sum <- customers %>% group_by(cluster, age_group) %>%
  summarise(sd_lp=sd(loyalty_points),
            var_lp=var(loyalty_points),
            mean_lp=mean(loyalty_points),
            min_lp=min(loyalty_points),
            max_lp=max(loyalty_points),
            .groups='drop')

# View the results.
customers_sum

## descriptive statistics 

# Determine the descriptive statistics.
summary(customers)

# average loyalty points & SD by cluster
aggregate(loyalty_points~cluster, customers, mean)
aggregate(loyalty_points~cluster, customers, sd)

# average loyalty points & SD by cluster & gender
aggregate(loyalty_points~cluster + gender, customers, mean)
aggregate(loyalty_points~cluster + gender, customers, min)
aggregate(loyalty_points~cluster + gender, customers, max)
aggregate(loyalty_points~cluster + gender, customers, sd)



# Range = Max - Min.
max(customers$loyalty_points)- min(customers$loyalty_points) 

# Use the summary() function.
summary(customers$loyalty_points)

# Calculate IQR.
IQR(customers$loyalty_points)  

# Determine the variance.
var(customers$loyalty_points)  


# Return the standard deviation.
sd(customers$loyalty_points)  

lower <- mean(customers$loyalty_points)  - 2 * sd(customers$loyalty_points)
upper <- mean(customers$loyalty_points)  + 2 * sd(customers$loyalty_points)  

lower
upper

#############
# loyalty points range from 25 to 6847
# average loyalty points balance is 1497, at the same time median value is 1187
# this means that the distribution is right skewed (with outliers?) in the upper end
# 50% of the observatins lie beetween 701 and 1658 loyalty points (IQR)
# SD is 1313 loyalty points 
# with 95% of certainty you can expect loyalty points to be below 4124 assuming z-distribution
#############


# 3. Distribution of the data.

# Specify boxplot function.
boxplot(customers$loyalty_points)

## Not symmetric; the right tail is longer than left tail. There are outliers; positive skewed
## does not resemple normal distribution 

# Specify histogram function.
hist(customers$loyalty_points)

## same observation 

###############################################################################

# 4. Determine normality of data.

# Specify qqnorm function (draw a qqplot).
qqnorm(customers$loyalty_points)
# Specify qqline function.
qqline(customers$loyalty_points) 

# the distribution is not normal - right skewed 


# qqplot for cluster 4
qqnorm(customers$loyalty_points[which(customers$cluster == '4')])
qqline(customers$loyalty_points[which(customers$cluster == '4')])

## good; 
## 

# qqplot for cluster 3
qqnorm(customers$loyalty_points[which(customers$cluster == '3')])
qqline(customers$loyalty_points[which(customers$cluster == '3')])
## very good 

# qqplot for cluster 2
qqnorm(customers$loyalty_points[which(customers$cluster == '2')])
qqline(customers$loyalty_points[which(customers$cluster == '2')])
## good

# qqplot for cluster 1
qqnorm(customers$loyalty_points[which(customers$cluster == '1')])
qqline(customers$loyalty_points[which(customers$cluster == '1')])

## good

# qqplot for cluster 0
qqnorm(customers$loyalty_points[which(customers$cluster == '0')])
qqline(customers$loyalty_points[which(customers$cluster == '0')])

# not very good 


## Shapiro-Wilk test:
# Null hypothesis: The data is NOT normally distributed (small p = reject)
# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(customers$loyalty_points) # 0 rejected
shapiro.test(customers$loyalty_points[which(customers$cluster == '4')]) # 0.048
shapiro.test(customers$loyalty_points[which(customers$cluster == '3')]) # 0.81
shapiro.test(customers$loyalty_points[which(customers$cluster == '2')]) # 0.002
shapiro.test(customers$loyalty_points[which(customers$cluster == '1')]) # 0.047
shapiro.test(customers$loyalty_points[which(customers$cluster == '0')]) # 0

# Determine sample size for each cluster and total.
customers %>% 
  count(cluster)

dim(customers)


######
# The Shapiro-Wilk normality test indicates p>0.05 (we assumed a>0.05), 
# supporting normally distributed loyalty points for the 3rd cluster.
# For all the other clusters p-values < 0.05 - indicating data is not normally distributed 
# the test is sample size sensitive; and is not likely to reject the null hypothesis on a small sample 
######


## Skewness and Kurtosis
#all
skewness(customers$loyalty_points) # 1.66 - positive skew
kurtosis(customers$loyalty_points) # 5.31 - heavy tailed (>3)
dim(customers)

# by cluster
skewness(customers$loyalty_points[which(customers$cluster == '4')]) # 0.25 POSITIVE
skewness(customers$loyalty_points[which(customers$cluster == '3')]) # 0.04 slight positive skew
skewness(customers$loyalty_points[which(customers$cluster == '2')]) # 0.63 - POSITIVE
skewness(customers$loyalty_points[which(customers$cluster == '1')]) # 0.46 - POSITIVE
skewness(customers$loyalty_points[which(customers$cluster == '0')]) # 0.45 - POSITIVE

###########
# The skewness falls between the range of -0.5 and 0.5 for each cluster (except for n2
# indicating a fairly symmetrical distribution, and supprots normal distribution.
# n2 of 0.63, which is slightly above 0.5 - right skewed 
# cluster 3 - almost perfect symmetry with 0 mectric
#############

kurtosis(customers$loyalty_points[which(customers$cluster == '4')]) # 2.39 light tailed (<3)
kurtosis(customers$loyalty_points[which(customers$cluster == '3')]) # 3.05 - (normal = 3)
kurtosis(customers$loyalty_points[which(customers$cluster == '2')]) # 2.96 - NORMAL
kurtosis(customers$loyalty_points[which(customers$cluster == '1')]) # 2.97 - NORMAL
kurtosis(customers$loyalty_points[which(customers$cluster == '0')]) # 1.85 light tailed (<3)

############
# Cluster 4 & 0 are light tailed. kurtosis indicates that the data has a peak.
# Cluster 3, 2 & 2 kurtosis is close to 3 and supports the normally distribution notion.
# Overall distribution in each cluster is close to normal.
##################


# Determine the correlation for the whole data frame.
round (cor(customers[sapply(customers, is.numeric)]),
       digits=2)


# cluster 4 corr: key features remun 76%; spend score 45%, age 34%
cust_4 <- customers[customers$cluster == '4', ]
round (cor(cust_4[sapply(cust_4, is.numeric)]),
       digits=2)

# cluster 3 corr: key features remun 58%; spend score 43%, 32% age
cust_3 <- customers[customers$cluster == '3', ]
round (cor(cust_3[sapply(cust_3, is.numeric)]),
       digits=2)

# cluster 2 corr: key features remun 79%; spend score 34%, 31% age
cust_2 <- customers[customers$cluster == '2', ]
round (cor(cust_2[sapply(cust_2, is.numeric)]),
       digits=2)

# cluster 1 corr: key features remun 91%; spend score 40%, 
cust_1 <- customers[customers$cluster == '1', ]
round (cor(cust_1[sapply(cust_1, is.numeric)]),
       digits=2)

# cluster 0 corr: key features remun 85%; spend score 36%, 
cust_0 <- customers[customers$cluster == '0', ]
round (cor(cust_0[sapply(cust_0, is.numeric)]),
       digits=2)


##########################
### full dataset: loyalty points have moderate positive correlation with renumeration & spending score 
## clusters: remun up to 91% with remuneration + over 30% with spending score
# in clusters 2 3 4 age is significant as well 
## approach: by cluster
# selected features for linear regression (to test):
# cluster 0 & 1: remuneration +  spending score
# clusters 2 & 3 & 4: remuneration +  spending score + age 
## more on this in python notebook 
######################


## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Evaluate the goodness of fit and interpret the model summary statistics.
##  - Create a visual demonstration of the model
##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.
##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).


########################################################
####################### cust_1 ##############################
#############################################################


names(cust_1)

# Create am MLR model
cust_1_mlr_1 <- lm(loyalty_points ~ remuneration + spending_score, 
                   data=cust_1)

summary(cust_1_mlr_1)
# p<0.5 -> all the variables are meaningful 
# negative intercept - not meaningful; 10.2 rem + 48.8 score
# model explains 90.5% 

##############
# test LR
#############
# Create a model with only one x variable.
cust_1_lr_1 <- lm(loyalty_points ~ remuneration, 
                   data=cust_1)

summary(cust_1_lr_1)

# Create a model with only one x variable.
cust_1_lr_2 <- lm(loyalty_points ~ spending_score, 
                  data=cust_1)

summary(cust_1_lr_2)
################
# stick with MLR: much higher R-squared

# plot residuals
plot(cust_1_mlr_1$residuals)
# no pattern identified 


##############################################################
####################### cust_0 ##############################
##############################################################


# Create am MLR model
cust_0_mlr_1 <- lm(loyalty_points ~ remuneration + spending_score, 
                   data=cust_0)

summary(cust_0_mlr_1)
# p<0.5 -> all the variables are meaningful 
# negative intercept - not meaningful; 13.7 rem + 12.2 score
# model explains 88% 


# plot residuals
plot(cust_0_mlr_1$residuals)
# no pattern identified 

##############################################################
####################### cust_2 ##############################
##############################################################


# Create am MLR model
cust_2_mlr_1 <- lm(loyalty_points ~ remuneration + spending_score + age, 
                   data=cust_2)

summary(cust_2_mlr_1)
# p<0.5 -> all the variables are meaningful 
# negative intercept - not meaningful; 49.7 rem + 12 score + 7 age
# model explains 87% 


# plot residuals
plot(cust_2_mlr_1$residuals)
# no pattern identified 

##############################################################
####################### cust_3 ##############################
##############################################################


# Create am MLR model
cust_3_mlr_1 <- lm(loyalty_points ~ remuneration + spending_score + age, 
                   data=cust_3)

summary(cust_3_mlr_1)
# p<0.5 -> all the variables are meaningful 
# negative intercept - not meaningful; 33 rem + 27 score + 8 age
# model explains 75% 


# plot residuals
plot(cust_3_mlr_1$residuals)
# no pattern identified 

##############################################################
####################### cust_4 ##############################
##############################################################


# Create am MLR model
cust_4_mlr_1 <- lm(loyalty_points ~ remuneration + spending_score + age, 
                   data=cust_4)

summary(cust_4_mlr_1)
# p<0.5 -> all the variables are meaningful 
# negative intercept - not meaningful; 54 rem + 47 score + 19 age
# model explains 85% 


# plot residuals
plot(cust_4_mlr_1$residuals)
# no pattern identified 


####################
#####
####################
# Load necessary libraries
library(broom)
library(dplyr)
library(tidyr)

# Function to create a single-row summary for each model
generate_summary_row <- function(model, data, target_var, model_name) {
  # Extract R-squared and adjusted R-squared
  rsq <- summary(model)$r.squared
  adj_rsq <- summary(model)$adj.r.squared
  intercept <- coef(model)[1]
  
  # Calculate residuals
  residuals <- model$residuals
  mae <- mean(abs(residuals), na.rm = TRUE)
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  # Get descriptive stats for the target variable
  target_stats <- data %>%
    summarise(
      mean = mean(!!sym(target_var), na.rm = TRUE),
      min = min(!!sym(target_var), na.rm = TRUE),
      max = max(!!sym(target_var), na.rm = TRUE),
      q1 = quantile(!!sym(target_var), 0.25, na.rm = TRUE),
      median = median(!!sym(target_var), na.rm = TRUE),
      q3 = quantile(!!sym(target_var), 0.75, na.rm = TRUE),
      IQR = q3 - q1
    ) %>%
    as.list()
  
  # Extract coefficients as a named vector
  coefficients <- tidy(model) %>%
    filter(term != "(Intercept)") %>%
    select(term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate) %>%
    as.list()
  
  # Combine everything into a single row
  summary_row <- tibble(
    model = model_name,
    intercept = intercept,
    r_squared = rsq,
    adjusted_r_squared = adj_rsq,
    mae = mae,
    rmse = rmse,
    mean = target_stats$mean,
    min = target_stats$min,
    max = target_stats$max,
    q1 = target_stats$q1,
    median = target_stats$median,
    q3 = target_stats$q3,
    IQR = target_stats$IQR
  ) %>%
    bind_cols(coefficients)
  
  return(summary_row)
}

# Apply the function to each model and combine into a single table
all_summaries <- bind_rows(
  generate_summary_row(cust_0_mlr_1, cust_0, "loyalty_points", "cust_0"),
  generate_summary_row(cust_1_mlr_1, cust_1, "loyalty_points", "cust_1"),
  generate_summary_row(cust_2_mlr_1, cust_2, "loyalty_points", "cust_2"),
  generate_summary_row(cust_3_mlr_1, cust_3, "loyalty_points", "cust_3"),
  generate_summary_row(cust_4_mlr_1, cust_4, "loyalty_points", "cust_4")
)

# Transpose the table
transposed_summaries <- all_summaries %>%
  pivot_longer(cols = -model, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = model, values_from = Value)

# View the transposed table
print(transposed_summaries)


## 5. Perform exploratory data analysis by using statistical analysis methods and comment on the descriptive 
##     statistics in the context of the review of how customers accumulate loyalty points.

# see above 


## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. (This will serve as input to your summary and final submission at the end of the course.)


### see above, technical report and presentation 

###############################################################################
###############################################################################




