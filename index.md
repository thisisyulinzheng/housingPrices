# A Statistical Analysis of Housing Prices in Saratoga Springs, NY

## I. Introduction

Housing is a basic necessity, and a very expensive one in New York. As New York residents, we experience the consequences of the high cost of living firsthand. New York has its benefits and downsides to living in it, such as job opportunities and the standard of living, and we weigh these factors when deciding where to live. Many immigrants come to New York just for the opportunity at a better life, only to be met with cramped living and high prices. We will analyze a sample of housing data to better understand the cost of living in New York.

In this statistical analysis, we take a close look at the prices of houses across various zip codes in Saratoga Springs, New York in 2006. In this suburban city, there are a variety of housing sizes, types, and included amenities. This makes data from Saratoga representative of the housing market of New York and a good candidate for statistical analysis. We sourced this data from Project MOSAIC; it has a sample size of 1,734 and contains various categories of data such as the prices of the house, dimensions, number of rooms, along with other variables. There are fifteen independent variables of which six are qualitative, such as the fuel type the house burns or whether the house is a waterfront property. The remaining are quantitative, such as the age of the house, and one quantitative variable is a percentage: the percentage of the neighborhood that graduated college. In order to understand the cause of the high cost of living in New York, we will use this dataset to determine which variables most impact the price of a house.

We seek to understand how internal and external factors affect the price of a house in Saratoga Springs, New York, so we will answer the question: Which properties of a house best predict the price of a house in Saratoga Springs, New York?
We want to understand how these factors interact to create various housing prices so we will use regression modeling and various forms of residual analysis. To build a model that will accurately represent the data, and therefore identify the most significant variables, we eliminate the extreme outliers and construct an algorithm that can best represent the remaining data set. 

## II. Creating the Model

To begin building the model in R studio, we must be able to import our dataset. Our dataset is Mosaic Data, which has an installable R package. Figure 1 is a sample of the data.

```
library(mosaicData)
data <- mosaicData::SaratogaHouses
head(data)
```

<img src="/housingPrices/assets/1a.png" alt="" width="500"/><br>
<img src="/housingPrices/assets/1b.png" alt="" width="500"/><br>
*Figure 1*

Since our data includes 6 qualitative variables, we must convert them to use in our model alongside the quantitative variables. We first build a full linear regression model using all 15 independent variables and the dependent variable (price). We will analyze the initial model to discern if interaction or quadratic terms are necessary, and to detect multicollinearity. Figure 2 is a summary of the coefficients’ values and p-values.

```
data$heating <- as.factor(data$heating)
data$fuel <- as.factor(data$fuel)
data$sewer <- as.factor(data$sewer)
data$waterfront <- as.factor(data$waterfront)
data$newConstruction <- as.factor(data$newConstruction)
data$centralAir <- as.factor(data$centralAir)

model_full <- lm(price ~ lotSize + age + landValue + livingArea +
              	pctCollege + bedrooms + fireplaces + bathrooms +
              	rooms + heating + fuel + sewer + waterfront +
              	newConstruction + centralAir,
              	data=data)
summary(model_full)

```

<img src="/housingPrices/assets/2a.png" alt="" width="500"/><br>
<img src="/housingPrices/assets/2b.png" alt="" width="500"/><br>
<img src="/housingPrices/assets/2c.png" alt="" width="500"/><br>
*Figure 2*

We can look at the adjusted R-squared value, which is 0.6498, to conclude that the model explains 64.98% of the variance in the dependent variable (price). This model also has at least one statistically significant term as the p-value for our F-test is 2.2e-16, which is nearly zero. We remove the variables whose beta coefficients have p-values that are higher than =0.05, the significance level, because they do not cause a significant difference in the dependent variable. 

To further understand which variables may overlap, we will conduct a Variance Inflation Factor (VIF) analysis to measure the amount of multicollinearity in our regression model. VIF analysis measures how much the variance of a regression coefficient increases due to correlations between independent variables. It is important to diminish the multicollinearity in the model because we want to isolate the effect of each independent variable on the dependent variable.

```
library(car)
vif_values <- vif(model_full)
Vif_values
```

<img src="/housingPrices/assets/3a.png" alt="" width="400"/><br>
*Figure 3*

Based on our VIF analysis in Figure 3, we can remove the following variables: rooms, heating, fuel, bathrooms, and bedrooms. Their values are greater than 1.49 and thus suggest moderate multicollinearity. The value for livingArea is also quite high, but this variable measures the number of rooms, bedrooms, and bathrooms in a house, so once we remove the prior variables, the VIF value should decrease. Referring to Figure 2, it is evident the beta coefficients for pctCollege, fireplaces, heating, fuel, and sewers all have p-values greater than =0.05, our chosen significance value. Thus to conclude our VIF analysis of the initial model, rooms, bedrooms, and bathrooms are removed from this model along with the insignificant variables.

We now create a summary in R to analyze our reduced model, as seen in Figure 4.

```
model_reduced <- lm(price ~ lotSize + age + landValue + livingArea +
               	waterfront + newConstruction + centralAir,
              	data=data)
summary(model_reduced)
```

<img src="/housingPrices/assets/4a.png" alt="" width="500"/><br>
*Figure 4*

The beta coefficients of our remaining variables have p-values that suggest they are statistically significant because they are less than the significance level =0.05. We ensure multicollinearity has been diminished by conducting a second VIF analysis. We observe in Figure 5 that the VIF value of livingArea has decreased to 1.374233, whereas previously it was 2.040932. This change supports our inference that the rooms, bedrooms, and bathrooms variables were in fact multicollinear with livingArea.

```
vif_values_reduced <- vif(model_reduced)
vif_values_reduced
```


<img src="/housingPrices/assets/5a.png" alt="" width="500"/><br>
*Figure 5*

However, the adjusted R-squared is 0.6371 in the summary output for the reduced model in Figure 4, which conveys that this model only accounts for 63.71% of the variance in the data. Thus we will introduce interaction terms to attempt to improve the model. After creating various models with different interaction terms, we found that the variables age and livingArea did interact and improved the model’s accuracy by 0.26%, as shown in Figure 6.

```
model_interaction <- lm(price ~ lotSize + age + landValue + livingArea +
               	waterfront + newConstruction + centralAir + age:livingArea,
              	data=data)
summary(model_interaction)
```

<img src="/housingPrices/assets/6a.png" alt="" width="500"/><br>
*Figure 6*

To further improve the reduced model, we experiment with quadratic terms. After some attempts, we find that adding quadratic terms for age, livingArea, and landValue increases the adjusted R-squared by 0.0029 or 0.29% (Figure 7).

```
model_quadratic <- lm(price ~ lotSize + age + landValue + livingArea +
               	waterfront + newConstruction + centralAir + 
               	I(age^2) + I(livingArea^2) + I(landValue^2),
              	data=data)
summary(model_quadratic)
```

<img src="/housingPrices/assets/7a.png" alt="" width="500"/><br>
*Figure 7*

After discerning the best interaction terms and quadratic terms, we can now combine the two models to create a model of best fit (Figure 8). As seen in Figure 8, the adjusted R-squared is now 0.6458, meaning 64.58% of the variance in the data can be attributed to the model. Furthermore, all of the beta coefficients’ p-values are less than the significance level =0.05, meaning they are statistically significant.

```
model_combined <- lm(price ~ lotSize + age + landValue + livingArea +
               	waterfront + newConstruction + centralAir + age:livingArea
               	+ I(age^2) + I(livingArea^2) + I(landValue^2),
              	data=data)
summary(model_combined)
```

<img src="/housingPrices/assets/8a.png" alt="" width="500"/><br>
*Figure 8*

Our model’s variables have been fine-tuned to best represent the dataset, however the adjusted R-squared of the combined model (Figure 8) is still not as high as the adjusted R-squared of the initial full model (Figure 2), meaning the initial full model explains more of the variance in the data than the combined model does. We tackle this problem by reconfiguring our dataset; the outliers in the dataset are influencing the validity of our model. There are a couple ways to handle influential points, including removing the points or obtaining more data. However, we are not in a position to collect more data, so we came to the decision to use Cook’s Distance, which is a measure of how much the beta coefficients would change if a particular data point were removed from our analysis. After removing the outliers, our model summary in Figure 9 shows that the adjusted R-squared increased significantly from Figure 8 by 0.0611. 

```
cooks_values <- cooks.distance(model_combined)
threshold <- 4 / nrow(data)
outliers <- which(cooks_values > threshold)

data_removed_outliers <- data[-outliers, ]
model_removed_outliers <- lm(price ~ lotSize + age + landValue + livingArea +
                         	waterfront + newConstruction + centralAir + 
age:livingArea + I(age^2) + I(livingArea^2) + I(landValue^2),
                         	data = data_removed_outliers)
summary(model_removed_outliers)
```

<img src="/housingPrices/assets/9a.png" alt="" width="500"/><br>
*Figure 9*

Although the model’s adjusted R-squared has increased, suggesting that it better explains the variance in the data, two of the variables are now insignificant: the quadratic terms livingArea and landValue. We can identify this by analyzing their p-values, which are both greater than =0.05. Furthermore, we can conduct a 95% confidence interval (Figure 10) for all of the variables in the model and see that the intervals for the quadratic terms for livingArea and landValue both contain 0, meaning they are statistically insignificant. Thus, we create a new model without these variables (Figure 11) and conduct a confidence interval of the new model (Figure 12) to test the significance of every variable. None of the confidence intervals for the variables contain 0, indicating that every variable is statistically significant. In addition, the adjusted R-squared increased by 0.0001, indicating a 0.01% increase in the model’s ability to explain the variance in the data. Thus, our final model includes the properties’ lot size, age, land value, square footage of the living area, if the property is a waterfront, if it is newly constructed, if there is central air, a quadratic term for age, and an interaction term between age and square footage of living area.

```
confi <- confint(model_removed_outliers, level=0.95)
```

<img src="/housingPrices/assets/10a.png" alt="" width="400"/><br>
*Figure 10*

```
model_removed_outliers2 <- lm(price ~ lotSize + age + landValue + livingArea +
                                waterfront + newConstruction + centralAir + 
                                age:livingArea + I(age^2),
                              data = data_removed_outliers)
summary(model_removed_outliers2)
```

<img src="/housingPrices/assets/11a.png" alt="" width="500"/><br>
*Figure 11*

```
confi2 <- confint(model_removed_outliers2, level=0.95)
```

<img src="/housingPrices/assets/12a.png" alt="" width="400"/><br>
*Figure 12*

The final model in Figure 11 can explain 70.7% of the variance in the prices of houses in Saratoga Spring, New York in 2006. Furthermore, it is a statistically significant model due to its high F-statistic, its p-value being very nearly zero, and all of the variables being statistically significant. Compared to the initial full linear model, this final model is more capable of representing prices of houses in Saratoga Springs accurately and implementing the most significant variables. 


## III. Variance/Residuals Analysis

Our analysis to this point has shown that our model is capable of representing the dataset accurately. We will conduct an analysis of variance and the residuals to support our regression model. The plot of residuals over fitted values in Figure 13 shows that the data points are randomly scattered, thus our assumption of homoscedasticity is verified.

```
plot(fitted(model_removed_outliers2), residuals(model_removed_outliers2), main="Residual Plot", xlab="Fitted Values", ylab="Residuals")

abline(h=0, col="red")
```

<img src="/housingPrices/assets/13a.png" alt="" width="500"/><br>
*Figure 13*

We also implement a Q-Q plot to visualize the points along the Q-Q line. Figure 14 shows that the points on the Q-Q plot follow the Q-Q line, thus further verifying our normality assumption. There is slight deviation at the bottom and top of the line, however we can continue to assume normality due to the high amount of points on the line.

```
qqnorm(residuals(model_removed_outliers2))
qqline(residuals(model_removed_outliers2), col = "red")
```
<img src="/housingPrices/assets/14a.png" alt="" width="600"/><br>
*Figure 14*

In order to meet the assumptions for multiple regression analysis, the mean of the residuals must be equal to 0. By plotting the residuals of our final model (Figure 11) versus each independent variable, we evaluate the distribution of points in each scatterplot. A random scatter of points can be visualized in each scatterplot to verify the assumption. If the variable is a factor, a boxplot of the residuals of the model is created for each level of the factor. We check to see that the box plot is symmetric around 0 to make sure that the residuals have a mean of zero. The scatter plots and boxplots can be found in the appendix.

Finally, we conduct an Analysis of Variance (ANOVA) test between the final model and the full model with no variables removed to understand the statistical difference between them. We will generate a report on the full model using the data with outliers removed so the ANOVA test is referencing the same dataset. The adjusted R-squared for the final model (Figure 11) is slightly greater than the full linear model (Figure 2) with no variables removed. However, the ANOVA table F-statistic is very small and the p-value is greater than the significance level =0.05, which suggests that the two models are not significantly different. 

```
model_removed_outliers_full <- lm(price ~ lotSize + age + landValue + 
livingArea + pctCollege + bedrooms + fireplaces + 
bathrooms + rooms + heating + fuel + sewer + 
waterfront + newConstruction + centralAir,
              	data=data_removed_outliers)
summary(model_removed_outliers_full)
anova(model_removed_outliers2, model_removed_outliers_full)
```

<img src="/housingPrices/assets/15a.png" alt="" width="500"/><br>
*Figure 15*

Both the final combined and full linear models are capable of accurately representing the prices of houses in Saratoga Springs, however the final combined model has advantages to it outside of statistical reasoning.

## IV. Results and Discussion

Our final model is statistically significant and can explain 70.7% of the variance in the data, however we also discovered it is not significantly different from the initial model. Although the two models may be equally capable of representing the data, our final model is favorable because it uses fewer variables than the initial model to obtain the same results. Researchers that would like to expand upon their collection of housing data in Saratoga Springs can focus on 8 variables instead of 16.

Through our statistical analysis, we found that the most significant predictors of the price of a house in Saratoga Springs were lot size, age, land value, square footage of the living area, if the property is a waterfront, if it is newly constructed, and if there is central air conditioning. Home buyers can use this analysis to understand the factors impacting the prices of houses, real estate agents can use it to market their properties accordingly, and evaluators can use it to accurately assess property prices. If our findings are implemented by the aforementioned parties, the housing market becomes more equitable because every price is chosen with thorough statistical reasoning. 

The housing market could not exist without its consumers, thus a subject for further analysis could be exploring the relationship between the buyer and the home in 2024. Possible independent variables are the buyer’s age, gender, ethnicity, occupation, number of dependents, veteran status, disability status, marriage status, and annual income in 2023. The dependent variable could be price or house size, measured by square feet. We can discern which independent variables correlate the most with the dependent variables through multiple linear regression. We can use the results to understand generational differences in home ownership status in 2024.

## V. Conclusion

Our statistical analysis highlights the strongest predictors of housing prices in Saratoga Springs, New York in 2006. We have used the dataset to highlight this relationship and explore the causes of the high cost of living in New York. The standard of housing determines the cost of living, and in turn the standard of living. New York residents continue to weigh the pros and cons of their state and take housing into great consideration.


## VI. Appendix

Corvetti, C. (2006). Houses in Saratoga County (2006) [Data set]. 
https://rdrr.io/cran/mosaicData/man/SaratogaHouses.html

Fox J, Weisberg S (2019). An R Companion to Applied Regression, Third edition. Sage, Thousand Oaks CA. https://www.john-fox.ca/Companion/.

Pruim, R., Kaplan, D. T., & Horton, N. J. (2017). The mosaic Package: Helping Students to “Think with Data” Using R. In The R Journal (Vol. 9, Issue 1, pp. 77–102). https://journal.r-project.org/archive/2017/RJ-2017-024/index.html

R Core Team (2024). _R: A Language and Environment for Statistical
  Computing_. R Foundation for Statistical Computing, Vienna, Austria.
  <https://www.R-project.org/>.

<img src="/housingPrices/assets/r1.png" alt="" width="350"/>
<img src="/housingPrices/assets/r2.png" alt="" width="350"/>
<img src="/housingPrices/assets/r3.png" alt="" width="350"/>
<img src="/housingPrices/assets/r4.png" alt="" width="350"/>
<img src="/housingPrices/assets/r5.png" alt="" width="350"/>
<img src="/housingPrices/assets/r6.png" alt="" width="350"/>
<img src="/housingPrices/assets/r7.png" alt="" width="350"/>
<img src="/housingPrices/assets/r8.png" alt="" width="350"/>
<img src="/housingPrices/assets/r9.png" alt="" width="350"/>
