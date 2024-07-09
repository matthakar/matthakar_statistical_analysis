# matthakar_statistical_analysis

The purpose of this R script is to examine two categorical variables and run statistical analyses to better understand the relationship between them.

To do this, the paths for the data and result directories must be defined alongside the categorical variables. This script covers 4 separate analyses:

1. Chi-Square Test of Independence --> This test measures whether there is a significant association between two categorical variables. A p-value < 0.05 indicates that there is evidence to reject the null hypothesis, suggesting that the variables are dependent (associated). --> When looking at the bar graph, if bars vary significantly in height and distribution, this suggests that there may be some association between the two categorical variables. If bars are similar in height and distribution, this suggests that the two variables are independent of each other.
   
2. Cramer's V Analysis --> This analysis quantifies the strength of association between two categorical variables on a scale from 0 to 1. A value of 0 indicates no association, while 1 indicates a perfect association. Values closer to 1 suggest a stronger relationship between the variables.
   
3. Correspondence Analysis --> This technique visualizes associations between categorical variables in a scatterplot. Points closer together indicate categories that are more strongly associated, while points further apart suggest differences between categories.
   
4. Poisson Regression Analysis --> This analysis assesses whether a Poisson Regression Model can effectively predict relationships between variables, and is typically used for count data. It models the expected count (rate) of occurrences based on predictor variables and evaluates how well these variables explain the distribution of count outcomes.

As an example, I attached the text files and graphical outputs generated when the "Blood Type" and "Medical Conditions" categorical variables are inputs. 

The dataset used for this script can be found here: https://www.kaggle.com/datasets/prasad22/healthcare-dataset
