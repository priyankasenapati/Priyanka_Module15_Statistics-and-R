# Priyanka_Module15_Statistics-and-R
## Challenge 

My objective is to perform statistical tests on the two datasets, MPG testing dataset and Mechacar suspension test results dataset, and create a technical report that will help drive key last minute decisions before the the grand product launch of the MechaCar. 

Linear regression is a statistical method to find the relationship between one dependent and one or
more independent variables. Regression analysis constitutes an important part of a statistical analysis
to explore and model the relationship between variables.

The variable I am predicting is called the dependent variable and is denoted as Y, while the variables
I am basing my predictions on are known as predictors or independent variables. These are referred
as X. Regression analysis helps in predicting the value of a dependent variable based on the values
of the independent variables. 

The four goals of this challenge are as follows:

1. Design and interpret a multiple linear regression analysis to identify variables of interest.
2. Calculate summary statistics for quantitative variables.
3. Perform a t-test in R and provide interpretation of results.
4. Design your own statistical study to compare vehicle performance of two vehicles.

Let's now do a deep dive into each analysis with these goals.

## Part 1: MPG Prediction
Following is the summary analysis from RStudio using the dataset supplied:
<img width="734" alt="summary_multilinear" src="https://user-images.githubusercontent.com/55486501/77246426-28bd5600-6c4d-11ea-84a8-59ba7ef9efae.png">


## Analysis
To forecast the mpg (miles per gallon) continuous dependent variable for the vehicle prototypes, I used
multiple regression analysis to analyze the non-random variance between the mpg and the continuous
independent variables, which were vehicle.length, vehicle.weight, spoiler.angle and ground.clearance.
The significance level was set to 0.05 or 5%. The null hypothesis is that the slope of the linear model
is zero.

From the above summary we observe the following
Multiple R-squared value is 0.7032 - The model is able to predict mpg with an accuracy of 0.70 or 70%.
This presents a resonably positive corelation.

p-value is 2.277e-11) - The p-value is less than the significance level set at 0.05. This means that
the null hypothesis can be rejected.

The Pr(>|t|) values indicates the probability of the respective independent variables contributing
random variance to the dependent variable. The significant ones are Intercept, vehicle.length and
ground.clearance. Since the Intercept is low and negative it means that there are no other variables
and factors that contribute to the variation in mpg. The model can predict of mpg from the independent
variables included in the model.

## Question 1. Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset?
The Pr(>|t|) values indicates the probability of the respective independent variables contributing
random variance to the dependent variable. The significant ones are Intercept, vehicle.length and
ground.clearance. Since the Intercept is low and negative it means that there are no other variables
and factors that contribute to the variation in mpg. All the others are not providing any non-random
amount of variance to the mpg values in the dataset.

## Question 2. Is the slope of the linear model considered to be zero? Why or why not?
No, the slope of the linear model is not zero. The p-value of 2.277e-11) is lower than the set
significant level of 0.05. The null hypothesis, that the slope of the linear model is zero, is
rejected.

## Question 3. Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not?
Multiple R-squared value is 0.7032 - The model is able to predict mpg with an accuracy of 0.70 or 70%.
This presents a resonably positive corelation.

The Pr(>|t|) values indicates the probability of the respective independent variables contributing
random variance to the dependent variable. The significant ones are Intercept, vehicle.length and
ground.clearance. Since the Intercept is low and negative it means that there are no other variables
and factors that contribute to the variation in mpg.

From the above Multiple R-squared value, Pr(>|t|), and the Intercept being negative, we can conclude
that the model can predict mpg from the independent variables included in the model.


## Part 2: Suspension Coil Summary

<img width="801" alt="summary_table" src="https://user-images.githubusercontent.com/55486501/77246492-ce70c500-6c4d-11ea-9547-025eae762a3a.png">

The mean and median PSI of the suspension coils manufactured in LOT1, LOT2 and LOT3 are around 1500 PSI
but the ones from LOT3 have a significantly variance of 170.28 and Standard Deviation of 13.049.

The design specifications for the MechaCar suspension coils dictate that the variance of the suspension
coils must not exceed 100 pounds per inch. The current manufacturing data, especially from LOT3, doesn't
meet this design specification.

## Part 3: Suspension Coil T-Test
Using the same suspension coil data and the MechaCarChallenge.RScript file, I determined if the suspension
coil’s pound-per-inch results are statistically different from the mean population results of 1,500
pounds per inch.

one-sample test was used to get the p-value for each of the lots.

- one-sample test:
* LOT1 one-sample t-test
data:  sample_lot1$PSI
t = 0, df = 49, p-value = 1
alternative hypothesis: true mean is not equal to 1500
95 percent confidence interval:
 1499.719 1500.281
sample estimates:
mean of x 
     1500)

* LOT2 one-sample t-test
data:  sample_lot2$PSI
t = 0.51745, df = 49, p-value = 0.6072
alternative hypothesis: true mean is not equal to 1500
95 percent confidence interval:
 1499.423 1500.977
sample estimates:
mean of x 
   1500.2)

* LOT3 one-sample t-test
data:  sample_lot3$PSI
t = -2.0916, df = 49, p-value = 0.04168
alternative hypothesis: true mean is not equal to 1500
95 percent confidence interval:
 1492.431 1499.849
sample estimates:
mean of x 
  1496.14

We set the significance value to 0.05 for the one-sample t-test. The mean PSI of LOT1 and LOT2 are
1500 each. The p-value of LOT1 is 1 and is more than the set significance level of 0.05, which means
that the NULL hypothesis cannot be rejected. The p-value of LOT2 is 0.6072, which also means that
the null hypothesis cannot be rejected. The mean PSI of LOT3 is 1498.5 but the variance 170.28 and
the p-value is 0.04168, which is lower than the set significance value of 0.05. In this case we can
derive a conclusion that the null hypothesis can be rejected and that the PSI data for LOT3 is
statistically different.


## Part 4 Design Your Own Study:
Design a study that compares the performance of the MechaCar prototype vehicle to other comparable
vehicles on the market. The following is a statistical study that can quantify how the MechaCar
outperforms the competition.

- Metrics you would think would be of interest to a consumer (cost, fuel efficiency, color options,
etc.).

The metrics that would be of interest to a customer are Cost, Range, Electric or Gas or Hybrid,
Fuel Efficiency, Density of Electric Charging stations, Safety Record, Reliability, Horsepower,
Torque, Depreciation Value, Customer Service, Availability of Service Centers and Spare Parts,
Accessory options, Colors, Car Reviews, Interior space, Trunk Space, Ease of driving, Acceleration,
Breaking power, Airbags, Rear Camera, Safety Features

## Determine what question we would ask, what the null and alternative hypothesis would be to answer that question, and what statistical test could be used to test this hypothesis. 

The question that needs to be asked is: Will MechaCar be in the top three highly rated cars in
the Car Reviews? This position will translate into higher customer demand, sales, revenue and
profit numbers. We will be considering the above metrics from 3 existing cars with the highest
Car Review Ratings and be comparing with the MechaCar's Car Rating forcast.

H0 - The slope of the Linear Model is 0.
Ha - The slope of the Linear Model is not 0.

The statistical test that will be performed is Multiple Linear Regression.

## Knowing what test should be used, what data should be collected?

The following data should be collected from existing 3 top reviewed cars and the MechaCar prototype.

1. Manufacturing Cost
2. MSRP
3. Range
4. Electric or Gas or Hybrid
5. Fuel Efficiency
6. Density of Electric Charging stations
7. Safety Record
8. Reliability
9. Horsepower
10. Torque
11. Depreciation Value
12. Customer Service
13. Availability of Service Centers and Spare Parts
14. Colors
15. Car Review Ratings
16. Car size
17. Interior space
18. Trunk Space
19. Ease of driving
20. Acceleration
21. Breaking power
22. Airbags (Safety Features)
23. Rear Camera (Safety Features)
24. Automatic breaking with proximity sensor
25. Quality of Entertainment System
26. Quality of Navigation System
