
This Shiny App is designed to perform analysis of variance (ANOVA) on powerlifting competition data.
It provides functionality to visualize box plots, compute descriptive statistics, conduct normality tests, and perform variance tests based on different equipment types used in competitions.

Features:

Wykresy Pudełkowe (Box Plots):

Select a specific equipment type from the dropdown menu.
Adjust the number of observations to display on the box plot using the slider.
Statystyki (Descriptive Statistics):

Provides basic descriptive statistics including minimum, maximum, mean, standard deviation, and variance for the selected equipment type.
Test Normalności (Normality Test):

Conducts the Shapiro-Wilk normality test for each equipment type.
Displays the p-value and the decision whether to reject the null hypothesis (H0) that the data come from a normally distributed population.
Test Wariancji (Variance Test):

Performs the Kruskal-Wallis rank sum test to determine if there are statistically significant differences in the total weight lifted among different equipment types.
Provides the chi-squared statistic, p-value, and degrees of freedom.

Instructions:

Wykresy Pudełkowe (Box Plots):

Select the equipment type from the dropdown menu.
Adjust the number of observations to display on the box plot using the slider.
Statystyki (Descriptive Statistics):

No user inputs required. Descriptive statistics will be displayed automatically for the selected equipment type.
Test Normalności (Normality Test):

No user inputs required. Normality tests will be conducted automatically for each equipment type.
Test Wariancji (Variance Test):


Important:

Ensure that you have the necessary R libraries installed (tidyr, dplyr, shiny, shinydashboard, ggplot2) before running the application.
The dataset (openpowerlifting_2024_01_06_4c732975) needs to be loaded before running the application.
Check the variable names and adjust the code accordingly if the dataset structure has changed.

No user inputs required. Variance tests will be performed automatically for all equipment types.
