The app can be found in [this](https://mlaracue.shinyapps.io/NYC-leading-causes-death/) link.

# NYC Leading Causes of Death

## About

This dashboard aims to analyze the leading causes of death by _sex_ and _ethnicity_ in New York City since 2007. The cause of death is derived from the NYC death certificate issued for every death that occurs in New York City.

At the bottom-left, users can use a global filter to select a subset of the data and analyze the results for a particular death leading cause.

The dashboard comprises three main pages: Exploratory Data Analysis -EDA-, Hypothesis Testing (a feature recently included), and Expected Incidence. Find the description below:

### EDA

The EDA module allows the user to see, at a glance, the main relationships between the demographic variables.

-   At the beginning of the page, users can see a pie chart that conveys the total number of deaths by each cause. For example, the user can see that historically the higher number of deaths has been caused by heart diseases.
-   On the top-right, three static values are shown: the total number of deaths from 2007 to 2017, the average number of deaths by year, and the average death rate.
-   Below the three boxes, users can see a bar chart displaying the total number of deaths for each year for the aggregate (All), by sex or by ethnicity. Additionally, for some charts, a "Data" option is displayed if the user wants to see the data used for plotting.
-   The bubble chart aims to analyze the female/male deaths ratio by year and ethnicity. A table is also displayed.
-   At the bottom of the page, users can see the distribution of deaths by sex and ethnicity.
-   Finally, on the bottom-right, the average death rates by year and sex are displayed.

### Hypothesis Testing

This module intends to perform a Student's Test on the available data. Users can select the demographic variable (sex or ethnicity), the comparison groups (i.e., category levels for the selected variable), the significance level of the test (default is .05), and the type of test (two-tails or one-tail). Once all the options are selected, users need to click on the "Analyze" button, and three outputs are displayed.

1.  A summary table with test results and the decision based on the p-value.
2.  A density plot showing the distribution differences between the selected groups.
3.  A statistics summary table displaying the sample means and other useful information.

### Expected Incidence

The last module shows the expected death proportion for the populations selected (composed of intersections of sex and ethnicity). The plot shows the leading causes on the x-axis and the expected incidence on the y-axis. The dots represent the sample means, and the vertical lines represent the respective confidence intervals for the sample means.

Note: This module is available only if the users select "All" in the global filter. Otherwise, it's hidden.

The source data can be found in [NYC OpenData](https://data.cityofnewyork.us/Health/New-York-City-Leading-Causes-of-Death/jb7j-dtam).

Enjoy!