# Capitol Bikeshare
This repository contains my code and data used for predicting bike availability in Washington D.C. using the publicly available [Capital Bikeshare](https://s3.amazonaws.com/capitalbikeshare-data/index.html) dataset. This was originally a project for my Master's program that I then completed on my own time and increasing the scale of the project. The final presentation can be found [here](./results/capital_bikeshare_presentation.pdf).

## Data collection
The [processing](./code/processing.ipynb) notebook details the code that was used to collect the data from publicly available sources

## Exploration
The [exploration](./code/exploration.ipynb) notebook shows the steps I took to explore the data and some written analysis about the decisions made based on this exploration.

### Activity

#### Over time

![year_month_activity](./results/year_month_activity.png)

![hourly_average_activity](./results/hourly_average_activity.png)

![hourly_activity_boxplot](./results/hourly_activity_boxplot.png)

#### Geographically

##### arrivals
![arrivals_average_geo](./results/arrivals_average_geo.png)

##### departures
![departures_average_geo](./results/departures_average_geo.png)

## Modeling

Based on the results of the exploration, I tested and evaluated multiple machine learning models using predictive modeling best practices including train-test splits, hyperparameter tuning, and cross-validation. The results of the modeling can be found [here](./code/modeling.ipynb).

### Variable Relationships

![correlation_heatmap](./results/correlation_heatmap.png)

![pairplot_quantitative](./results/pairplot_quantitative.png)

### XGBoost Modeling Results

![feature_importances](./results/model_feature_importances.png)

#### Errors

![models_error_hist](./results/models_error_hist.png)

![arrivals_error_geo](./results/arrivals_error_geo.png)

![departures_error_geo](./results/departures_error_geo.png)