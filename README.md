## Project Overview: Prediction of CO and NOx Emissions from Gas Turbines

This project aims to predict the emissions of CO and NOx produced by gas turbines using data from 2011. The analysis begins with clustering methods, including K-means, hierarchical clustering, and DBSCAN, to explore natural groupings within the data. We then proceed with regression models to predict CO and NOx levels. The models applied include K-Nearest Neighbors, Random Forest, Support Vector Machines, Neural Networks, and Gaussian Processes.

### Clustering Analysis
- **K-means**: Applied to determine natural groupings in the data, with the optimal number of clusters determined using the elbow method.
- **Hierarchical Clustering**: We used Euclidean distance and various linkage methods, ultimately choosing complete-linkage and Ward's method for optimal clustering.
- **DBSCAN**: Though less effective in this context, DBSCAN highlighted dense regions within the dataset.

### Regression Models
- **K-Nearest Neighbors**: Utilized to predict emission levels, with hyperparameters optimized based on MSE.
- **Random Forest**: This ensemble method with 150 trees provided the most accurate predictions, particularly for NOx emissions.
- **Support Vector Machines**: Applied to model the emissions with a focus on hyperparameter optimization.
- **Gaussian Processes**: A non-parametric probabilistic method tested for predicting emission levels.
- **Neural Networks**: Optimized with hidden layers and learning rates, offering competitive predictions alongside other models.

### Forecasting
Using the Random Forest model, we extended our predictions to the years 2012-2015. The model demonstrated strong predictive power, particularly for NOx emissions, indicating the robustness of the selected variables in forecasting emissions over time.

### Conclusion
The Random Forest model emerged as the best performer, both in terms of accuracy and generalization to future data. The clustering and regression analyses provided deep insights into the data, confirming the effectiveness of these techniques in predicting gas turbine emissions.
