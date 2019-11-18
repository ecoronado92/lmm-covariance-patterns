# LMM Covariance Structure Shiny App
Shiny app demonstrating covariance structure and correlation among observations in mixed effects models using two sample data sets.

## Shiny App Functionality

The shiny was built with an instructor in mind, allowing for easy modifications to expand its capabilities to additional datasets as necessary (see Appendix). 

Currently the app runs on two preset datasets used in STA610 class - the National Education Longitudinal Study (`nels.rds`) and Household Radon data (`srrs2.rds`). The user interface allows the instructor to select the dataset to be used to build a model and subsequent covariance/correlation visualization. The user interface has the following options once a dataset has been selected

* __Response variable__: Loads available variables based on the dataset, allows for a unique selection
* __Fixed effects__: Loads available variables (excluding the response selection), allows for multiple selection
* __Grouping variable (i.e. Random Intercept)__: Loads available variables (excluding the response selection), allows for a unique selection
* __Random Slope__: Loads available variables (excluding the response and grouping selections), allows for a unique selection
* __State selection__ (available for `srrs2` only): Allows user to filter dataset for a specific state to reduce sample size to be rendered

As the user selects these, the app will show the formula it will use to fit the model. This is helpful to ensure the user is fitting the model he/she intends before running any computations or fits.

Once the user defines the model to be fit, the \textbf{Fit Model} button fits the model and generates the visualization. Once these are done, the app will show the formula it fit, the intraclass correlation $\rho$, as well as an interactive grid chart where the user can zoom-in into specific groups using the cursor and use the hover functionality to get the specific variance/covariance values of each sample.

## Appendix 

The shiny app can easily be updated to visualize new datasets as longs as these files are `.rds`. However, it is important that for datasets with many observations or groups additionals modifications are necessary when rendering the plots (i.e. samples $n>1,000$ will require these additional modifications).

The visualization capabilities can be updated via the following:
* Add `.rds` file to same folder as the shiny app script (`shiny_varcov_lmm.R`)
* In the `ui` component of the shiny script, add the name of the `rds` file without the `rds` extension
* Save and run

Again, as noted above, if the number of samples to be visualized is $n>1000$ we need to add code to the `server` component of the shiny script where the data set is loaded to build the variance covariance structure, NOT to where it the model is fit.
