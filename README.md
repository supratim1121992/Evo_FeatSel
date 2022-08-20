# Evo_FeatSel

## Feature selection using Genetic Algorithm

This function utilizes the optimisation capability of Genetic Algorithm to select the best set of minimal features from any dataset based on evaluation metrics generated from a model on a held-out (validation) set. The selected features are then used to train a final model which is returned as an output along with relevant details.

![GA_fig](https://github.com/supratim1121992/Evo_FeatSel/blob/main/GA_Summary.jpg?raw=true)

**Picture Courtesy:** Serg Masís, Data Scientist (https://serg.ai)

The function currently supports feature selection for both regression and classification tasks. For regression, both a parametric linear regression model or a non-parametric and non-linear Random Forest model can be used. For classification cases, feature selection is performed using a Random Forest model.

> **The function takes the following input arguments:**
 * **Data:** `dataframe,datatable` The dataset to select features from. The data should ideally be pre-processed to remove/impute missing values. The function will remove any records with missing values. Dummification should be performed as a pre-requisite for Linear regression with categorical features present in the data.
 * **Target:** `character` The name of the target variable in the input dataset.
 * **Norm:** `logical`(*Default FALSE*) Whether or not to normalise the numeric variables in the input dataset.
 * **Keep:** `character` Vector of independent variable names that should always be retained as part of the selected variables. This can be used in cases where any known drivers should always be saved for modelling.
 * **Ban:** `character` Vector of independent variables that should be removed from being selected in all cases.
 * **Treat_Out:** `logical` (*Default FALSE*) Whether or not to cap the outliers in input dataset. The outliers are detected using IQR and capped at the 10th and 90th percentile values as lower and upper limits respectively.
 * **Data_Split:** `character` (*Allowed values: Random, Ordered; Default Random*) Defines how the input dataset should be split. Random option will split the data randomly based on the **Train_Size** argument. Ordered option will retain the first n observations provided as input to the **Train_Size** argument.
 * **Train_Size:** `numeric` (*Default 0.8*) Number between 0 and 1, providing the split ratio for Random split. For Ordered split, the first n number of observations to use for training. The rest of the data will be used for validation. 
 * **Model:** `character` (*Allowed values: RF,LR; Default RF*) The type of model to be used for evaluation. RF for Random Forest and LR for Linear Regression. In case of Linear Regression, all variables in the input dataset should be numeric.
 * **Rsq_Threshold:** `numeric` (*Default 0.7*) The minimum R-squared value that must be obtained from modelling in case of Regression tasks for the set of selected variables to be considered a viable solution. In case of RF models, pseudo R-squared is used.
 * **Eval_Metric:** `character` The metric to be used for evaluating the predictions on validation set. Supported values are the names of evaluation functions provided in the **Metrics** R package. Refer to the package documentation for more details on the functionsavailable. https://cran.r-project.org/web/packages/Metrics/Metrics.pdf
 * **Maximize:** `logical` (*Default FALSE*) Whether the evaluation metric should be maximised or minimised to obtain the optimum result. For instance, regression tasks with RMSE would need the evaluation metric to be minimised whereas in classification tasks with accuracy, the evaluation metric would need to be maximised for best results.
 * **Par:** `logical` (*Default TRUE*) Whether or not the function would be run in parallel across the cores present in the system. Parallelisation helps in improving the computation time involved.
 
**NOTE:** The hyperparameters associated with the GA function can also be modified where the `ga` function is called in the function. Refer to https://cran.r-project.org/web/packages/GA/GA.pdf for details. 
  
> **Output:** The function will generate a list with the following components as output:
 * **Sol_Variables:** Vector with the set of minimum independent variables that resulted in the best model performance.
 * **Predictions:** Vector of predictions generated using the model on the validation set.
 * **Actual:** Vector of actual values in the validation set.
 * **Eval_Res:** Value of the evaluation metric from the best model with selected features on the validation set.
 * **Model:** The model object trained on the selected features.
 * **Model_Summary:** Summary of the above model object.
 * **GA_Model:** The GA model object run as part of the function.
 * **GA_Summary:** Summary of the GA model with more interpretable model parameters anddetails.
 * **Correlation:** (*Generated only in case of LR model*) The Pearson's correlation of each of the selected features with respect to the target variable.
 * **Conf_Matrix:** (*Generated only in case of classification with RF model*) The confusion matrix of predictions made on the validation set.
 * **Pred_Prob:** (*Generated only in case of classification with RF model*) The predicted probabilities for each class in the target variable from the validation set.
