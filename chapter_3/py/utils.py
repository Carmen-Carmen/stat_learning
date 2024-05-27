import os
import numpy as np
import pandas as pd
from matplotlib.pyplot import subplots

import statsmodels.api as sm
from statsmodels.stats.outliers_influence \
import variance_inflation_factor as VIF
from statsmodels.stats.anova import anova_lm

from ISLP import load_data
from ISLP.models import (ModelSpec as MS, 
                            summarize, 
                            poly)

def plot_diagnostics(results): 
    # diagnostic plots
    axes = subplots(1, 2, figsize = (16, 8))[1]
    ax_resid_fitted = axes[0]
    ax_resid_leverage = axes[1]
    
    ax_resid_fitted.scatter(results.fittedvalues, results.resid)
    ax_resid_fitted.set_xlabel("Fitted values")
    ax_resid_fitted.set_ylabel("Residual")
    ax_resid_fitted.axhline(0, c = "k", ls = "--")

    infl = results.get_influence()
    indices = np.arange(results.nobs) # array range from 0 to length of X, i.e. indices of observations
    leverages = infl.hat_matrix_diag
    ax_resid_leverage.scatter(indices, leverages)
    ax_resid_leverage.set_xlabel("Index")
    ax_resid_leverage.set_ylabel("Leverage")
    max_index = np.argmax(infl.hat_matrix_diag)
    ax_resid_leverage.scatter(indices[max_index], leverages[max_index], color = "red", label = "index: %d" %max_index)
    ax_resid_leverage.legend()