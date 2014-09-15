### NLME project (Still in progress)
-------------------------------------
A detailed report is provided in [nonlinear_mixed-effects_Regression_Project.pdf](https://github.com/thomastskng/Stats-Project/blob/master/Nonlinear-Regressions-and-mixed-effects-models/report3/nonlinear_mixed-effects_Regression_Project.pdf)

An plot from Exploratory Data Analysis is show below:
![alt text](https://github.com/thomastskng/Stats-Project/blob/master/Nonlinear-Regressions-and-mixed-effects-models/report3/Plots/7.2-Plot_after_adjustment.png)


An image of Level I of the population model (grouped by calibration point (CP) level):
    The thin lines represent the data vectors, while the black lines represent the level I fitted values from the nonlinear mixed-effects model. The color changes across different CP levels. 
![alt text](https://github.com/thomastskng/Stats-Project/blob/master/Nonlinear-Regressions-and-mixed-effects-models/report3/Plots/7.3-Plot_with_nlme_function_superimposed.png) 


To examine level 2 of the population model and compare it to level I, an *example* of the individual plots is shown below: 

The thick cyan line represents the fitted values of Level I model, which evaluates all the subjects under a particular CP level.

The individual (subject) data is represented by the unfilled triangles. 

The fitted values of Level II of the population are represented by the black color. 

![alt text](https://github.com/thomastskng/Stats-Project/blob/master/Nonlinear-Regressions-and-mixed-effects-models/report3/Plots/7.4-Plot_For_individual_observation15.png)



#### NLME Diagnostics plot (in progress)
------------------------------------------
From the first two reports, we suspect that there is a saturation effect within
the devices. This means the alcohol gets trapped in the device and thus, leads to
bias in the parameter estimates. 

An exploratory plot of all the devices (those with CP1 measurements only) looks like:

![alt text](https://github.com/thomastskng/Stats-Project/blob/master/Nonlinear-Regressions-and-mixed-effects-models/report3/Exploratory_Data_Analysis.png)

Observations were grouped by the time that these observations were taken, i.e. \[15,30)
means the observations were taken between the 15th minute and 30th minute. 

It is evident that in the first 20 seconds that observations in Group 1 tend to
have slower decay rate than the rest. 

After fitting the nlme function, we have the 2 images below: 
(1) contains only the fitted values of the *group effects* only; 
(2) have the group effects fitted values superimposed on all individual observations (dashed lines)

![alt text](https://github.com/thomastskng/Stats-Project/blob/master/Nonlinear-Regressions-and-mixed-effects-models/report3/group-effects-nlme_function.png)


![alt text](https://github.com/thomastskng/Stats-Project/blob/master/Nonlinear-Regressions-and-mixed-effects-models/report3/Plot_with_nlme_function_superimposed.png)


