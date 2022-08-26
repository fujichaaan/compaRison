# compaRison

A Shiny app for comparisons two measurements in laboratory settings

## Step 0. Running this App
Please access the following URL to run this web app:

https://rf-epidemiol.shinyapps.io/compaRison/

## Step 1. Import dataset
Please prepare .txt or .csv dataset
The column of your input data should be arrigned like Example data:
- Column 1: ID/obs, etc... (Not use for plots)
- Column 2: Value for Measurement A
- Column 3: Value for Measurement B

![screencapture-rf-epidemiol-shinyapps-io-compaRison-2022-08-24-02_09_09](https://user-images.githubusercontent.com/19466700/186287313-b51c11e7-a1cd-4244-8635-e75d2e46c5bc.png)

## Step 2. Scatter plot with regression lines
Please select combinations of various regression models.
This application implements five regression models as follows:

1. Least square method
2. Deming regression
3. Weighted Deming regression
4. Passing-Bablok regression
5. Approximative Passing-Bablok regression

The graphical options are available in the left column.

![screencapture-rf-epidemiol-shinyapps-io-compaRison-2022-08-24-02_06_54](https://user-images.githubusercontent.com/19466700/186287295-63952f35-4c7d-481e-b44b-20ff06748e86.png)

## Step 3. Bland-Altman plot & Limits of agreement
Please select plot types & assumptions about distribution of y-axis.
The graphical options are available in the left column.

![screencapture-rf-epidemiol-shinyapps-io-compaRison-2022-08-24-02_08_58](https://user-images.githubusercontent.com/19466700/186287325-44f0db8a-afa1-48f7-af61-503d716c8483.png)

## Cautions
- Passing-Bablok regression will take a some time compared with other two regression models. Please be patience with large N data.
- Be sure NOT to flip back min and max of axis limits (X- and Y-), thus be careful to assign them properly.

## Version control
Ver.1.0.0: September 1 2022

## Credits & Info
This app is developed by <a href="https://scholar.google.com/citations?user=IFFZUGcAAAAJ&hl=en">Ryosuke Fujii</a> (Fujita Health Univ 🇯🇵 / Eurac Research 🇮🇹). <br>
We thank Mr. Ishihara for inspiring us to develop this web app.

### Contact info:
- Twitter: <a href="https://twitter.com/RF_epidemiol">@RF_epidemiol</a><br>
- E-mail: <a href="mailto:rfujii@fujita-hu.ac.jp;">rfujii@fujita-hu.ac.jp</a><br>
