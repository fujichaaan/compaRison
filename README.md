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

## Step 2. Scatter plot with regression lines
Please select combinations of various regression models.
This application implements five regression models as follows:

1. Least square method
2. Deming regression
3. Weighted Deming regression
4. Passing-Bablok regression
5. Approximative Passing-Bablok regression

The graphical options are available in the left column.

## Step 3. Bland-Altman plot & Limits of agreement
Please select plot types & assumptions about distribution of y-axis.
The graphical options are available in the left column.

## Cautions
- Passing-Bablok regression will take a some time compared with other two regression models. Please be patience with large N data.
- Be sure NOT to flip back min and max of axis limits (X- and Y-), thus be careful to assign them properly.

## Credits & Info
This app is developed by <a href="https://scholar.google.com/citations?user=IFFZUGcAAAAJ&hl=en">Ryosuke Fujii</a> (Fujita Health Univ 🇯🇵 / Eurac Research 🇮🇹). <br>
We thank Mr. Ishihara for inspiring us to develop this web app.

### Contact info:
- Twitter: <a href="https://twitter.com/RF_epidemiol">@RF_epidemiol</a><br>
- E-mail: <a href="mailto:rfujii@fujita-hu.ac.jp;">rfujii@fujita-hu.ac.jp</a><br>
