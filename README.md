# compaRison

A Shiny app for comparisons two measurements in laboratory settings

## 0. Running this App
Please access the following URL to run this web app:

https://rf-epidemiol.shinyapps.io/compaRison/

## 1. Example plots & results
### Scatter plot with regression lines
This R shiny app is available for five regression models (least square method, Deming regression, Weighted Deming regression, Passing-Bablok regression, and approximative Passing-Bablok regression).
![Scatter](https://user-images.githubusercontent.com/19466700/184702134-b69b9e0a-65a7-4537-9e59-ff285be325a3.png)

### Bland-Altman plot
![BAplot](https://user-images.githubusercontent.com/19466700/184702119-fe826242-786d-4b7a-b357-5b0f7596e8e5.png)

## 2. Cautions
- The column of your input data should be arrigned like Example data:
    - Column 1: ID/obs, etc... (Not use for plots)
    - Column 2: Value for Measurement A
    - Column 3: Value for Measurement B
- Passing-Bablok regression will take a some time compared with other two regression models. Please be patience with large N data.
- Be sure NOT to flip back min and max of axis limits (X- and Y-), thus be careful to assign them properly.

## 3. Credits & Info
This app is developed by <a href="https://scholar.google.com/citations?user=IFFZUGcAAAAJ&hl=en">Ryosuke Fujii</a> (Fujita Health Univ 🇯🇵 / Eurac Research 🇮🇹). <br>
We thank Mr. Ishihara for inspiring us to develop this web app.

### Contact info:
- Twitter: <a href="https://twitter.com/RF_epidemiol">@RF_epidemiol</a><br>
- E-mail: <a href="mailto:rfujii@fujita-hu.ac.jp;">rfujii@fujita-hu.ac.jp</a><br>
