# QC4LAB

A Shiny app for comparisons two measurements in laboratory settings

## 0. Running this App
Please access the following URL to run this web app:

https://rf-epidemiol.shinyapps.io/compaRison/

## 1. Example plots
### Scatter plot with regression lines
![Scatter](https://user-images.githubusercontent.com/19466700/181629603-15d7bca5-a496-4ff2-a6eb-802aa594fe8d.png)

### Bland-Altman plot
![BAplot](https://user-images.githubusercontent.com/19466700/181629622-d4f44de5-1590-402f-be9a-a6ac603d6804.png)

## 2. Cautions
- The column of your input data should be arrigned like Example data:
    - Column 1: ID/obs, etc... (Not use for plots)
    - Column 2: Value for Measurement A
    - Column 3: Value for Measurement B
- Passing-Bablok regression will take a some time compared with other two regression models. Please be patience with large N data.
- File name of PDF output does not assign well.
- Be sure NOT to flip back min and max of axis limits (X- and Y-), thus be careful to assign them properly.

## 3. Credits & Info
This app is developed by <a href="https://scholar.google.com/citations?user=IFFZUGcAAAAJ&hl=en">Ryosuke Fujii</a> (Fujita Health Univ 🇯🇵 / Eurac Research 🇮🇹). <br>
We thank Mr. Ishihara for inspiring us to develop this web app.

### Contact info:
- Twitter: <a href="https://twitter.com/RF_epidemiol">@RF_epidemiol</a><br>
- E-mail: <a href="mailto:rfujii@fujita-hu.ac.jp;">rfujii@fujita-hu.ac.jp</a><br>
