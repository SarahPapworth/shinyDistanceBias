# ShinyDistanceBias App

## Outline

This app helps simulate abundances based on sampling along a transect.

## 1. Installing the application

This app should be able to be installed and run by using the following code.

```{r}
# run the code using shiny
shiny::run_github('sankeydan/shinyDistanceBias')
```

The following packages may need to be installed to aid running of the app.

```{r}
# install packages
install.packages(c('shiny', 'circular', 'shinybusy', 'shinyWidgets', 'Distance'))
```

## 2. Running the application

All instructions to run your data are provided in the app itself. You will need to load your data into the app, adjust the parameters as needed, and initiate the analysis.

The expected output is thoroughly explained in the ShinyDistanceBias app. You can download the output by clicking the provided button within the app.

The expected run time varies according to the number of replicates. For a thorough analysis, it could take over 10 hours.

## 3. Instructions for Use

All instructions for use are provided in the ShinyDistanceBias app. Load your data, adjust the settings as needed, and start the analysis. For additional questions or troubleshooting, refer to the "Help" section within the app.

## 4. System Requirements

### Software Dependencies

This ShinyDistanceBias application requires the following software packages:
  
- R version 4.1.2 or higher
- RStudio version 1.4 or higher
- Shiny package version 1.6.0 or higher
- circular package (latest version)
- shinybusy package (latest version)
- shinyWidgets package (latest version)
- Distance package (latest version)

### Operating Systems

The application is platform-independent but has only been tested on Windows 10. However, it should run on any operating system where R and RStudio can be installed, including:
  
- Windows 10 or higher
- macOS Mojave (10.14) or higher
- Ubuntu 18.04 LTS or higher

### Tested Versions

The software ShinyDistanceBias v1.0 has been tested on Windows 10 using R version 4.1.2 and RStudio version 1.4.

Required Non-standard Hardware

No non-standard hardware is required.