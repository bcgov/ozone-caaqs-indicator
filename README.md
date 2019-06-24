<a rel="Delivery" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/delivery.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a>[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# Status of Ground-Level Ozone in B.C.

A set of R scripts to calculate the Canadian Ambient Air Quality Standards (CAAQS) for  Ground-Level Ozone for British Columbia. These scripts reproduce the analysis and data visualizations supporting the Status of Ground-Level Ozone in B.C. indicator presented on [Environmental Reporting BC](http://www.env.gov.bc.ca/soe/indicators/air/ozone.html).

This analysis makes use of the [rcaaqs](https://github.com/bcgov/rcaaqs) package, and [verified air quality monitoring data](https://catalogue.data.gov.bc.ca/dataset/77eeadf4-0c19-48bf-a47a-fa9eef01f409) from the B.C. Ministry of Environment.

## Usage

There are four core scripts that are required for the analysis, they need to be run in order:

- `01_load.R` - downloads the data from DataBC
- `02_clean.R` - cleans and prepares data for analysis
- `03_analysis.R` - performs the analysis
- `04_output.R` - creates maps and graphs and saves outputs

The `run_all.R` script can be `source`ed to run it all at once.

Most packages used in the analysis can be installed from CRAN using `install.packages()`, but you will need to install [envreportutils](https://github.com/bcgov/envreportutils) and [rcaaqs](https://github.com/bcgov/rcaaqs) using remotes:

```r
install.packages("remotes") # If you don't already have it installed

library(remotes)
install_github("bcgov/rcaaqs")
install_github("bcgov/envreportutils")
```

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/ozone-caaqs-indicator/issues).

## How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## License

    Copyright 2015 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

This repository is maintained by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B). Click [here](https://github.com/bcgov/EnvReportBC) for a complete list of our repositories on GitHub.