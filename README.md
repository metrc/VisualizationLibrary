# VisualizationLibrary 

The VisualizationLibrary is an R package developed by METRC (Major Extremity Trauma and Rehabilitation Consortium) to generate standardized data visualizations from study analytic datasets.

## Project Description

The VisualizationLibrary provides a consistent, data-matrix-driven workflow for generating visualizations commonly used in METRC studies. It defines common variables through standardized names and interfaces called *constructs*, which are defined from a data matrix. This allows consistency across REDCap projects and enables analysts to leverage information across studies.

The package contains base-level (i.e., 'user-level') code for creating data visualizations. Study-specific analytic datasets can be found in the private [AnalyticCodebase repository](https://github.com/metrc/AnalyticCodebase/).

## Features

- Generates standard tables and figures for METRC study reports
- Uses a data-matrix driven approach for consistency across studies
- Provides functions for visualizing:
    - Enrollment status and totals
    - Baseline characteristics 
    - Injury characteristics
    - Follow-up visit status
    - Adverse events and protocol deviations
    - And more!

## Installation 

To install the VisualizationLibrary package:

1. Ensure you have R installed. You can download it from the [R Project website](https://www.r-project.org/).

2. Install the `devtools` package if you haven't already. You can do this by running the following command in R:

   ```R
   install.packages('devtools')
   ```

   Install the VisualizationLibrary package using devtools:
   ```R
   devtools::install_github('metrc/VisualizationLibrary')
   ```

## Usage

Load the package in your R script or R Markdown file:

```R
library(VisualizationLibrary)
```

Then use the package functions to generate visualizations from your analytic dataset. For example:

```R
enrollment_status_by_site(analytic)
baseline_characteristics_percent(analytic)
consort_diagram(analytic)
```

See the package documentation for a full list of available functions and their usage.

## Configuration

No additional configuration is necessary to use the VisualizationLibrary package. However, your input analytic dataset must contain the necessary variables (constructs) for each visualization function you wish to use. Refer to the function documentation for the required constructs.

## Dependencies

The VisualizationLibrary depends on the following R packages:

- tidyverse
- janitor
- kableExtra
- DiagrammeR
- DiagrammeRsvg
- rsvg
- base64enc
- VisualizationTools

These will be automatically installed when you install the VisualizationLibrary.

## Contributing

Contributions to the VisualizationLibrary package are welcome. If you encounter a bug or have a feature request, please open an issue on the GitHub repository. If you would like to contribute code, please fork the repository and open a pull request with your changes.

## License
This project is proprietary software developed by METRC. All rights reserved.

## Contact Information
For any questions or support related to the AutoPayments package, please contact:
- Elias Weston-Farber: eweston4@jhu.edu
