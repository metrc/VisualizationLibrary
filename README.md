# VisualizationTools

The VisualizationTools is a [METRC](metrc.org/)-specific R toolkit that is designed to generate data visualizations from raw [REDCap](https://projectredcap.org/) data. An data visualization is an R environment filled with pre-defined functions and constructs (i.e. variables) which makes it convenient to work with data from a study. data visualizations generated from the VisualizationTools offer the following benefits:

* **Consistent, data-matrix-driven workflow:** Common variables are available through standardised names and interfaces called *constructs*. These constructs are defined from the data matrix: which allows consistency across REDCap projects and enables analysts to leverage information across studies.

The [VisualizationTools](https://github.com/metrc/VisualizationTools) repository contains only the infrastructure-level (i.e. 'system-level') code for the creation of data visualizations. Study-specific data visualizations can be found in the [VisualizationLibrary repository](https://github.com/metrc/VisualizationLibrary/).

## Overview

Todo: overview of the package files and their functions.

## Development Setup

The following setup instructions are for manually setting up a development environment on your local
machine. It assumes a Ubuntu or Debian based operating system, and reasonable familiarity with
the command line.

### Installing R

First, ensure that you have R available on your system. In order to install R, navigate to the R Project website, and follow the instructions provided to add a new apt repository and download the packages.

* [R Project Website](https://www.r-project.org/)

### Installing R Devtools

Now that we have the R environment, we must install the R devtools package, The R devtools includes all the programs and components that are needed to develop and build R packages from source. 

To begin, we must first install some system-level tools that R devtools depends upon. These libraries are often used for compiling software:

```bash
sudo apt install build-essential libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev libjpeg-dev libpng-dev libssl-dev libtiff5-dev libxml2-dev
```

Once those dependencies are installed, we may install devtools directly from within the R environment. This can be done either by starting an interactive R session (using `R`), or by using `RScript` to execute the installation command directly from the command line.

```bash
Rscript -e "install.packages('devtools', dependencies=TRUE)"
```

Now your R environment should be properly setup with devtools.

### Installing R Packages that VisualizationTools depends on

Next, we must install the R packages which VisualizationTools depends on. These packages are:

* [tidyverse](https://www.tidyverse.org/): R Packages for Data Science.
* [httr](https://httr.r-lib.org/) R Library to work with URLs and HTTP.
* [feather](https://posit.co/blog/feather/): Fast on-disk file format for tabular data.
* [googlesheets4](https://googlesheets4.tidyverse.org/) R API Wrapper for Google Sheets.
* [igraph](https://r.igraph.org): R package of the igraph network analysis library.
* [writexl](https://cran.r-project.org/web/packages/writexl/index.html): Library to write to XLSX files.
* [readr](https://readr.tidyverse.org/): Read rectangular Text Data.

Just like with the R devtools installation process, we must first install some systems-level dependencies since we will be compiling them from source.

```bash
sudo apt install libsodium-dev gfortran libglpk-dev
```

Next, run the following to install the R packages.

```bash
sudo Rscript -e "install.packages(c('tidyverse', 'httr', 'feather', 'googlesheets4', 'igraph', 'writexl', 'readr'), dependencies=TRUE)"
```

Now all of the R package dependencies for VisualizationTools should be installed.

### Cloning the VisualizationTools Repository

In order to begin development on the VisualizationTools, you must switch to the `dev` branch of this repository.

```bash
git clone https://github.com/metrc/VisualizationTools.git
cd VisualizationTools
git switch dev
git pull
```

Now you should have an up-to-date version of the development branch.

### Building & Installing the VisualizationTools Package Manually

Before building and installing the VisualizationTools from source, we must first run R-Check on the package to ensure it is free of errors. This step is especially important if you are developing on the VisualizationTools from source:

```bash
R CMD check VisualizationTools/
```

Once R-Check passes, it is time to build the VisualizationTools package. This will create a `.tar.gz` archive file which can be then installed.

```bash
R CMD build VisualizationTools/
```

If the build passes successfully, you should have a `.tar.gz` archive with the version number of the package. This file may look like `VisualizationTools_0.1.1.tar.gz`. In order to install it, simply run `R CMD INSTALL <package_name>`. Note that the command is case-sensitivie.

```bash
R CMD INSTALL VisualizationTools_0.1.1.tar.gz
```

Now this version of the VisualizationTools package should be installed and available for use within R.

## Continuous Integration Pipelines

In order to ensure stability, all commits will be built by [Github Actions](https://docs.github.com/en/actions). The GA workflows for VisualizationTools can be found in the [`.github/workflows/`](./github/workflows/) directory. For more information regarding the Github Actions workflows, please see the VisualizationTools Github Actions [documentation](.github/workflows/README.md).

## `.gitmessage.txt` Configuration
This project aims to follow the Linux Kernel commit message policy. For ease of use, a commit message template is provided at `gitmessage.txt`. In order to enable this commit message template locally (only for this repository), set `gitmessage.txt` as the [`commit.template`](https://www.git-scm.com/book/en/v2/Customizing-Git-Git-Configuration#_commit_template) in your git config:

```bash
git config --local commit.template ./gitmessage.txt
```
