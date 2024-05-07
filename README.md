
<!-- README.md is generated from README.Rmd. Please edit that file -->

# duflor.gui

<!-- badges: start -->

[![R-CMD-check:
Dev](https://github.com/Claudius-Appel/duflor.gui/actions/workflows/R-CMD-check.yaml/badge.svg?branch=dev)](https://github.com/Claudius-Appel/duflor.gui/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The `duflor.gui`-package contains a shiny-application built for the use
of the `duflor`-package.

## Installation

This package requires the installation of the `duflor`-package, which
implements most of the back-end code for the analysis-pipelines in this
application.

### Prerequisites

#### Install `renv`

As this application is quite complex, it relies on a certain number of
packages to work. The package `renv` can help with managing a
reproducible package environment for a project. As an additional
advantage, this also prevents packages in your system-library from being
updated when installing this package.

For a proper documentation and information on its use-cases, refer to
<https://rstudio.github.io/renv/index.html>.

To set up a stand-alone R-project with renv, the easiest way is through
R-Studio:

1.  Go to `File` \> `New Project`.
2.  Select `New Directory`.
3.  Select `New Project` as project-type[^1].
4.  Give a distinct name to the project,
    e.g. `duflor.gui_installed`[^2].
5.  Choose a location for the R-project.
6.  <u>**Make sure that the checkbox `Use renv with this project` is
    checked.**</u>
7.  Make sure the checkbox `Open in new session` is checked.
8.  Create the project.

From now on, this project can be opened from within R-Studio via the
project-selection drop-down-menu in the top-right corner of the
RStudio-application.

Before continuing, open this project if you did not do so.

#### Install `devtools`

First of all, make sure to have followed the steps outlined in
“\[Install renv\]”, and that RStudio is currently in your created
project.

Installing this package from GitHub requires the installation of the
R-package `devtools`. `devtools` can be used to compile source-packages
to installed binaries.

``` r
install.packages("devtools")
```

When being asked to proceed, make sure that the installation-path is in
the R-project you created:

If I created my R-project with `renv` at

    C:/Users/User_Main/Desktop/TempTemporal/test_duflorgui_install

, the path would look something like

    C:/Users/User_Main/Desktop/TempTemporal/test_duflorgui_install/renv/library/R-X.Y/....

If this is correct, you can go ahead and answer the prompt with `Y`. If
this is not the case, install devtools via the RStudio-GUI:

1.  Open the `packages`-panel.
2.  Search for and select `devtools`.
3.  Under “Install to library”, make sure the path selected is similar
    to the example outlined above. In other words, make sure you do
    **not** install to the library containing the string
    `R/cache/R/renv/sandbox`.

Once you have installed `renv`, go on to “[Installing application
itself](#installing-application-itself)”.

### Installing application itself

Both the `duflor`- and `duflor.gui`-packages must be installed from
their respective GitHub-repositories.

You can install the development and release versions of `duflor.gui`
like so:

``` r
# release:
devtools::install_github("https://github.com/Claudius-Appel/duflor.gui@master")
# development version:
devtools::install_github("https://github.com/Claudius-Appel/duflor.gui@dev")
```

As with installing `devtools`, make sure the `renv`-library is selected
when installing the package.

### Installing help-vignettes

The application is documented in detail via pkgdown, and its site can be
reached from the github-repository.

Most of the articles outlined on the site may also be viewed as
vignettes from within RStudio. To do so, you must explicitly declare
vignettes to be installed:

``` r
devtools::install_github("https://github.com/Claudius-Appel/duflor.gui@master"
  ,build_vignettes = T)
devtools::install_github("https://github.com/Claudius-Appel/duflor.gui@dev"
  ,build_vignettes = T)
```

Vignettes are rendered documents discussing specific topics within a
package. Installing them is recommended, but not required for utilizing
the package itself.

[^1]: This might sound confusing, as this is a R-package wrapping a
    shiny-application. However, those two options are for
    ***developing*** either of those, not for ***installing*** them.

[^2]: The name is not really important, but it should be distinct and
    indicate that this is the installed application
