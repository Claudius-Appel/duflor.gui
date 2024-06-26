
<!-- README.md is generated from README.Rmd. Please edit that file -->

# duflor.gui

<!-- badges: start -->

[![R-CMD-check:
Dev](https://github.com/Claudius-Appel/duflor.gui/actions/workflows/R-CMD-check.yaml/badge.svg?branch=dev)](https://github.com/Claudius-Appel/duflor.gui/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The `duflor.gui`-package contains a shiny-application built for the use
of the `duflor`-package.

## Installation - from within RStudio

This package requires the installation of the `duflor`-package, which
implements most of the back-end code for the analysis-pipelines in this
application.

### Prerequisites

As this application is quite complex, it relies on a certain number of
packages to work. The package `renv` can help with managing a
reproducible package environment for a project. As an additional
advantage, this also prevents packages in your system-library from being
updated when installing this package.

For a proper documentation and information on its use-cases, refer to
“<https://rstudio.github.io/renv/index.html>”.

#### Setup r-project with `renv`

To set up a stand-alone R-project with `renv`, the easiest way is
through R-Studio:

1.  Go to `File` \> `New Project`.
2.  Select `New Directory`.
3.  Select `New Project` as project-type[^1].
4.  Give a distinct name to the project,
    e.g. `duflor.gui_installed`[^2].
5.  Choose a location for the R-project.
6.  <u>**Make sure that the check-box `Use renv with this project` is
    checked.**</u>[^3]
7.  Make sure the check-box `Open in new session` is checked.
8.  Create the project.

From now on, this project can be opened from within R-Studio via the
project-selection drop-down-menu in the top-right corner of the
RStudio-application.

Before continuing, open this project if you did not do so.

#### Install `devtools`

First of all, make sure to have followed the steps outlined in “\[Setup
r-project with renv\]”, and that RStudio is currently in your created
project.

Installing this package from GitHub requires the installation of the
R-package `devtools`. `devtools` can be used to compile source-packages
to installed binaries.

``` r
install.packages("devtools")
```

When being asked to proceed, make sure that the installation-path is in
the R-project you created:

If you created your R-project with `renv` at

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

Once you have installed `devtools`, go on to the next step.

#### Install R-build-toolchain

The underlying package `duflor` contains C++ code. As it is distributed
as source-code, users will require the appropriate R-build-toolchain for
their respective operating system. For further information and
setup-instructions, refer to
“<https://r-pkgs.org/setup.html#setup-tools>”.

For windows, the R-package `installr` may be used to check if Rtools is
already installed, and whether or not it has been found by R.
Additionally, it will notify the user if a new R-update is available
(major and minor versions only, ignores patch versions):

``` r
install.packages("installr")
# once pkgbuild is installed, run:
installr::install.Rtools(check = T,check_r_update = T,GUI = T)
```

### Installation-steps

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

## Installation - from the command-line

### Prerequisites

- R\>=4.3.3 is installed on the system, and can be launched from the
  command-line
  - You can try executing `where R` or `which R` to see if R this
    prerequisite is met. Each command will print the path to the
    R-installation which is used from the command-line. If no path is
    found, R is not accessible from the command-line.
    - In this case, check how to make it accessible from the
      command-line for your respective system
- The underlying package `duflor` contains C++ code. As it is
  distributed as source-code, users will require the appropriate
  R-build-toolchain for their respective operating system. For further
  information and setup-instructions, refer to
  “<https://r-pkgs.org/setup.html#setup-tools>”.

### CLI-Installation-steps

To install the application, follow the steps below:

1.  open the terminal, and move to the location where you want to
    install the program
2.  create a folder to place the application in, e.g. `mkdir duflor_app`
3.  move into the folder via `cd duflor_app`
4.  launch R by executing `R`
5.  executing `1+1` should now successfully return `[1] 2`
6.  check if the package `renv` is installed by executing
    `system.file(package='renv')`
    - if it is not, install it by executing `install.packages("renv")`,
      then follow instructions for installing it/potential dependencies
7.  execute `renv::init()`
8.  repeat step 6) for the package `devtools`
9.  install `duflor.gui` by executing either:
    - release-version:
      `devtools::install_github("https://github.com/Claudius-Appel/duflor.gui@master")`
    - development-version:
      `devtools::install_github("https://github.com/Claudius-Appel/duflor.gui@dev")`
    - follow instructions for installing it/potential dependencies
10. once it is installed, try executing `duflor.gui::duflor_gui()` to
    see if the package was installed successfully
    - If it was, the GUI should open in the default-browser, and the
      application can be used from now on

### Installing help-vignettes

The application is documented in detail via pkgdown, and its site can be
reached from the github-repository.

Most of the articles outlined on the site may also be viewed as
vignettes from within RStudio. To do so, you must explicitly declare
vignettes to be installed when performing step 9 in
“[CLI-Installation-steps](#cli-installation-steps)”:

``` r
devtools::install_github("https://github.com/Claudius-Appel/duflor.gui@master",build_vignettes = T)
devtools::install_github("https://github.com/Claudius-Appel/duflor.gui@dev",build_vignettes = T)
```

Vignettes are rendered documents discussing specific topics within a
package. Installing them is recommended, but not required for utilizing
the package itself.

# Running the app from the command-line

After installation, the application can be launched as described below:

1.  `cd` into the folder in which you have installed the application in,
    e.g. `cd duflor_app`
2.  launch R via `R`
3.  launch the app via `duflor.gui::duflor_gui()`

# Running the app from within RStudio

1.  Follow the installation-steps outlined in “[Installation - from
    within RStudio](#installation---from-within-rstudio)”
2.  Open the project via the drop-down-menu in the top-right corner of
    the RStudio-application.
3.  Launch the app by executing `duflor.gui::duflor_gui()` in the
    console.

# Parallelisation requires installation of package, **not just loading**

In order for the shiny application to be able to run the analysis in
parallel, the packages `duflor` and `duflor.gui` **must** be installed.
If this package is developed locally, executing `devtools::load_all()`
will not be sufficient. To counteract this, make sure that both packages
are actually installed as such, and show up in the listed packages.

[^1]: This might sound confusing, as this is a R-package wrapping a
    shiny-application. However, those two options are for
    ***developing*** either of those, not for ***installing*** them.

[^2]: The name is not really important, but it should be distinct and
    indicate that this is the installed application

[^3]: If `renv` is not installed/must be updated, you should be prompted
    to install it here.
