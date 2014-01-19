R-Mildew
========

Setup
-----
Set up [Git](http://git-scm.com/) version control system by following the instructions
[here](http://www.rstudio.com/ide/docs/version_control/overview).
Create a new project in [RStudio](http://www.rstudio.com/) as explained in the instructions
with the Git repository URL `https://github.com/statguy/R-Mildew.git`.
You can update the project to the latest version from the RStudio menu Tools &rarr; Version
Control &rarr; Pull Branches.
Alternatively, you can just download the `*.R` files from the repository URL to your local system.

Code files
----------
* `classes.R`
contains code to load mildew data, set up models, estimate models with [`INLA`](http://www.r-inla.org/), save results and print results.
* `estimate.R`
contains high level code to estimate all models.
* `reports.R`
contains high level code to load all results and print reports.

Configuration
-------------
Set `basePath` in `estimate.R` and `reports.R` to point to your mildew data directory.
In case you want to `source` the files from your local disk, then change the lines
```r
source_url("https://raw.github.com/statguy/RSnippets/master/Cluster/Cluster.R")
source_url("https://raw.github.com/statguy/R-Mildew/master/classes.R")
```
to point to the local files, e.g.
```r
source("Cluster.R")
source("classes.R")
```
Beside cloning the R-Mildew Git repository, you also need to download ```Cluster.R``` to your system from the above address.
If you experience problems with parallel processing, set
```r
runParallel <- FALSE
``` 

Todo
----
* Imputation
* Plotting

Bugs
----
Send bug reports to [`jvj@iki.fi`](mailto:jvj@iki.fi).
