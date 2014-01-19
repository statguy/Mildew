R-Mildew
========

Setup
-----
Download and install `git` from [http://git-scm.com/downloads](http://git-scm.com/downloads) to your system.
Create directory `git` and in it, run command
```
git clone https://github.com/statguy/R-Mildew.git
```
Now you should have directory `R-Mildew` under `git` with all the files.
You may open the project file in [RStudio](http://www.rstudio.com/).

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
source("~/git/R-Mildew/classes.R")
```
You need to download and save ```Cluster.R``` to your system first from the above address.
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
