Mildew
======

Setup
-----
First, install [R-INLA](http://www.r-inla.org/) testing version with the command
```r
source("http://www.math.ntnu.no/inla/givemeINLA-testing.R")
```
Then run Mildew setup script with the command
```r
source_url("https://raw.github.com/statguy/R-Mildew/master/inst/process/setup.R")
```
Be sure you have the `devtools` package installed first.

Code files
----------
In the subdirectory `process` of the Mildew package installation directory
(which is shown by the command `path.package("Mildew")` after loading
the `Mildew` package), you can find the following files to
preprocess, estimate and report results for the mildew data:
* `preprocess.R`
contains high-level code to preprocess the data so that it is useful for the analysis.
* `estimate.R`
contains high-level code to estimate all models.
* `reports.R`
contains high-level code to load all results and print reports.

Configuration
-------------
Set `basePath` in to point your mildew data directory, for example
```r
basePath <- "~/mildew"
```
and set `runParallel <- TRUE` for parallel processing.
In case you have trouble with the parallel processing, disable it by
`runParallel <- FALSE`.

Imputation and estimation can both take advantage of high performance cluster
if available. If you can access one, take a look at
[R-Cluster](https://github.com/statguy/R-Cluster) for configuring it.

Preprocessing, estimation, reporting
------------------------------------
You may run the code directly with
```r
source(file.path(path.package("Mildew"), "process", "preprocess.R"))
source(file.path(path.package("Mildew"), "process", "estimate.R"))
source(file.path(path.package("Mildew"), "process", "reports.R"))
```

With the current configuration, estimation may take up to 1 day per model in a powerful system.
You may want to construct a mesh with less nodes by adjusting the mesh parameters
`occ.mesh.params`, `col.mesh.params`, `ext.mesh.params` in `estimate.R` or use
a subset of the data for testing. Use the `plotMesh` method, e.g.
```r
occ$plotMesh()
```
to plot the mesh.

Extensions
----------
Extending the current code can be done by inheriting the classes defined in
`https://raw.github.com/statguy/R-Mildew/master/R/classes.R`, which contains low-level code to
load the mildew data, set up models, estimate models with INLA, save results and print results.
See more info about the references classes with the R command
```r
?ReferenceClasses
```
You may want to consider [forking](https://help.github.com/articles/fork-a-repo) the repository for your own use first.

TODO
----
* Plotting

Feedback
--------
Send any feedback to [`jvj@iki.fi`](mailto:jvj@iki.fi).
