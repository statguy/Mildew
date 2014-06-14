Mildew
======

This is the supplementary R code for the paper 

[**Jousimo J, Tack AJM, Ovaskainen O, Mononen T, Susi H, Tollenaere C, Laine A-L.
Ecological and evolutionary effects of fragmentation on infectious disease dynamics. Science 344 (6189): 1289-1293**]
(http://www.sciencemag.org/content/344/6189/1289.abstract).

to preprocess the data, estimate the spatio-temporal models for each response and plot the results.

Setup
-----
Install [R-INLA](http://www.r-inla.org/) testing version with the [R](http://www.r-project.org/) command
```r
source("http://www.math.ntnu.no/inla/givemeINLA-testing.R")
```
Then run Mildew setup script with the command
```r
source_url("https://raw.github.com/statguy/R-Mildew/master/inst/process/setup.R")
```
Be sure you have the `devtools` package installed first.

Optional: For parallel processing on a HPC, there is a [Python script](https://github.com/statguy/Parallel-R-SSH).
However, it needs to be configured for your local system first.

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

Preprocessing, estimation, reporting
------------------------------------
You may run the code from command line at the installation root directory with
```sh
R --vanilla --args 1 < inst/process/preprocess.R
R --vanilla --args 11 < inst/process/estimate.R
R --vanilla < inst/process/reports.R
```
where the number is a task id. For preprocess, there are three tasks, one for each response.
Task id 1 must be run first as the results are used for the other responses.
For estimate, there are 6 * 3 tasks, so each response has six models and the task ids run
as 11, 12, 13, 21, 22, 23,..., 61, 62, 63.

With the current configuration, estimation may take up to 1 day per model depending on your setup.
You may want to construct a mesh with less nodes by adjusting the mesh parameters
`occ.mesh.params`, `col.mesh.params`, `ext.mesh.params` in `estimate.R` or use
a subset of the data for testing. Use the `plotMesh` method, e.g.
```r
occ$plotMesh()
```
to plot the mesh.

Feedback
--------
Send any feedback to [`jvj@iki.fi`](mailto:jvj@iki.fi).
