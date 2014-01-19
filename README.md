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
If you experience problems with parallel processing, set
```r
runParallel <- FALSE
```

Estimation
----------

With the current configuration, estimation may take up to 1 day per model in a powerful system.
You may want to construct a mesh with less nodes by adjusting the mesh parameters
`occ.mesh.params, col.mesh.params, ext.mesh.params` in `estimate.R` or use
a subset of the data for testing. Use the `plotMesh` method, e.g.
```r
occ$plotMesh()
```
to plot the mesh.

Extensions
----------

Extending the current code can be done by inheriting the classes. See more info with the `R` command
```r
?ReferenceClasses
```
You may want to consider [forking](https://help.github.com/articles/fork-a-repo) the repository for your own use first.

Todo
----
* Imputation
* Plotting

Bugs
----
Send bug reports to [`jvj@iki.fi`](mailto:jvj@iki.fi).
