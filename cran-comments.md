## Test environments
* local OS X install, R 3.5.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were three NOTEs

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Justin de Benedictis-Kessner <justindbk@gmail.com>'
New submission
Package was archived on CRAN

* checking examples ...
** running examples for arch 'i386' ... [61s] NOTE
Examples with CPU or elapsed time > 10s
              user system elapsed
Tstopslookup 35.58   0.78   59.41
** running examples for arch 'x64' ... [64s] NOTE
Examples with CPU or elapsed time > 10s
              user system elapsed
Tstopslookup 39.01   0.31   62.37

These two notes occur because this function (Tstopslookup) must query the API several times to return results, which takes a variable amount of time depending on server availability.


## Downstream dependencies
There are currently no downstream dependencies for this package
