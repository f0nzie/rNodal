## 20170709 0.0.2.9000
* rename VLP scripts to author names separated by dashes.
* move functions in dunsros.R to own script.
* move any gas fucntion to gas_correlations.R
* rename FLUIDPROPS.R to oil_correlations.R
* move functions in zfactor.R to gas_correlations.R
* remove data.R. move its functions to rNodal-package.r
* no Rd for .saveSlot function

## 20170709 0.0.2
* document few functions
* ignore folder notebooks


## 20170709 0.0.1.9003
* use Hall-Yarborough from zFactor
* remove old HY built-in in this package
* new datasets in testthat regenerated with new values of z with zFactor
* add more tests to test_zfactor.R
* use zFactor::HallYarborough directly in the tests supplying sequences of Ppr and Tpr that are in a reasonable range of oil wells.
* using zFactor::HallYarborough at low and high Ppr, Tpr
* create a rda file for test check zFactor with returning lists of z, pres.pr, temp.pr and temp.r



## 20170709 0.0.1.9002
* add links to h5 files in vignettes
* h5 temp file is the same for all vignettes VLP runs
* VLP tables are added to h5 in temp file unless R session is restarted
* add word test to all test files
* add new test for all VLP runs that were in vignettes. 
* each VLP test now has its own rda file for test check
* add verbose to functions VLPControl and hdf5 creation function
* rename surf.params to well.params

## 20170707 0.0.1.9001
* change source of notebooks to ../R
* re-running vignettes
* remove html and R from vignettes
* rename one vignette
* add title to index vignette


## 20170706 0.0.1.9000
* all tests running ok
* move folders from inst to notebooks
* improve strategy


## 20170706 0.0.1
* keeping 9012 as a base
* changes to z return values were discarded; causing problems

## 20170611 9012
* fix two tests in test_nodal_status.R because of a change of folder in rNodal.

## 20170611 9011
* add new file STRATEGY.md

## 20170611 9010
* changes in z factor
* new equation for z.
* strategy to extract single values instead of list

## 20170605 9008
* FIX problem with interpolation.R. It was renamed but this causes conflicts 
with existing file in the repository because the old file INTERPOLATION.R is not
removed or renamed. Fixed by moving `interpolation.R` to another folder and then
bringing it back.
* Next time, instead of renaming a file try `Save As`.
* Tag 0.0.0.9007, 9008 deleted.


## 20170605 9006
* complete low level tests adding test_that
* tried to use subfolders under testthat bu was not succesful
* rename tests to be able to group them later. Low level tests now have a tag LL.
* change name and references of file INTERPOLATION.R to lowercase.
* rename data file used in tests. Start with `data`.


## 20170604 0.0.0.9003
* running OK
* rename get_extdataDir
* add getDefaultDataContainerName
* new test is_checking_package
* remove warnings in settings.R


## 20170604 0.0.0.9002
* Implemented a way to capture if we are in build package mode using a new function is_checking_package.
* added a bunch of small tests for testing paths.
* change logical function to something like this: is_xxx_yyy(). Example: 
    * is_checking_package
    * is_saved_session
* can copy a HDF5 data container in "local" mode. Similar to "project". 
* can check if the user has already a data comtainer so it is not overwritten,


## 20170531 `rPetroleum` package
* Found problem with rhdf5 which did not allow to build the package. Problem is with lack of declaration as S3 method. Found solution in forums. Issue is known. Fix is to manually edit NAMESPACE and add an `import(rhdf5)`. 
* For the moment using new functions to create temporary HDF5 files with `setHDF5DumpFile` and `getHDF5DumpFile`. This creates and saves the `h5` file somewhere under the user folder. Later will have to implement a way to copy or of creating the first `h5` file in the user folder. `./inst/extdata` perhaps?
* Two vignettes running. Examples `C13` and `C44`.
* Only three precarious unit tests. Need to test more and improve the coverage.
* No sub folders under `inst` yet.
* New scripts under `./R`: 
  * `settings.R `
  * `hdf5.R`
