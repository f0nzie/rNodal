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
