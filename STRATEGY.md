## z-factor
* functions that have dots could return a list.
* maybe better to name the functions that return a list like this: `theFunctionL`, where `L` represents the list.
* the core function should a have a dot . like in .z.HallYarborough()
* The core functions are called by functions that do more complex activities like sapply, lapply or create matrices from the core.

## functions with dots
* these functions are hidden and are special. some may be core functions.


## The major blocks of rNodal
* PVT
    * the oil properties
    * gas properties
    * water properties

* the tubing at-depth iterator (marching algorithm)
* the VLP correlations
* the IPR correlations

* plotting modules (ggplot2, base)
* statistical modules
    * interpolation
    * data.table, dataframes

* compressibility factor, sweet hydrocarbon gases (zFactor)

* utilities
    * the hierarchical structure storage (hdf5)
    * list.names: makes unnecssary to type the variable twice
    * matrix conversion

    

