## z-factor
* rNodal from now on will use compressibility factors calculated in zFactor.
* functions that have a dot as prefix could return a list.
* if the z factor function returns a list it must be explictely declared as an argument. Example: z.hallyarborough(..., as_list = TRUE)
* the core function should a have a dot . like in .z.HallYarborough(). The core function performs the Newton-Raphson root finding on the function and its derivative.
* The core functions are called by high-level functions that do more complex activities like sapply, lapply or create matrices calling the core function.


## functions with dots
* these functions are hidden and are special. some may be core functions.


## The major blocks of rNodal
* PVT
    * the oil properties
    * gas properties
    * water properties

* the tubing at-depth iterator (marching algorithm). class
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

    

## TODO
* add z factor for sour gases. now using a temporariry weak function.
* convert VLP control to object-oriented with S4
* Move Duns-Ros functions to its own script
* finish Fancher-Brown
* finish Poettman-Carpenter
* finish Baxendell-Thomas
* finish Orkiziewski
* compare experimental VLP functions
* continue with PVT correlations
* perform comparison with PVT correlations
* start Aziz mechanistic
