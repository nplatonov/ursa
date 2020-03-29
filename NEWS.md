2020-03-29

### version 3.8.19

-   Minor adjustments for timing of examples.

### version 3.8.18

-   `spatial_write()`: fixed deprecated (**`sf`** &gt;= 0.9-0) argument
    `update`-&gt;`append` in suggested `sf::sf_write()`.

-   Re-check “don’t test” examples.

### version 3.8.17

-   Fixed for suggested package **`proj4`** (&gt;=1.0.10).

-   Tested on R-devel (r77878, r77936) and Rtools 4.0 (2020-02-05); no
    errors. For R-devel (r77936) suggested package **`raster`** should
    be reinstalled.

-   Added argument `title` in function `print()` for objects of class
    `ursaRaster` for optional header printing.

### version 3.8.16

-   C-level: `memcpy` is replaced by `memmove` for overlapped memory
    areas.

-   C-level: `fseek` is replaced by `fseeko/fseeko64` for positions in
    large files.

-   Added argument `retina` in functions for plotting. Not supported for
    tiles.

-   Package **`RColorBrewer`** is added as suggested for cases when
    `pal` attrubute in `colorize()` is equal to any palette name or
    category from **`RColorBrewer`**.

-   Fixed replace function `spatial_geometry<-` for Spatial (**`sp`**)
    objects.

-   The environental variable `PROJ_PATH` is temporal specified for GDAL
    utils for their execution, if GDAL is found (optional).

-   Coastline is updated to version 2020-01-09T05:30

### version 3.8.15

-   Fixed behaviour for using in rmarkdown for self-contained documents

-   Path to R is taken from `R.home("bin")`

### version 3.8.14

-   Fixed appeared “length &gt; 1 in coercion to logical” in examples
    during CRAN check.

-   Packages **`leaflet`**, **`leafem`**, **`leafpop`** are added as
    suggested for polar web mapping.

-   ‘http’ protocol is changed to ‘https’ one for `style=polarmap` in
    `glance()`

-   packages **`htmlwidgets`**, **`htmltools`** are added as suggested
    for development purpose without any next steps for involvement.

-   Added new argument `ref` to functions `band_blank()` and
    `ursa_blank()` for more flexible blank detection.

-   Changed interface to ‘mapnik’ (https instead of http) and
    ‘mapsurfer’ (Tokenized Openroute service) tile services.

-   Improved formatting of numeric labels in `legend_colorbar()`

-   Coastline is updated to version 2019-09-21T05:39.

### version 3.8.13

-   R function are used for reading of large binary files, if their size
    overflows ‘long’ capacity.

-   Duplicated band names are allowed now for `print()` of `ursa`
    objects.

-   Cancelled replacement “ESRI WKT” by “OGC WKT” format of projection
    file when writting “ESRI Shapefile”.

-   Connections (writting and simplified reading) are made public:
    `spatial_write` and `spatial_read` functions.

-   File extensions `.webp` (if **webp** is installed), `.jpg`, `.jpeg`
    can be specified for attribute `fileout` for saving image file,
    produced by `display`, `glance`, `compose_open`. Package **webp** is
    added as suggested.

-   Coastline is updated to version 2018-12-13 09:34.

### version 3.8.12

-   Default style for web basemaps is OSM Mapnik tiles insead of OSM
    static map due to
    [limitations](http://staticmap.openstreetmap.de/staticmap.php).

-   Parsing of metadata from object of package **stars** (&gt;=0.2).

-   Domain changed from “arcticconnect.org” to “arcticconnect.ca” for
    `style=polarmap` in `glance()`

-   Coastline is updated to version 2018-11-17 09:50.

### version 3.8.11

-   Package **tools** is added as *Suggested* for MD5 manipulations.

-   Selective improvement of compressed raster matrices in `c`,
    `display`. Still far for full support.

-   `obj[2] <- NULL` removes second band of raster object `obj`.

-   Package **rgeos** is added as *Suggested* for geometry operations
    with `sp` objects.

-   Added `spatial_intersection`, `spatial_symdifference`, and other
    functions for operations with geometries of spatial objects.

-   Added **Author@R** field in `DESCRIPTION`.

-   Changed behaviour for opening of visualization for `interactive()`
    session. Now, figures are opened outsize of GUI, by default. Use
    `session_pngviewer(FALSE)` for restoring of previous behaviour.

### version 3.8.10

-   Forced UTF-8 encoding for non-shapefiles for “sp” engine

-   PNG is removed in embedded Rscript by `file.remove()` after
    `Sys.which()`

-   Accepting `sf::st_set_geometry(x,NULL)` for extracting
    `spatial_data()` of `sf` objects

### version 3.8.9

-   Version 3.8.8 is removed from CRAN

-   Fixed opening ENVI .hdr Labelled Rasters for Read-only file systems

-   Fixed ignoring of `stdout` in `system2` for `interactive()`
    (appeared on Gui for Windows)

-   Added recipe to use visualization in **shiny**
    (`imageOutput`/`renderImage`)

### version 3.8.8

-   Initial submission to CRAN

-   Coastline is updated to version 2018-06-03 09:34

-   Package `IRdisplay` is added as ‘Suggested’.

-   In adopting to CRAN policy and reviews of CRAN members:

    -   Reduced time of examples, fixed some features.

    -   Changed cache management (after Uwe Ligges code review).

    -   In \*.Rd files ‘\\{dontrun}’ is replaced to ‘\\{donttest}’
        (after Swetlana Herbrandt review). Packages `caTools`, `ggmap`
        are added as ‘Suggested’ for running of examples.

### version 3.8.7

-   Support importing of 3-dimensional
    ‘[stars](https://github.com/r-spatial/stars)’ arrays

-   Improvement of character encoding for attributes of spatial objects

### version 3.8.6

-   Coastline is updated to version 2018-03-27 09:31

-   Corrected graticules and scalebar for ‘+proj=merc’ projection class
    for non zero latitude of true scale (e.g., ‘+lat\_ts=80.8’)

-   Package `fasterize` is suggested. Version of suggested package `sf`
    should be `>=0.6-1`.

### version 3.8.5

-   Imporing results of `sf::gdal_read`.

-   Increased contrasting in cubehelix for low-colored palettes.

-   Fixed cubehelix for colorizing with `breakvalue=0`

-   Corrected band names for opening rasters using GDAL

-   Performance improvement for Group Summary (comparisson) of
    `ursaRaster` class.

-   Coastline is updated 2018-03-07

### version 3.8.4

-   Added argument `coords` for function `allocate`.

-   New wrappers and checkers for spatial (vector GIS) objects.

-   Some issues for web services are fixed.

-   in DESCRIPTION file ‘exportPattern’ is replaced by multiple ‘export’
    for ability to use non-public funstions which names don’t start with
    “.”

-   In `panel_coastline` improved detection of polygons’ coodrinates
    spreading in result of reprojection

### version 3.8-3

-   Visualization output is included to R-markdown document and
    R-Jupyter code without additional controls. Currently, some outputs
    are not supported (e.g., bookdown::gitbook)

### version 3.8-2

-   Changed registration of native routines.

-   Package `knitr` is added as suggested.

-   In `spatial_coordinates()`: fixed return value for ‘POINT’ geometry
    for ‘sf’ engine.

-   In `glance()`: if all attributes are `NA`, then plot only geometry.

-   In `band_blank()` 1) fixed inaccuracy for values not in memory 2)
    added argument `verbose`.

-   In `cubehelix()` canceled auto brightness changing if `dark` and
    `light` are specified

-   In `read_envi` and `open_envi` added argument `cache` to use cache
    for compressed files.

### version 3.8-1

-   Internal land polygons (coastline) data are replaced from GSHHG to
    OSM. Function `update_coastline` is added to update coastline data
    personally.

-   Caching is introduced for downloaded files.

-   Changed structure of `inst` directory by adding sudirectories
    `requisite` with neccessary files and `optional` with secondary
    files and directories.

-   Added family of functions `spatial_xxxxxxx` to retrieve properties
    from non-raster spatial objects: simple features (package **sf**)
    and Spatial abstract class (package **sp**).

### version 3.7-19

-   Argument ‘attr’ is replaced to ‘field’ in internal functions
    `.spatialize` and `.glance`.

-   Argument ‘r’ is replaced to ‘rotate’ in public function `cubehelix`.

-   In coersion from “stack” to “brick”: if nodata values are the same,
    then nodata is assigned.

-   in `polygonize` added choice of “engine” by means applying functions
    from either **sp** or **sf** packages.

### version 3.7-18

-   Improved consistence beetween ‘dim’ interity in non-public
    `.regrid()`

-   Supporting categories in exporting to `data.frame`

-   Back to patch of failure with ‘rgdal’ of Unix build machine at
    r-forge

### version 3.7-17

-   Correction for bounding around 180 degree longitude

### version 3.7-16

-   minor improvement to spatial allocation of vector objects with
    crossing of 180 degree longitude

-   allocate(): slightly improvement for regular grid detection

-   background for future functionality

### version 3.7-15

-   added possibility of image annotation; argument ‘label’ in
    ‘panel\_annotation’ can be object of class ‘array’

-   fixed divergent coloring for (only) two values in ‘cubehelix()’

-   ‘ggmap’, ‘foreign’ are removed from the list of suggested packages;
    ‘ncdf4’ is added to the list of suggested packages.

### version 3.7-14

-   fixed export to Raster(Layer|Brick|Stack) with NA nodata

### version 3.7-13

-   gentle requirements to “chessboard” grid in ‘panel\_new()’

-   fix layout in ‘compose\_design()’ for images like strips

-   in suggestion, sf (&gt;= 0.5-0)

### version 3.7-12

-   Minor changes for geocoded glance()

### version 3.7-11

-   ‘nominatim’ geocoding for 180-longitute-objects is more correct, but
    traffic is higher

-   alternate geocoding service in the case of base one failure

### version 3.7-10

-   Adapation glance() for if argument “dsn” is “point” ‘c(lon,lat)’ and
    “boundary” ‘c(minx,miny,maxx,maxy)’

### version 3.7-9

-   Vectors without data table - fixed

### version 3.7-8

-   Bypass for ‘rgdal’ usage diring examples on r-forge UNIX building
    machine. E.g.: Error in dyn.load(file, DLLpath = DLLpath, …) :
    unable to load shared object
    ‘/home/rforge/lib/R/3.4/rgdal/libs/rgdal.so’: libgdal.so.1: cannot
    open shared object file: No such file or directory

### version 3.7-7

-   Better matching for floating-point coordinates

### version 3.7-6

-   Minor fixes for categories after resample

-   ‘glance()’ is recoded

### version 3.7-5

-   Adaptation for R-exts’ “5.4 Registering native routines” for
    R-3.4.0.

### version 3.7-4

-   Non-ascii for geocoding in ‘glance’

-   New function ‘get\_earthdata’ for retreiving MODIS mosaics.

-   Added package ‘jpeg’ in the category ‘Imported’.

### version 3.7-3

-   Non-ascii for geocoding in ‘glance’

### version 3.7-2

-   Introduce geocode to ‘glance’. There is no relation between data and
    geocoded place.

-   Introduce tiles to ‘glance’. Now static maps and tiles for basemap
    in “+proj=merc”

-   Dismiss from dQuote() and sQuote(), which put non-symmetrical quotes
    in Rgui; GDAL does’t understad it.

-   ‘inst/glance’ contains mock-up to create vector/raster file
    associantion with glance()

-   ‘glance’ can work without package ‘sf’; however “package:methods”
    should be in “search()”

-   Rename ‘panel\_gridline’ to ‘panel\_graticule’.

### version 3.7-1

-   Public wrapper ‘glance()’ for non-public ‘.glance()’: quick-look of
    GIS raster and vector data

### version 3.6-3

-   Documentation for ‘ursaProgressBar’

### version 3.6-2

-   Added argument “…” to function ‘read\_gdal’. Now, if ‘as.ursa(uri)’
    or ‘display(uri)’, then additional arguments can be passed to
    ‘download.file’. For example, if you need ‘mode=“wb”’ or ignore
    certificate for HTTPS

### version 3.6-1

-   Added ‘session\_pngviewer()’ and ‘session\_tempdir()’ to follow CRAN
    policy. If “Rscript”, then external software is used to open PNG;
    current directory is used to write files If ‘interactive()’ or “R
    CMD BATCH”, no external software for PNG; ‘tempdir()’ is used to
    write files

### version 3.5-2

-   Initial submission to R-Forge

<!--
<style>
   code,
   kbd,
   pre {
     font-family: Cousine, Consolas, Menlo, "Liberation Mono", Courier, monospace;
     font-weight: regular;
     line-height: 115%;
     font-size: 90%;
     background-color: #E6F1FF;
   }
</style>
-->
