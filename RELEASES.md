## GitHub release v1.0.0

This release marks the **first stable and complete version of AniSpace**.
Version 1.0.0 provides an integrated framework for the processing, analysis, 
and visualisation of high-resolution spatio-temporal animal position data.

The package now supports complete workflows from data import and
pre-processing to the analysis of individual movement, animal–animal
spatial interactions, animal–environment relationships, utilisation
distributions, and dyadic interaction models.

#### 🚀 NEW FEATURES

-   **Extended `AniSpace` object structure:** the `AniSpace` S4 class now
    integrates individual information, position data, environmental or
    facility areas, spatial-interaction results, utilisation
    distributions, and utilisation-distribution similarities within a
    common object.

-   **Expanded data import and integration (💾):**
    -   `load.Space()`, `load.Info()`, and `load.Area()` support the
        construction of `AniSpace` objects from information already
        available in R.
    -   `read.Space()` supports position data stored in CSV and JSON
        formats.
    -   Individual metadata and environmental-area information can be
        incorporated into the same analytical object.

-   **Improved S4 integration:**
    -   Standard `plot()` methods are now available for `AniSpace`
        objects.
    -   Standard `as.data.frame()` and `as.list()` methods provide
        convenient export of information stored in `AniSpace` objects.
    -   `show()` provides a concise overview of the information and
        analyses stored within an object.

-   **Area-use analysis (️🌍):**
    -   `areaUse()` quantifies individual use of predefined spatial
        areas, including time budgets and area entry and exit events.
    -   `square2poly()` converts rectangular area definitions into
        polygon structures compatible with `AniSpace`.

-   **Trajectory processing and data-quality tools (📏):**
    -   `filterAniSpace()` provides flexible filtering of individuals,
        time periods, and spatial areas while maintaining object
        consistency.
    -   `spikes()` identifies anomalous movements from speed thresholds
        and allows these positions to be filtered or smoothed.
    -   `smooth()` applies running-median smoothing to individual
        movement trajectories.
    -   `interpolate()` provides several methods for reconstructing
        missing positions, including previous-position, linear, spline,
        and modified Akima interpolation.

-   **Movement and behavioural analysis (📐):**
    -   `stats.Mov()` estimates step distance, speed, and turning angle
        for each individual.
    -   Optional k-means clustering allows movement observations to be
        classified into behavioural clusters and summarised within
        predefined areas.

-   **Expanded descriptive statistics (📊):**
    -   `stats.Pos()` summarises individual temporal and spatial
        position information.
    -   `stats.Area()` describes the size, occupation, and density of
        predefined spatial areas.
    -   `stats.Still()` estimates positioning-system accuracy using
        stand-still reference devices.

-   **Spatial interaction analysis (🐄🐄):**
    -   `spatialInt()` estimates pairwise spatial interactions among
        individuals using distance- and time-based definitions.
    -   `plotSI()` provides matrix- and network-based visualisations of
        the resulting spatial-interaction structure.

-   **Utilisation-distribution analysis (🪢):**
    -   `areaUD()` estimates individual space use using grid-based,
        polygon-based, or kernel home-range approaches.
    -   `UDsim()` quantifies pairwise similarity in individual
        utilisation distributions using binary or continuous
        similarity measures.
    -   `plotUD()` visualises stored utilisation distributions and
        kernel home-range contours.
    -   `stats.UD()` provides descriptive statistics for grid,
        polygon, and kernel-based utilisation distributions.

-   **Dyadic modelling of spatial interactions (📈):**
    -   `fitERGM()` provides tools for evaluating associations between
        pairwise spatial interactions and individual- or dyad-level
        characteristics.

#### 📚 DOCUMENTATION

Version 1.0.0 includes an expanded example dataset and updated
documentation demonstrating the main `AniSpace` workflow, including:

1.  importing position, individual, and area information;
2.  cleaning and processing animal trajectories;
3.  estimating movement characteristics;
4.  quantifying spatial interactions;
5.  estimating area use and utilisation distributions;
6.  comparing individual space-use patterns; and
7.  modelling dyadic spatial interactions.

This release establishes the public API and analytical framework for the
**AniSpace 1.0.0** series.

------------------------------------------------------------------------

## GitHub release v0.0.2

This second release builds upon the foundation established in v0.0.1,
focusing on data integration, export flexibility, and expanded spatial
usage tools.

#### 🚀 NEW FEATURES

-   `merge()` method: enables seamless merging of multiple AniSpace
    objects (e.g., multiple days or datasets). It maintains internal
    consistency and metadata integrity across merged objects.

-   Data export functions for AniSpace objects: `Ani2list()` and
    `Ani2tbl()`, allow conversion of AniSpace objects to standard R
    types.

-   Data Import (JSON Support): `read.Space()` now supports input in
    JSON format, expanding interoperability with web and database
    systems.

-   Spatial Usage Analysis: `areause.AniSpace()` is a new tool for
    quantifying area use (e.g., cubicle occupancy) from position data.
    It estimates individuals' time budgets and the time of entry and
    exit when an animal uses the areas.

------------------------------------------------------------------------

## GitHub release v0.0.1

This first release was prepared to coincide with the execution of our
bi-annual **PhD Animal Movement course** (🐄🦌). It provides the
foundation of the package, covering the main data handling and
introductory analytical modules.

-   Data handling and import (💾):
    -   Support for reading from files and in-memory objects using the
        functions `read.\*()` and `load.\*()`.
    -   Plotting utilities for visualising positional and spatial data.
-   Descriptive statistics tools (📈):
    -   Descriptive statistics for position information.
    -   Descriptive statistics for individuals' information.
    -   Descriptive statistics for area information.
    -   Stand-still analysis for evaluating system precision.
-   Scaling tools (📏):
    -   Interpolation methods for positional data.
-   Spatial analysis tools (📐):
    -   Movement analysis functions (turning angles, speed, step
        length,...)
    -   Spatial interaction functions.

📚 Documentation

This release also includes a tutorial and example dataset (🐄)
demonstrating core package functionality.
