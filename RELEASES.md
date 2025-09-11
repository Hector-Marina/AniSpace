---
editor_options: 
  markdown: 
    wrap: 72
---

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
