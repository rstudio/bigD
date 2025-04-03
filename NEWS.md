# bigD 0.3.1

* Introduced performance improvements (@olivroy, #15, #16, #17).

  * Internal functions `dt_MM()`, `dt_yyy()` and friends are no longer used, as we
    use a new internal structure to avoid duplicate calculations

# bigD 0.3.0

* The locale's territory is now better resolved for week in month calculations.

* Corrected the formatting of localized GMT formats.

* Depend on R >= 3.3 to use `strrep()` and `startsWith()`

# bigD 0.2.0

* New package with everything you need to format dates and times.
