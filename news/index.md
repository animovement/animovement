# Changelog

## animovement 0.8.0 (2026-08-28)

### Changed

- The minimum `anicore` is 0.8.0, the first version published under that
  name — the metapackage depended on `anicore` with no version
  constraint, so nothing recorded that a pre-rename `aniframe` will not
  do.

- `aniframe` is now `anicore`. The core package was renamed, so the set
  that [`library(animovement)`](https://animovement.dev) attaches,
  [`animovement_packages()`](https://animovement.dev/animovement/reference/animovement_packages.md),
  and the `.animovement` project file all name `anicore` instead.

- Removed the unused `cyan()`, `lightblue()` and `yellow()` styling
  helpers, left over from the fastverse adaptation
  ([\#159](https://github.com/animovement/animovement/issues/159)).

### Fixed

- The stray `LICENSE` file is removed and `AGENTS.md` is kept out of the
  built package, clearing the two notes `R CMD check` reported.
  `LICENSE` held a `YEAR:`/`COPYRIGHT HOLDER:` template, the form R uses
  for `MIT + file LICENSE`; it was left behind when 0.7.4 corrected the
  declaration to `GPL-3`, whose full text is `LICENSE.md`.

- [`print()`](https://rdrr.io/r/base/print.html) on an
  `animovement_conflicts` object returns the object invisibly, as an S3
  print method should, instead of `NULL`
  ([\#159](https://github.com/animovement/animovement/issues/159)).

## animovement 0.7.4 (2026-08-19)

### Changed

- The licence declaration is corrected from `GPL-3 + file LICENSE` to
  `GPL-3`, with the full text as `LICENSE.md`. The `+ file LICENSE` form
  asserts additional restrictions beyond GPL-3, and the file held
  nothing but the stock GPL-3 text. The licence that applies is
  unchanged; only how it is declared. `CITATION.cff` said `MIT`, left
  over from before the metapackage was rebuilt on the fastverse pattern,
  and now agrees at `GPL-3.0-only`.
- Sebastian Krantz (fastverse) and Hadley Wickham (tidyverse) are
  credited as contributors, for the package management code adapted from
  the fastverse.
- [`animovement_update()`](https://animovement.dev/animovement/reference/animovement_update.md),
  [`animovement_install()`](https://animovement.dev/animovement/reference/animovement_install.md)
  and
  [`animovement_deps()`](https://animovement.dev/animovement/reference/animovement_deps.md)
  look in the animovement r-universe by default, via the new
  [`animovement_repos()`](https://animovement.dev/animovement/reference/animovement_repos.md),
  instead of failing unless `repos` was set by hand
  ([\#144](https://github.com/animovement/animovement/issues/144)).
- Installation under webR goes through `webr::install()` for every
  install function, not just
  [`animovement_install_suggested()`](https://animovement.dev/animovement/reference/animovement_install_suggested.md)
  ([\#139](https://github.com/animovement/animovement/issues/139)).
- Suggested packages no longer include development tooling, or packages
  already required by the suite, cutting the set from 21 to 8
  ([\#143](https://github.com/animovement/animovement/issues/143)).
- Documentation is scoped to the meta-package itself. The ecosystem
  tutorials have moved to [animovement.dev](https://animovement.dev);
  the README and the introductory vignette cover attaching the suite,
  conflicts, `.animovement` project configuration, extending, updating
  and suggested packages
  ([\#136](https://github.com/animovement/animovement/issues/136),
  [\#148](https://github.com/animovement/animovement/issues/148)).

### Removed

- `rhdf5` and the Bioconductor r-universe from `Suggests` — needed only
  by the tutorials that have moved.
  [`animovement_install_suggested()`](https://animovement.dev/animovement/reference/animovement_install_suggested.md)
  still resolves it for the ecosystem packages that suggest it.

### Fixed

- [`animovement_install_suggested()`](https://animovement.dev/animovement/reference/animovement_install_suggested.md)
  works again: it no longer passes an unsupported `repos` argument to
  [`pak::pkg_install()`](https://pak.r-lib.org/reference/pkg_install.html),
  and finds `rhdf5` on the Bioconductor r-universe
  ([\#146](https://github.com/animovement/animovement/issues/146)).
- The DOI badge uses the static Zenodo badge rather than the
  repository-ID one, which answered a redirect before serving anything
  and often failed to render.
- The R-universe and Zulip badges in the README, which Quarto was
  rewriting with a spurious `.png` extension.

## animovement 0.7.3

### Added

- `here` and `signal` to `Suggests`, and `circular` for
  `summarise_aniframe()`.
- webR / `r-wasm` support, so the package can run in the browser.

### Changed

- Suggested packages can be installed from the r-universe and
  Bioconductor r-universe mirrors.
- Articles converted to Quarto, trackball articles modernised to the
  current API, and shared pkgdown theming inherited from
  `animovementtemplate`.

## animovement 0.7.2

### Added

- `anispace` joins the bundled suite, bringing spatial transformations
  into animovement.

## animovement 0.7.1

### Added

- [`animovement_install_suggested()`](https://animovement.dev/animovement/reference/animovement_install_suggested.md),
  to install the optional packages some functions use.

### Changed

- CI workflows, README and citation metadata updated.

## animovement 0.7.0

**animovement is now a metapackage.** The codebase is split into a suite
of focused packages that animovement bundles and re-exports: `aniframe`
(the shared data class), `aniread` (readers), `aniprocess` (cleaning and
filtering), `anicheck` (quality control), `animetric` (metrics) and
`anivis` (visualisation).

### Added

- The `ani_df` class, with accompanying improvements and tests.

### Changed

- Continued metadata improvements.
- `frame rate` is replaced by `sampling rate` throughout.

### Removed

- The `classify_*()` functions.

## animovement 0.6.0

A large feature release, focused on filtering, calculation and movement
classification.

### Added

- Kalman filters, and improved bandwidth filters for smoothing.
- `replace_na()` and the NA-handling family, and
  `classify_low_periods()`.
- Coordinate rotation and egocentric transformation, with centroid and
  coordinate transformations.
- Peak and trough detection, and timeseries alignment, with improved
  detection of active periods.
- Kinematics calculations.
- `set_individual()` and `set_framerate()`, and a `return_type` argument
  on several functions.

### Fixed

- `filter_by_speed()`.

## animovement 0.5.1

### Changed

- `smooth_track()` is renamed `smooth_movement()` and now smooths any
  movement data, not only trackball data. It gains `use_derivatives`
  (default `FALSE`), which is how trackball data should be smoothed; the
  non-derivative path is new.

## animovement 0.5.0

Three major changes: a data format stabilised across every reader, the
first metadata, and tests to hold both in place.

### Added

- Metadata on the returned data frames. Experimental, but the start of
  the contract that became aniframe.
- Tests that reader output always carries the expected columns and
  column classes, and that metadata survives a read.

### Changed

- The data format is settled and implemented for all readers.

## animovement 0.4.1

### Changed

- A parameter in `read_animalta()`.

## animovement 0.4.0

### Added

- `read_animalta()` for AnimalTA, and `read_idtracker()` for
  idtracker.ai.

## animovement 0.3.0

### Added

- `read_bonsai()` reads centroid tracking from Bonsai files.

## animovement 0.2.0

**The package is renamed from `trackballr` to `animovement`**,
reflecting a scope that now covers movement data generally rather than
trackballs alone. The rename brings a great deal of new functionality
and a correspondingly large set of breaking changes.

For the former `trackballr` functionality, install from source from the
[repository history](https://github.com/roaldarbol/animovement).

### Added

- Readers for pose-estimation output: `read_deeplabcut()`,
  `read_sleap()`, `read_trex()` and others.
- `smooth_tracks()`, for standardised data cleaning.
- Substantially more tests, and much improved documentation.

### Changed

- The package name, and with it every function that carried the
  trackball-specific naming.

## animovement 0.1.2 (as trackballr)

### Added

- Tests for `read_trackball_data()`.

### Deprecated

- The `format` parameter of `read_trackball_data()`.

## animovement 0.1.1 (as trackballr)

### Added

- Sample data, used in the README example.

### Changed

- `configuration` in `read_trackball_data()` becomes `fixed` / `free`.

## animovement 0.1.0 (as trackballr)

First upload to GitHub.
