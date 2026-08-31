# Scopus FLCA Explorer

A local Shiny application for exploring Scopus exports with follower-leading clustering analysis (FLCA). It prepares role-specific author metadata, builds Top-20 co-occurrence networks, and provides network, SSPlot, Sankey, chord, Kano, slope, country-map, and summary views.

## Features

- Load the bundled Scopus demo or upload a Scopus CSV (up to 50 MB).
- Automatically normalize authors, affiliations, dates, journals, keywords, and references after upload.
- Match author-name variants by Scopus Author ID when available; `FA_Author_ID` and `CA_Author_ID` are retained in the normalized metadata.
- Analyse first/corresponding-author pairs, countries, institutes, departments, author keywords, index keywords, and journals.
- Run FLCA with weighted (`Qw`) and unweighted (`Qu`) modularity, silhouette scores, reduced networks, Sankey, chord, Kano, and SSPlot views.
- Map FA/CA country counts with a log colour scale, so lower non-zero counts remain visible beside dominant countries.
- Extract cited-reference first authors and journals for co-word analysis and journal-by-year trends.
- Parse manually pasted Scopus references and produce an author/journal FLCA network.

## Requirements

- R 4.1 or later (the app uses the base R pipe, `|>`).
- R packages: `shiny`, `readr`, `dplyr`, `ggplot2`, `tibble`, `jsonlite`, and `curl`.

Optional packages enable extra plots:

- `circlize` for chord diagrams
- `maps` for the country map
- `cluster` for the package silhouette implementation

Install the required packages:

```r
install.packages(c("shiny", "readr", "dplyr", "ggplot2", "tibble", "jsonlite", "curl"))
install.packages(c("circlize", "maps", "cluster")) # optional visualisations
```

## Run locally

Clone or download this repository, then start the app from the project directory:

```r
shiny::runApp()
```

## Use the app

1. Select **Run supplied Scopus demo** or upload a Scopus CSV. Uploading automatically normalizes the data and precomputes the main FLCA results with staged progress.
2. Open **FLCA Process** to inspect author, country, institute, department, keyword, index-keyword, and journal results.
3. For cited-reference analysis, select **Build cited-reference analysis**. Each cited work contributes only its first author and journal as co-word terms.
4. Use **Reference journal trends** for cited journals grouped by cited publication year.
5. Use **Scopus references** to paste one reference per line for a separate author/journal analysis.

The **Build co-occurrence** button recomputes the cached FLCA results for the currently loaded dataset.

## Input data

The app maps common Scopus headers automatically. The most useful columns are:

- `Author full names` (recommended, because it includes Scopus Author IDs)
- `Authors with affiliations`
- `Correspondence Address`
- `Affiliations`
- `Year`
- `Source title` or `Abbreviated Source Title`
- `Author Keywords`
- `Index Keywords`
- `References`
- `Cited by`
- `Document Type`

The included example is at `demo_data/scopus_export_Aug_31_2026.csv`.

## Interpreting counts and FLCA values

`NodeCount` is the original number of records containing an entity. In SSPlot, the first number (`Count`) is kept equal to this original count. The second number (`Edge`) is the FLCA Top-20 edge strength after sampling, so it answers a different question and can differ from the node count.

`Qw` is weighted modularity and `Qu` is unweighted modularity. If every displayed node is assigned to one cluster, both modularity values are mathematically zero.

## Project files

- `app.R` — Shiny application
- `flca_ms_sil_module.R` — FLCA calculation and Top-20 sampling module
- `renderSSplot.R` — SSPlot renderer
- `kano.R` — Kano plot renderer
- `demo_data/` — bundled Scopus-format example data

## Notes

- The app runs locally and analyses uploaded data in the current R session.
- Crossref-derived references are retained as structured flattened fields so journals can be extracted even without volume or issue metadata.
- No license file is currently included; add one before distributing or reusing this repository under a specific license.
