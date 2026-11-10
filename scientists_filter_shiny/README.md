# Scientist filter Shiny app

This folder is ready to run or deploy as a Shiny app. It contains `app.R` and
two compact 2025 datasets in `data/`:

- `career_2025.csv.gz` was prepared from `career.xlsx` (`nc9625`, `h25`, `hm25`).
- `singleyr_2025.csv.gz` was prepared from `career (2).xlsx` (`nc2525`, `h25`, `hm25`).

The older `singleyr.xlsx` contains 2023 metrics and is not used by this app.

## Run locally

In RStudio, install packages once if needed:

```r
install.packages(c("shiny", "DT", "rsconnect"))
```

Then run:

```r
shiny::runApp("F:/submissionTO/2510top2%scientistsinscopus/scientists_filter_shiny")
```

Run the folder with `runApp()` or RStudio's **Run App** button. Do not paste
`app.R` into the console line by line. The app can also locate its data files
when run from another local working directory.

## Deploy to shinyapps.io

After connecting your shinyapps.io account in RStudio, publish this folder, or
run:

```r
rsconnect::deployApp("F:/submissionTO/2510top2%scientistsinscopus/scientists_filter_shiny")
```

Deploy this app folder, not the parent research folder. The bundled data files
use relative paths, so the original Excel workbooks are not needed on the host.

## Filters and downloads

The three terms are discipline (1), nation (2), and institute name (3).
`|` means AND and `/` means OR. Both operators can be used inside a term; the
field expression (default `1|2|3`) combines the three terms. The initial
values are `Gastroenterology & Hepatology`, `twn / chn / jpn / kor`, and
`Hospital / Medical Center`. The institute term accepts either phrase. A blank
term is skipped; for example, a blank discipline makes the expression `2|3`.
The initial dataset option is Both. Its default results include 33 scientists,
with country totals shown above the tables (twn 4, chn 4, jpn 22, kor 3).

The app displays filtered scientists and a count table with institute rows,
country columns, a final Total column, and a final Total row. Institute labels
include their country code, and institutes are sorted by Total count from high
to low. A Nation label is the last column, after Total. The scientists stay in
source workbook order. Each table has a CSV
download. For the Both option, each dataset is filtered separately. Only names
found in both filtered datasets are retained, once per name, using the career
row and its metrics. The `metric_source` column identifies that metric window.
Scientists stay in career workbook order.
