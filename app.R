# Journal metadata and FLCA explorer.


`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

# MDPI journal metadata and FLCA explorer — reconstructed application shell.
required <- c("shiny", "readr", "dplyr", "ggplot2", "tibble", "jsonlite", "curl")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "), call. = FALSE)
suppressPackageStartupMessages({ library(shiny); library(readr); library(dplyr); library(ggplot2); library(tibble) })
# Scopus exports commonly exceed Shiny's default 5 MB request limit.
options(shiny.maxRequestSize = 50 * 1024^2)

metadata_path <- "Metrics_MDPI_metadata.csv"
keyword_cache_path <- "Metrics_DOI_author_keywords.csv"
demo_scopus_path <- file.path("demo_data", "scopus_export_Aug_31_2026.csv")
default_dois <- tryCatch({
  cache_dois <- readr::read_csv(keyword_cache_path, show_col_types = FALSE)
  names(cache_dois) <- sub("^\\ufeff", "", names(cache_dois))
  doi_name <- intersect(c("DOI", "doi"), names(cache_dois))[1]
  if (length(doi_name) && !is.na(doi_name)) paste(unique(na.omit(cache_dois[[doi_name]])), collapse = "\n") else ""
}, error = function(e) "")
default_article_content <- paste(c(
  "Abstract", "General-purpose bibliometric tools do not address domain-specific needs of occupational safety and health (OSH) evidence mapping, such as quantifying whether the literature connects hazard characterisation with real worker exposure or identifying which articles best bridge science and preventive practice. This paper presents ORISMA (Occupational Risk Integrated Systematic Mapping and Analysis), an open-source R package that introduces five preventive bibliometric indicators: the Worker-Risk Disconnection Index (WRDI), Risk Category Saturation Index (RCS), Material-Gap Profile (MGP), Abstract Sufficiency Score (ASS), and Bridge Article Score (BAS). A complete evidence map is produced through three function calls. To demonstrate the package, we apply it to a corpus of bibliographic records on occupational risks in metal additive manufacturing (metal AM). The global WRDI was 0.4474, indicating moderate disconnection between hazard characterisation and worker exposure evidence. Industrial hygiene and emerging technology domains dominated preventive coverage, while safety, ergonomics, psychosocial, and biological risk domains were largely absent. ORISMA provides a reproducible, domain-aware framework that requires further validation across independent OSH domains before broad generalization.",
  "Keywords: bibliometrics; bridge articles; evidence mapping; metal additive manufacturing; occupational safety and health; R package; worker-risk disconnection index"
), collapse = "\n")
default_scopus_references <- paste(c(
  "L. Hubert, P. Arabie. Comparing partitions. J. Classif., 2 (1) (1985), pp. 193-218",
  "A. Strehl, J. Ghosh. Cluster ensembles—a knowledge reuse framework for combining multiple partitions. J. Mach. Learn. Res., 3 (2002), pp. 583-617",
  "MEJ. Newman. Modularity and community structure in networks. Proc. Natl. Acad. Sci. U S A, 103 (23) (2006), pp. 8577-8582",
  "PJ. Rousseeuw. Silhouettes: a graphical aid to the interpretation and validation of cluster analysis. J. Comput. Appl. Math., 20 (1987), pp. 53-65",
  "V.D. Blondel, J.L. Guillaume, R. Lambiotte, E. Lefebvre. Fast unfolding of communities in large networks. J. Stat. Mech., 2008 (10) (2008), Article P10008",
  "S.W. Lim, W. Chou, L. Chen. SankeyNetwork: A clear and concise visualization tool for bibliometric data. MethodsX, 14 (2025), Article 103379",
  "H.Y. Chuang, W. Chou. SilhouetteScoreinR: Beyond traditional network layouts by leveraging local cohesion and nearest neighbor separation. MethodsX, 15 (2025), Article 103622, 10.1016/j.mex.2025.103622",
  "T.Y. Cheng, S.Y. Ho, T.W. Chien, J.C. Chow, W. Chou. A comprehensive approach for clustering analysis using follower-leading clustering algorithm (FLCA): Bibliometric analysis. Medicine (Baltimore), 102 (42) (2023), Article e35156, 10.1097/MD.0000000000035156",
  "Y.Z. Cheng, T.W. Chien, S.Y. Ho, W. Chou. Visual impact beam plots: Analyzing research profiles and bibliometric metrics using the following-leading clustering algorithm (FLCA). Medicine (Baltimore), 102 (28) (2023), Article e34301, 10.1097/MD.0000000000034301",
  "T.W. Chien, W. Chou. Visualizing leadership classifications in rectangular data using a basket model and co-word network analysis: a case study of U.S. HCAHPS survey results. BMC Med. Res. Methodol., 25 (1) (2025 Aug 20), p. 195, 10.1186/s12874-025-02643-w",
  "M.J. Li, FJ. Lai. Classifying research leadership in infantile hemangioma studies: A bibliometric analysis using Kano diagrams and absolute advantage coefficients. Medicine (Baltimore), 104 (40) (2025), Article e44905, 10.1097/MD.0000000000044905",
  "T.W. Chien, L. Chen, W. Chou. SilhouettePlot: An R-based App Integrating Silhouette Scores into Network Plots for Enhanced Clarity and Meaning. SoftwareX, 34 (2026), Article 102652",
  "M. Rosvall, CT. Bergstrom. Maps of random walks on complex networks reveal community structure. Proc. Natl. Acad. Sci. U S A, 105 (4) (2008), pp. 1118-1123",
  "P. Pons, M. Latapy. Computing communities in large networks using random walks. J. Graph Algorithms Appl., 10 (2) (2006), pp. 191-218",
  "V.A. Traag, L. Waltman, NJ. van Eck. From Louvain to Leiden: guaranteeing well-connected communities. Sci. Rep., 9 (1) (2019), p. 5233",
  "P.C. Yen, W. Chou, T.W. Chien, TH. Jen. Analyzing fulminant myocarditis research trends and characteristics using the follower-leading clustering algorithm (FLCA): A bibliometric study. Medicine (Baltimore), 102 (26) (2023), Article e34169, 10.1097/MD.0000000000",
  "T.W. Chien. ClusterChoice. available at. https://smilechien.shinyapps.io/flcacompare/ (May 1, 2026)",
  "L. Hubert, P. Arabie. Comparing partitions. J. Classif., 2 (1) (1985), pp. 193-218",
  "A. Strehl, J. Ghosh. Cluster ensembles—a knowledge reuse framework for combining multiple partitions. J. Mach. Learn. Res., 3 (2002), pp. 583-617",
  "Github. ClusterChoice depository. May 1, 2026 available at https://github.com/smilechien/clusterchoice/.",
  "Chien TW. ClusterChoice via MP4 video. May 1, 2026 available at https://youtu.be/uRhJljxbPS0.",
  "C. Chen. CiteSpace II: Detecting and visualizing emerging trends and transient patterns in scientific literature. J. Am. Soc. Inf. Sci. Technol., 57 (3) (2006), pp. 359-377, 10.1002/asi.20317",
  "N.J. van Eck, L. Waltman. VOSviewer, a computer program for bibliometric mapping. Scientometrics, 84 (2) (2010), pp. 523-538, 10.1007/s11192-009-0146-3",
  "M. Aria, C. Cuccurullo. bibliometrix: An R-tool for comprehensive science mapping analysis. J. Informetr., 11 (4) (2017), pp. 959-975, 10.1016/j.joi.2017.08.007",
  "Noriaki Kano, N. Seraku, F. Takahashi, S. Tsuji. Attractive quality and must-be quality. J. Jpn. Soc. Qual. Control, 14 (2) (1984), pp. 39-48"
), collapse = "\n")
empty_text <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
unique_terms <- function(x) {
  x <- x[!empty_text(x)]
  if (!length(x)) return(character())
  terms <- trimws(unlist(strsplit(paste(x, collapse = ";"), "[;|]", perl = TRUE)))
  unique(terms[nzchar(terms) & !tolower(terms) %in% c("na", "n/a", "null")])
}
# Scopus stores all cited references in one cell. Semicolons occur inside
# author lists, so split an entry only after its terminal publication year.
cited_reference_terms <- function(x) {
  x <- x[!empty_text(x)]
  if (!length(x)) return(character())
  entries <- unlist(lapply(as.character(x), function(value) {
    chunks <- trimws(unlist(strsplit(value, "||", fixed = TRUE)))
    trimws(unlist(lapply(chunks, function(chunk) {
      strsplit(chunk, "(?<=\\((?:19|20)\\d{2}\\))\\s*;\\s*", perl = TRUE)[[1]]
    })))
  }), use.names = FALSE)
  entries <- gsub("\\s+", " ", entries)
  unique(entries[nzchar(entries) & !tolower(entries) %in% c("na", "n/a", "null")])
}
# Remove generic human-participant and publication-indexing labels from Scopus
# Index Keywords before they enter the dedicated Index Keywords FLCA analysis.
filter_index_keywords <- function(x) {
  excluded <- c(
    "human", "humans", "adult", "adults", "male", "males", "female", "females",
    "aged", "middle aged", "middle-aged", "child", "children", "adolescent", "adolescents",
    "controlled study", "major clinical study", "clinical article", "clinical trial", "article",
    "priority journal", "human experiment", "human tissue", "human cell", "human cells"
  )
  vapply(x, function(value) {
    terms <- unique_terms(value)
    normalized <- tolower(trimws(gsub("[[:punct:]]", " ", terms)))
    normalized <- gsub("\\s+", " ", normalized)
    paste(terms[!normalized %in% excluded], collapse = "; ")
  }, character(1))
}
derive_affiliation_fields <- function(df) {
  if (!"Affiliations" %in% names(df)) return(df)
  split_affiliations <- function(x) trimws(unlist(strsplit(ifelse(is.na(x), "", x), ";", fixed = TRUE)))
  clean_institute <- function(x) trimws(x)
  clean_department <- function(x) trimws(sub("^Department of\\s+", "", x, ignore.case = TRUE))
  departments <- lapply(df$Affiliations, function(x) {
    parts <- split_affiliations(x); hits <- unlist(regmatches(parts, gregexpr("Department of[^,;]+", parts, ignore.case = TRUE, perl = TRUE)))
    paste(unique(clean_department(hits[nzchar(hits)])), collapse = "; ")
  })
  institutes <- lapply(df$Affiliations, function(x) {
    parts <- split_affiliations(x)
    bits <- trimws(unlist(strsplit(parts, ",", fixed = TRUE)))
    hits <- bits[grepl("University|Institute|College|Hospital|School", bits, ignore.case = TRUE)]
    paste(unique(clean_institute(hits[nzchar(hits)])), collapse = "; ")
  })
  country_dictionary <- c("United States", "United Kingdom", "South Korea", "New Zealand", "Saudi Arabia", "United Arab Emirates", "Hong Kong", "Taiwan", "China", "Italy", "Spain", "Australia", "Germany", "France", "Japan", "Canada", "Brazil", "India", "Netherlands", "Sweden", "Norway", "Finland", "Denmark", "Belgium", "Switzerland", "Portugal", "Greece", "Singapore", "Mexico", "Turkey", "Austria", "Poland", "Ireland", "Israel", "UK", "USA")
  countries_from_affiliations <- lapply(df$Affiliations, function(x) {
    text <- paste(split_affiliations(x), collapse = "; ")
    hits <- country_dictionary[vapply(country_dictionary, function(country) grepl(tolower(country), tolower(text), fixed = TRUE), logical(1))]
    hits[hits == "UK"] <- "United Kingdom"; hits[hits == "USA"] <- "United States"
    paste(unique(hits), collapse = "; ")
  })
  df$Countries_From_Affiliations <- unlist(countries_from_affiliations)
  df$Institutes_From_Affiliations <- unlist(institutes)
  df$Departments_From_Affiliations <- unlist(departments)
  # Keep the general fields complete while retaining the publisher-raw affiliation extractions.
  df$Countries <- mapply(function(a, b) paste(unique(c(unique_terms(a), unique_terms(b))), collapse = "; "), df$Countries, df$Countries_From_Affiliations, USE.NAMES = FALSE)
  if (!"Departments" %in% names(df)) df$Departments <- unlist(departments)
  if (!"Institutes" %in% names(df)) df$Institutes <- unlist(institutes)
  if (!"Countries" %in% names(df)) df$Countries <- ""
  df$Departments <- vapply(strsplit(ifelse(is.na(df$Departments), "", df$Departments), ";", fixed = TRUE), function(z) paste(unique(trimws(z[nzchar(trimws(z))])), collapse = "; "), character(1))
  df$Institutes <- vapply(strsplit(ifelse(is.na(df$Institutes), "", df$Institutes), ";", fixed = TRUE), function(z) paste(unique(trimws(z[nzchar(trimws(z))])), collapse = "; "), character(1))
  df
}

# Extract role-specific metadata from Scopus' author-affiliation and correspondence fields.
first_semicolon_value <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  trimws(sub(";.*$", "", x))
}
strip_scopus_author_id <- function(x) trimws(gsub("\\s*\\([^)]*\\)\\s*$", "", x))

# Scopus Author IDs are stable across name abbreviations. Keep them beside
# the display name so author and affiliation analyses share one identity.
scopus_author_identities <- function(x) {
  entries <- trimws(unlist(strsplit(ifelse(is.na(x), "", as.character(x)), ";", fixed = TRUE)))
  entries <- entries[nzchar(entries)]
  ids <- vapply(entries, function(entry) { hit <- regmatches(entry, regexpr("(?<=\\()[0-9]+(?=\\)\\s*$)", entry, perl = TRUE)); if (length(hit)) hit[1] else NA_character_ }, character(1))
  tibble(Author_Name = strip_scopus_author_id(entries), Author_ID = ids)
}

author_id_for_name <- function(author, full_names) {
  vapply(seq_along(author), function(i) {
    identities <- scopus_author_identities(full_names[i])
    hit <- which(tolower(identities$Author_Name) == tolower(trimws(author[i])))
    if (length(hit)) identities$Author_ID[hit[1]] else NA_character_
  }, character(1))
}

canonicalize_author_roles <- function(df) {
  role_rows <- bind_rows(tibble(Author_Name = as.character(df$FA_Author), Author_ID = as.character(df$FA_Author_ID)), tibble(Author_Name = as.character(df$CA_Author), Author_ID = as.character(df$CA_Author_ID))) |> filter(!is.na(Author_ID), nzchar(Author_ID), !empty_text(Author_Name))
  if (!nrow(role_rows)) return(df)
  name_map <- role_rows |> mutate(name_length = nchar(Author_Name)) |> arrange(Author_ID, desc(name_length), Author_Name) |> distinct(Author_ID, .keep_all = TRUE) |> select(Author_ID, Canonical_Author_Name = Author_Name)
  for (role in c("FA_Author", "CA_Author")) {
    id_col <- paste0(role, "_ID")
    matched <- name_map$Canonical_Author_Name[match(df[[id_col]], name_map$Author_ID)]
    use_match <- !is.na(matched) & nzchar(matched)
    df[[role]][use_match] <- matched[use_match]
  }
  df
}
clean_scopus_author_names <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  vapply(strsplit(x, ";", fixed = TRUE), function(names) {
    names <- trimws(gsub("\\s*\\([0-9]+\\)", "", names))
    paste(names[nzchar(names)], collapse = "; ")
  }, character(1))
}
affiliation_after_author <- function(x) {
  x <- first_semicolon_value(x)
  trimws(sub("^[^,]+,\\s*", "", x))
}
# Match the corresponding author named in Correspondence Address to that
# author entry in Authors with affiliations. The address itself is not used
# as an affiliation source.
corresponding_affiliation_from_authors <- function(corresponding_author, authors_with_affiliations, affiliations) {
  vapply(seq_along(corresponding_author), function(i) {
    correspondent <- tolower(trimws(corresponding_author[i]))
    entries <- trimws(unlist(strsplit(ifelse(is.na(authors_with_affiliations[i]), "", authors_with_affiliations[i]), ";", fixed = TRUE)))
    entries <- entries[nzchar(entries)]
    if (nzchar(correspondent) && length(entries)) {
      words <- unlist(regmatches(correspondent, gregexpr("[[:alpha:]]+", correspondent)))
      surname <- if (length(words)) tail(words, 1) else ""
      initial <- if (length(words)) substr(words[1], 1, 1) else ""
      author_labels <- tolower(trimws(sub(",.*$", "", entries)))
      initials <- gsub("[^[:alpha:]]", "", sub("^[^[:space:]]+\\s*", "", author_labels))
      matched <- which(grepl(surname, author_labels, fixed = TRUE) & (!nzchar(initial) | startsWith(initials, initial)))
      if (length(matched)) return(trimws(sub("^[^,]+,\\s*", "", entries[matched[1]])))
    }
    # Use the record-level Affiliations field only when author-specific
    # affiliations were not supplied by Scopus.
    if (!length(entries)) return(ifelse(is.na(affiliations[i]), "", affiliations[i]))
    ""
  }, character(1))
}
# Match a Correspondence Address name to the full Scopus author name so
# FA_Author and CA_Author use one canonical label for the same person.
canonical_corresponding_author <- function(corresponding_author, full_names) {
  vapply(seq_along(corresponding_author), function(i) {
    raw <- trimws(ifelse(is.na(corresponding_author[i]), "", corresponding_author[i]))
    words <- unlist(regmatches(tolower(raw), gregexpr("[[:alpha:]]+", tolower(raw))))
    surname <- if (length(words)) tail(words, 1) else ""
    initial <- if (length(words)) substr(words[1], 1, 1) else ""
    candidates <- trimws(unlist(strsplit(ifelse(is.na(full_names[i]), "", full_names[i]), ";", fixed = TRUE)))
    candidates <- strip_scopus_author_id(candidates)
    candidates <- candidates[nzchar(candidates)]
    if (nzchar(surname) && length(candidates)) {
      candidate_surnames <- tolower(trimws(sub(",.*$", "", candidates)))
      candidate_given <- tolower(trimws(sub("^[^,]*,\\s*", "", candidates)))
      matched <- which(candidate_surnames == surname & (!nzchar(initial) | startsWith(gsub("[^[:alpha:]]", "", candidate_given), initial)))
      if (length(matched)) return(candidates[matched[1]])
    }
    raw
  }, character(1))
}

role_affiliation_fields <- function(affiliations, prefix) {
  role_df <- tibble(Affiliations = affiliations, Countries = "", Departments = "", Institutes = "")
  role_df <- derive_affiliation_fields(role_df)
  names(role_df)[match(c("Countries_From_Affiliations", "Institutes_From_Affiliations", "Departments_From_Affiliations"), names(role_df))] <-
    paste0(prefix, c("_Countries", "_Institutes", "_Departments"))
  role_df[, paste0(prefix, c("_Countries", "_Institutes", "_Departments")), drop = FALSE]
}
add_author_role_fields <- function(df) {
  pick <- function(candidates) {
    hit <- intersect(candidates, names(df))
    if (length(hit)) as.character(df[[hit[1]]]) else rep("", nrow(df))
  }
  full_names <- pick(c("Author full names", "Authors"))
  df$First_Author <- strip_scopus_author_id(first_semicolon_value(full_names))
  df$First_Author_ID <- author_id_for_name(df$First_Author, full_names)
  corresponding_address_name <- first_semicolon_value(pick(c("Corresponding Author", "Corresponding author", "Correspondence Address")))
  df$Corresponding_Author <- canonical_corresponding_author(corresponding_address_name, full_names)
  df$Corresponding_Author_ID <- author_id_for_name(df$Corresponding_Author, full_names)
  author_affiliations <- pick(c("Authors with affiliations"))
  first_affiliations <- ifelse(nzchar(author_affiliations), affiliation_after_author(author_affiliations), pick(c("Affiliations")))
  corresponding_affiliations <- corresponding_affiliation_from_authors(corresponding_address_name, author_affiliations, pick(c("Affiliations")))
  first_fields <- role_affiliation_fields(first_affiliations, "First_Author")
  corresponding_fields <- role_affiliation_fields(corresponding_affiliations, "Corresponding_Author")
  # Stable, analysis-ready role columns created before any FLCA or AAC calculation.
  df$FA_Author <- df$First_Author
  df$FA_Author_ID <- df$First_Author_ID
  df$FA_Country <- first_fields$First_Author_Countries
  df$FA_Institute <- first_fields$First_Author_Institutes
  df$FA_Department <- first_fields$First_Author_Departments
  df$CA_Author <- df$Corresponding_Author
  df$CA_Author_ID <- df$Corresponding_Author_ID
  df$CA_Country <- corresponding_fields$Corresponding_Author_Countries
  df$CA_Institute <- corresponding_fields$Corresponding_Author_Institutes
  df$CA_Department <- corresponding_fields$Corresponding_Author_Departments
  cbind(df, first_fields, corresponding_fields)
}
# Convert a standard Scopus export into the fields used by every visualisation tab.
prepare_scopus_export <- function(df) {
  names(df) <- sub("^\\ufeff", "", names(df))
  first_column <- function(candidates) {
    hit <- intersect(candidates, names(df))
    if (length(hit)) as.character(df[[hit[1]]]) else rep("", nrow(df))
  }
  df$Authors <- clean_scopus_author_names(first_column(c("Authors", "Author full names", "Authors with affiliations")))
  df$Publication_Date <- first_column(c("Publication_Date", "Year", "Date"))
  df$Journal <- first_column(c("Journal", "Source title", "Abbreviated Source Title"))
  df$Abbreviated_Source_Title <- first_column(c("Abbreviated Source Title", "Abbreviated_Source_Title", "Journal", "Source title"))
  df$Citation_Count <- suppressWarnings(as.numeric(first_column(c("Citation_Count", "Cited by"))))
  df$Author_Keywords <- first_column(c("Author Keywords", "Keywords"))
  df$Index_Keywords_Raw <- first_column(c("Index Keywords"))
  df$Index_Keywords <- filter_index_keywords(df$Index_Keywords_Raw)
  # Keep the existing field as the author-keyword source for compatibility.
  df$Keywords <- df$Author_Keywords
  df$References <- first_column(c("References"))
  df$Document_Type <- first_column(c("Document_Type", "Document Type"))
  df$Countries <- first_column(c("Countries"))
  df$Departments <- first_column(c("Departments"))
  df$Institutes <- first_column(c("Institutes"))
  df$Source <- ifelse(nzchar(first_column(c("Source"))), first_column(c("Source")), "Scopus export")
  df <- add_author_role_fields(df)
  df <- canonicalize_author_roles(df)
  derive_affiliation_fields(df)
}
read_metadata <- function() {
  if (!file.exists(metadata_path)) return(tibble())
  x <- read_csv(metadata_path, show_col_types = FALSE)
  names(x) <- sub("^\\ufeff", "", names(x))
  derive_affiliation_fields(x)
}

normalise_issn <- function(x) {
  x <- toupper(gsub("[^0-9Xx]", "", trimws(as.character(x))))
  if (length(x) != 1L || !grepl("^[0-9]{7}[0-9X]$", x)) return(NA_character_)
  paste0(substr(x, 1, 4), "-", substr(x, 5, 8))
}

http_json <- function(url, headers = character()) {
  handle <- curl::new_handle()
  if (length(headers)) do.call(curl::handle_setheaders, c(list(handle), as.list(headers)))
  response <- curl::curl_fetch_memory(url, handle = handle)
  if (response$status_code < 200L || response$status_code >= 300L) {
    detail <- rawToChar(response$content)
    stop(sprintf("Request failed (HTTP %s): %s", response$status_code, substr(detail, 1, 300)), call. = FALSE)
  }
  jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE)
}

crossref_date <- function(item) {
  for (field in c("published-online", "published-print", "issued", "created")) {
    date_info <- item[[field]] %||% list()
    date_parts <- date_info[["date-parts"]] %||% list()
    parts <- if (length(date_parts)) date_parts[[1]] else NULL
    if (length(parts)) {
      y <- parts[1]; m <- if (length(parts) >= 2) parts[2] else 1; d <- if (length(parts) >= 3) parts[3] else 1
      return(as.Date(sprintf("%04d-%02d-%02d", as.integer(y), as.integer(m), as.integer(d))))
    }
  }
  as.Date(NA)
}

crossref_authors <- function(authors) {
  if (!length(authors)) return("")
  paste(vapply(authors, function(a) {
    name <- trimws(paste(a$given %||% "", a$family %||% ""))
    if (!nzchar(name)) a$name %||% "" else name
  }, character(1)), collapse = "; ")
}

crossref_affiliations <- function(authors) {
  if (!length(authors)) return("")
  affiliations <- unlist(lapply(authors, function(a) vapply(a$affiliation %||% list(), function(x) x$name %||% "", character(1))), use.names = FALSE)
  paste(unique(affiliations[nzchar(affiliations)]), collapse = "; ")
}

crossref_references <- function(references) {
  if (!length(references)) return("")
  # Preserve source fields so reference analysis can identify journals even
  # when Crossref omits volume and issue information.
  entries <- vapply(references, function(r) {
    parts <- c(
      if (!empty_text(r$author %||% "")) paste0("Author: ", r$author) else NULL,
      if (!empty_text(r$`article-title` %||% "")) paste0("Title: ", r$`article-title`) else NULL,
      if (!empty_text(r$`journal-title` %||% "")) paste0("Journal: ", r$`journal-title`) else NULL,
      if (!empty_text(r$year %||% "")) paste0("Year: ", r$year) else NULL,
      if (!empty_text(r$DOI %||% "")) paste0("DOI: ", r$DOI) else NULL
    )
    paste(parts, collapse = " | ")
  }, character(1))
  paste(entries[nzchar(entries)], collapse = " || ")
}
crossref_item_row <- function(item) {
  authors <- item$author %||% list()
  tibble(
    DOI = item$DOI %||% "", Title = paste(unlist(item$title %||% list()), collapse = " "),
    Authors = crossref_authors(authors), Affiliations = crossref_affiliations(authors), Countries = "",
    Publication_Date = as.character(crossref_date(item)), Journal = paste(unlist(item$`container-title` %||% list()), collapse = " "),
    ISSN = paste(unlist(item$ISSN %||% list()), collapse = "; "), Keywords = paste(unlist(item$subject %||% list()), collapse = "; "),
    Citation_Count = suppressWarnings(as.numeric(item$`is-referenced-by-count` %||% NA_real_)),
    References = crossref_references(item$reference %||% list()), Source = "Crossref"
  )
}

fetch_crossref_journal <- function(issn, date_from = NULL, date_to = NULL) {
  filters <- c()
  if (!is.null(date_from) && !is.na(date_from)) filters <- c(filters, paste0("from-pub-date:", format(date_from, "%Y-%m-%d")))
  if (!is.null(date_to) && !is.na(date_to)) filters <- c(filters, paste0("until-pub-date:", format(date_to, "%Y-%m-%d")))
  rows <- 1000L; offset <- 0L; out <- list()
  repeat {
    url <- paste0("https://api.crossref.org/journals/", utils::URLencode(issn, reserved = TRUE), "/works?rows=", rows, "&offset=", offset)
    if (length(filters)) url <- paste0(url, "&filter=", utils::URLencode(paste(filters, collapse = ","), reserved = TRUE))
    payload <- http_json(url, c("User-Agent" = "Journal-Metadata-FLCA-Explorer/1.0 (mailto:contact@example.org)"))
    items <- payload$message$items %||% list()
    if (!length(items)) break
    out <- c(out, lapply(items, crossref_item_row))
    if (length(items) < rows) break
    offset <- offset + length(items)
  }
  bind_rows(out)
}

scopus_item_row <- function(item) {
  tibble(
    DOI = item$`prism:doi` %||% "", Title = item$`dc:title` %||% "", Authors = item$`dc:creator` %||% "", Affiliations = "", Countries = "",
    Publication_Date = item$`prism:coverDate` %||% "", Journal = item$`prism:publicationName` %||% "",
    ISSN = item$`prism:issn` %||% item$`prism:eIssn` %||% "", Keywords = item$authkeywords %||% "",
    Citation_Count = suppressWarnings(as.numeric(item$`citedby-count` %||% NA_real_)), References = "", Source = "Scopus"
  )
}

fetch_scopus_journal <- function(issn, api_key, date_from = NULL, date_to = NULL) {
  years <- c()
  if (!is.null(date_from) && !is.na(date_from)) years <- c(years, paste0("PUBYEAR AFT ", as.integer(format(date_from, "%Y")) - 1L))
  if (!is.null(date_to) && !is.na(date_to)) years <- c(years, paste0("PUBYEAR BEF ", as.integer(format(date_to, "%Y")) + 1L))
  query <- paste(c(paste0("ISSN(", issn, ")"), years), collapse = " AND ")
  start <- 0L; count <- 25L; out <- list()
  repeat {
    url <- paste0("https://api.elsevier.com/content/search/scopus?query=", utils::URLencode(query, reserved = TRUE), "&start=", start, "&count=", count)
    payload <- http_json(url, c("X-ELS-APIKey" = api_key, "Accept" = "application/json"))
    results <- payload$`search-results`; items <- results$entry %||% list()
    if (!length(items)) break
    out <- c(out, lapply(items, scopus_item_row))
    total <- suppressWarnings(as.integer(results$`opensearch:totalResults` %||% 0L)); start <- start + length(items)
    if (length(items) < count || start >= total) break
  }
  x <- bind_rows(out)
  if (nrow(x)) {
    dates <- suppressWarnings(as.Date(x$Publication_Date))
    x <- x[(is.na(dates) | is.null(date_from) | dates >= date_from) & (is.na(dates) | is.null(date_to) | dates <= date_to), , drop = FALSE]
  }
  x
}

read_keyword_cache <- function() {
  if (!file.exists(keyword_cache_path)) return(tibble())
  x <- tryCatch(read_csv(keyword_cache_path, show_col_types = FALSE), error = function(e) tibble())
  names(x) <- sub("^\\ufeff", "", names(x))
  x
}
normalise_doi <- function(x) {
  x <- trimws(tolower(as.character(x)))
  sub("^https?://(dx\\.)?doi\\.org/", "", x, perl = TRUE)
}
column_or_blank <- function(df, choices) {
  found <- choices[choices %in% names(df)][1]
  if (is.na(found) || !length(found)) rep("", nrow(df)) else as.character(df[[found]])
}
extract_year <- function(x) suppressWarnings(as.integer(substr(as.character(x), 1, 4)))
extract_reference_journal <- function(entry) {
  entry <- trimws(as.character(entry))
  # Structured Crossref references explicitly retain their source journal.
  labelled <- regmatches(entry, regexec("(?i)(?:^|[|;])\\s*journal(?:-title)?\\s*:\\s*([^|;]+)", entry, perl = TRUE))[[1]]
  if (length(labelled) >= 2L) {
    journal <- trimws(gsub("\\s+", " ", labelled[2]))
    if (nzchar(journal) && !grepl("10\\.|/", journal)) return(journal)
  }
  # Scopus exports normally encode: title, Journal, volume, issue, pages,
  # (year).  Select the text after the last comma immediately before the
  # numeric volume; this prevents page ranges from becoming journal names.
  before_year <- sub("(?s)(.*?)(?:\\.\\s*)?(?:19|20)[0-9]{2}.*$", "\\1", entry, perl = TRUE)
  volume_at <- regexpr(",\\s*(?:vol\\.?\\s*)?[0-9]+(?:\\s*\\([^)]*\\))?", before_year, perl = TRUE)
  if (volume_at[1] > 0) {
    before_volume <- substr(before_year, 1, volume_at[1] - 1)
    comma_at <- gregexpr(",", before_volume, fixed = TRUE)[[1]]
    if (length(comma_at) && comma_at[1] > 0) {
      journal <- trimws(substr(before_volume, max(comma_at) + 1, nchar(before_volume)))
      journal <- trimws(gsub("\\s+", " ", journal))
      if (nzchar(journal) && !grepl("10\\.|/|^(pp?|article)\\b", journal, ignore.case = TRUE)) return(journal)
    }
  }
  # Older flattened records without a regular Scopus volume segment.
  before_vol <- sub("\\.\\s*Vol\\.?\\s*[0-9].*$", "", before_year, perl = TRUE)
  parts <- trimws(unlist(strsplit(before_vol, "\\.\\s+", perl = TRUE)))
  parts <- gsub("\\.$", "", parts)
  parts <- parts[nzchar(parts) & !grepl("^(author|title|doi)\\s*:", parts, ignore.case = TRUE)]
  if (!length(parts)) return(NA_character_)
  journal <- trimws(gsub("\\s+", " ", parts[length(parts)]))
  if (!nzchar(journal) || grepl("10\\.|/", journal)) NA_character_ else journal
}

extract_reference_first_author <- function(entry) {
  entry <- trimws(as.character(entry))
  labelled <- regmatches(entry, regexec("(?i)(?:^|[|;])\\s*author\\s*:\\s*([^|;]+)", entry, perl = TRUE))[[1]]
  author <- if (length(labelled) >= 2L) labelled[2] else strsplit(entry, "\\.\\s+", perl = TRUE)[[1]][1]
  author <- trimws(sub("\\s*(;|\\band\\b).*$", "", author, ignore.case = TRUE, perl = TRUE))
  if (!nzchar(author) || grepl("^(title|journal|doi|year)\\s*:", author, ignore.case = TRUE)) NA_character_ else author
}

reference_author_journal_terms <- function(references) {
  unlist(lapply(references, function(value) {
    if (length(value) != 1L || empty_text(value)) return(list())
    entries <- trimws(unlist(strsplit(value, "||", fixed = TRUE)))
    lapply(entries, function(entry) {
      author <- extract_reference_first_author(entry)
      journal <- extract_reference_journal(entry)
      terms <- c(if (!empty_text(author)) paste0("First author: ", author) else character(), if (!empty_text(journal)) paste0("Journal: ", journal) else character())
      unique(terms)
    })
  }), recursive = FALSE)
}
reference_journal_year <- function(references) {
  all_rows <- lapply(references, function(value) {
    if (length(value) != 1 || empty_text(value)) return(NULL)
    entries <- trimws(unlist(strsplit(value, "||", fixed = TRUE)))
    rows <- lapply(entries, function(entry) {
      year_text <- regmatches(entry, regexpr("(19|20)[0-9]{2}", entry, perl = TRUE))
      if (!length(year_text) || empty_text(year_text)) return(NULL)
      year <- suppressWarnings(as.integer(year_text[1]))
      journal <- extract_reference_journal(entry)
      if (length(year) != 1 || is.na(year) || length(journal) != 1 || empty_text(journal)) return(NULL)
      tibble(journal = journal, year = year, n = 1L)
    })
    bind_rows(Filter(Negate(is.null), rows))
  })
  bind_rows(Filter(Negate(is.null), all_rows))
}

parse_mdpi_references <- function(text) {
  if (empty_text(text)) return(tibble())
  z <- trimws(unlist(strsplit(text, "\n"))); z <- z[nzchar(z)]; starts <- grepl("^\\s*\\d+\\.\\s+", z)
  if (any(starts)) { g <- cumsum(starts); g[g == 0] <- seq_len(sum(g == 0)) - sum(g == 0); z <- unname(tapply(z, g, paste, collapse = " ")) }
  bind_rows(lapply(seq_along(z), function(i) { x <- gsub("^\\s*\\d+\\.\\s+", "", z[i]); m <- regexpr("(19|20)\\d{2}", x); pre <- if(m[1]>0) trimws(substr(x,1,m[1]-1)) else x; p <- trimws(unlist(strsplit(pre,"\\.\\s+"))); a <- if(length(p))p[1]else ""; au <- trimws(unlist(strsplit(a,";",fixed=TRUE))); au <- au[grepl(",",au,fixed=TRUE)]; tibble(Reference=i, Raw_Reference=x, Authors=paste(au,collapse="; "), Journal=if(length(p)>=3)p[length(p)]else NA_character_, Title=if(length(p)>=2)p[2]else NA_character_, Year=if(m[1]>0)as.integer(regmatches(x,m))else NA_integer_) }))
}
article_text_without_authors_or_references <- function(text) {
  text <- as.character(text %||% "")
  # Journal front matter (title, authors, affiliations, DOI) is excluded when
  # a standard Abstract heading is supplied.
  abstract_at <- regexpr("(?im)^\\s*abstract\\s*$", text, perl = TRUE)
  if (abstract_at[1] > 0) text <- substr(text, abstract_at[1] + attr(abstract_at, "match.length"), nchar(text))
  # References, including author lists embedded in citation entries, never
  # contribute candidate terms or co-occurrences.
  text <- sub("(?is)\\n\\s*(references|bibliography)\\s*\\n.*$", "", text, perl = TRUE)
  text
}

extract_article_content_terms <- function(text, top_n = 20L, min_frequency = 2L) {
  text <- article_text_without_authors_or_references(text)
  if (empty_text(text)) return(list(terms = tibble(), sets = list()))
  abbreviations <- unique(unlist(regmatches(text, gregexpr("\\b[A-Z][A-Z0-9-]{1,12}\\b", text, perl = TRUE))))
  abbreviations <- abbreviations[!abbreviations %in% c("ABSTRACT", "KEYWORDS") & !grepl("^[0-9]+$", abbreviations)]
  raw_keywords <- regmatches(text, regexpr("(?i)keywords?\\s*:\\s*[^\\n]+", text, perl = TRUE))
  keywords <- if (length(raw_keywords) && nzchar(raw_keywords)) trimws(unlist(strsplit(sub("(?i)^keywords?\\s*:\\s*", "", raw_keywords, perl = TRUE), "[;,]", perl = TRUE))) else character()

  # A carriage return embedded in a copied PDF/HTML word is a wrap marker, not a word boundary. Remove it; ordinary line feeds remain sentence whitespace.
  text <- gsub("\r", "", text, fixed = TRUE)
  named_phrases <- unique(unlist(regmatches(text, gregexpr("\\b[A-Z][A-Za-z-]+(?:[ -][A-Z][A-Za-z-]+){1,4}\\b", text, perl = TRUE))))
  sentences <- trimws(unlist(strsplit(gsub("\\n+", " ", text), "(?<=[.!?])\\s+", perl = TRUE)))
  sentences <- sentences[nchar(sentences) > 10]
  # Complement declared keywords with repeated two-word phrases from article
  # sentences. This handles wording variants such as "bibliometric tools"
  # versus the declared keyword "bibliometrics".
  stop_words <- c("the","and","for","with","that","this","from","into","through","which","were","was","are","not","but","its","their","than","also","have","has","been","being","such","these","those","about","between","while","after","before","within","using","used","use","based","across","under","into","onto","where","when","what","who","how","all","any","each","other")
  sentence_phrases <- lapply(sentences, function(s) {
    words <- tolower(unlist(regmatches(s, gregexpr("[A-Za-z][A-Za-z-]{2,}", s, perl = TRUE))))
    words <- words[!words %in% stop_words]
    if (length(words) < 2) return(character())
    unique(vapply(seq_len(length(words) - 1L), function(i) paste(words[i:(i + 1L)], collapse = " "), character(1)))
  })
  phrase_freq <- sort(table(unlist(sentence_phrases)), decreasing = TRUE)
  min_frequency <- max(1L, as.integer(min_frequency))
  auto_phrases <- names(phrase_freq)[phrase_freq >= min_frequency]
  canonical_keyphrase <- function(term) {
    words <- strsplit(tolower(trimws(term)), "\\s+", perl = TRUE)[[1]]
    if (!length(words)) return("")
    last <- words[length(words)]
    if (grepl("ies$", last) && nchar(last) > 4) last <- sub("ies$", "y", last)
    else if (grepl("s$", last) && nchar(last) > 3 && !grepl("(ss|is)$", last)) last <- sub("s$", "", last)
    words[length(words)] <- last
    paste(words, collapse = " ")
  }
  declared_keywords <- unique(vapply(keywords, canonical_keyphrase, character(1)))
  priority_terms <- unique(vapply(c(keywords, named_phrases), canonical_keyphrase, character(1)))
  candidates <- unique(c(keywords, named_phrases, auto_phrases))
  candidates <- candidates[nzchar(candidates)]
  term_in_sentence <- function(term, sentence) {
    words <- strsplit(canonical_keyphrase(term), "\\s+", perl = TRUE)[[1]]
    if (!length(words) || !nzchar(words[1])) return(FALSE)
    words[length(words)] <- paste0(words[length(words)], "(?:s|es)?")
    grepl(paste0("\\b", paste(words, collapse = "\\s+"), "\\b"), sentence, ignore.case = TRUE, perl = TRUE)
  }
  sets <- lapply(sentences, function(s) { found <- candidates[vapply(candidates, term_in_sentence, logical(1), sentence = s)]; unique(vapply(found, canonical_keyphrase, character(1))) })
  freq <- sort(table(unlist(sets)), decreasing = TRUE)
  if (!length(freq)) return(list(terms = tibble(), sets = sets))
  qualified_terms <- names(freq)[freq >= min_frequency]
  priority_present <- intersect(priority_terms, qualified_terms)
  shorter_overlap <- qualified_terms[vapply(qualified_terms, function(term) term %in% priority_present || any(startsWith(priority_present, paste0(term, " "))), logical(1))]
  ranked_terms <- c(priority_present, setdiff(qualified_terms, shorter_overlap))
  keep <- ranked_terms[seq_len(min(as.integer(top_n), length(ranked_terms)))]
  sets <- lapply(sets, function(z) intersect(z, keep))
  list(terms = tibble(Term = keep, Count = as.integer(freq[keep]), Type = ifelse(keep %in% declared_keywords, "Declared keyword", ifelse(keep %in% priority_terms, "Named article concept", "Article keyphrase"))), sets = sets, abbreviations = abbreviations)
}
article_term_sets <- function(df, column) {
  if (!column %in% names(df)) return(list())
  lapply(df[[column]], function(x) unique_terms(x))
}
cooccurrence <- function(term_sets, top_n = 20) {
  singles <- sort(table(unlist(term_sets)), decreasing = TRUE)
  singles <- singles[seq_len(min(length(singles), top_n))]
  nodes <- names(singles)
  pairs <- lapply(term_sets, function(set) {
    set <- intersect(set, nodes)
    if (length(set) < 2) return(NULL)
    combn(sort(set), 2, simplify = FALSE)
  })
  pairs <- unlist(pairs, recursive = FALSE)
  if (!length(pairs)) return(list(nodes = tibble(term = nodes, frequency = as.integer(singles)), edges = tibble(from = character(), to = character(), weight = integer())))
  keys <- vapply(pairs, function(p) paste(p, collapse = "\r"), character(1))
  edge_tab <- sort(table(keys), decreasing = TRUE)
  edge_split <- strsplit(names(edge_tab), "\r", fixed = TRUE)
  list(nodes = tibble(term = nodes, frequency = as.integer(singles)), edges = tibble(from = vapply(edge_split, `[`, "", 1), to = vapply(edge_split, `[`, "", 2), weight = as.integer(edge_tab)))
}

frequency_entity_tab <- function(id, title) tabPanel(title,
  tabsetPanel(
    tabPanel("Top-20 node counts", tableOutput(paste0(id, "_nodes"))),
    tabPanel("Slope plot", plotOutput(paste0(id, "_slope"), height = "900px"))
  )
)
flca_entity_tab <- function(id, title) tabPanel(title,
  tabsetPanel(
    tabPanel("Top-20 node counts", tableOutput(paste0(id, "_nodes"))),
        tabPanel("Full network", plotOutput(paste0(id, "_full"), height = "650px")),
    tabPanel("Reduced network", plotOutput(paste0(id, "_reduced"), height = "650px")),
    tabPanel("Sankey plot", plotOutput(paste0(id, "_sankey"), height = "650px")),
    tabPanel("Chord plot", plotOutput(paste0(id, "_chord"), height = "650px")),
    tabPanel("SSPlot", plotOutput(paste0(id, "_ss"), height = "900px")),
    tabPanel("Kano plot", plotOutput(paste0(id, "_kano"), height = "900px")),
    tabPanel("Slope plot", plotOutput(paste0(id, "_slope"), height = "900px")),
    tabPanel("Top-20 cluster table", tableOutput(paste0(id, "_clusters")), tableOutput(paste0(id, "_edges")))
  )
)

summary_h <- function(x) { x <- sort(as.numeric(x[is.finite(x)]), decreasing=TRUE); if(!length(x)) return(0L); sum(x >= seq_along(x)) }
summary_aac <- function(counts) {
  v <- sort(as.numeric(counts[is.finite(counts) & counts > 0]), decreasing = TRUE)
  if (length(v) < 3 || v[2] == 0 || v[3] == 0) return(NA_real_)
  r <- (v[1] / v[2]) / (v[2] / v[3]); r / (1 + r)
}
summary_domain <- function(df, domain, columns, citation_col = "Citation_Count") {
  columns <- intersect(columns, names(df))
  if (!length(columns)) return(tibble(Domain=character(), Element=character(), Count=integer(), Citations=numeric(), AAC=numeric()))
  cites <- if (citation_col %in% names(df)) suppressWarnings(as.numeric(df[[citation_col]])) else rep(NA_real_, nrow(df)); cites[!is.finite(cites)] <- 0
  long <- bind_rows(lapply(seq_len(nrow(df)), function(i) {
    z <- unique(unlist(lapply(columns, function(column) unique_terms(df[[column]][i])), use.names = FALSE))
    if(!length(z)) return(NULL); tibble(article=i, Element=z, Citations=cites[i])
  }))
  if (!nrow(long)) return(tibble(Domain=character(), Element=character(), Count=integer(), Citations=numeric(), AAC=numeric()))
  out <- long |> distinct(article, Element, .keep_all=TRUE) |> group_by(Element) |> summarise(Count=n(), h=summary_h(Citations), Citations=sum(Citations), .groups="drop") |> arrange(desc(Count), desc(Citations), Element) |> slice_head(n=5)
  mutate(out, Domain=domain, AAC=summary_aac(Count), .before=1)
}
summary_year <- function(df) {
  y <- extract_year(df$Publication_Date); cites <- if("Citation_Count" %in% names(df)) suppressWarnings(as.numeric(df$Citation_Count)) else rep(0,nrow(df)); cites[!is.finite(cites)]<-0
  z <- tibble(Element=as.character(y), Citations=cites) |> filter(!is.na(Element) & nzchar(Element)) |> group_by(Element) |> summarise(Count=n(), h=summary_h(Citations), Citations=sum(Citations), .groups="drop") |> arrange(desc(Count),desc(Citations),Element) |> slice_head(n=5)
  mutate(z,Domain="Year",AAC=summary_aac(Count),.before=1)
}

summary_reference_journal <- function(df) {
  if (!"References" %in% names(df)) return(tibble(Domain=character(), Element=character(), Count=integer(), Citations=numeric(), AAC=numeric()))
  cites <- if ("Citation_Count" %in% names(df)) suppressWarnings(as.numeric(df$Citation_Count)) else rep(0,nrow(df)); cites[!is.finite(cites)]<-0
  long <- bind_rows(lapply(seq_len(nrow(df)), function(i) { z<-reference_journal_year(df$References[i]); if(!nrow(z)) return(NULL); tibble(article=i,Element=unique(z$journal),Citations=cites[i]) }))
  if(!nrow(long)) return(tibble(Domain=character(), Element=character(), Count=integer(), Citations=numeric(), AAC=numeric()))
  out<-long |> distinct(article,Element,.keep_all=TRUE) |> group_by(Element) |> summarise(Count=n(),Citations=sum(Citations),.groups="drop") |> arrange(desc(Count),desc(Citations),Element) |> slice_head(n=5)
  mutate(out,Domain="Journal (references)",AAC=summary_aac(Count),.before=1)
}

draw_summary_report <- function(summary_df, n_articles = NA_integer_, dataset_h = NA_integer_) {
  if (!nrow(summary_df)) { plot.new(); text(.5,.5,"Collect metadata to create the summary report.",cex=1.2); return() }
  domains <- unique(summary_df$Domain); old <- par(no.readonly=TRUE); on.exit(par(old),add=TRUE)
  n_rows <- max(1L, ceiling(length(domains) / 2)); n_slots <- n_rows * 2L
  par(mfrow=c(n_rows,2),mar=c(.2,.2,.2,.2),oma=c(0,0,4,0),xpd=NA)
  for (domain in domains) {
    d <- summary_df[summary_df$Domain == domain,,drop=FALSE]; d <- d[order(d$Count,d$Citations,decreasing=TRUE),,drop=FALSE]
    plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1)); text(.5,.93,domain,col="red",font=3,cex=2.35)
    text(.77,.93,"h",font=2,cex=1.75); text(.91,.93,"n",font=2,cex=1.75)
    yy <- seq(.73,.22,length.out=max(1,nrow(d))); for(i in seq_len(nrow(d))){ text(.04,yy[i],substr(as.character(d$Element[i]),1,31),adj=0,font=2,cex=1.55); text(.77,yy[i],d$h[i],font=2,cex=1.5); text(.91,yy[i],d$Count[i],font=2,cex=1.5) }
    text(.72,.07,paste0("AAC = ",ifelse(is.finite(d$AAC[1]),sprintf("%.2f",d$AAC[1]),"NA")),col="red",font=2,cex=1.55,adj=0)
  }
  if(length(domains)<n_slots) for(i in seq_len(n_slots-length(domains))) plot.new()
  title <- paste0("Summary Report (n=",ifelse(is.finite(n_articles),n_articles,"NA"),", h=",ifelse(is.finite(dataset_h),dataset_h,"NA"),")")
  mtext(title,outer=TRUE,font=2,cex=2.85,line=1)
}

ui <- fluidPage(
  titlePanel("Journal Metadata & FLCA Explorer for Scopus metadata"),
  tags$head(tags$style(HTML(".flca-entity-heading { font-weight: 700; color: #1f4e79; margin-top: 14px; } .tab-content { overflow-x: auto; } .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus, .nav-pills > li.active > a, .nav-pills > li.active > a:hover, .nav-pills > li.active > a:focus { color: #fff !important; background-color: #c9302c !important; border-color: #a52824 !important; font-weight: 700; }"))),
  sidebarLayout(
    sidebarPanel(width = 3,
      tags$h4("Demo data"),
      actionButton("run_demo", "Run supplied Scopus demo", class = "btn-primary btn-block"),
      helpText("Loads the bundled 607-record Scopus export and builds the FLCA visualisations."),
      fileInput("demo_csv", "Upload a Scopus CSV (automatically builds FLCA)", accept = c("text/csv", ".csv"), buttonLabel = "Browse..."),
      helpText("After upload completes, the app automatically normalizes the CSV and builds cached FLCA results."),
      helpText("Scopus columns such as Author full names, Author Keywords, Affiliations, Year, Source title, and Cited by are mapped automatically."),
      tags$hr(),
      tags$h4("A. FLCA Process"),
      actionButton("flca", "Build co-occurrence", class = "btn-warning btn-block"),
      tags$hr(),
      tags$h4("D. References: journal and 1st-author co-word analysis"),
      actionButton("references", "Build cited-reference analysis", class = "btn-info btn-block"),
      tags$hr(),
      numericInput("top_n", "Top nodes / journals", value = 20, min = 5, max = 50),
      sliderInput("recent_years", "Recent reference years", min = 5, max = 30, value = 10),
      tags$hr(), tags$h4("E. Scopus references"),
      textAreaInput("mdpi_references", "Scopus references (one reference per line)", value = default_scopus_references, rows = 9),
      fluidRow(column(7, actionButton("parse_references", "Extract authors & journals", class = "btn-info btn-block")), column(5, actionButton("clear_references", "Clear references", class = "btn-default btn-block"))),
      tags$hr(), tags$h4("F. Graphical abstract"),
      textAreaInput("article_content", "Article content (authors are ignored)", value = default_article_content, rows = 9),
      fluidRow(column(7, actionButton("extract_article_content", "Extract keywords", class = "btn-success btn-block")), column(5, actionButton("clear_article_content", "Clear content", class = "btn-default btn-block"))),
      sliderInput("graphical_phrase_min", "Minimum keyphrase occurrences", min = 1, max = 10, value = 2, step = 1)
    ),
    mainPanel(width = 9,
      tabsetPanel(id = "main_tabs",
        tabPanel("Metadata",
          h3("Collected article metadata"),
          textOutput("metadata_status"), br(),
          h4("DOI keyword extraction"),
          textOutput("keyword_status"), br(),
          tableOutput("keyword_table"), br(),
          downloadButton("download_metadata", "Download metadata CSV"), br(), br(),
          tableOutput("metadata_table")
        ),
        tabPanel("FLCA Process",
          h3("FLCA Process: co-occurrence by metadata entity"),
          textOutput("flca_status"), br(),
          tabsetPanel(id = "entity_tabs",
            flca_entity_tab("authors", "FA/CA authors"),
            flca_entity_tab("countries", "FA/CA countries"),
            tabPanel("FA/CA country map", plotOutput("country_choropleth", height = "800px"), tableOutput("country_choropleth_counts")),
            flca_entity_tab("institutes", "FA/CA institutes"),
            flca_entity_tab("departments", "FA/CA departments"),
            flca_entity_tab("keywords", "Author Keywords"),
            flca_entity_tab("index_keywords", "Index Keywords"),
            frequency_entity_tab("abbreviated_source_titles", "Journals"),
            flca_entity_tab("cited_references", "Reference first authors & journals")
          )
        ),
        tabPanel("Reference journal trends",
          h3("Referenced journals by year"),
          p("Counts are extracted from the References fields in the collected article records."),
          textOutput("reference_status"), br(),
          plotOutput("reference_plot", height = "900px"), tableOutput("reference_counts")
        ),
        tabPanel("Article references",
          h3("Article-reference author and journal FLCA"),
          textOutput("article_reference_status"), br(),
          tabsetPanel(
            tabPanel("Extracted references", tableOutput("article_reference_table")),
            flca_entity_tab("reference_flca", "Reference author & journal FLCA")
          )
        ), 
        tabPanel("Graphical abstract", h3("Keyword FLCA"), textOutput("graphical_abstract_status"), br(), tabsetPanel(tabPanel("Extracted terms", tableOutput("graphical_abstract_terms")), flca_entity_tab("graphical_abstract_flca", "Graphical abstract FLCA"))),
        tabPanel("Summary",
          h3("Top-5 first/corresponding-author summary"),
          p("Count = distinct articles containing the role-specific element; citations are article totals; AAC is calculated only from each first/corresponding-author domain Top-5 counts."),
          downloadButton("download_summary", "Download summary CSV"), downloadButton("download_summary_png", "Download summary PNG"), br(), br(),
          plotOutput("summary_report", height = "3000px"), br(),
          tableOutput("summary_table")
        ),
        tabPanel("ReadMe",
          h3("Scopus FLCA workflow"),
          tags$ol(
            tags$li("Choose Run supplied Scopus demo, or upload a Scopus CSV."),
            tags$li("For an upload, click Normalize selected CSV. This creates FA_* and CA_* author, country, institute, and department fields."),
            tags$li("Click Build co-occurrence, then inspect the FA/CA authors, countries, institutes, departments, Author Keywords, and Index Keywords tabs."),
            tags$li("Index Keywords exclude generic human, demographic, and publication-indexing labels before FLCA.")
          ),
          h4("Interpretation of FLCA outputs"),
          tags$ul(
            tags$li("The local fallback retains the Top-20 entities and reduces each cluster to one leader link per follower."),
            tags$li("SS is a local normalised connection score when the original FLCA module is unavailable."),
            tags$li("Qw is weighted modularity and Qu is unweighted modularity, computed from the local Top-20 co-word matrix and displayed in SSPlot.")
          ),
          downloadButton("download_demo_metadata_format", "Download demo metadata format"), br(), br(),
          p("The download contains the Scopus column-header format used by the supplied demo. The app analyses the uploaded Scopus export locally. The original FLCA module is used automatically if its file is available.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(tibble())
  collection_status <- reactiveVal("Enter an ISSN and click Collect metadata.")
  keywords_done <- reactiveVal(FALSE)
  flca_ready <- reactiveVal(FALSE)
  flca_cache <- reactiveVal(list())
  references_ready <- reactiveVal(FALSE)
  keyword_results <- reactiveVal(tibble())
  article_references <- reactiveVal(parse_mdpi_references(default_scopus_references))
  graphical_abstract <- reactiveVal(extract_article_content_terms(default_article_content))
  selected_scopus_csv <- reactiveVal(NULL)
  observeEvent(data(), { flca_cache(list()) }, ignoreInit = TRUE)
  observeEvent(session$clientData$url_search, {
    query <- parseQueryString(session$clientData$url_search)
    supplied_issn <- query$ISSN %||% query$issn
    if (!is.null(supplied_issn) && nzchar(trimws(supplied_issn))) updateTextInput(session, "issn", value = trimws(supplied_issn))
  }, ignoreInit = FALSE, once = TRUE)

  observeEvent(input$collect, {
    issn <- normalise_issn(input$issn)
    if (is.na(issn)) {
      data(tibble())
      collection_status("Enter a valid eight-character ISSN, for example 2352-7110 for SoftwareX.")
      return()
    }
    api_key <- trimws(input$scopus_api_key %||% "")
    source_name <- if (nzchar(api_key)) "Scopus" else "Crossref"
    fallback_used <- FALSE
    x <- tryCatch(withProgress(message = paste("Collecting", source_name, "journal metadata"), value = 0, {
      incProgress(.15, detail = paste("Looking up ISSN", issn))
      result <- if (nzchar(api_key)) fetch_scopus_journal(issn, api_key, input$date_from, input$date_to) else fetch_crossref_journal(issn, input$date_from, input$date_to)
      if (nzchar(api_key) && !nrow(result)) {
        incProgress(.35, detail = "Scopus returned no records; checking Crossref")
        result <- fetch_crossref_journal(issn, input$date_from, input$date_to)
        fallback_used <- TRUE
      }
      incProgress(.85, detail = "Preparing records for analysis")
      derive_affiliation_fields(result)
    }), error = function(e) {
      collection_status(paste("Could not retrieve", source_name, "records:", conditionMessage(e)))
      tibble()
    })
    data(x)
    if ("DOI" %in% names(x)) updateTextAreaInput(session, "doi_list", value = paste(unique(na.omit(x$DOI)), collapse = "\n"))
    if (nrow(x)) {
      source_label <- if (fallback_used) "Crossref fallback (Scopus returned no records)" else source_name
      collection_status(paste(nrow(x), source_label, "records retrieved for ISSN", issn, "within the selected date range."))
    }
    else if (!grepl("^Could not", collection_status())) collection_status(paste("No", source_name, "records found for ISSN", issn, "within the selected date range."))
    keyword_results(tibble())
    keywords_done(FALSE); flca_ready(FALSE); references_ready(FALSE)
    updateTabsetPanel(session, "main_tabs", selected = "Metadata")
  }, ignoreInit = FALSE)

  load_scopus_data <- function(path, label, build_visualisations = FALSE) {
    x <- tryCatch(read_csv(path, show_col_types = FALSE, name_repair = "minimal"), error = function(e) NULL)
    if (is.null(x)) { collection_status(paste("Could not read", label)); return(FALSE) }
    x <- prepare_scopus_export(x)
    if (!nrow(x) || !any(nzchar(x$Authors))) {
      collection_status("The CSV needs an Authors, Author full names, or Authors with affiliations column.")
      return(FALSE)
    }
    data(x); keywords_done(FALSE); flca_ready(build_visualisations); references_ready(FALSE); keyword_results(tibble())
    collection_status(paste(nrow(x), "Scopus records loaded from", label))
    updateTabsetPanel(session, "main_tabs", selected = if (build_visualisations) "FLCA Process" else "Metadata")
    TRUE
  }

  observeEvent(input$run_demo, {
    if (!file.exists(demo_scopus_path)) { collection_status("The bundled Scopus demo CSV is missing."); return() }
    withProgress(message = "Loading Scopus demo", value = 0, {
      incProgress(.3, detail = "Reading Scopus fields")
      loaded <- load_scopus_data(demo_scopus_path, "the supplied demo CSV", build_visualisations = TRUE)
      if (loaded) incProgress(.7, detail = "Preparing visualisation tabs")
    })
  })
  observeEvent(input$demo_csv, {
    req(input$demo_csv$datapath)
    selected <- list(path = input$demo_csv$datapath, name = input$demo_csv$name)
    selected_scopus_csv(selected)
    withProgress(message = "Preparing uploaded Scopus CSV", value = 0, {
      incProgress(.12, detail = "Reading uploaded file")
      loaded <- load_scopus_data(selected$path, selected$name, build_visualisations = TRUE)
      if (loaded) {
        incProgress(.28, detail = "Normalizing authors, IDs, and affiliations")
        build_flca_results()
      }
    })
  })
  observeEvent(input$clear_references, { updateTextAreaInput(session, "mdpi_references", value = ""); article_references(tibble()) })
  observeEvent(input$parse_references, { article_references(parse_mdpi_references(input$mdpi_references)); updateTabsetPanel(session, "main_tabs", selected = "Article references") })
  observeEvent(input$clear_article_content, { updateTextAreaInput(session, "article_content", value = ""); graphical_abstract(list(terms=tibble(), sets=list(), abbreviations=character())) })
observeEvent(input$extract_article_content, {
    withProgress(message = "Analysing graphical abstract", value = 0, {
      incProgress(0.2, detail = "Removing author and reference sections")
      cleaned <- article_text_without_authors_or_references(input$article_content)
      incProgress(0.5, detail = "Extracting article keyphrases")
      graphical_abstract(extract_article_content_terms(cleaned, 20L, input$graphical_phrase_min))
      incProgress(0.3, detail = "Building the top-20 FLCA network")
    })
    updateTabsetPanel(session, "main_tabs", selected = "Graphical abstract")
  })

  observeEvent(input$keywords, {
    all_records <- data(); req(nrow(all_records))
    if (!"DOI" %in% names(all_records)) { keyword_results(tibble(Status = "Collected metadata has no DOI column.")); return() }
    requested <- normalise_doi(unlist(strsplit(input$doi_list, "\\n", perl = TRUE)))
    requested <- unique(requested[nzchar(requested)])
    use <- if (length(requested)) normalise_doi(all_records$DOI) %in% requested else rep(TRUE, nrow(all_records))
    x <- all_records[use, , drop = FALSE]
    if (!nrow(x)) { keyword_results(tibble(Status = "None of the entered DOI values match collected metadata.")); return() }
    if (!"Keywords" %in% names(x)) x$Keywords <- NA_character_
    source_name <- ifelse(empty_text(x$Keywords), NA_character_, "Collected metadata")
    mdpi_col <- intersect(c("Keywords_MDPI", "Keywords MDPI"), names(x))[1]
    if (length(mdpi_col) && !is.na(mdpi_col)) {
      take_mdpi <- empty_text(x$Keywords) & !empty_text(x[[mdpi_col]])
      x$Keywords[take_mdpi] <- x[[mdpi_col]][take_mdpi]; source_name[take_mdpi] <- "MDPI metadata"
    }
    cache <- read_keyword_cache()
    doi_col <- intersect(c("DOI", "doi"), names(cache))[1]
    key_col <- intersect(c("Author_Keywords", "Keywords", "Keyword"), names(cache))[1]
    if (nrow(cache) && length(doi_col) && length(key_col)) {
      lookup <- setNames(as.character(cache[[key_col]]), normalise_doi(cache[[doi_col]]))
      cached <- unname(lookup[normalise_doi(x$DOI)])
      take_cache <- empty_text(x$Keywords) & !empty_text(cached)
      x$Keywords[take_cache] <- cached[take_cache]; source_name[take_cache] <- "Verified DOI keyword CSV"
    }
    match_rows <- match(normalise_doi(x$DOI), normalise_doi(all_records$DOI))
    all_records$Keywords[match_rows] <- x$Keywords
    data(all_records); keywords_done(TRUE); flca_ready(FALSE); references_ready(FALSE)
    keyword_results(tibble(DOI = x$DOI, Keywords = x$Keywords, Keyword_Source = source_name, Status = ifelse(empty_text(x$Keywords), "No verified keywords found", "Keywords extracted")))
  })

  build_flca_results <- function() {
    req(nrow(data()))
    flca_ready(TRUE)
    flca_cache(list())
    tasks <- list(
      authors = list(label = "FA/CA authors", network = author_net),
      countries = list(label = "FA/CA countries", network = country_net),
      institutes = list(label = "FA/CA institutes", network = institute_net),
      departments = list(label = "FA/CA departments", network = department_net),
      keywords = list(label = "Author keywords", network = keyword_net),
      index_keywords = list(label = "Index keywords", network = index_keyword_net)
    )
    if (isTRUE(references_ready())) tasks$cited_references <- list(label = "Cited-reference authors and journals", network = cited_reference_net)
    withProgress(message = "Building FLCA results", value = 0, {
      for (i in seq_along(tasks)) {
        task <- tasks[[i]]
        incProgress(0, detail = paste("Running", task$label))
        flca_result_cached(names(tasks)[i], task$network())
        incProgress(1 / length(tasks), detail = paste("Completed", task$label))
      }
    })
    updateTabsetPanel(session, "main_tabs", selected = "FLCA Process")
  }
  observeEvent(input$flca, build_flca_results())
  observeEvent(input$references, {
    req(nrow(data()))
    flca_ready(TRUE)
    references_ready(TRUE)
    updateTabsetPanel(session, "main_tabs", selected = "FLCA Process")
    updateTabsetPanel(session, "entity_tabs", selected = "Reference first authors & journals")
  })

  output$article_reference_status <- renderText({ paste(nrow(article_references()), "references parsed") })
  output$graphical_abstract_status <- renderText({ z <- graphical_abstract(); paste(nrow(z$terms), "keywords extracted from article text only; author and reference sections are excluded; abbreviations are removed immediately before FLCA.") })
  output$graphical_abstract_terms <- renderTable({ graphical_abstract()$terms }, striped=TRUE, bordered=TRUE)
  output$article_reference_table <- renderTable({ article_references() }, striped=TRUE, bordered=TRUE)

  output$metadata_status <- renderText({
    collection_status()
  })
  output$metadata_table <- renderTable({ head(data(), 30) }, striped = TRUE, bordered = TRUE)
  output$keyword_table <- renderTable({ keyword_results() }, striped = TRUE, bordered = TRUE)
  output$download_metadata <- downloadHandler(
    filename = function() paste0("metadata_", gsub("[^0-9Xx]", "", input$issn), ".csv"),
    content = function(file) write_csv(data(), file, na = "")
  )

  entity_network <- function(columns) {
    req(flca_ready(), nrow(data()))
    column <- intersect(columns, names(data()))[1]
    if (!length(column) || is.na(column)) return(list(nodes = tibble(), edges = tibble()))
    cooccurrence(article_term_sets(data(), column), min(20, input$top_n))
  }
  entity_pair_network <- function(columns) {
    req(flca_ready(), nrow(data()))
    x <- data(); columns <- intersect(columns, names(x))
    if (!length(columns)) return(list(nodes = tibble(), edges = tibble()))
    sets <- lapply(seq_len(nrow(x)), function(i) unique(unlist(lapply(columns, function(column) unique_terms(x[[column]][i])), use.names = FALSE)))
    cooccurrence(sets, min(20, input$top_n))
  }
  role_affiliation_network <- function(author_column, affiliation_columns) {
    req(flca_ready(), nrow(data()))
    x <- data()
    if (!author_column %in% names(x)) return(list(nodes = tibble(), edges = tibble()))
    available <- affiliation_columns[unname(affiliation_columns) %in% names(x)]
    if (!length(available)) return(list(nodes = tibble(), edges = tibble()))
    sets <- lapply(seq_len(nrow(x)), function(i) {
      terms <- if (!empty_text(x[[author_column]][i])) paste0("Author: ", x[[author_column]][i]) else character()
      for (label in names(available)) terms <- c(terms, paste0(label, ": ", unique_terms(x[[available[[label]]]][i])))
      unique(terms)
    })
    cooccurrence(sets, min(20, input$top_n))
  }
  draw_network <- function(net, label) {
    if (!nrow(net$nodes)) { plot.new(); text(.5, .5, paste("No", label, "terms are available.")); return() }
    n <- nrow(net$nodes); theta <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
    coords <- data.frame(term = net$nodes$term, x = cos(theta), y = sin(theta), frequency = net$nodes$frequency)
    plot(coords$x, coords$y, type = "n", xlim = c(-1.35, 1.35), ylim = c(-1.35, 1.35), axes = FALSE, xlab = "", ylab = "", main = paste(label, "co-occurrence — Top", n), cex.main = 1.3, font.main = 2, cex.main = 1.35)
    if (nrow(net$edges)) for (i in seq_len(nrow(net$edges))) {
      a <- coords[match(net$edges$from[i], coords$term), ]; b <- coords[match(net$edges$to[i], coords$term), ]
      segments(a$x, a$y, b$x, b$y, col = grDevices::adjustcolor("grey35", alpha.f = .55), lwd = 1 + log1p(net$edges$weight[i]))
    }
    palette <- grDevices::hcl.colors(max(n, 3), "Set 2")
    points(coords$x, coords$y, pch = 21, bg = palette[seq_len(n)], col = "grey15", cex = 1.3 + 2.4 * (coords$frequency / max(coords$frequency)))
    text(coords$x, coords$y, labels = coords$term, cex = 1.05, font = 2)
  }
  author_net <- reactive(entity_pair_network(c("FA_Author", "CA_Author")))
  first_author_net <- reactive(role_affiliation_network("FA_Author", c(Country = "FA_Country", Institute = "FA_Institute", Department = "FA_Department")))
  corresponding_author_net <- reactive(role_affiliation_network("CA_Author", c(Country = "CA_Country", Institute = "CA_Institute", Department = "CA_Department")))
  first_author_country_net <- reactive(entity_network(c("FA_Country")))
  first_author_institute_net <- reactive(entity_network(c("FA_Institute")))
  first_author_department_net <- reactive(entity_network(c("FA_Department")))
  corresponding_author_country_net <- reactive(entity_network(c("CA_Country")))
  corresponding_author_institute_net <- reactive(entity_network(c("CA_Institute")))
  corresponding_author_department_net <- reactive(entity_network(c("CA_Department")))
  keyword_net <- reactive(entity_network(c("Author_Keywords", "Keywords")))
  index_keyword_net <- reactive(entity_network(c("Index_Keywords")))
  country_net <- reactive(entity_pair_network(c("FA_Country", "CA_Country")))
  institute_net <- reactive(entity_pair_network(c("FA_Institute", "CA_Institute")))
  department_net <- reactive(entity_pair_network(c("FA_Department", "CA_Department")))
  fa_ca_country_counts <- reactive({
    x <- data(); if (!nrow(x)) return(tibble(Country = character(), Count = integer(), map_region = character()))
    columns <- intersect(c("FA_Country", "CA_Country"), names(x))
    if (!length(columns)) return(tibble(Country = character(), Count = integer(), map_region = character()))
    countries <- bind_rows(lapply(seq_len(nrow(x)), function(i) {
      values <- unique(unlist(lapply(columns, function(column) unique_terms(x[[column]][i])), use.names = FALSE))
      if (!length(values)) return(NULL)
      tibble(article = i, Country = values)
    }))
    if (!nrow(countries)) return(tibble(Country = character(), Count = integer(), map_region = character()))
    countries |> distinct(article, Country) |> count(Country, name = "Count", sort = TRUE) |>
      mutate(map_region = dplyr::recode(Country, "United States" = "USA", "United Kingdom" = "UK", .default = Country))
  })
  abbreviated_source_title_net <- reactive(entity_network(c("Journal")))
  cited_reference_net <- reactive({
    req(flca_ready(), references_ready(), nrow(data()))
    cooccurrence(reference_author_journal_terms(data()$References), min(20, input$top_n))
  })
  reference_author_journal_net <- reactive({ x <- article_references(); if (!nrow(x)) return(list(nodes=tibble(), edges=tibble())); x$Author_Journal <- vapply(seq_len(nrow(x)), function(i) paste(c(unique_terms(x$Authors[i]), if (!empty_text(x$Journal[i])) paste0("Journal: ", x$Journal[i]) else character()), collapse="; "), character(1)); cooccurrence(article_term_sets(x, "Author_Journal"), min(20, input$top_n)) })
  graphical_abstract_net <- reactive({ z <- graphical_abstract(); blocked <- z$abbreviations %||% character(); final_sets <- lapply(z$sets, function(s) setdiff(s, blocked)); cooccurrence(final_sets, 20L) })
  output$country_choropleth_counts <- renderTable({ fa_ca_country_counts() |> select(Country, Count) }, striped = TRUE, bordered = TRUE)
  output$country_choropleth <- renderPlot({
    counts <- fa_ca_country_counts()
    if (!nrow(counts)) return(draw_message("No FA/CA country values are available."))
    if (!requireNamespace("maps", quietly = TRUE)) return(draw_message("Install the maps package to render the country choropleth."))
    world <- ggplot2::map_data("world")
    mapped <- left_join(world, counts, by = c("region" = "map_region"))
    # Counts span several orders of magnitude (e.g., US=563 vs China=12).
    # A log colour scale keeps lower non-zero country counts visible while the
    # legend labels remain the original article counts.
    mapped$map_count <- log10(mapped$Count + 1)
    legend_counts <- unique(c(1, 2, 3, 5, 10, 20, 50, 100, 200, 500, max(counts$Count)))
    legend_counts <- sort(legend_counts[legend_counts <= max(counts$Count)])
    ggplot(mapped, aes(long, lat, group = group, fill = map_count)) +
      geom_polygon(colour = "white", linewidth = .15) +
      coord_quickmap() +
      scale_fill_gradient(low = "#dbeafe", high = "#08306b", na.value = "grey92", name = "Articles\n(n)", breaks = log10(legend_counts + 1), labels = legend_counts) +
      labs(title = "FA/CA countries by article count", subtitle = "Log colour scale; each country is counted once per article across FA_Country and CA_Country") +
      theme_void(base_size = 14) + theme(plot.title = element_text(face = "bold", hjust = .5), plot.subtitle = element_text(hjust = .5), legend.position = "right")
  })
  output$flca_status <- renderText({ if (!flca_ready()) "Click C. Build FLCA process after collecting metadata and extracting keywords." else paste("FLCA co-occurrence prepared for", nrow(data()), "articles.") })
  fallback_flca <- function(net) {
    nd <- as.data.frame(net$nodes, stringsAsFactors = FALSE)
    ed <- as.data.frame(net$edges, stringsAsFactors = FALSE)
    if (nrow(nd) < 2 || !nrow(ed)) return(NULL)
    names(nd) <- c("name", "value")
    nd$name <- as.character(nd$name); nd$value <- as.numeric(nd$value)
    ed <- ed[ed$from %in% nd$name & ed$to %in% nd$name, , drop = FALSE]
    if (!nrow(ed)) return(NULL)
    weight <- matrix(0, nrow(nd), nrow(nd), dimnames = list(nd$name, nd$name))
    for (i in seq_len(nrow(ed))) {
      weight[ed$from[i], ed$to[i]] <- ed$weight[i]
      weight[ed$to[i], ed$from[i]] <- ed$weight[i]
    }
    max_weight <- max(weight)
    distance <- max_weight - weight
    diag(distance) <- 0
    groups <- if (nrow(nd) == 2) rep(1L, 2L) else stats::cutree(stats::hclust(stats::as.dist(distance), method = "average"), k = min(5L, max(2L, floor(sqrt(nrow(nd))))))
    nd$carac <- as.integer(groups)
    nd$value2 <- rowSums(weight)
    nd$ssi <- if (max_weight > 0) nd$value2 / ((nrow(nd) - 1) * max_weight) else 0
    modularity_components <- function(adjacency, membership) {
      two_m <- sum(adjacency)
      group_ids <- sort(unique(membership))
      if (!is.finite(two_m) || two_m <= 0) return(list(total = NA_real_, by_cluster = setNames(rep(NA_real_, length(group_ids)), as.character(group_ids))))
      degree <- rowSums(adjacency)
      residual <- adjacency - outer(degree, degree) / two_m
      by_cluster <- setNames(vapply(group_ids, function(group) {
        idx <- membership == group
        sum(residual[idx, idx, drop = FALSE]) / two_m
      }, numeric(1)), as.character(group_ids))
      list(total = sum(by_cluster), by_cluster = by_cluster)
    }
    qw <- modularity_components(weight, groups)
    qu <- modularity_components((weight > 0) * 1, groups)
    nd$Qw_total <- qw$total; nd$Qu_total <- qu$total
    # One-link FLCA reduction: keep the highest-frequency node as leader in
    # each cluster, then connect every follower to that leader exactly once.
    clusters <- sort(unique(nd$carac))
    leader_index <- setNames(vapply(clusters, function(cluster) {
      candidates <- which(nd$carac == cluster)
      candidates[order(-nd$value[candidates], nd$name[candidates])][1]
    }, integer(1)), as.character(clusters))
    nd$neighbor_name <- vapply(seq_len(nrow(nd)), function(i) {
      leader <- leader_index[[as.character(nd$carac[i])]]
      if (i == leader) NA_character_ else nd$name[leader]
    }, character(1))
    nd$neighborC <- nd$carac[match(nd$neighbor_name, nd$name)]
    follower_index <- which(!is.na(nd$neighbor_name))
    out_edges <- do.call(rbind, lapply(follower_index, function(i) {
      leader <- match(nd$neighbor_name[i], nd$name)
      direct_weight <- weight[leader, i]
      # A cluster can contain an indirect connection; retain it as a visible
      # leader relationship using the follower's strongest observed tie.
      if (!is.finite(direct_weight) || direct_weight <= 0) direct_weight <- max(1, max(weight[i, ], na.rm = TRUE))
      data.frame(Leader = nd$name[leader], Follower = nd$name[i], WCD = direct_weight, stringsAsFactors = FALSE)
    }))
    if (is.null(out_edges)) out_edges <- data.frame(Leader = character(), Follower = character(), WCD = numeric())
    clusters <- sort(unique(nd$carac))
    modularity <- data.frame(carac = clusters, Qw_cluster = unname(qw$by_cluster[as.character(clusters)]), Qu_cluster = unname(qu$by_cluster[as.character(clusters)]))
    list(modes = nd, data = out_edges, modularity_by_cluster = modularity)
  }
  real_flca <- function(net) {
    if (!nrow(net$nodes) || !nrow(net$edges)) return(NULL)
    module_paths <- c("flca_ms_sil_module_76_v40_top20_max4_preserve_values.R", "flca_ms_sil_module.R")
    module_path <- module_paths[file.exists(module_paths)][1]
    if (!length(module_path) || is.na(module_path)) return(fallback_flca(net))
    tryCatch({
      source(module_path, local = TRUE)
      nodes <- data.frame(name = net$nodes$term, value = net$nodes$frequency, stringsAsFactors = FALSE)
      edges <- data.frame(Leader = net$edges$from, Follower = net$edges$to, WCD = net$edges$weight, stringsAsFactors = FALSE)
      result <- run_flca_ms_sil_runner(nodes, edges, cfg = list(target_n = min(20, nrow(nodes)), top_clusters = 5, base_per_cluster = 4, max_per_cluster = 4), verbose = FALSE)
      # FLCA recalculates value from the sampled edge set. Keep that edge
      # strength in value2, but restore value to the original article count so
      # the SSPlot Count field agrees with the Top-20 node-count table.
      if (is.list(result) && is.data.frame(result$modes) && "name" %in% names(result$modes)) {
        raw_count <- unname(setNames(nodes$value, nodes$name)[as.character(result$modes$name)])
        result$modes$Original_Count <- raw_count
        result$modes$value <- raw_count
      }
      result
    }, error = function(e) fallback_flca(net))
  }
  # Cache computed Top-20 FLCA results. The Build button fills this cache with
  # visible progress, and opening a visualisation reuses the completed result.
  flca_result_cached <- function(id, net) {
    cache <- flca_cache()
    if (id %in% names(cache)) return(cache[[id]])
    result <- real_flca(net)
    cache[[id]] <- result
    flca_cache(cache)
    result
  }
  draw_message <- function(message) { plot.new(); text(.5, .5, message, cex = 1.1) }
  draw_reduced <- function(result, title) {
    if (is.null(result) || inherits(result, "flca_error")) return(draw_message(if (is.null(result)) "Not enough co-occurrences for FLCA." else result$message))
    nd <- result$modes; ed <- result$data; n <- nrow(nd); th <- seq(0, 2*pi, length.out=n+1)[-(n+1)]; xy <- data.frame(name=nd$name,x=cos(th),y=sin(th),carac=as.factor(nd$carac),value=nd$value)
    plot(xy$x,xy$y,type="n",xlim=c(-1.35,1.35),ylim=c(-1.35,1.35),axes=FALSE,xlab="",ylab="",main=title,font.main=2)
    pal <- grDevices::hcl.colors(max(3,length(unique(xy$carac))),"Dark 3"); cols <- setNames(pal,sort(unique(xy$carac)))
    if(nrow(ed)) for(i in seq_len(nrow(ed))){a<-xy[match(ed$Leader,xy$name),];b<-xy[match(ed$Follower,xy$name),];segments(a$x,a$y,b$x,b$y,col="grey65",lwd=1+log1p(ed$WCD[i]))}
    points(xy$x,xy$y,pch=21,bg=cols[as.character(xy$carac)],cex=1.5+2*xy$value/max(xy$value),col="grey10"); text(xy$x,xy$y,xy$name,cex=1.05,font=2)
  }
  draw_sankey <- function(result, title) {
    if (is.null(result) || inherits(result, "flca_error")) return(draw_message("Not enough FLCA links for Sankey plot."))
    nd <- as.data.frame(result$modes); ed <- as.data.frame(result$data); nd$name <- as.character(nd$name); nd$value <- as.numeric(nd$value); nd$carac <- as.integer(nd$carac)
    nd <- nd[order(nd$carac, -nd$value, nd$name), , drop = FALSE]; nd$y <- seq_len(nrow(nd)); ymap <- setNames(nd$y, nd$name); nd$lab <- sprintf("%s (%.0f; C%s)", nd$name, nd$value, nd$carac)
    ed <- ed[ed$Leader %in% nd$name & ed$Follower %in% nd$name & is.finite(ed$WCD) & ed$WCD > 0, , drop = FALSE]; ed$y1 <- ymap[ed$Leader]; ed$y2 <- ymap[ed$Follower]
    pal <- grDevices::hcl.colors(max(3, length(unique(nd$carac))), "Dark 3"); node_col <- setNames(pal[match(nd$carac, sort(unique(nd$carac)))], nd$name)
    old <- par(no.readonly=TRUE); on.exit(par(old),add=TRUE); par(mar=c(5,16,4,16),xpd=NA)
    plot(NA,xlim=c(0,1),ylim=c(nrow(nd)+1.2,0),axes=FALSE,xlab="",ylab="",main=title,cex.main=1.45,font.main=2)
    L <- c(.12,.18); R <- c(.82,.88); h <- .32
    for(k in seq_len(nrow(nd))){rect(L[1],nd$y[k]-h,L[2],nd$y[k]+h,col=node_col[nd$name[k]],border="grey25");rect(R[1],nd$y[k]-h,R[2],nd$y[k]+h,col=node_col[nd$name[k]],border="grey25");text(L[1]-.015,nd$y[k],nd$lab[k],adj=c(1,.5),cex=.95,font=2);text(R[2]+.015,nd$y[k],nd$lab[k],adj=c(0,.5),cex=.95,font=2)}
    text(mean(L),.2,"Leader",cex=1.2,font=2);text(mean(R),.2,"Follower",cex=1.2,font=2)
    if(nrow(ed)){mw<-max(ed$WCD);for(k in seq_len(nrow(ed))){t<-seq(0,1,length.out=90);x<-(L[2]+.01)+(R[1]-L[2]-.02)*t;e<-3*t^2-2*t^3;ym<-(1-e)*ed$y1[k]+e*ed$y2[k];hw<-.025+.14*ed$WCD[k]/mw;polygon(c(x,rev(x)),c(ym-hw,rev(ym+hw)),col=grDevices::adjustcolor(node_col[ed$Leader[k]],alpha.f=.48),border=NA);lines(x,ym,col=grDevices::adjustcolor("grey20",alpha.f=.35),lwd=.8)}}
    mtext("Identical FLCA Top-20 nodes at both sides; ribbon width = WCD; colours = leader cluster.",side=1,line=2,cex=.95,font=2)
  }
  draw_chord <- function(result, title) {
    if (is.null(result) || inherits(result, "flca_error")) return(draw_message("Not enough FLCA links for chord plot."))
    if (!requireNamespace("circlize", quietly = TRUE)) return(draw_message("Install circlize for the chord diagram."))
    nd <- result$modes; ed <- result$data; groups <- sort(unique(as.character(nd$carac)))
    cols <- setNames(grDevices::hcl.colors(max(3, length(groups)), "Dark 3")[seq_along(groups)], groups)
    grid_col <- setNames(unname(cols[as.character(nd$carac)]), nd$name)
    circlize::circos.clear(); on.exit(circlize::circos.clear(), add = TRUE)
    circlize::chordDiagram(ed[, c("Leader", "Follower", "WCD")], grid.col = grid_col, transparency = .25, annotationTrack = "grid", preAllocateTracks = list(track.height = .12))
    circlize::circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) { sector <- circlize::get.cell.meta.data("sector.index"); circlize::circos.text(circlize::get.cell.meta.data("xcenter"), circlize::get.cell.meta.data("ylim")[1], sector, facing = "clockwise", niceFacing = TRUE, adj = c(0, .5), cex = 1.05, font = 2) }, bg.border = NA)
    title(title, cex.main = 1.5, font.main = 2, cex.main = 1.35)
  }
  make_ss_results <- function(result) {
    m <- result$modes; cl <- sort(unique(m$carac)); mod <- result$modularity_by_cluster
    qw <- qu <- setNames(rep(0, length(cl)), as.character(cl))
    if (is.data.frame(mod) && nrow(mod)) { qw[as.character(mod$carac)] <- mod$Qw_cluster; qu[as.character(mod$carac)] <- mod$Qu_cluster }
    tibble(Cluster = c("OVERALL", paste0("C", cl)), SS = c(mean(m$ssi, na.rm = TRUE), vapply(cl, function(z) mean(m$ssi[m$carac == z], na.rm = TRUE), numeric(1))), Qw = c(unique(m$Qw_total)[1], unname(qw[as.character(cl)])), Qu = c(unique(m$Qu_total)[1], unname(qu[as.character(cl)])))
  }
  entity_slope <- function(columns, label) {
    x <- data(); req(nrow(x), "Publication_Date" %in% names(x))
    columns <- intersect(columns, names(x)); if (!length(columns)) return(NULL)
    years <- extract_year(x$Publication_Date); keep <- is.finite(years); x <- x[keep, , drop = FALSE]; years <- years[keep]
    if (!length(years)) return(NULL); recent <- max(years) - 9; sets <- lapply(seq_len(nrow(x)), function(i) unique(unlist(lapply(columns, function(column) unique_terms(x[[column]][i])), use.names = FALSE)))
    rows <- bind_rows(lapply(seq_along(sets), function(i) if(length(sets[[i]])) tibble(term=sets[[i]], year=years[i]) else NULL)); if(!nrow(rows)) return(NULL)
    rows <- rows |> filter(year >= recent) |> count(term, year, name="value"); top <- rows |> group_by(term) |> summarise(total=sum(value),.groups="drop") |> arrange(desc(total),term) |> slice_head(n=min(20, input$top_n)) |> pull(term)
    rows <- filter(rows, term %in% top); yrs <- sort(unique(rows$year)); grid <- expand.grid(term=top,year=yrs,stringsAsFactors=FALSE); rows <- left_join(grid,rows,by=c("term","year")) |> mutate(value=ifelse(is.na(value),0,value))
    wide <- xtabs(value ~ term + year, data=rows); wide <- wide[order(wide[,1],rownames(wide)),,drop=FALSE]; gap <- max(.5,.05*diff(range(wide))); shifts <- numeric(nrow(wide)); if(nrow(wide)>1) for(k in 2:nrow(wide)){dm<-min(diff(as.matrix(wide[(k-1):k,,drop=FALSE])));shifts[k]<-ifelse(is.finite(dm)&&dm<gap,gap-dm,0)}; shifts<-cumsum(shifts)
    sl <- do.call(rbind,lapply(seq_along(yrs),function(j)data.frame(term=rownames(wide),year=yrs[j],value=as.numeric(wide[,j]),ypos=as.numeric(wide[,j])+shifts))); sl <- sl |> group_by(term) |> mutate(at_or_above_mean = value >= mean(value, na.rm = TRUE)) |> ungroup(); first<-sl[sl$year==yrs[1],,drop=FALSE]
    ggplot(sl,aes(year,ypos))+geom_line(aes(group=term),colour="red",linewidth=1)+geom_point(aes(fill=at_or_above_mean,colour=at_or_above_mean),shape=21,size=5,stroke=1)+geom_text(aes(label=ifelse(value>0,value,"")),size=4,fontface="bold")+scale_fill_manual(values=c(`FALSE`="white",`TRUE`="blue"),guide="none")+scale_colour_manual(values=c(`FALSE`="grey35",`TRUE`="blue"),guide="none")+scale_x_continuous(breaks=yrs)+scale_y_continuous("",breaks=first$ypos,labels=first$term)+labs(title=paste(label,"Top-20 FLCA terms by publication year"),x="Publication year")+theme_classic(base_size=15)+theme(axis.title.y=element_blank(),plot.title=element_text(face="bold",hjust=.5,size=18),axis.text=element_text(face="bold"),plot.margin=margin(10,15,10,25),axis.text.y=element_text(margin=margin(r=4)))
  }

  cited_reference_slope <- function() {
    req(references_ready())
    x <- data(); years <- extract_year(x$Publication_Date); keep <- is.finite(years); x <- x[keep, , drop = FALSE]; years <- years[keep]
    if (!length(years)) return(NULL)
    rows <- bind_rows(lapply(seq_len(nrow(x)), function(i) {
      sets <- reference_author_journal_terms(x$References[i])
      bind_rows(lapply(sets, function(terms) if (length(terms)) tibble(term = terms, year = years[i]) else NULL))
    }))
    if (!nrow(rows)) return(NULL)
    recent <- max(years) - 9
    rows <- rows |> filter(year >= recent) |> count(term, year, name = "value")
    top <- rows |> group_by(term) |> summarise(total = sum(value), .groups = "drop") |> arrange(desc(total), term) |> slice_head(n = min(20, input$top_n)) |> pull(term)
    rows <- rows |> filter(term %in% top)
    yrs <- sort(unique(rows$year)); grid <- expand.grid(term = top, year = yrs, stringsAsFactors = FALSE)
    rows <- left_join(grid, rows, by = c("term", "year")) |> mutate(value = ifelse(is.na(value), 0, value))
    ggplot(rows, aes(year, value, group = term)) + geom_line(colour = "red", linewidth = 1) + geom_point(shape = 21, fill = "blue", colour = "blue", size = 4) + geom_text(aes(label = ifelse(value > 0, value, "")), vjust = -0.8, size = 3) + facet_wrap(~term, scales = "free_y") + scale_x_continuous(breaks = yrs) + labs(title = "Cited-reference first authors and journals: Top-20 by publication year", x = "Publication year", y = "Citing articles") + theme_classic(base_size = 13) + theme(plot.title = element_text(face = "bold", hjust = .5), strip.text = element_text(size = 7))
  }
  register_flca_outputs <- function(id, net, label) {
    real <- reactive(flca_result_cached(id, net()))
    output[[paste0(id,"_nodes")]] <- renderTable({ n <- net()$nodes; if (!nrow(n)) return(data.frame(Status = "No terms available.")); names(n) <- c("Node", "Count"); n }, striped = TRUE, bordered = TRUE)
    output[[paste0(id,"_full")]] <- renderPlot(draw_network(net(), paste(label,"full co-occurrence network")))
    output[[paste0(id,"_reduced")]] <- renderPlot(draw_reduced(real(), paste(label,"real FLCA Top-20 reduced network")))
    output[[paste0(id,"_sankey")]] <- renderPlot(draw_sankey(real(), paste(label,"FLCA Sankey plot")))
    output[[paste0(id,"_chord")]] <- renderPlot(draw_chord(real(), paste(label,"FLCA chord plot")))
    output[[paste0(id,"_ss")]] <- renderPlot({ r<-real(); if(is.null(r)||inherits(r,"flca_error")) draw_message("Not enough data for SSPlot.") else { source("renderSSplot.R",local=TRUE); s<-r$modes; s$sil_width<-s$ssi; render_panel(s,make_nodes0(s),results=make_ss_results(r),nodes=s,top_n=min(20,nrow(s)),font_scale=1.15) } })
    output[[paste0(id,"_kano")]] <- renderPlot({ r<-real(); if(is.null(r)||inherits(r,"flca_error")) draw_message("Not enough data for Kano plot.") else { source("kano.R",local=TRUE); print(plot_kano_real(r$modes,edges=r$data,title_txt=paste(label,"Kano plot"),visual_ratio=1)) } })
    output[[paste0(id,"_slope")]] <- renderPlot({ if (id == "cited_references") { p <- cited_reference_slope() } else p <- entity_slope(switch(id, authors=c("FA_Author","CA_Author"), first_authors=c("FA_Author"), corresponding_authors=c("CA_Author"), first_author_countries=c("FA_Country"), first_author_institutes=c("FA_Institute"), first_author_departments=c("FA_Department"), corresponding_author_countries=c("CA_Country"), corresponding_author_institutes=c("CA_Institute"), corresponding_author_departments=c("CA_Department"), keywords=c("Author_Keywords","Keywords"), index_keywords=c("Index_Keywords"), countries=c("FA_Country","CA_Country"), institutes=c("FA_Institute","CA_Institute"), departments=c("FA_Department","CA_Department")), label); if(is.null(p)) draw_message("No dated terms available for slope plot.") else print(p) })
    output[[paste0(id,"_clusters")]] <- renderTable({ r<-real(); if(is.null(r)||inherits(r,"flca_error")) return(data.frame(Status="Not enough co-occurrences for FLCA.")); r$modes }, striped=TRUE,bordered=TRUE)
    output[[paste0(id,"_edges")]] <- renderTable({ r<-real(); if(is.null(r)||inherits(r,"flca_error")) return(NULL); r$data }, striped=TRUE,bordered=TRUE)
  }
  register_flca_outputs("authors", author_net, "FA/CA authors")
  register_flca_outputs("first_authors", first_author_net, "FA_* co-word")
  register_flca_outputs("corresponding_authors", corresponding_author_net, "CA_* co-word")
  register_flca_outputs("first_author_countries", first_author_country_net, "First author country")
  register_flca_outputs("first_author_institutes", first_author_institute_net, "First author institute")
  register_flca_outputs("first_author_departments", first_author_department_net, "First author department")
  register_flca_outputs("corresponding_author_countries", corresponding_author_country_net, "Corresponding author country")
  register_flca_outputs("corresponding_author_institutes", corresponding_author_institute_net, "Corresponding author institute")
  register_flca_outputs("corresponding_author_departments", corresponding_author_department_net, "Corresponding author department")
  register_flca_outputs("keywords", keyword_net, "Author keyword")
  register_flca_outputs("index_keywords", index_keyword_net, "Index keyword")
  register_flca_outputs("countries", country_net, "FA/CA countries")
  register_flca_outputs("institutes", institute_net, "FA/CA institutes")
  register_flca_outputs("departments", department_net, "FA/CA departments")
  output$abbreviated_source_titles_nodes <- renderTable({
    n <- abbreviated_source_title_net()$nodes
    if (!nrow(n)) return(data.frame(Status = "No abbreviated source titles available."))
    names(n) <- c("Journal", "Count"); n
  }, striped = TRUE, bordered = TRUE)
  output$abbreviated_source_titles_slope <- renderPlot({
    p <- entity_slope(c("Journal"), "Journal")
    if (is.null(p)) draw_message("No dated journals are available.") else print(p)
  })
  register_flca_outputs("cited_references", cited_reference_net, "Cited-reference authors and journals")
  register_flca_outputs("reference_flca", reference_author_journal_net, "Reference author and journal")
  register_flca_outputs("graphical_abstract_flca", graphical_abstract_net, "Graphical abstract keyword")

  summary_data <- reactive({
    x <- data(); if(!nrow(x)) return(tibble())
    bind_rows(
      # Each domain combines only the matching FA_* and CA_* entity columns.
      # No author-country, author-institute, or other cross-entity pairs are used.
      summary_domain(x,"FA/CA authors",c("FA_Author","CA_Author")),
      summary_domain(x,"FA/CA countries",c("FA_Country","CA_Country")),
      summary_domain(x,"FA/CA institutes",c("FA_Institute","CA_Institute")),
      summary_domain(x,"FA/CA departments",c("FA_Department","CA_Department")),
      summary_domain(x,"Journal",c("Journal")),
      summary_domain(x,"Author keywords",c("Author_Keywords")),
      summary_domain(x,"Index keywords",c("Index_Keywords")),
      summary_year(x),
      summary_domain(x,"Document type",c("Document_Type", "Document Type"))
    )
  })
  output$summary_table <- renderTable({ summary_data() }, striped=TRUE,bordered=TRUE)
  output$summary_report <- renderPlot({ x<-data(); cites<-if("Citation_Count"%in%names(x))as.numeric(x$Citation_Count) else numeric(); draw_summary_report(summary_data(),nrow(x),summary_h(cites)) }, width=1400, height=3000, res=150)
  output$download_summary_png <- downloadHandler(filename=function() "top5_metadata_summary.png", content=function(file) { grDevices::png(file,width=1400,height=3000,res=150); on.exit(grDevices::dev.off(),add=TRUE); x<-data(); cites<-if("Citation_Count"%in%names(x))as.numeric(x$Citation_Count) else numeric(); draw_summary_report(summary_data(),nrow(x),summary_h(cites)) })
  output$download_demo_metadata_format <- downloadHandler(
    filename = function() "scopus_demo_metadata_format.csv",
    content = function(file) {
      template <- read_csv(demo_scopus_path, n_max = 0, show_col_types = FALSE, name_repair = "minimal")
      write_csv(template, file)
    }
  )
  output$download_summary <- downloadHandler(filename=function() "top5_metadata_summary.csv", content=function(file) write_csv(summary_data(),file,na=""))

  ref_data <- reactive({
    x <- data(); if (!nrow(x) || !"References" %in% names(x)) return(tibble())
    x <- reference_journal_year(x$References); if (!nrow(x)) return(x)
    latest <- max(x$year, na.rm = TRUE)
    x <- filter(x, year >= latest - input$recent_years + 1)
    top <- x |> group_by(journal) |> summarise(total = sum(n), .groups = "drop") |> arrange(desc(total), journal) |> slice_head(n = input$top_n)
    filter(x, journal %in% top$journal)
  })
  output$reference_status <- renderText({ if (!nrow(ref_data())) "No reference journal/year data are available in the collected records." else paste(nrow(ref_data()), "reference journal/year pairs shown for the selected recent period.") })
  output$reference_plot <- renderPlot({
    x <- ref_data(); req(nrow(x))
    counts <- x |> group_by(journal, year) |> summarise(value = sum(n), .groups = "drop")
    top10 <- counts |> group_by(journal) |> summarise(total = sum(value), .groups = "drop") |> arrange(desc(total), journal) |> slice_head(n = min(10, input$top_n)) |> pull(journal)
    years <- sort(unique(counts$year)); grid <- expand.grid(journal = top10, year = years, stringsAsFactors = FALSE)
    counts <- left_join(grid, filter(counts, journal %in% top10), by = c("journal", "year")) |> mutate(value = ifelse(is.na(value), 0, value))
    # Exact Tufte slopegraph layout: each journal receives one y-shift across its series.
    wide <- xtabs(value ~ journal + year, data = counts)
    wide <- wide[order(wide[, 1], rownames(wide)), , drop = FALSE]
    min_space <- 0.05 * diff(range(wide)); if (!is.finite(min_space) || min_space <= 0) min_space <- 0.5
    yshift <- numeric(nrow(wide))
    if (nrow(wide) > 1) for (i in 2:nrow(wide)) {
      d_min <- min(diff(as.matrix(wide[(i - 1):i, , drop = FALSE])))
      yshift[i] <- ifelse(is.finite(d_min) && d_min < min_space, min_space - d_min, 0)
    }
    yshift <- cumsum(yshift)
    slope <- do.call(rbind, lapply(seq_along(years), function(j) data.frame(group = rownames(wide), year = years[j], value = as.numeric(wide[, j]), ypos = as.numeric(wide[, j]) + yshift)))
    first <- slope[slope$year == years[1], , drop = FALSE]
    ggplot(slope, aes(x = year, y = ypos)) +
      geom_line(aes(group = group), colour = "red", linewidth = 0.9) +
      geom_point(colour = "white", fill = "white", shape = 21, stroke = 1, size = 5) +
      geom_text(aes(label = ifelse(value > 0, sprintf("%.0f", value), "")), size = 3.5, fontface = "bold") +
      scale_x_continuous(breaks = years) +
      scale_y_continuous(name = "", breaks = first$ypos, labels = first$group) +
      labs(title = "Top 10 referenced journals by year", subtitle = "Tufte-style slopegraph; labels are ordered by the first displayed year", x = "Reference year") +
      theme_classic(base_size = 14) +
      theme(axis.title.y = element_blank(), plot.title = element_text(hjust = .5, face = "bold", size = 18), axis.text = element_text(face = "bold"), plot.margin = margin(10, 15, 10, 120))
  })
  output$reference_counts <- renderTable({ ref_data() |> group_by(journal) |> summarise(Reference_Count = sum(n), .groups = "drop") |> arrange(desc(Reference_Count), journal) }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
