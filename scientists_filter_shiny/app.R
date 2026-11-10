library(shiny)
library(DT)

# These compact data files are bundled with the app. Both source workbooks are
# 2025 editions: career.xlsx and career (2).xlsx (single-year ranking).
data_candidates <- c(
  "data",
  "F:/submissionTO/2510top2%scientistsinscopus/scientists_filter_shiny/data"
)
valid_data_dir <- vapply(data_candidates, function(path) {
  file.exists(file.path(path, "career_2025.csv.gz")) &&
    file.exists(file.path(path, "singleyr_2025.csv.gz"))
}, logical(1))
if (!any(valid_data_dir)) {
  stop("Data files not found. Run shiny::runApp('F:/submissionTO/2510top2%scientistsinscopus/scientists_filter_shiny') or keep app.R beside its data/ folder.")
}
data_dir <- data_candidates[which(valid_data_dir)[1]]
career_data <- read.csv(gzfile(file.path(data_dir, "career_2025.csv.gz")),
                        check.names = FALSE, stringsAsFactors = FALSE)
singleyr_data <- read.csv(gzfile(file.path(data_dir, "singleyr_2025.csv.gz")),
                          check.names = FALSE, stringsAsFactors = FALSE)

shared_metrics <- c("nps (ns)", "ncs (ns)", "cpsf (ns)", "ncsf (ns)",
                    "npsfl (ns)", "ncsfl (ns)", "c (ns)")
fields <- c("sm-subfield-1", "cntry", "inst_name")

name_key <- function(x) {
  gsub("[^a-z0-9]", "", tolower(trimws(as.character(x))))
}

parse_logic <- function(expression, terms) {
  expression <- gsub("[[:space:]]", "", expression)
  if (!grepl("^[123](\\|[123])*(/[123](\\|[123])*)*$", expression)) {
    stop("Use 1, 2, and 3 with | for AND and / for OR, such as 1|2|3 or 1|2/3.")
  }
  active <- nzchar(trimws(terms))
  groups <- strsplit(expression, "/", fixed = TRUE)[[1]]
  group_ids <- lapply(groups, function(group) {
    ids <- as.integer(strsplit(group, "|", fixed = TRUE)[[1]])
    ids[active[ids]]
  })
  group_ids <- Filter(length, group_ids)
  if (!length(group_ids)) stop("Enter at least one nonblank search term.")
  effective <- paste(vapply(group_ids, function(ids)
    paste(ids, collapse = "|"), character(1)), collapse = "/")
  list(groups = group_ids, effective = effective, active = active)
}

# Within any one term, | requires all phrases and / accepts any phrase.
matches_term <- function(values, query) {
  values <- tolower(as.character(values))
  values[is.na(values)] <- ""
  or_groups <- strsplit(query, "/", fixed = TRUE)[[1]]
  result <- rep(FALSE, length(values))
  for (group in or_groups) {
    phrases <- trimws(strsplit(group, "|", fixed = TRUE)[[1]])
    phrases <- phrases[nzchar(phrases)]
    if (!length(phrases)) next
    group_result <- rep(TRUE, length(values))
    for (phrase in phrases) {
      group_result <- group_result & grepl(tolower(phrase), values,
                                          fixed = TRUE)
    }
    result <- result | group_result
  }
  result
}

filter_one <- function(data, terms, logic) {
  hits <- lapply(seq_along(fields), function(id) {
    if (logic$active[id]) matches_term(data[[fields[id]]], terms[id])
    else rep(FALSE, nrow(data))
  })
  keep <- rep(FALSE, nrow(data))
  for (ids in logic$groups) {
    group_keep <- rep(TRUE, nrow(data))
    for (id in ids) group_keep <- group_keep & hits[[id]]
    keep <- keep | group_keep
  }
  data[keep, , drop = FALSE]
}

format_one <- function(data, kind) {
  first_metrics <- if (kind == "career")
    c("nc9625 (ns)", "h25 (ns)", "hm25 (ns)")
  else c("nc2525 (ns)", "h25 (ns)", "hm25 (ns)")
  result <- data[, c("authfull", first_metrics, shared_metrics,
                     "inst_name", "cntry"), drop = FALSE]
  names(result)[c(1, 12, 13)] <- c("name", "unit", "country")
  rownames(result) <- NULL
  result
}

format_both <- function(career, singleyr) {
  career_keys <- name_key(career$authfull)
  singleyr_keys <- unique(name_key(singleyr$authfull))
  keep <- nzchar(career_keys) & career_keys %in% singleyr_keys &
    !duplicated(career_keys)
  matched_career <- career[keep, , drop = FALSE]
  common_names <- c("name", "nc (ns)", "h (ns)", "hm (ns)", shared_metrics,
                    "metric_source", "unit", "country")
  result <- format_one(matched_career, "career")
  result$metric_source <- rep("career: nc9625/h25/hm25", nrow(result))
  result <- result[, c(1:11, 14, 12:13), drop = FALSE]
  names(result) <- common_names
  result
}

make_institute_country_table <- function(filtered) {
  if (!nrow(filtered)) {
    return(data.frame(institute = "Total", Total = 0L, Nation = "All",
                      check.names = FALSE))
  }
  institute <- trimws(as.character(filtered$unit))
  country <- trimws(as.character(filtered$country))
  institute[is.na(institute) | !nzchar(institute)] <- "(Missing institute)"
  country[is.na(country) | !nzchar(country)] <- "(Missing country)"
  institute_label <- paste0(institute, " (", country, ")")
  counts <- as.matrix(table(institute_label, country))
  counts <- cbind(counts, Total = rowSums(counts))
  counts <- counts[order(-counts[, "Total"], rownames(counts)), , drop = FALSE]
  counts <- rbind(counts, Total = colSums(counts))
  result <- data.frame(institute = rownames(counts), counts,
                       check.names = FALSE, row.names = NULL)
  nation_names <- c(twn = "Taiwan", chn = "China", jpn = "Japan", kor = "Korea")
  country_by_institute <- setNames(country, institute_label)
  nation_code <- unname(country_by_institute[result$institute])
  result$Nation <- ifelse(is.na(nation_code), nation_code,
                          ifelse(nation_code %in% names(nation_names),
                                 unname(nation_names[nation_code]), nation_code))
  result$Nation[result$institute == "Total"] <- "All"
  result
}

ui <- fluidPage(
  titlePanel("2025 Scientist Filter and Institute × Country Table"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Dataset option",
                  choices = c("Career (2025)" = "career",
                              "Single-year (2025)" = "singleyr",
                              "Both, names matching in both datasets" = "both"),
                  selected = "both"),
      textInput("discipline", "1 · Discipline (sm-subfield-1)",
                value = "Gastroenterology & Hepatology",
                placeholder = "Blank = any discipline"),
      textInput("nation", "2 · Nation (cntry)", value = "twn / chn / jpn / kor"),
      textInput("institute", "3 · Institute involvement (inst_name)",
                value = "Hospital / Medical Center"),
      textInput("expression", "Combine fields", value = "1|2|3"),
      helpText("| means AND; / means OR. The default institute term accepts Hospital or Medical Center. Blank terms are skipped."),
      actionButton("apply", "Generate tables", class = "btn-primary"),
      tags$hr(),
      downloadButton("download_filtered", "Download filtered scientists CSV"),
      tags$br(), tags$br(),
      downloadButton("download_summary", "Download institute × country CSV")
    ),
    mainPanel(
      verbatimTextOutput("annotation"),
      tabsetPanel(
        tabPanel("Filtered scientists", DTOutput("filtered_table")),
        tabPanel("Institute × country", DTOutput("summary_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  result <- eventReactive(input$apply, {
    tryCatch({
      terms <- c(input$discipline, input$nation, input$institute)
      logic <- parse_logic(input$expression, terms)
      career <- if (input$dataset %in% c("career", "both"))
        filter_one(career_data, terms, logic) else NULL
      singleyr <- if (input$dataset %in% c("singleyr", "both"))
        filter_one(singleyr_data, terms, logic) else NULL
      filtered <- switch(input$dataset,
        career = format_one(career, "career"),
        singleyr = format_one(singleyr, "singleyr"),
        both = format_both(career, singleyr)
      )
      list(filtered = filtered,
           summary = make_institute_country_table(filtered),
           effective = logic$effective,
           career_n = if (is.null(career)) NA_integer_ else nrow(career),
           singleyr_n = if (is.null(singleyr)) NA_integer_ else nrow(singleyr),
           option = input$dataset,
           terms = terms,
           error = NULL)
    }, error = function(e) list(error = conditionMessage(e)))
  }, ignoreNULL = FALSE)

  output$annotation <- renderText({
    x <- result()
    if (!is.null(x$error)) return(paste("Filter error:", x$error))
    term_labels <- ifelse(nzchar(trimws(x$terms)), x$terms, "(any)")
    counts <- if (x$option == "both")
      paste0("Career matches: ", x$career_n,
             "; single-year matches: ", x$singleyr_n,
             "; names matching in both: ")
    else ""
    nation_counts <- table(factor(tolower(trimws(x$filtered$country)),
                                  levels = c("twn", "chn", "jpn", "kor")))
    by_nation <- paste(names(nation_counts), as.integer(nation_counts),
                       sep = ": ", collapse = "; ")
    paste0("Option: ", x$option, " | Effective expression: ", x$effective,
           "\n1 Discipline: ", term_labels[1],
           " | 2 Nation: ", term_labels[2],
           " | 3 Institute: ", term_labels[3],
           "\n", counts, "Unique scientists: ", nrow(x$filtered),
           " | Country totals: ", by_nation,
           ". The institute table has a Total row and Total count column.")
  })

  output$filtered_table <- renderDT({
    x <- result()
    validate(need(is.null(x$error), x$error))
    datatable(x$filtered, rownames = FALSE,
              options = list(pageLength = 100, scrollX = TRUE, order = list()))
  }, server = TRUE)

  output$summary_table <- renderDT({
    x <- result()
    validate(need(is.null(x$error), x$error))
    datatable(x$summary, rownames = FALSE,
              options = list(pageLength = 100, scrollX = TRUE, ordering = FALSE))
  }, server = TRUE)

  output$download_filtered <- downloadHandler(
    filename = function() paste0("scientists_", result()$option, "_filtered.csv"),
    content = function(file) {
      x <- result()
      req(is.null(x$error))
      write.csv(x$filtered, file, row.names = FALSE, na = "",
                fileEncoding = "UTF-8")
    }
  )

  output$download_summary <- downloadHandler(
    filename = function() paste0("institute_country_", result()$option, ".csv"),
    content = function(file) {
      x <- result()
      req(is.null(x$error))
      write.csv(x$summary, file, row.names = FALSE, na = "",
                fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
