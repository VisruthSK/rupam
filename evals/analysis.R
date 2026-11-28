library(vitals)
library(ellmer)
library(tidyverse)
use("here", "here")

results_dir <- here("evals", "results")
vitals_log_dir_set(here("evals", "logs"))

rds_files <- c(
  R = file.path(results_dir, "r.rds"),
  Rupam = file.path(results_dir, "rupam.rds")
)
tasks <- map(rds_files, readRDS)

# Native Vitals scores and sample metadata.
samples <- vitals_bind(R = tasks$R, Rupam = tasks$Rupam) |>
  unnest(metadata)

metric_names <- c(
  "seconds",
  "input_tokens",
  "output_tokens",
  "thinking_tokens",
  "cached_tokens",
  "total_tokens",
  "tool_calls"
)

# Read both legacy .json streams and new .jsonl streams. A malformed or
# incomplete line is not a result and must not abort the whole analysis.
read_agent_log <- function(path) {
  events <- read_lines(path, skip_empty_rows = TRUE) |>
    str_trim() |>
    discard(\(x) x == "") |>
    map(possibly(\(x) jsonlite::fromJSON(x, simplifyVector = FALSE), NULL)) |>
    keep(is.list)

  final <- keep(events, \(x) identical(x$event, "result"))
  if (length(final) != 1L || !identical(final[[1]]$result$status, "SUCCESS")) {
    return(NULL)
  }
  out <- final[[1]]$result
  if (!is.character(out$response) || length(out$response) != 1L) {
    return(NULL)
  }

  tibble(
    task = if_else(str_starts(basename(path), "R-dplyr-"), "R", "Rupam"),
    result = out$response,
    log_file = path,
    modified = file.info(path)$mtime,
    conversation_id = out$conversation_id %||% NA_character_,
    seconds = out$duration_seconds %||% NA_real_,
    input_tokens = out$usage$input_tokens %||% NA_real_,
    output_tokens = out$usage$output_tokens %||% NA_real_,
    thinking_tokens = out$usage$thinking_tokens %||% NA_real_,
    cached_tokens = out$usage$cache_read_tokens %||% NA_real_,
    total_tokens = out$usage$total_tokens %||% NA_real_,
    tool_calls = sum(map_lgl(events, \(e) {
      identical(e$step_update$step_type, "tool") &&
        identical(e$step_update$state, "DONE")
    }))
  )
}

if (
  "solver_metadata" %in%
    names(samples) &&
    all(map_lgl(samples$solver_metadata, \(x) all(metric_names %in% names(x))))
) {
  # New evaluations: use metadata recorded by Vitals itself.
  results <- samples |>
    select(
      task,
      any_of(c("id", "epoch")),
      input,
      target,
      score,
      result,
      solver_metadata
    ) |>
    unnest_wider(solver_metadata)
} else {
  # Existing evaluations: recover agent metadata by matching saved responses.
  # Do not guess based only on which logs are newest.
  logs <- list.files(
    results_dir,
    "^(R-dplyr|Rupam)-.*\\.jsonl?$",
    full.names = TRUE
  ) |>
    map(read_agent_log) |>
    list_rbind()

  if (nrow(logs) == 0L) {
    stop("No completed Antigravity logs found.", call. = FALSE)
  }

  normalize <- \(x) str_trim(str_replace_all(x, "\\r\\n", "\n"))
  logs <- logs |>
    mutate(response_key = normalize(result)) |>
    select(-result) |>
    filter(modified <= file.info(unname(rds_files[task]))$mtime)

  duplicates <- logs |> count(task, response_key) |> filter(n > 1)
  if (nrow(duplicates) > 0L) {
    warning(
      "Some legacy responses match multiple logs; using the latest before the RDS was saved.",
      call. = FALSE
    )
  }
  logs <- logs |>
    slice_max(modified, n = 1, with_ties = FALSE, by = c(task, response_key))

  results <- samples |>
    select(task, any_of(c("id", "epoch")), input, target, score, result) |>
    mutate(response_key = normalize(result)) |>
    left_join(
      logs,
      by = join_by(task, response_key),
      relationship = "many-to-one"
    ) |>
    select(-response_key, -modified)

  if (anyNA(results$log_file)) {
    warning(
      "Some saved samples have no matching completed log; their timing and token counts are NA.",
      call. = FALSE
    )
  }
}

# Per-sample data and native Vitals accuracy.
print(results |> select(-result, -log_file, -conversation_id), width = Inf)

accuracy <- tibble(
  task = c("R", "Rupam"),
  accuracy = c(
    unname(tasks$R$metrics[["accuracy"]]),
    unname(tasks$Rupam$metrics[["accuracy"]])
  )
)

mean_known <- \(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
median_known <- \(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)

comparison <- results |>
  summarize(
    samples = n(),
    timed_samples = sum(!is.na(seconds)),
    mean_seconds = mean_known(seconds),
    median_seconds = median_known(seconds),
    across(
      all_of(setdiff(metric_names, "seconds")),
      mean_known,
      .names = "mean_{.col}"
    ),
    .by = task
  ) |>
  left_join(accuracy, by = join_by(task)) |>
  relocate(accuracy, .after = samples)

print(comparison, width = Inf)
write_csv(results, file.path(results_dir, "samples.csv"))
write_csv(comparison, file.path(results_dir, "comparison.csv"))

plot_data <- results |>
  select(task, all_of(c("seconds", "total_tokens", "tool_calls"))) |>
  pivot_longer(-task, names_to = "metric", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(
    metric = factor(
      metric,
      levels = c("seconds", "total_tokens", "tool_calls"),
      labels = c("Agent latency (s)", "Total tokens", "Tool calls")
    )
  )

if (nrow(plot_data) > 0L) {
  plot <- ggplot(plot_data, aes(task, value, colour = task)) +
    geom_point(
      position = position_jitter(width = 0.08, height = 0, seed = 0),
      size = 2.5
    ) +
    facet_wrap(vars(metric), scales = "free_y") +
    labs(x = NULL, y = NULL, title = "R/dplyr vs Rupam") +
    theme_minimal() +
    theme(legend.position = "none")

  ggsave(
    file.path(results_dir, "comparison.png"),
    plot,
    width = 9,
    height = 4,
    dpi = 150
  )
}

# Open the native Inspect log viewer from an interactive R session.
if (interactive()) {
  vitals_view()
}
