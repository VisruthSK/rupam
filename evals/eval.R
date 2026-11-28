library(vitals)
library(ellmer)
library(tidyverse)
use("here", "here")

results_dir <- here("evals", "results")
logs_dir <- here("evals", "logs")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
vitals_log_dir_set(logs_dir)

questions <- tibble(
  input = c(
    "Calculate the mean of x, excluding missing values.",
    "Count the observations with missing x."
  ),
  target = c("20", "1")
)

agy_solver <- function(language) {
  force(language)

  function(inputs, ...) {
    samples <- map(inputs, function(input) {
      wd <- tempfile()
      dir.create(wd)

      if (language == "Rupam") {
        docs <- file.path(wd, "docs")
        dir.create(docs)
        stopifnot(file.copy(
          file.path(here("docs"), c("language.md", "tables.md", "repl.md")),
          docs
        ))
        stopifnot(file.copy(here("docs", "spec", "agent-interface.md"), docs))
        stopifnot(file.copy(here("evals", "rupam.exe"), wd))
      }

      write_lines(c("id,x", "1,10", "2,", "3,30"), file.path(wd, "data.csv"))

      prompt <- paste(
        "Use only", language, "for the analysis.",
        "The dataset is data.csv.",
        if (language == "Rupam") {
          paste(
            "Read docs/agent-interface.md.",
            "Use ./rupam.exe session --target r-dplyr.",
            "Pipe newline-delimited JSON-RPC requests directly into stdin.",
            "Do not implement a client or wrapper.",
            "Use capabilities to discover the available methods.",
            "Every new session starts at revision 0.",
            "The submit method requires source and revision parameters.",
            "Use the revision returned by the previous successful request.",
            "Batch dependent requests in the same session.",
            "Execute the requested analysis."
          )
        },
        "Execute your code.", input,
        "Finish with ANSWER: followed by the number."
      )

      log_file <- tempfile(
        pattern = paste0(str_replace_all(language, "/", "-"), "-"),
        tmpdir = results_dir,
        fileext = ".jsonl"
      )

      run <- processx::run(
        "agy",
        c("-p", prompt, "--model", "gemini-4-hydrogen-high",
          "--output-format", "stream-json", "--sandbox"),
        wd = wd,
        timeout = as.difftime(180, units = "secs"),
        echo = TRUE,
        echo_cmd = TRUE,
        error_on_status = FALSE,
        stdout_line_callback = function(line, process) {
          write_lines(line, log_file, append = TRUE)
        },
        env = c("current", PATH = paste(wd, Sys.getenv("PATH"), sep = .Platform$path.sep))
      )

      # Keep the full stream, including a partial stream if the agent times out.
      write_lines(run$stdout, log_file)
      if (isTRUE(run$timeout) || !isTRUE(run$status == 0L)) {
        stop("Antigravity failed; see ", log_file, call. = FALSE)
      }

      events <- read_lines(log_file, skip_empty_rows = TRUE) |>
        str_trim() |>
        discard(\(x) x == "") |>
        map(possibly(\(x) jsonlite::fromJSON(x, simplifyVector = FALSE), NULL)) |>
        keep(is.list)

      final <- keep(events, \(x) identical(x$event, "result"))
      if (length(final) != 1L || !identical(final[[1]]$result$status, "SUCCESS")) {
        stop("No successful final result in ", log_file, call. = FALSE)
      }
      out <- final[[1]]$result
      stopifnot(is.character(out$response), length(out$response) == 1L)

      # This chat stores the transcript only; it never makes a model API call.
      chat <- chat_openai_compatible(
        base_url = "http://127.0.0.1:1",
        model = "gemini-4-hydrogen-high",
        credentials = function() "offline"
      )
      chat$set_turns(list(
        UserTurn(contents = list(ContentText(prompt))),
        AssistantTurn(contents = list(ContentText(out$response)))
      ))

      list(
        result = out$response,
        chat = chat,
        metadata = list(
          conversation_id = out$conversation_id,
          log_file = log_file,
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
      )
    })

    list(
      result = map_chr(samples, "result"),
      solver_chat = map(samples, "chat"),
      solver_metadata = map(samples, "metadata")
    )
  }
}

r <- Task$new(
  dataset = questions,
  solver = agy_solver("R/dplyr"),
  scorer = detect_answer(),
  name = "hydrogen-r",
  epochs = 1
)

rupam <- Task$new(
  dataset = questions,
  solver = agy_solver("Rupam"),
  scorer = detect_answer(),
  name = "hydrogen-rupam",
  epochs = 1
)

rupam$eval(view = FALSE)
saveRDS(rupam, file.path(results_dir, "rupam.rds"))

r$eval(view = FALSE)
saveRDS(r, file.path(results_dir, "r.rds"))

saveRDS(
  list(r = r$metrics, rupam = rupam$metrics),
  file.path(results_dir, "metrics.rds")
)
