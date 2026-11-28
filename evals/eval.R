library(vitals)
library(ellmer)
library(processx)
library(jsonlite)
library(tibble)
use("here", "here")

vitals::vitals_log_dir_set(here("evals", "logs"))

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
    chats <- lapply(inputs, function(input) {
      wd <- tempfile()
      dir.create(wd)
      if (language == "Rupam") {
        dir.create(file.path(wd, "docs"))
        stopifnot(file.copy(
          here("docs", c("language.md", "tables.md", "repl.md")),
          file.path(wd, "docs")
        ))
        stopifnot(file.copy(
          here("docs", "spec", "agent-interface.md"),
          file.path(wd, "docs")
        ))
        stopifnot(file.copy(here("evals", "rupam.exe"), wd))
      }

      writeLines(
        c("id,x", "1,10", "2,", "3,30"),
        file.path(wd, "data.csv")
      )

      prompt <- paste(
        "Use only",
        language,
        "for the analysis.",
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
        "Execute your code.",
        input,
        "Finish with ANSWER: followed by the number."
      )

      run <- processx::run(
        "agy",
        c(
          "-p",
          prompt,
          "--model",
          "gemini-4-hydrogen-high",
          "--output-format",
          "stream-json",
          "--sandbox"
        ),
        wd = wd,
        timeout = 60000,
        echo = TRUE,
        echo_cmd = TRUE,
        error_on_status = FALSE,
        env = c(
          "current",
          PATH = paste(wd, Sys.getenv("PATH"), sep = .Platform$path.sep)
        )
      )

      writeLines(
        run$stdout,
        tempfile(
          pattern = paste0(gsub("/", "-", language), "-"),
          tmpdir = here("evals", "results"),
          fileext = ".json"
        )
      )
      cat(run$stderr, file = stderr())
      stopifnot(run$status == 0)

      events <- lapply(
        strsplit(trimws(run$stdout), "\n")[[1]],
        fromJSON,
        simplifyVector = FALSE
      )
      out <- tail(
        Filter(
          function(x) identical(x$event, "result"),
          events
        ),
        1
      )[[1]]$result
      stopifnot(out$status == "SUCCESS")

      # Offline transcript container; no API request.
      ch <- chat_openai_compatible(
        base_url = "http://127.0.0.1:1",
        model = "gemini-4-hydrogen",
        credentials = function() "offline"
      )

      ch$set_turns(list(
        UserTurn(contents = list(ContentText(prompt))),
        AssistantTurn(contents = list(ContentText(out$response)))
      ))

      ch
    })

    list(
      result = vapply(chats, function(x) x$last_turn()@text, ""),
      solver_chat = chats
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
saveRDS(rupam, "results/rupam.rds")

r$eval(view = FALSE)
saveRDS(r, "results/r.rds")

saveRDS(
  list(r = r$metrics, rupam = rupam$metrics),
  "results/metrics.rds"
)
