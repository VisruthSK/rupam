Output demo.mp4

Require rupam

Set Shell "bash"
Set FontSize 24
Set Width 1920
Set Height 1080
Set Padding 24
Set Theme "Catppuccin Mocha"
Set TypingSpeed 30ms
Set Framerate 30

Hide
Type "rupam repl --target r-dplyr"
Enter
Sleep 3s
Show

Type 'let penguins = read_csv("penguins.csv")'
Enter
Sleep 3s

Type "penguins |> summarize(mass = mean(body_mass), by = species)"
Enter
Sleep 4s

Type "penguins |> summarize(mass = mean(body_mass), by = species, missing = skip)"
Enter
Sleep 3s

Type "let year = 2008"
Enter
Type "penguins |> filter(year > 2007)"
Enter
Sleep 3.5s

Type "let profile(data, x, g) = data |> summarize(n = n(), average = mean(x), spread = sd(x), by = g, missing = skip)"
Enter
Sleep 1s
Type "penguins |> profile(bill_len / bill_dep, island)"
Enter
Sleep 3s

Type ":view penguins"
Enter
Sleep 2s
Right
Right
Right
Right
Right
Type "S"
Sleep 1.5s
Type "d"
Sleep 2.5s
PageDown
Sleep 1s
Escape
Sleep 1s

Type ":target python-polars"
Enter
Sleep 2.5s

Type 'let trips = read_parquet("trips.parquet")'
Enter
Sleep 3s

Type "trips |> profile(fare, city)"
Enter
Sleep 3s

Type ":view trips"
Enter
Sleep 2s
PageDown
PageDown
PageDown
Sleep 1s
Type "G"
Sleep 1.5s
Type "g"
Sleep 1s
Right
Right
Right
Type "S"
Sleep 2s
Type "d"
Sleep 2.5s
Escape
Sleep 1s

Type ":save analysis.rp"
Enter
Sleep 2s
Ctrl+D
Sleep 1s
Type "clear && cat analysis.rp"
Enter
Sleep 4s
