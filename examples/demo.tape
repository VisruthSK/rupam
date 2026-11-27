Output demo.gif

Require rupam

Set Shell "bash"
Set FontSize 18
Set Width 1440
Set Height 900
Set Padding 24
Set Theme "Catppuccin Mocha"
Set TypingSpeed 30ms

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
Sleep 3.5s

Type "penguins |> filter(body_mas > 4000, missing = skip)"
Enter
Sleep 3.5s

Type "let year = 2008"
Enter
Sleep 1s
Type "penguins |> filter(year > 2007)"
Enter
Sleep 4s

Type "penguins |> filter($year > ^year) |> select(species, island, year)"
Enter
Sleep 3.5s

Type "let profile(data, x, g) = data |> summarize(n = n(), average = mean(x), spread = sd(x), by = g, missing = skip)"
Enter
Sleep 1.5s
Type "penguins |> profile(bill_len / bill_dep, island)"
Enter
Sleep 3.5s

Type 'let species_info = read_csv("species.csv")'
Enter
Sleep 2s
Type "let joined = penguins |> left_join(species_info, by = species)"
Enter
Sleep 4s
Type "let joined = penguins |> left_join(species_info, by = species, relationship = many_to_one)"
Enter
Sleep 2.5s

Type ":target python-polars"
Enter
Sleep 3s
Type "joined |> profile(body_mass / 1000.0, genus)"
Enter
Sleep 3.5s

Type ":save analysis.rp"
Enter
Sleep 2s
Ctrl+D
Sleep 1s

Type "cat analysis.rp"
Enter
Sleep 4s
