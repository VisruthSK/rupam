Output demo.gif

Require rupam

Set Shell "powershell"
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

Type 'penguins |> filter(body_mass > 4500, missing = skip) |> select(species, island, body_mass, sex)'
Enter
Sleep 3.5s

Type 'let size(mass) = match mass { ..<3500 => "small", 3500=..<4500 => "medium", 4500=.. => "large" }'
Enter
Sleep 1.5s

Type 'penguins |> filter(some(body_mass)) |> mutate(size = size(body_mass), sex = sex ?? "unrecorded") |> select(species, body_mass, size, sex)'
Enter
Sleep 4s

Type 'let profile(data, x, g) = data |> summarize(n = n(), average = mean(x), spread = sd(x), by = g, missing = skip)'
Enter
Sleep 1.5s

Type 'penguins |> profile(body_mass / 1000.0, species)'
Enter
Sleep 3.5s

Type 'penguins |> profile(bill_len / bill_dep, island)'
Enter
Sleep 3.5s

Type 'let imputed = penguins |> mutate(mass = body_mass ?? mean(body_mass, missing = skip), by = species)'
Enter
Sleep 2.5s

Type ':code python-polars imputed'
Enter
Sleep 3.5s

Type ':view imputed'
Enter
Sleep 2.5s
Type "?"
Sleep 3s
Type "?"
PageDown
Sleep 1s
Type "G"
Sleep 1.5s
Escape
Sleep 1s

Ctrl+D
Sleep 500ms
