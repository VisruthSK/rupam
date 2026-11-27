Output explore.gif

Require rupam

Set Shell "powershell"
Set FontSize 18
Set Width 1440
Set Height 900
Set Padding 24
Set Theme "Catppuccin Mocha"
Set TypingSpeed 30ms

Hide
Type "rupam repl --target python-polars"
Enter
Sleep 3s
Show

Type 'let penguins = read_csv("penguins.csv")'
Enter
Sleep 3s

Type "penguins |> !bar(species)"
Enter
Sleep 3s

Type "penguins |> !hist(body_mass)"
Enter
Sleep 3.5s

Type "penguins |> !hist(bill_len)"
Enter
Sleep 3.5s

Type ":view penguins"
Enter
Sleep 2.5s

Right
Right
Right
Right
Right
Sleep 500ms
Type "S"
Sleep 2s
Type "d"
Sleep 3s

Left
Left
Left
Left
Left
Down
Down
Type "="
Sleep 2.5s

Tab
Sleep 500ms
Type " |> mutate(ratio = bill_len / bill_dep)"
Sleep 3s
Shift+Tab
Sleep 500ms
Right
Right
Right
Right
Right
Right
Right
Right
Type "S"
Sleep 2.5s
Type "d"
Sleep 3s

Tab
Sleep 500ms
Enter
Sleep 1s
Type "adelie_bills"
Sleep 500ms
Enter
Sleep 2s

Type "adelie_bills |> summarize(n = n(), ratio = mean(ratio), spread = sd(ratio), by = sex, missing = skip, group_missing = skip)"
Enter
Sleep 3.5s

Type "adelie_bills |> !hist(ratio)"
Enter
Sleep 3.5s

Type ":save explore.rp"
Enter
Sleep 1.5s

Type "Get-Content explore.rp"
Sleep 200ms
Ctrl+D
Sleep 500ms
Enter
Sleep 3s
