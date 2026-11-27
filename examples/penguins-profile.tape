# From the repository root, with ttyd and native ffmpeg.exe on PATH:
# & (mise which vhs) examples/penguins-profile.tape
Output "_build/penguins-profile.mp4"
Require rupam
Set Shell pwsh
Set Width 1260
Set Height 820
Set FontSize 18
Set Padding 20
Set TypingSpeed 24ms
Set WaitTimeout 20s

Hide
Type "Set-Location examples"
Enter
Type "New-Item -ItemType Directory -Force ../_build | Out-Null"
Enter
Ctrl+L
Show

Type "Get-Content penguins-profile.rp"
Enter
Sleep 5s

Hide
Type "Clear-Host"
Enter
Show

Type "rupam repl penguins-profile.rp"
Enter
Wait+Screen /rupam ❯/
Type ":bindings"
Enter
Wait+Screen /profile : .*predicate/
Sleep 3s

Type "penguins |> profile(year >= 2008, body_mass / 1000.0, species, sex)"
Enter
Wait+Screen /6 rows × 6 columns/
Sleep 4s

Type "penguins |> profile(body_mass > 3500, bill_len / bill_dep, species, island)"
Enter
Wait+Screen /5 rows × 6 columns/
Sleep 4s

Type `penguins |> profile(sex == "female", flipper_len, species, year)`
Enter
Wait+Screen /9 rows × 6 columns/
Sleep 6s
