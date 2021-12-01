#!/usr/bin/env fish
argparse --name=setupDay 'd/day=' -- $argv
or exit

set -l day $_flag_day

if not string match --quiet --regex '^\d{2,}$' $day
    echo $day must be a 2 digit number
    exit 1
end

set -l dayStr Day$day

# easy way to remove starting 0 if needed
set -l dayNum (math $day)

if test -d ./app/$dayStr
    echo "Day already exists"
    exit 1
end

cp -r ./_dayTemplate ./app/$dayStr

sed -i "s/DayXX/$dayStr/" ./app/$dayStr/Mod.hs

sed -i "s/other-modules:/other-modules:\n        $dayStr.Mod/" ./adventOfCode2021.cabal

sed -i "s/-- Add day import/import qualified $dayStr.Mod as $dayStr (dispatch)\n-- Add day import/" ./app/Main.hs

sed -i "s/-- Add day dispatch/,($dayNum, $dayStr.dispatch)\n    -- Add day dispatch/" ./app/Main.hs

echo format Main.hs