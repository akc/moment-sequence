#!/run/current-system/sw/bin/fish
for anum in (cat moment-A-numbers.jsonl); hops --prec=20 $anum; end
