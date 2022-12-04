input←⊃⎕NGET'../inputs/day04' 1
input←{{a b←⍵⋄a↓⍳(b+1)}¨{⍎¨⍵⊆⍨~'-'⍷⍵}¨⍵⊆⍨~','⍷⍵}¨input
⎕←+/∧/↑×¨{a b←⍵⋄l←a∩b⋄((≢b~l) (≢a~l))}¨input
⎕←+/{×≢¨∩/⍵}¨input
