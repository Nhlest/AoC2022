⎕IO←0
input←⊃⎕NGET'../inputs/day02' 1
i←(⊂0 2)⌷⍤1 ⎕UCS¨↑input                           
i[;0]-←65                                         
i[;1]-←88                                         
⎕←+/{(a b)←⍵⋄(a{1+b+3×3|(1+⍵-⍺)}b)}¨↓i ⍝ Part 1
⎕←+/{(a b)←⍵⋄c←(a{3|(⍺-⍵)}b)⋄a{1+c+3×3|(1+⍵-⍺)}c}¨↓i ⍝ Part 2
