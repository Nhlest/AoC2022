⎕IO←0
input←{⍵>96:⍵-96⋄⍵-38}¨¨⎕UCS¨¨⊃⎕NGET'../inputs/day03' 1
⎕←+/{(a b)←↓(2,2÷⍨≢⍵)⍴⍵⋄⊃a∩b}¨input ⍝ Part 1
⎕←+/({(a b c)←⍵⋄⊃(a∩b)∩(c∩a)}⍤1)100 3⍴input ⍝ Part 2
