⎕IO←0
input←⊃⎕NGET'../inputs/day01'1
parsed←⍎¨¨(⊢⊆⍨0≠≢¨)input
p1←⌈/+/¨parsed
p2←+/3↑{⍵[⍒⍵]}+/¨parsed
⎕←p1
⎕←p2
