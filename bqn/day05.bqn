a←•Flines "../inputs/day05"
c←{(1+4×↕9)⊏𝕩}¨¯1↓(⊑⊔(∾×≢¨a))↑a
m←{<((¬' '⍷𝕩)/𝕩)}˘⍉>c
i←(1+⊑⊔(∾×≢¨a))↓a
SplitBy←{(¬𝕨⍷𝕩)((¬-˜⊢×·+`»⊸>)⊸⊔)𝕩}
moves←' '⊸SplitBy¨i
moves↩{•BQN¨({∨´"0123456789"∊𝕩}¨𝕩)/𝕩}¨moves
_Domove←{
  0⊸=≠𝕩?𝕨;
  count‿from‿to←⊑𝕩
  moving←𝔽count↑(from-1)⊸⊑𝕨
  (moving⊸∾⌾((to-1)⊸⊑) count⊸↓⌾((from-1)⊸⊑) 𝕨) 𝔽_Domove 1↓𝕩
}
•Show ⊑¨m ⌽_Domove moves
•Show ⊑¨m ⊢_Domove moves
