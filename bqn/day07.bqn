input←•FLines "../inputs/day07"
Cd←{pwd 𝕊 dir:{"/":{⟨⟩};"..":{¯1↓pwd};pwd∾<dir}dir}
SplitBy←{(¬𝕨⍷𝕩)((¬-˜⊢×·+`»⊸>)⊸⊔)𝕩}
proc←⟨⟩
Process←{pwd 𝕊 ⟨⟩:⟨⟩;
         pwd 𝕊 ins:{
             x←⊑ins⋄xs←1↓ins⋄newpwd←{
               "$ cd":(pwd Cd 5↓x);
               "$ ls":pwd;
               "dir ":{proc↩proc∾<(pwd∾(0))⋄pwd};
               size←⊑' ' SplitBy x⋄proc↩proc∾<(pwd∾(•BQN size))⋄pwd
             }4↑x⋄newpwd Process xs}
}
⟨⟩ Process input
Get←{+´⊑¨¯1↑¨((∨´¨𝕩⊸≡¨¨↑¨proc)/proc)}
sizes←Get¨⍷¯1⊸↓¨proc
•Show +´{𝕩≤100000?𝕩;0}¨sizes
•Show ⌊´{𝕩>40000000-˜⌈´sizes?𝕩;∞}¨sizes