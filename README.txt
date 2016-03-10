README -- LambdaReducer.hs

--------------------------

Features:
-Performs proper alpha reduction techniques, then applies beta reduction until it cannot, then applies eta reduction until it cannot.

-Returns a fully reduced lambda expression as computed using applicative ordering techniques.

--------------------------

Potential Bugs:
-Alpha renaming will produce an alpha equivalent expression to that of the expected output in the assignment description. There is only the ability for 26 total variable names, I could make it do more but I figured that that was not the purpose of the assignment.
	-Previously, I had implemented it so that it just adds a number to the end of the variable to be renamed. That may be safer, but this looks better. If theres something wrong with the alpha renaming, such as renaming something to an already used variable - that is easily fixed by changing <Atom (pickFreshVar (allVars e))> in line 52 with <Atom (x ++ "1")>. This now appends a 1 to the end of variables when renaming, creating NEW variable names and not just trying to find one that's not been used previously.
