# hw01
RPI Programming Languages Fall 2023

Group Members: 
Yifu Liu 
Zuriel Aviles

functionalities: 

alphaRename: 
This function replaces all occurrences of a specific variable with a new expression.

betaReduction: 
This functionsubstitutes an argument expression for a bound variable in the body of a lambda abstraction.

EtaReduction: 
This function simplifies expressions of the form \x -> f x to f, where x is not free in f.

Reducer: 
This function is the main reducer. It applies beta reduction, eta conversion, and simplification as necessary.

