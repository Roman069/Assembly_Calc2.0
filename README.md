# Assembly_Calc2.0
A calculator created in Assembly code using the EMU8086 library

The program has the following features: adding,
subtraction, multiplication, division, finding the factorial, elevation to
square, modulo division and finding the root.

The program works with TASM16.

Example of program operation:

; example of calculation:
; input 1 <- number:   10 
; input 2 <- operator: - 
; input 3 <- number:   5 
; ------------------- 
;     10 - 5 = 5 
; output  -> number:   5

; example of calculation:
; input 1 <- number:   10 
; input 2 <- operator: + 
; input 3 <- number:   5 
; ------------------- 
;     10 + 5 = 15 
; output  -> number:   15

; example of calculation:
; input 1 <- number:   10 
; input 2 <- operator: / 
; input 3 <- number:   2 
; ------------------- 
;     10 / 2 = 5 
; output  -> number:   5

; example of calculation:
; input 1 <- number:   7 
; input 2 <- operator: fact 
; ------------------- 
; result = 5040 
; output  -> number:   5040

; example of calculation:
; input 1 <- number:   55 
; input 2 <- operator: square 
; ------------------- 
; result = 3025 
; output  -> number:   3025

; example of calculation:
; input 1 <- number:   177
; input 2 <- operator: % 
; input 3 <- number:   6 
; ------------------- 
;     177 % 6 = 5 
; output  -> number:   3

; example of calculation:
; input 1 <- number:   432 
; input 2 <- operator: sqrt 
; ------------------- 
; result = 20.80 
; output  -> number:   20.80

