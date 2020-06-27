name "calc2"

; this macro prints a char in AL and advances
; the current cursor position:
PUTC    MACRO   char
        PUSH    AX
        MOV     AL, char
        MOV     AH, 0Eh
        INT     10h     
        POP     AX
ENDM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

org 100h  

jmp header
jmp start 
;jmp ask55

; define variables:  
     ;
star1 db '                      ----- Good morning! -----     $'
star2 db 0dh,0ah,0dh,0ah,'            ----My program has the following capabilities----    $' 

msg11:    db 0dh,0ah, 0dh,0ah,"1-Simple operations",0dh,0ah,"2-Factorial",0dh,0ah,"3-Square (x^2)",0dh,0ah,"4-Divide", 0Dh,0Ah,"5-Sqrt()", 0Dh,0Ah, '$'
msg1 db 0Dh,0Ah, 0Dh,0Ah, 'enter first number: $'
msg2 db "enter the operator:    +  -  *  /     : $"
msg3 db "enter second number: $"
msg4 db  0dh,0ah , 'result of my calculations is : $' 
msg5 db  0dh,0ah ,'thank you for using the calculator! press any key... ', 0Dh,0Ah, '$'
err1 db  "wrong operator!", 0Dh,0Ah , '$'
smth db  " and something.... $"

opr db '?'    ; operator can be: '+','-','*','/' or 'q' to exit in the middle.
num1 dw ?    ; first and second number:
num2 dw ?  
num3 dw ?
;************************ On qadro ************************  
printX DB  0Dh,0Ah, 0Dh,0Ah,'enter number:   ',"$"  
printResult db 13,'result of my calculations is :   $ ' 
null db 13,10, '$'      
INAREA DB 6,7 DUP(?)  
x    dd ? 
y    dd ?  
;************************ On qadro ************************ 

 ; ************************ On Module ************************
E dw ?
F dw ? 
G dw ?

s4 db 0dh,0ah,'Press % to mod $'
t db 0dh,0ah,'Want to calculate again? : $'
s1 db 0dh,0ah,0dh,0ah,'Enter a number: $'
t1 db 0dh,0ah,0dh,0ah,'Enter another number: $' 

err db 0dh,0ah,'Wrong input! Start from the beginning $'
err3 db 0dh,0ah, 0dh,0ah, 'Out of range !$' 

r db 0dh,0ah,0dh,0ah,'the result is : $'
nl db 0dh,0ah,'$'

 ; ************************ On Module ************************
 ; ************************ On Sqrt ************************

readMsg db "Enter to number SquareRoot:$"
overflowMsg db "The number must be lower than 65534!$"   
resultMsg db "Average SquareRoot: $"   
   
stackCacheLevel1 dw 0
stackCacheLevel2 dw 0
inputArray db 6 DUP(?)
precisionFactor dw 100   
       
 ; ************************ On Sqrt ************************      
 ; ************************ On Fact ************************
 resultats dw ?
 ; ************************ On Fact ************************

header:
    lea dx,star1
     mov ah,9
    int 21h
    lea dx,star2
    int 21h
start:    
    mov ah,9
    mov dx, offset msg11 
    int 21h
    mov ah,0                       
    int 16h      
    cmp al,31h 
    je simple_operations
    cmp al,32h
    je Factorus
    cmp al,33h
    je STR
    cmp al,34h
    je Modules
    cmp al,35h
    je startsqrt     
  ;  mov dx, offset msg11
  ;  mov ah, 9
 ;   int 21h 
ask5:      
    lea dx,t
    mov ah,9       
    int 21h     
simple_operations:  
    lea dx, msg1
    mov ah, 09h    ; output string at ds:dx
    int 21h  
; get the multi-digit signed number
; from the keyboard, and store
; the result in cx register:
    call scan_num
; store first number:
    mov num1, cx 
; new line:
    putc 0Dh
    putc 0Ah
    lea dx, msg2
    mov ah, 09h     ; output string at ds:dx
    int 21h  
; get operator:
    mov ah, 1   ; single char input to AL.
    int 21h
    mov opr, al
; new line:
    putc 0Dh
    putc 0Ah

    cmp opr, 'q'      ; q - exit in the middle.
    je exit

    cmp opr, '*'
    jb wrong_opr
    cmp opr, '/'
    ja wrong_opr
; output of a string at ds:dx
    lea dx, msg3
    mov ah, 09h
    int 21h  
; get the multi-digit signed number
; from the keyboard, and store
; the result in cx register:
    call scan_num        
; store second number:
    mov num2, cx 
    lea dx, msg4
    mov ah, 09h      ; output string at ds:dx
    int 21h  
; calculate:
    cmp opr, '+'
    je do_plus
    
    cmp opr, '-'
    je do_minus

    cmp opr, '*'
    je do_mult

    cmp opr, '/'
    je do_div
; none of the above....
wrong_opr:
    lea dx, err1
    mov ah, 09h     ; output string at ds:dx
    int 21h  
    call scan_num
exit:
; output of a string at ds:dx
    lea dx, msg5
    mov ah, 09h
    int 21h  
; wait for any key...
    mov ah, 0
    int 16h
    jmp start
ret  ; return back to os.

do_plus:
    mov ax, num1
    add ax, num2
    call print_num    ; print ax value.
    jmp exit
do_minus:
    mov ax, num1
    sub ax, num2
    call print_num    ; print ax value.
    jmp exit
do_mult:
    mov ax, num1
    imul num2 ; (dx ax) = ax * num2. 
    call print_num    ; print ax value.
; dx is ignored (calc works with tiny numbers only).
    jmp exit
do_div:
; dx is ignored (calc works with tiny integer numbers only).
    mov dx, 0
    mov ax, num1
    idiv num2         ; ax = (dx ax) / num2.
    cmp dx, 0
    jnz approx
    call print_num    ; print ax value.
    jmp exit
approx:
    call print_num    ; print ax value.
    lea dx, smth
    mov ah, 09h       ; output string at ds:dx
    int 21h  
    jmp exit  
do_quadrat:
    mov ax, num1
    imul num2 ; (dx ax) = ax * num2. 
    call print_num    ; print ax value.
; dx is ignored (calc works with tiny numbers only).
    jmp exit           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; these functions are copied from emu8086.inc ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; gets the multi-digit SIGNED number from the keyboard,
; and stores the result in CX register:
SCAN_NUM        PROC    NEAR
        PUSH    DX
        PUSH    AX
        PUSH    SI        
        MOV     CX, 0
        ; reset flag:
        MOV     CS:make_minus, 0
next_digit:
        ; get char from keyboard
        ; into AL:
        MOV     AH, 00h
        INT     16h
        ; and print it:
        MOV     AH, 0Eh
        INT     10h
        ; check for MINUS:
        CMP     AL, '-'
        JE      set_minus
        ; check for ENTER key:
        CMP     AL, 0Dh  ; carriage return?
        JNE     not_cr
        JMP     stop_input
not_cr:
        CMP     AL, 8                   ; 'BACKSPACE' pressed?
        JNE     backspace_checked
        MOV     DX, 0                   ; remove last digit by
        MOV     AX, CX                  ; division:
        DIV     CS:ten                  ; AX = DX:AX / 10 (DX-rem).
        MOV     CX, AX
        PUTC    ' '                     ; clear position.
        PUTC    8                       ; backspace again.
        JMP     next_digit
backspace_checked:
        ; allow only digits:
        CMP     AL, '0'
        JAE     ok_AE_0
        JMP     remove_not_digit
ok_AE_0:        
        CMP     AL, '9'
        JBE     ok_digit
remove_not_digit:       
        PUTC    8       ; backspace.
        PUTC    ' '     ; clear last entered not digit.
        PUTC    8       ; backspace again.        
        JMP     next_digit ; wait for next input.       
ok_digit:
        ; multiply CX by 10 (first time the result is zero)
        PUSH    AX
        MOV     AX, CX
        MUL     CS:ten                  ; DX:AX = AX*10
        MOV     CX, AX
        POP     AX
        ; check if the number is too big
        ; (result should be 16 bits)
        CMP     DX, 0
        JNE     too_big
        ; convert from ASCII code:
        SUB     AL, 30h
        ; add AL to CX:
        MOV     AH, 0
        MOV     DX, CX      ; backup, in case the result will be too big.
        ADD     CX, AX
        JC      too_big2    ; jump if the number is too big.
        JMP     next_digit
set_minus:
        MOV     CS:make_minus, 1
        JMP     next_digit
too_big2:
        MOV     CX, DX      ; restore the backuped value before add.
        MOV     DX, 0       ; DX was zero before backup!
too_big:
        MOV     AX, CX
        DIV     CS:ten  ; reverse last DX:AX = AX*10, make AX = DX:AX / 10
        MOV     CX, AX
        PUTC    8       ; backspace.
        PUTC    ' '     ; clear last entered digit.
        PUTC    8       ; backspace again.        
        JMP     next_digit ; wait for Enter/Backspace.        
stop_input:
        ; check flag:
        CMP     CS:make_minus, 0
        JE      not_minus
        NEG     CX
not_minus:
        POP     SI
        POP     AX
        POP     DX
        RET
make_minus      DB      ?       ; used as a flag.
SCAN_NUM        ENDP
; this procedure prints number in AX,
; used with PRINT_NUM_UNS to print signed numbers:
PRINT_NUM       PROC    NEAR
        PUSH    DX
        PUSH    AX
        CMP     AX, 0
        JNZ     not_zero
        PUTC    '0'
        JMP     printed
not_zero:
        ; the check SIGN of AX,
        ; make absolute if it's negative:
        CMP     AX, 0
        JNS     positive
        NEG     AX    
        PUTC    '-'
positive:
        CALL    PRINT_NUM_UNS
printed:
        POP     AX
        POP     DX
        RET
PRINT_NUM       ENDP
; this procedure prints out an unsigned
; number in AX (not just a single digit)
; allowed values are from 0 to 65535 (FFFF)
PRINT_NUM_UNS   PROC    NEAR
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
       ; flag to prevent printing zeros before number:
        MOV     CX, 1
        ; (result of "/ 10000" is always less or equal to 9).
        MOV     BX, 10000       ; 2710h - divider.
        ; AX is zero?
        CMP     AX, 0
        JZ      print_zero
begin_print:
        ; check divider (if zero go to end_print):
        CMP     BX,0
        JZ      end_print
        ; avoid printing zeros before number:
        CMP     CX, 0
        JE      calc
        ; if AX<BX then result of DIV will be zero:
        CMP     AX, BX
        JB      skip
calc:
        MOV     CX, 0   ; set flag.
        MOV     DX, 0
        DIV     BX      ; AX = DX:AX / BX   (DX=remainder).
        ; print last digit
        ; AH is always ZERO, so it's ignored
        ADD     AL, 30h    ; convert to ASCII code.
        PUTC    AL
        MOV     AX, DX  ; get remainder from last div.

skip:
        ; calculate BX=BX/10
        PUSH    AX
        MOV     DX, 0
        MOV     AX, BX
        DIV     CS:ten  ; AX = DX:AX / 10   (DX=remainder).
        MOV     BX, AX
        POP     AX
        JMP     begin_print        
print_zero:
        PUTC    '0'       
end_print:
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
PRINT_NUM_UNS   ENDP

ten DW  10      ; used as multiplier/divider by SCAN_NUM & PRINT_NUM_UNS.

GET_STRING      PROC    NEAR
        PUSH    AX
        PUSH    CX
        PUSH    DI
        PUSH    DX
        
        MOV     CX, 0                   ; char counter.
        CMP     DX, 1                   ; buffer too small?
        JBE     empty_buffer            ;
        DEC     DX                      ; reserve space for last zero.
;============================
; Eternal loop to get
; and processes key presses:
wait_for_key:
        MOV     AH, 0                   ; get pressed key.
        INT     16h
        CMP     AL, 0Dh                  ; 'RETURN' pressed?
        JZ      exit_GET_STRING
        CMP     AL, 8                   ; 'BACKSPACE' pressed?
        JNE     add_to_buffer
        JCXZ    wait_for_key            ; nothing to remove!
        DEC     CX
        DEC     DI
        PUTC    8                       ; backspace.
        PUTC    ' '                     ; clear position.
        PUTC    8                       ; backspace again.
        JMP     wait_for_key
add_to_buffer:
        CMP     CX, DX          ; buffer is full?
        JAE     wait_for_key    ; if so wait for 'BACKSPACE' or 'RETURN'...
        MOV     [DI], AL
        INC     DI
        INC     CX
   ; print the key:
        MOV     AH, 0Eh
        INT     10h
        JMP     wait_for_key
;============================

exit_GET_STRING:

; terminate by null:
MOV     [DI], 0             

;============================
empty_buffer:
POP     DX
POP     DI
POP     CX
POP     AX    
RET         
;_____________________________________  
;********************************;********************************
GET_STRING      ENDP

STR:    
    Mov dx, offset printX
    CALL print_pereryvanna
    CALL function_bars
    mov ax,y 
    mov x,ax      
    mov dx, offset printResult
    CALL print_pereryvanna
    call function
    CALL PrintLnAX
    mov ah,9
    lea dx, null
    int 21h
    jmp exit   
    jmp start       
;********************************   
print_pereryvanna PROC NEAR
    mov ah,09h
    int 21h  
    ret
print_pereryvanna ENDP
;********************************
function_bars PROC NEAR  
    mov     cx,0
    mov     ah,0ah
    xor      di,di
    mov     dx,offset INAREA          
    int       21h                                
    mov     dl,0ah
    mov     ah,02
    int       21h                                      
    mov     si,offset INAREA+2                       
    cmp     byte ptr [si],"-"                      
    jnz      NoSetFlag
    mov     di,1                                         
    inc      si                                           
NoSetFlag:
    xor      ax,ax
    mov     bx,10                                       
NextChat:
    mov     cl,[si]                                      
    cmp     cl,0dh                                       
    jz        ENDCHAR
    sub      cl,'0'                                        
    mul     bx                                         
    add     ax,cx                                        
    inc      si                                             
    jmp     NextChat                                            
ENDCHAR:
    cmp     di,1                                          
    jnz      NoNegative
    neg     ax                                            
NoNegative: 
    mov y,ax 
ret
function_bars ENDP 
;********************************
function PROC NEAR  
    mov ax,x      ;x
    mul ax        ;^2        
    mov y,ax   
    mov ax,y  
  ;  cmp x,0ah     
ret 
function ENDP    
PrintLnAX PROC NEAR
    mov ax,y  
    test       ax, ax
    jns       NoNegative1: 
    mov      cx, ax
    mov     ah, 02h
    mov     dl, '-'
    int     21h
    mov     ax, cx
    neg     ax
    NoNegative1: 
    xor     cx, cx
    mov     bx, 10   
    NoZero:
    xor     dx,dx
    div     bx
    push    dx
    inc     cx
    test    ax, ax
    jnz     NoZero
    mov     ah, 02h
    NoLost:
    pop     dx
    add     dl, '0'
    int       21h
    loop    NoLost    
ret 
PrintLnAX ENDP
;********************************;******************************** 
;END
Modules:  
    
common_input:  
    lea dx,s1
    mov ah,9
    int 21h    
indec proc       
    push bx
    push cx
    push dx     
begin: 
    xor bx,bx      ;holds total    
    xor cx,cx       ;sign                      
    mov ah,1        ;char in al
    int 21h
    
    repeat:   
    cmp al,48
    jl error
    cmp al,57
    jg error
    AND AX,00fh       ;convert to digit
    push ax           ;save on stack   
    mov ax,10          ;get 10
    mul bx              ;ax=total * 10
    pop bx              ;get digit back
    add bx,ax           ;total = total x 10 +digit    
    mov ah,1
    int 21h    
    cmp al,0dh        ;carriage return
    jne repeat      ;no keep going
    mov ax,bx         ;store in ax
    cmp bx,32767
    ja error3
    jo error3    
    cmp bx,65535
    ja error3
    jo error3
       
    or cx,cx          ;neg num    
    je next    
    neg ax            ;yes, negate    
    jmp next    
    pop dx            ;restore registers
    pop cx
    pop bx
    ret                    ;and return
indec endp
input_mod: 
    lea dx,t1
    mov ah,9
    int 21h
indec5 proc    
    push bx
    push cx
    push dx      
begin5:
    xor bx,bx      ;holds total    
    xor cx,cx       ;sign                    
    mov ah,1        ;char in al
    int 21h
    ;read a char    
    repeat6:     
    cmp al,48
    jl error    
    cmp al,57
    jg error
    AND AX,00fh       ;convert to digit
    push ax           ;save on stack    
    mov ax,10          ;get 10
    mul bx              ;ax=total * 10
    pop bx              ;get digit back
    add bx,ax           ;total = total x 10 +digit        
    mov ah,1
    int 21h   
    cmp al,0dh        ;carriage return
    jne repeat6     ;no keep going   
    cmp bx,32767
    ja error3
    jo error3    
    cmp bx,65535
    ja error3
    jo error3    
    mov ax,bx         ;store in ax    
    or cx,cx          ;neg num
    je mod_    
    neg ax            ;yes, negate   
    jmp mod_
    pop dx            ;restore registers
    pop cx
    pop bx
    ret                    ;and return   
indec5 endp 
error:   
    lea dx,err
    mov ah,9
    int 21h   
    jmp common_input     
error3:
    lea dx,err3
    mov ah,9
    int 21h      
    jmp common_input
next:
;first value stored in a
mov E,ax  
jmp ask

ask: 
    lea dx,s4                   ;printing message
    mov ah,9
    int 21h 
    mov ah,1
    int 21h 
    mov bl,al                        ;comparing with operator
    cmp bl,'%'
    je input_mod  
    jmp error      
;ask55: 
;    
;    lea dx,t              ;if again want to calculate
;    mov ah,9
;    int 21h
    
;   mov ah,1
;    int 21h 
;    mov bl,al 
    
;    cmp bl,'y'
;    je common_input 
    
;    cmp bl,'Y'
;    je common_input 
    
;    cmp bl,'n'
;    je end_ 
   
;    cmp bl,'N'
;    je end_ 
    
;    jmp error2

mod_:                     ;second value stored in b  
    mov F,ax     
    mov G,dx    
    lea dx,r
    mov ah,9
    int 21h    
    xor ax,ax
    xor dx,dx    
    mov dx,G
    mov ax,E     
    div  F                         ;dividing   
    cmp ax,32767
    ja error3
    jo error3    
    cmp ax,65535
    ja error3
    jo error3    
    push ax 
    push dx    
    jmp output2 
       
output2:

outdec2 proc      
    push ax            ;save registers
    push bx
    push cx
    push dx    
    mov ax,dx    
    or ax,ax          ;ax < 0?   
    JGE end_if2       ;no, >0    
    PUSH AX            ;save number
    MOV DL,'-'         ;get '-'
    mov ah,2           ;print char function 
    int 21h            ;print '-'
    pop ax             ; get ax back
    neg ax             ; ax= -ax  
    cmp dx,65535
    jo error3    
    end_if2:
    xor cx,cx         ;cx counts digits
    mov bx,10d        ;bx has divisor    
    repeat_:    
    xor dx,dx         ;prep high word
    div bx            ;ax = quotient, dx=remainder    
    push dx           ;save remainder on stack
    inc cx            ;count = count +1   
    or ax,ax          ;quotient = 0?
    jne repeat_      ;no, keep going    
    mov ah,2          ;print char function    
    print_loop1:
    pop dx            ;digit in dl
    or dl,30h         ;convert to char
    int 21h           ;print digit
    loop print_loop1  ;loop untill done   
    pop dx
    pop cx            ;restore registers
    pop bx
    pop ax 
;    jmp ask1 
      mov ah,9
      lea dx, null
      int 21h  
      jmp exit 
      jmp start 
    ret
    outdec2 endp
    
        
startsqrt:
;call clearScreen
    call read
    mov cx, ax
;call verifyNumber
;lea dx, loadingMsg
;call printString
    mov ax, cx
    call calcSqrt
    mov cx, ax
    
    call printCRLF
    lea dx, resultMsg
    call printString
    mov ax, cx
    call printFloat   
       mov ah,9
       lea dx, null
       int 21h 
        jmp exit  
       jmp start  
;call finish
ret
;call exit
; Saves registers state
saveState:
    pop stackCacheLevel2
    push ax
    push bx
    push cx
    push dx
    push stackCacheLevel2
ret
; Loads registers state
loadState:
    pop stackCacheLevel2
    pop dx
    pop cx
    pop bx
    pop ax
    push stackCacheLevel2
ret
; Reads a number from Cin with 5 characters max.
;@returns: Number -> AX
read:
    pop stackCacheLevel1
    lea dx, readMsg
    call printString
    call printCRLF
    mov ah, 02
    mov dl, 41
    int 21h
    xor bx, bx
    lea si, inputArray
    mov cx, 6
readLoop:
    mov ah, 01
    int 21h
    cmp al, 13
    je fnshRead
    xor ah, ah
    sub al, 48
    mov [si], ax
    inc si
    inc bx ; BX = char length
    Loop readLoop:
fnshRead:
    lea si, inputArray
    mov cx, bx
    xor dx, dx
convertLoop:
    mov ax, 10
    mul dx ; AX = DX * 10
    mov bx, [si]
    xor bh, bh
    inc si
    add ax, bx ; AX = (DX * 10) + BX
    mov dx, ax
    Loop convertLoop
fnshConvert:
    mov ax, dx
    push stackCacheLevel1
ret
; Calculates the Sqrt of a 16 bit integer
; @args: source -> AX
; @returns: integer result -> AX, decimal result -> BX
calcSqrt:
    pop stackCacheLevel1
    push 0
    push 0
    push ax ; Stack = source
; aprox = (source / 200) + 2
    xor dx, dx
    mov bx, 200
    div bx
    add ax, 2
    mov bx, ax
    push ax ; Stack = aprx
; BX -> aprx
; CX -> source
calcLoop:
    pop bx ; Stack = aprox(current)
    pop cx ; Stack = source
; If aprx == aprx(cache) it means that the squareroot of source
;   is a floating point number, which is not supported by the 8068
;   and thus, can't be calculated precisely.
    pop ax
    cmp bx, ax
    je fnshAvrgSqrt
; checking if aprx² == source
    mov ax, bx ; AX = aprx
    xor dx, dx
    mul bx
    cmp ax, cx
    je fnshSqrt
    pop dx ; Clearing aprx(decimal) as result was not achieved
    push bx ; Store cache of aprx to be compared in the next loop iteration
; aprx(BX) = ((source(CX) / aprx) + aprx) / 2
    mov ax, cx ; AX = source
    xor dx, dx
    div bx
    add ax, bx
    shr ax, 1
    mov bx, ax
    push cx ; Stack = source
    push bx ; Stack = aprx(current)
; Stack = aprx * 100
    mov dx, precisionFactor
    mul dx
    push ax
; aprox = ((source * 100 / aprx) + aprx * 100) / 2
    mov ax, precisionFactor
    mul cx ; source * 100
    div bx ; (source * 100 / aprx)
    mov cx, ax
    mov ax, precisionFactor
    mul bx
    add ax, cx ; (source * 100 / aprx) + aprx * 100)
    adc dx, 0  ; (source * 100 / aprx) + aprx * 100) + Carry
    mov cx, 2
    div cx ; AX = ((source * 100 / aprx) + aprx * 100) / 2
    pop bx ; BX = aprx * 100 lower
    sub ax, bx ; AX = (((source * 100 / aprx) + aprx * 100) / 2) - aprx * 100

    pop bx ; BX = aprx
    pop cx ; CX = source
    pop dx ; DX = aprx(cache)

    push ax ; Stack = aprx(decimal)
    push dx
    push cx
    push bx
    JMP calcLoop
; The result is an integer number
fnshSqrt:
    mov ax, bx ; AX = aprox(Sqrt)
    pop bx
    xor bx, bx ; BX = Sqrt(decimal)
    push stackCacheLevel1
ret
; The result is a floating point number
fnshAvrgSqrt:
    mov ax, bx ; AX = aprox(Sqrt)
    pop bx ; BX = Sqrt(decimal)
    push stackCacheLevel1
ret
; printString a String in Cout.
; @args: String pointer -> DX
printString:
    call printCRLF
    mov ah, 09
    int 21h
    mov dx, 0
ret
; printString a 16Bit-Integer + 8Bit-Precision(decimal) Number
;@args: Number -> AX, Precision -> BX
printFloat:
    call printNumber
    mov ah, 02
    mov dl, 46
    int 21h
    mov ax, bx
    call printNumber
fnshprintFloat:
ret
; printString an Integer number.
;@args: Number -> AX
printNumber:
    call saveState
    cmp ax, 100
    jae printThreeDigits
    cmp ax, 10
    jae printTwoDigits
; 1 digit:
printOneDigit:
    mov ah, 02
    mov dl, al
    add dl, 48
    int 21h
    call loadState
ret
; 2 digits:
printTwoDigits:
    aam
    add ah, 48
    add al, 48
    mov cx, ax
    mov ah, 02
    mov dl, ch
    int 21h
    mov dl, cl
    int 21h
    call loadState
ret
; 3 digits:
printThreeDigits:
    aam
    mov dh, al
    mov al, ah
    mov ah, 0
    aam
    add ah, 48
    add al, 48
    add dh, 48
    mov cx, ax
    mov ah, 02
    mov dl, ch
    int 21h
    mov dl, cl
    int 21h
    mov dl, dh
    int 21h
    call loadState
ret
verifyNumber:
    cmp ax, 65534
    jbe continue
; Overflow:
    lea dx, overflowMsg
    call printString
;call finish
; Continue:
continue:
ret
; Printing CRLF (carriage return + line feed)
printCRLF:
    call saveState
    mov ah, 02
    mov dl, 13
    int 21h
    mov dl, 10
    int 21h
    call loadState
ret
clearScreen:
    xor ah, ah
    mov al, 03h
    int 10h
ret
 
 
Factorus:
; get first number:
    mov dx, offset mrg1
    mov ah, 9
    int 21h
    jmp n1
    mrg1 db 0Dh,0Ah, 'enter the number: $'
n1:
    call    scan_num
; factorial of 0 = 1:
    mov     ax, 1
    cmp     cx, 0
    je      print_result
; move the number to bx:
; cx will be a counter:
    mov     bx, cx
    mov     ax, 1
    mov     bx, 1
calc_it:
    mul     bx
    cmp     dx, 0
    jne     overflow
    inc     bx
    loop    calc_it
    mov resultats, ax
print_result:
; print result in ax:
    mov dx, offset mrg2
    mov ah, 9
    int 21h
    jmp n2
    mrg2 db 0Dh,0Ah, 'factorial: $'
n2:
    mov     ax, resultats
    call    print_num_uns
    jmp     exit
overflow:
    mov dx, offset mrg3
    mov ah, 9
    int 21h
    jmp n3
    mrg3 db 0Dh,0Ah, 'the result is too big!', 0Dh,0Ah, 'use values from 0 to 8.$'
n3:
    jmp     Factorus
ret

  