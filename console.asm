; いいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいい�
    include \masm32\include\masm32rt.inc
; いいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいい�

comment * -----------------------------------------------------
                     Build this console app with
                  "MAKEIT.BAT" on the PROJECT menu.
        ----------------------------------------------------- *

  

    .code

start:
.LISTALL
   
; いいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいい�

    call main
    inkey
    exit

; いいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいい�

main proc

    cls
    print "Hello World",13,10

    ret

main endp

; いいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいいい�
 .data?
      value dd ?

    .data
      item dd 0

end start
