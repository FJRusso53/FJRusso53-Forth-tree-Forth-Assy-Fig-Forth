; �������������������������������������������������������������������������
    include \masm32\include\masm32rt.inc
; �������������������������������������������������������������������������

comment * -----------------------------------------------------
                     Build this console app with
                  "MAKEIT.BAT" on the PROJECT menu.
        ----------------------------------------------------- *

  

    .code

start:
.LISTALL
   
; �������������������������������������������������������������������������

    call main
    inkey
    exit

; �������������������������������������������������������������������������

main proc

    cls
    print "Hello World",13,10

    ret

main endp

; �������������������������������������������������������������������������
 .data?
      value dd ?

    .data
      item dd 0

end start