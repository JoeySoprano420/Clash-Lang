%macro print 1
    mov     edx, %1
    call    _PrintClash
%endmacro

%macro exit 0
    call    ExitProcess
%endmacro
