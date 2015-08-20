curon:  mov ah,1
        mov cx,607h
        int 10h
        retn

curoff: mov ah,1
        mov cx,201fh
        int 10h
        retn

initxt: mov ax,0c003h    ;draw frame vertical borders
        mov di,19
        mov si,19+2000h
        mov cx,96
.c1:    mov [es:di],al
        mov [es:di+2000h],al
        mov [es:di+41],ah
        mov [es:di+41+2000h],ah
        add di,80
        loop .c1

initxt2: mov ah,2     ;must follow initxt
        mov bx,1   ;color
        mov dx,24*256
        int 10h

        mov ax,9*256+'G'
        mov cx,1
        int 10h

        mov ah,2
        mov dl,18
        int 10h

        mov ax,9*256+'%'
        int 10h

        mov ah,2
        mov dl,32
        int 10h

        mov ax,9*256+'X'
        int 10h

        mov ah,2
        mov dl,36
        int 10h

        mov ax,9*256+'Y'
        int 10h
        retn

totext:    mov ax,1    ;set video mode #4 = 40x25x16
           int 10h
           retn

tograph:   mov ax,4    ;set video mode #4 = 320x200x4
           int 10h
tograph0:  call initxt
           call showscn
           call showtopology
           ;;call @#showrules2
           jmp xyout

printstr:  pop dx
           mov si,dx
.l1:       lodsb
           cmp al,'$'
           jnz .l1

           mov ah,9
           int 21h
           jmp si

digiout:        ;;in: r1 - length, r2 - scrpos, r0 - data
                ;in: dx - length, di - srcpos, bx - data
;;1$:      movb (r0)+,r3
.c1:       xor ax,ax
           mov al,[bx]
           inc bx

;;         asl r3
;;         asl r3
;;         asl r3
;;         asl r3
         mov cl,4
         shl ax,cl
         mov si,ax
         add si,digifont
;;         mov digifont+2(r3),64(r2)
;;         mov digifont+4(r3),128(r2)
;;         mov digifont+6(r3),192(r2)
;;         mov digifont+8(r3),256(r2)
;;         mov digifont+10(r3),320(r2)
;;         mov digifont+12(r3),384(r2)
;;         mov digifont(r3),(r2)+
         movsw
         lodsw
         mov [es:di+2000h-2],ax
         lodsw
         mov [es:di+80-2],ax
         lodsw
         mov [es:di+80+2000h-2],ax
         lodsw
         mov [es:di+80*2-2],ax
         lodsw
         mov [es:di+80*2+2000h-2],ax
         lodsw
         mov [es:di+80*3-2],ax

;;         sob r1,1$
         dec dx
         jnz .c1
         retn

digiout2:        ;in: cx - length, si - srcpos, bx - data
.c1:     lodsb
         or al,'0'
         stosb
         loop .c1
         retn
