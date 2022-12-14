curon:  mov ah,1
        mov cx,607h
        int 10h
        retn

curoff: mov ah,1
        mov cx,201fh
        int 10h
        retn

curonz: mov ah,1
        mov cx,7
        int 10h
        retn

initxt: mov ax,0c003h    ;draw frame vertical borders
        mov di,39-hormax
        mov cx,96
.c1:    mov [es:di+2000h],al
        mov [es:di+hormax*2+1],ah
        mov [es:di+hormax*2+1+2000h],ah
        stosb
        add di,79
        loop .c1

initxt2: call showtopology.l1    ;must follow initxt
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

totext: mov ax,1    ;set video mode #4 = 40x25x16
.e1:    int 10h
        jmp stop_timer2

tograph:cmp [zoom],0
        je .l1

        call totext
        call curonz
        call initxt2
        call setviewport
        call showmode2
        call showscn
        call showtopology2
        call showrules
        call xyout2
        jmp dispatcher.c270

.l1:    mov ax,4    ;set video mode #4 = 320x200x4
        int 10h

        mov bl,[palette]
        mov bh,1
        mov ah,0bh
        int 10h

        call initxt
        call showmode
        call showscn
        call start_timer2
        call showtopology
        call showrules
        jmp xyout

showmode2: cmp [mode],1
        mov al,[zbgr]
        jz .modego

        mov al,[zbgs]
.modego: mov [czbg],al
        retn

showmode: xor bx,bx    ;bg=0=black
        mov bl,[bgr]
        cmp [mode],1
        jz .modego

        mov bl,[bgs]
.modego: mov ah,0bh
        int 10h
        retn

showtopology:
        cmp [topology],0
        jz .l1

        mov di,192*40
        mov cx,4
.loop:  mov ax,[es:di+2000h]
        not ax
        mov [es:di+2000h],ax
        mov ax,[es:di]
        not ax
        stosw
        add di,78
        loop .loop
        retn

.l1:    mov ah,2     ;must follow initxt
        mov bx,1   ;color
        mov dx,24*256
        int 10h

        mov ax,9*256+'G'
        mov cx,1
        int 10h
        retn

showtopology2:
        mov al,9
        cmp [topology],0
        jz .l1

        mov al,69h
.l1:    mov [es:1921],al
        retn

printstr:pop dx         ;uses: si,dx,ax
         mov si,dx
.l1:     lodsb
         cmp al,'$'
         jnz .l1

         mov ah,9
         int 21h
         jmp si

digiout:        ;;in: r1 - length, r2 - scrpos, r0 - data
                ;in: dx - length, di - srcpos, bx - data
;;1$:      movb (r0)+,r3
.c1:     xor ax,ax
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

digiout2:        ;in: cx - length, di - srcpos, si - data
         mov ah,5   ;color
.c1:     lodsb
         add al,'0'
         stosw
         loop .c1
         retn
