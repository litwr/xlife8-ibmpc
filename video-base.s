initxt: mov ah,2
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

          ;;mov #65535,r2   ;draw frame
          ;;mov #20,r3
          ;;mov #16384+12+64,r1
          ;;mov #16384+12+<64*194>,r0
;;1$:       mov r2,(r1)+
          ;;mov r2,(r0)+
          ;;sob r3,1$
        mov ax,0c003h    ;draw frame vertical borders
        mov di,19
        mov si,19+2000h
        mov cx,96
.c1:    mov [es:di],al
        mov [es:di+2000h],al
        mov [es:di+41],ah
        mov [es:di+41+2000h],ah
        add di,80
        loop .c1
        retn

          ;;mov #194,r3
          ;;mov #16384+11+64,r1
;;2$:       movb #192,@r1
          ;;movb #3,41(r1)
          ;;add #64,r1
          ;;sob r3,2$
          ;;jmp @#gexit3

totext:    ;call @#clrscn
           ;mov #toandos,@#pageport
           ;mov #12,r0
           ;emt ^O16
exit88:    ;return

galign:    ;mov #10,r0
;1$:       cmp #^O1330,@#yshift
           ;beq exit88

           ;emt ^O16
           ;br 1$

tograph:   ;mov #toandos,@#pageport
           ;jsr r3,@#printstr
           ;.byte 145,0
           ;call @#galign

           ;jsr r3,@#printstr
           ;.byte 12,0
tograph0:  ;call @#clrscn
           ;call @#initxt
           ;call @#showscn
           ;call @#showmode
           ;call @#showtopology
           ;call @#showrules2
           ;jmp @#xyout

printstr:  ;;movb (r3)+,r0  ;use: r0, r1
           ;;beq 2$
           pop si
           lodsb
           cmp al,0
           jz .c2

           ;;cmp #9,r0
           ;;bne 3$

           ;;mov #9,r1
           ;;mov #32,r0
;;1$:        emt ^O16
           ;;sob r1,1$

;;3$:        emt ^O16
           ;;br printstr

;;2$:        inc r3
           ;;bic #1,r3
           ;;rts r3
.c2:       push si
           retn


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

