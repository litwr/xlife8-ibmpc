clear:    call zerocc
          call zerogc
;;          mov @#startp,r0
          mov si,[startp]

;;10$:      tstb sum(r0)
;;          beq 11$
.c10:     xor ax,ax
          cmp al,[si+sum]
          jz .c11

;;          clrb sum(r0)
;;          clr @r0
;;          clr 2(r0)
;;          clr 4(r0)
;;          clr 6(r0)
          mov [si+sum],al
          mov [si],ax
          mov [si+2],ax
          mov [si+4],ax
          mov [si+6],ax

;;11$:      mov next(r0),r0
;;          cmp r0,#1
;;          bne 10$
.c11:     mov si,[si+next]
          cmp si,1
          jnz .c10

          call showscn
          call cleanup0
          jmp infoout

chkaddt: ;;tst r3
;;         beq exit2
         or cx,cx
         jz exit2

chkadd:  ;;tst next(r2)
;;         bne exit2
         cmp word [di+next],0    ;in: di
         jnz exit2

addnode:
;;         mov @#startp,next(r2)
         mov ax,[startp]
         mov [di+next],ax

;;         mov r2,@#startp
         mov [startp],di

;;         ;*inc tilecnt
;;         ;*bne exit2

;;         ;*inc tilecnt+1
;;         inc @#tilecnt
         inc [tilecnt]

;;exit2:   return
exit2:    retn

;;;*chkadd2  ldy #next
;;         ;*lda (adjcell2),y
;;         ;*iny
;;         ;*ora (adjcell2),y
chkadd2: ;;tst next(r5)
         cmp word [ds:bp+next],0

;;         ;*beq addnode2
;;         bne exit2
         jnz exit2

;;;*addnode2 .block
addnode2:                 ;in: R5
;;         ;*dey
;;         ;*lda startp
;;         ;*sta (adjcell2),y
;;         ;*iny
;;         ;*lda startp+1
;;         ;*sta (adjcell2),y
;;         mov @#startp,next(r5)
         mov ax,[startp]
         mov [ds:bp+next],ax

;;         ;*#assign16 startp,adjcell2
;;         mov r5,@#startp
         mov [startp],bp

;;         ;*inc tilecnt
;;         ;*bne exit

;;         ;*inc tilecnt+1
;;         inc @#tilecnt
         inc [tilecnt]

;;;*exit     rts
;;         return
         retn

;;         ;*.bend

;;;inctiles .block
;;;         clc
;;;         lda i1
;;;         adc #tilesize
;;;         sta i1
;;;         bcc l1

;;;         inc i1+1
;;;l1       rts
;;;         .bend

torus:
;;         mov #tiles,r0
;;         mov #hormax,r1
        mov si,tiles       ;top border
        mov cx,hormax

;;5$:      mov r0,r2
;;         add #<hormax*<vermax-1>-1>*tilesize,r2
;;         mov r2,ul(r0)
.c5:     lea ax,[si+(hormax*(vermax-1)-1)*tilesize]
         mov [si+ul],ax

;;         mov r0,r2
;;         add #hormax*<vermax-1>*tilesize,r2
;;         mov r2,up(r0)
         lea ax,[si+hormax*(vermax-1)*tilesize]
         mov [si+up],ax

;;         mov r0,r2
;;         add #<hormax*<vermax-1>+1>*tilesize,r2
;;         mov r2,ur(r0)
         lea ax,[si+(hormax*(vermax-1)+1)*tilesize]
         mov [si+ur],ax

;;         add #tilesize,r0
;;         sob r1,5$
         add si,tilesize
         loop .c5

;;         mov #tiles+<<vermax-1>*hormax*tilesize>,r0
;;         mov #hormax,r1
         mov si,tiles+(vermax-1)*hormax*tilesize
         mov cx,hormax

;;4$:      mov r0,r2
;;         sub #<<vermax-1>*hormax-1>*tilesize,r2
;;         mov r2,dr(r0)
.c4:     mov ax,si
         sub ax,((vermax-1)*hormax-1)*tilesize
         mov [si+dr],ax

;;         mov r0,r2
;;         sub #<vermax-1>*hormax*tilesize,r2
;;         mov r2,down(r0)
         mov ax,si
         sub ax,(vermax-1)*hormax*tilesize
         mov [si+down],ax

;;         mov r0,r2
;;         sub #<<vermax-1>*hormax+1>*tilesize,r2
;;         mov r2,dl(r0)
         mov ax,si
         sub ax,((vermax-1)*hormax+1)*tilesize
         mov [si+dle],ax

;;         add #tilesize,r0
;;         sob r1,4$
         add si,tilesize
         loop .c4

;;         mov #tiles,r0
;;         mov #vermax,r1
        mov si,tiles
        mov cx,vermax

;;3$:      mov r0,r2
;;         add #<hormax-1>*tilesize,r2
;;         mov r2,left(r0)
.c3:     lea ax,[si+(hormax-1)*tilesize]
         mov [si+left],ax

;;         mov r0,r2
;;         sub #tilesize,r2
;;         mov r2,ul(r0)
         lea ax,[si-tilesize]
         mov [si+ul],ax

;;         mov r0,r2
;;         add #<2*hormax-1>*tilesize,r2
;;         mov r2,dl(r0)
         lea ax,[si+(2*hormax-1)*tilesize]
         mov [si+dle],ax

;;         add #hormax*tilesize,r0
;;         sob r1,3$
         add si,hormax*tilesize
         loop .c3

;;         mov #tiles+<<hormax-1>*tilesize>,r0
;;         mov #vermax,r1
         mov si,tiles+(hormax-1)*tilesize
         mov cx,vermax

;;2$:      mov r0,r2
;;         sub #<2*hormax-1>*tilesize,r2
;;         mov r2,ur(r0)
.c2:     lea ax,[si-(2*hormax-1)*tilesize]
         mov [si+ur],ax

;;         mov r0,r2
;;         sub #<hormax-1>*tilesize,r2
;;         mov r2,right(r0)
         lea ax,[si-(hormax-1)*tilesize]
         mov [si+right],ax

;;         mov r0,r2
;;         add #tilesize,r2
;;         mov r2,dr(r0)
         lea ax,[si+tilesize]
         mov [si+dr],ax

;;         add #hormax*tilesize,r0
;;         sob r1,2$
         add si,hormax*tilesize
         loop .c2

;;         mov #tiles + <<hormax*vermax-1>*tilesize>,@#tiles+ul
;;         mov #tiles + <<hormax*<vermax-1>>*tilesize>,@#tiles+ur+<<hormax-1>*tilesize>
;;         mov #tiles+<<hormax-1>*tilesize>,@#tiles+dl+<hormax*<vermax-1>*tilesize>
;;         mov #tiles,@#tiles+dr+<<vermax*hormax-1>*tilesize>
;;         return
         mov word [tiles+ul],tiles + (hormax*vermax-1)*tilesize
         mov word [tiles+ur+(hormax-1)*tilesize],tiles + hormax*(vermax-1)*tilesize
         mov word [tiles+dle+hormax*(vermax-1)*tilesize],tiles+(hormax-1)*tilesize
         mov word [tiles+dr+(vermax*hormax-1)*tilesize],tiles
         retn

plain:
;;         mov #tiles,r0
;;         mov #hormax,r1
;;         mov #plainbox,r2
         mov si,tiles
         mov cx,hormax
         mov ax,plainbox

;;5$:      mov r2,ul(r0)
;;         mov r2,up(r0)
;;         mov r2,ur(r0)
;;         add #tilesize,r0
;;         sob r1,5$
.c5:     mov [si+ul],ax
         mov [si+up],ax
         mov [si+ur],ax
         add si,tilesize
         loop .c5

;;         mov #tiles+<<vermax-1>*hormax*tilesize>,r0
;;         mov #hormax,r1
         mov si,tiles+(vermax-1)*hormax*tilesize
         mov cx,hormax

;;4$:      mov r2,dr(r0)
;;         mov r2,down(r0)
;;         mov r2,dl(r0)
;;         add #tilesize,r0
;;         sob r1,4$
.c4:     mov [si+dr],ax
         mov [si+down],ax
         mov [si+dle],ax
         add si,tilesize
         loop .c4

;;         mov #tiles,r0
;;         mov #vermax,r1
         mov si,tiles
         mov cx,vermax

;;3$:      mov r2,left(r0)
;;         mov r2,ul(r0)
;;         mov r2,dl(r0)
;;         add #tilesize*hormax,r0
;;         sob r1,3$
.c3:     mov [si+left],ax
         mov [si+ul],ax
         mov [si+dle],ax
         add si,tilesize*hormax
         loop .c3

;;         mov #tiles+<<hormax-1>*tilesize>,r0
;;         mov #vermax,r1
         mov si,tiles+(hormax-1)*tilesize
         mov cx,vermax

;;2$:      mov r2,ur(r0)
;;         mov r2,right(r0)
;;         mov r2,dr(r0)
;;         add #tilesize*hormax,r0
;;         sob r1,2$
;;         return
.c2:     mov [si+ur],ax
         mov [si+right],ax
         mov [si+dr],ax
         add si,tilesize*hormax
         loop .c2
         retn

random:
;;;uses: adjcell:2 - r2, i1:2 - r3/r5, i2 - r4, t1 - r1
;;         clr r1   ;dir: 0 - left, 1 - right
;;         mov #tiles+<<hormax*4+3>*tilesize>,r2
;;         mov #16,r3    ;ver rnd max
;;         mov #right,r5
;;         mov #14,r4    ;hor rnd max
         in al,61h
         or al,1
         out 61h,al         ;enable timer 2 gate
         MOV     AL,94H          ;SET TIMER 2 HARDWARE
         OUT     43H,AL
         mov     al,251
         OUT     42H,AL

         xor bp,bp   ;dir: 0 - left, 1 - right
         mov di,tiles+(hormax*4+3)*tilesize
         mov dx,(vermax-8)*256+hormax-6    ;dh - ver rnd max, dl - hor rnd max
         mov bx,right


;;;cont3    ldy #sum
;;;         sta (adjcell),y
;;;         lda #8
;;;         sta t3
;;23$:     mov #1,sum(r2)
;;         mov #8,r0
.cont3:  mov word [di+sum],1
         mov cx,8

;;;loop1    jsr rndbyte
;;;         dec t3
;;;         bne loop1
;;1$:      call @#rndbyte
;;         sob r0,1$
;;         sub #8,r2
.loop1:  call rndbyte
         loop .loop1
         sub di,8

;;;         jsr chkadd
;;;         dec i2
;;;         beq cont2
;;        call @#chkadd
;;        dec r4
;;        beq 22$
         call chkadd
         dec dl
         jz .cont2

;;;         ldy i1+1
;;;cont4    lda (adjcell),y
;;;         tax
;;;         iny
;;;         lda (adjcell),y
;;;         stx adjcell
;;;         sta adjcell+1
;;;         bne cont3
;;         add r5,r2
;;24$:     mov @r2,r2
;;         br 23$
.cont4:  mov di,[di+bx]
         jmp .cont3


;;;cont2    dec i1
;;;         beq cont5
;;22$:     dec r3
;;         beq calccells
.cont2:  dec dh
         jz calccells

;;         mov #14,r4   ;hor rnd max
;;         mov #left,r5
;;         mov #1,r0
;;         xor r0,r1
;;         bne 21$
         mov bl,left
         mov dl,hormax-6       ;hor rnd max
         xor bp,1
         jnz .cont1

;;;         ldy #right
;;;cont1    sty i1+1
;;;         ldy #down
;;;         bne cont4
;;         mov #right,r5
;;21$:     add #down,r2
;;         br 24$
        mov bl,right
.cont1: mov di,[di+down]
        jmp .cont3

calccells: call zerocc
         cmp [tilecnt],0
         jnz .c12
         retn

.c12:    mov si,[startp]
.c2:     mov cx,8
         xor ax,ax
.c4:     lodsb
         or al,al
         jz .c5

         mov bx,tab3
         xlatb
         call inctsum
         mov ah,cl
.c5:     loop .c4
         mov [si+sum-8],ah
         mov si,[si+next-8]
         cmp si,1
         jnz .c2
         jmp infoout

inctsum:            ;in: al
         cellsum .l1
.l1:     retn

putpixel:     ;IN: x0,y0; DON'T USE: SI,BP
         call xchgxy
         mov dx,word [x0]
         call calcx

         mov cl,dl
         or al,[crsrx]
         cmp [xdir],0
         jz .c4

         cmp al,cl
         jc .c100

         sub al,cl
         jmp .c2

.c4:     add al,cl
         cmp al,cl
         jc .c100
if hormax<>32
         cmp al,hormax*8
         jnc .c100
end if
.c2:     mov ch,[crsry]
         add ch,[crsrbyte]
         mov cl,dh
         cmp [ydir],0
         jz .c3

         cmp ch,cl
         jc .c100

         sub ch,cl
         jmp .c1

.c3:     add ch,cl
         cmp ch,cl
         jc .c100

         cmp ch,vermax*8
         jc .c1

.c100:   retn

.c1:     xor cl,cl
         xchg cl,ch
         xor dh,dh
         mov dl,[crsry]
         sub cx,dx
         xor ah,ah
         mov dl,[crsrx]
         sub ax,dx
         mov di,[crsrtile]     ;for chkadd
.c22:    test cx,0fff8h
         js .cup           ;12$
         jne .cdown        ;11$

.c23:    test ax,0fff8h
         js .cleft         ;13$
         jne .cright       ;10$

         mov bx,7
         sub bl,al
         mov dl,[bittab+bx]
         ;and ch,7
         cmp [ppmode],bh
         jne putpixel3
         jmp putpixel2

.cright: mov di,[di+right]   ;y=0, x=/=0
         sub ax,8
         jmp .c23

.cdown:  mov di,[di+down]   ;y=/=0
         sub cx,8
         jmp .c22

.cup:    mov di,[di+up]   ;y=/=0
         add cx,8
         jmp .c22

.cleft:  mov di,[di+left]   ;y=0, x=/=0
         add ax,8
         jmp .c23

putpixel3:
         mov bl,cl
         or [di+bx],dl
         jmp chkadd
