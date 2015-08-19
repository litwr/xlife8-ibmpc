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
.c4:     lea ax,[si-((vermax-1)*hormax-1)*tilesize]
         mov [si+dr],ax

;;         mov r0,r2
;;         sub #<vermax-1>*hormax*tilesize,r2
;;         mov r2,down(r0)
         lea ax,[si-(vermax-1)*hormax*tilesize]
         mov [si+down],ax

;;         mov r0,r2
;;         sub #<<vermax-1>*hormax+1>*tilesize,r2
;;         mov r2,dl(r0)
         lea ax,[si-((vermax-1)*hormax+1)*tilesize]
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

;;;cont3    ldy #sum
;;;         sta (adjcell),y
;;;         lda #8
;;;         sta t3
;;23$:     mov #1,sum(r2)
;;         mov #8,r0

;;;loop1    jsr rndbyte
;;;         dec t3
;;;         bne loop1
;;1$:      call @#rndbyte
;;         sob r0,1$
;;         sub #8,r2

;;;         jsr chkadd
;;;         dec i2
;;;         beq cont2
;;        call @#chkadd
;;        dec r4
;;        beq 22$

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

;;;cont2    dec i1
;;;         beq cont5
;;22$:     dec r3
;;         beq calccells

;;         mov #14,r4   ;hor rnd max
;;         mov #left,r5
;;         mov #1,r0
;;         xor r0,r1
;;         bne 21$

;;;         ldy #right
;;;cont1    sty i1+1
;;;         ldy #down
;;;         bne cont4
;;         mov #right,r5
;;21$:     add #down,r2
;;         br 24$

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

putpixel:     ;IN: x0,y0; USE: R1, R2, R3, R4; DON'T USE: R0,R5
;;;x8pos    = adjcell2 = R1 low
;;;y8pos    = t1  = R3 low
;;;         jsr xchgxy
;;         call @#xchgxy
;;         mov @#x0,r4
;;         call @#calcx

;;;         stx m1+1
;;;         lda crsrx
;;;         lsr
;;;         asl
;;;         asl
;;;         asl
;;;m1       adc #0
;;;         ldx xdir
;;;         beq cont4
;;         movb r4,r2
;;         bisb @#crsrx,r1
;;         tstb @#xdir
;;         beq 4$

;;;         sec
;;;         sbc x0
;;;         bcc exit
;;;         bcs cont2
;;         cmpb r1,r2
;;         bcs 100$

;;         sub r2,r1
;;         br 2$

;;;cont4    adc x0
;;;         bcs exit
;;4$:      add r2,r1
;;         cmpb r1,r2
;;         bcs 100$

;;;         cmp #160
;;;         bcs exit
;;         cmpb r1,#160
;;         bcc 100$

;;;cont2    sta x8pos
;;;         lda crsry
;;;         asl
;;;         asl
;;;         asl
;;;         adc crsrbyte
;;;         ldx ydir
;;;         beq cont3
;;2$:      movb @#crsry,r3
;;         add @#crsrbyte,r3
;;         mov r4,r2
;;         swab r2
;;         tstb @#ydir
;;         beq 3$

;;;         sec
;;;         sbc y0
;;;         bcc exit
;;;         bcs cont1
;;         cmpb r3,r2
;;         bcs 100$
;;
;;         sub r2,r3
;;         br 1$

;;;cont3    adc y0
;;;         bcs exit

;;;         cmp #192
;;;         bcc cont1
;;3$:      add r2,r3
;;         cmpb r3,r2
;;         bcs 100$

;;         cmpb r3,#192
;;         bcs 1$

;;;exit     rts
;;100$:    return

;;;cont1    sta y8pos
;;;         and #7
;;;         sta y8byte
;;;         lda y8pos
;;;         lsr
;;;         lsr
;;;         lsr
;;;         sec
;;;         sbc crsry
;;;         sta y8pos
;;1$:      mov #65280,r2
;;         movb @#crsry,r4
;;         bic r2,r3
;;         bic r2,r4
;;         sub r4,r3

;;;         lda x8pos
;;;         and #7
;;;         sta x8bit
;;;         lda crsrx
;;;         lsr
;;;         sta t2
;;;         lda x8pos
;;;         lsr
;;;         lsr
;;;         lsr
;;;         sec
;;;         sbc t2
;;;         sta x8pos
;;         movb @#crsrx,r4
;;         bic r2,r1
;;         bic r2,r4
;;         sub r4,r1

;;;         #assign16 adjcell,crsrtile
;;         mov @#crsrtile,r2     ;r2 for chkadd

;;;         sta $ff3f
;;;         lda y8pos
;;;loop2    bmi cup
;;;         bne cdown
;;22$:     bit #65528,r3
;;         bmi 12$
;;         bne 11$

;;;         lda x8pos
;;;loop3    bmi cleft
;;;         bne cright
;;23$:     bit #65528,r1
;;         bmi 13$
;;         bne 10$
;;
;;;         lda #7
;;;         sec
;;;         sbc x8bit
;;;         tay
;;;         lda bittab,y
;;;         ldy ppmode
;;;         bne putpixel3
;;;         jmp putpixel2
;;         mov #7,r4
;;         sub r1,r4
;;         bic #65528,r4
;;         movb bittab(r4),r4
;;         bic #65528,r3
;;         tstb @#ppmode
;;         bne putpixel3
;;         jmp @#putpixel2

;;;cright   ldy #right     ;y=0, x=/=0
;;;         jsr nextcell
;;;         dec x8pos
;;;         bpl loop3
;;10$:     mov right(r2),r2
;;         sub #8,r1
;;         br 23$

;;;cdown    ldy #down      ;y=/=0
;;;         jsr nextcell
;;;         dec y8pos
;;;         bpl loop2
;;11$:     mov down(r2),r2
;;         sub #8,r3
;;         br 22$

;;;cup      ldy #up       ;y=/=0
;;;         jsr nextcell
;;;         inc y8pos
;;;         jmp loop2
;;12$:     mov up(r2),r2
;;         add #8,r3
;;         br 22$

;;;cleft    ldy #left      ;y=0, x=/=0
;;;         jsr nextcell
;;;         inc x8pos
;;;         jmp loop3
;;13$:     mov left(r2),r2
;;         add #8,r1
;;         br 23$

putpixel3:       ;IN: r2,r3,r4
;;;         ldy y8byte
;;;         ora (adjcell),y
;;;         sta (adjcell),y
;;         add r2,r3
;;         bisb r4,@r3
;; 
;;;         jsr chkadd	;uses adjcell!
;;;         sta $ff3e
;;;         rts
;;         jmp @#chkadd
