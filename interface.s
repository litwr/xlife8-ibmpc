getkey:  xor ah,ah
         int 16h
         retn

getkey2: mov ah,1
         int 16h
         jnz getkey

         xor ax,ax
         retn
         

dispatcher: call getkey2
;;dispat0: cmpb #'g,r0
;;         bne 3$
         cmp al,'g'
         jnz .c3

;;         tstb @#mode
;;         beq 2$
         cmp [mode],0
         jz .c2

;;53$:     decb @#mode
;;         beq 40$
.c53:    dec [mode]
         jz .c40

;;         call @#initxt
;;         call @#showtopology
;;         call @#xyout
;;         br 40$
         jmp .c40

;;2$:      incb @#mode
;;40$:     jmp @#showmode
.c2:       inc [mode]
.c40:
       retn

.c3:     cmp al,'Q'
         jnz .c5

         mov [mode],3
.c101:   retn

.c5:     cmp al,'h'
         jnz .c4

;;         cmpb #2,@#mode
;;         beq 53$

;;         movb #2,@#mode
;;         call @#clrscn
;;         jmp @#showmode

.c4:     cmp al,'T'
         jnz .c6

;;         tstb @#topology
;;         beq 84$

;;         call @#torus
;;         clrb @#topology
;;         br 86$

;;84$:     call @#plain
;;         incb @#topology
;;86$:     jmp @#showtopology

.c6:     cmp al,'o'
         jnz .c7

         cmp [mode],0
         jnz .c101

         cmp [tilecnt],0
         jnz .c108

         call incgen
         jmp .c202

.c108:   call zerocc
         call generate
         call showscn
         jmp cleanup

.c7:     cmp al,'?'
         jnz .c8

         cmp [mode],2
         jz .c8
         jmp help

.c8:     cmp al,'C'
         jnz .c10

         cmp [tilecnt],0
         jnz .c201

         call zerogc
.c202:   jmp infoout
.c201:   jmp clear

.c10:    cmp al,'E'
         jnz .c11

;;         decb @#pseudoc
;;         beq 111$

;;         movb #1,@#pseudoc
;;111$:    jmp @#showscn

.c11:    cmp al,'!'
         jnz .c12

;;         call @#random
;;         jmp @#showscn

.c12:    cmp al,'%'
         jnz .c14

;;         cmpb #2,@#mode
;;         beq 14$
;;         jmp @#indens

;;14$:     cmpb #'B,r0
;;         beq 159$
;;         jmp @#15$
.c14:    cmp al,'B'
         jnz .c15

.c159:   call insteps
         mov ax,[temp2]
         or ax,ax
         jz .c142

         mov word [x0],ax
         call inmode
         js .c402
         jnz .c400
         jmp .c500

.c402:   call start_timer
.c146:   cmp [tilecnt],0
         jnz .c147

         call incgen
         jmp .c148

.c147:   call generate
         call cleanup
.c148:   dec word [x0]
         jnz .c146

.c401:   call benchcalc
.c142:   call tograph
         jmp calccells

.c400:   call tograph
         call start_timer

.c5146:  cmp [tilecnt],0
         jnz .c5147

         call incgen
         jmp .c5148

.c5147:  call zerocc
         call generate
         call showscn
         call cleanup
.c5148:  dec word [x0]
         jnz .c5146
         jmp .c401

.c500:   call tograph
         call start_timer
.c4147:  call showscn
         dec word [x0]
         jnz .c4147
         jmp .c401 

.c15:    cmp al,'R'
         jnz .c16

;;         call @#totext
;;         call @#inborn
;;         cmpb #3,r5         ;kt/esc
;;         beq 200$

;;         mov #born,r5
;;         call @#setrconst
;;         call @#instay
;;         mov #live,r5
;;         call @#setrconst
;;         call @#fillrt
;;200$:    call @#tograph
;;;*         jsr calccells    ;for load sequence
;;;*         jsr crsrset      ;showscn also calls crsrset! but crsrset is fast now...
;;;*         jmp crsrcalc
;;         return

;;16$:     cmpb #25,r0    ;cursor right
;;         bne 160$
.c16:    cmp ax,4d00h   ;cursor right
         jnz .c160

;;;*         jsr crsrclr
;;;*         ldy #right
;;         call @#crsrclr
;;         mov #crsrbit,r4
;;         mov #right,r1

;;;*         jsr shift
;;;*         bcc cright
;;         call @#shift
;;         beq 80$

;;;*         lda vptilecx
;;;*         adc #7
;;;*         jmp qleft
;;         add #8,@#vptilecx
;;         br 273$

;;;*cright   inc vptilecx
;;;*         lda crsrbit
;;;*         cmp #1
;;;*         beq cxright

;;;*         lsr crsrbit
;;;*         jmp cont17u
;;80$:     incb @#vptilecx
;;         movb @r4,r0
;;         cmpb r0,#1
;;         beq 71$

;;         rorb @#crsrbit    ;CY=0 by CMPB
;;         br 273$

;;;*cxright  lda #$80
;;;*         bne cm6
;;71$:    mov #128,r0
;;        br 72$

;;160$:    cmpb #8,r0   ;cursor left
;;         bne 161$
.c160:   cmp ax,4b00h ;cursor left
         jnz .c161

;;;*         jsr crsrclr
;;;*         ldy #left
;;         call @#crsrclr
;;         mov #crsrbit,r4
;;         mov #left,r1

;;;*         jsr shift
;;;*         bcc cleft
;;         call @#shift
;;         beq 81$

;;;*         lda vptilecx
;;;*         sbc #8
;;;*qleft    sta vptilecx
;;;*         jmp cont17u
;;         movb @#vptilecx,r0
;;         sub #8,r0
;;         movb r0,@#vptilecx
;;273$:    br 270$

;;;*cleft    dec vptilecx
;;;*         lda crsrbit
;;;*         cmp #$80
;;;*         beq cxleft

;;;*         asl crsrbit
;;;*         jmp cont17u
;;81$:     decb @#vptilecx
;;         movb @r4,r0
;;         cmpb #128,r0
;;         beq 76$

;;         aslb @#crsrbit
;;         br 270$

;;;*cxleft   lda #1
;;76$:     mov #1,r0

;;;*cm6      ldx #0
;;;*cm1      sta t1
;;;*         stx i2
;;;*         lda (crsrtile),y
;;;*         tax
;;;*         iny
;;;*         lda (crsrtile),y
;;;*         cmp #>plainbox
;;;*         bne cm4
;;;*
;;;*         cpx #<plainbox
;;;*         bne cm4
;;72$:     mov @#crsrtile,r2
;;         add r1,r2
;;         cmp @r2,#plainbox
;;         bne 73$

;;;*         ldx i2
;;;*         lda crsrbit,x
;;;*         sta t1
;;;*         bcs cm5
;;         movb @r4,r0
;;         br 74$
;;         
;;;*cm4      sta crsrtile+1
;;;*         stx crsrtile
;;;*cm5      lda t1
;;;*         ldx i2
;;;*         sta crsrbit,x
;;;*         jmp cont17u
;;73$:     mov @r2,@#crsrtile
;;74$:     movb r0,@r4
;;         br 270$

;;161$:    cmpb #26,r0   ;cursor up
;;         bne 162$
.c161:   cmp ax,4800h  ;cursor up
         jnz .c162

;;;*         jsr crsrclr
;;;*         ldy #up
;;         call @#crsrclr
;;         mov #crsrbyte,r4
;;         mov #up,r1

;;;*         jsr shift
;;;*         bcc cup
;;         call @#shift
;;         beq 82$

;;;*         lda vptilecy
;;;*         sbc #8
;;;*qup      sta vptilecy
;;;*         jmp cont17u
;;         sub #8*256,@#vptilecx
;;         br 270$

;;;*cup      dec vptilecy
;;;*         lda crsrbyte
;;;*         beq cxup
;;82$:     decb @#vptilecy
;;         tstb @r4
;;         beq 77$

;;;*         dec crsrbyte
;;;*         jmp cont17u
;;         decb @r4
;;         br 270$

;;;*cxup     lda #7
;;;*cm3      ldx #1
;;;*         bpl cm1
;;77$:     mov #7,r0
;;         br 72$

;;162$:    cmpb #27,r0   ;cursor down
;;         bne 17$
.c162:   cmp ax,5000h  ;cursor down
         jnz .c17

;;;*         jsr crsrclr
;;;*         ldy #down
;;         call @#crsrclr
;;         mov #crsrbyte,r4
;;         mov #down,r1

;;;*         jsr shift
;;;*         bcc cdown
;;         call @#shift
;;         beq 83$

;;;*         lda vptilecy
;;;*         adc #7
;;;*         bcc qup
;;         add #8*256,@#vptilecx
;;         br 270$

;;;*cdown    inc vptilecy
;;;*         lda crsrbyte
;;;*         cmp #7
;;;*         beq cxdown
;;83$:     incb @#vptilecy
;;         cmpb #7,@r4
;;         beq 78$

;;;*         inc crsrbyte
;;;*         bne cont17u
;;         incb @r4
;;         br 270$

;;;*cxdown   lda #0
;;;*         beq cm3
;;78$:     clr r0
;;         br 72$

;;17$:     cmpb #32,r0   ;space
;;         bne 170$
.c17:    cmp al,' '    ;space
         jnz .c170

;;;*         #assign16 adjcell,crsrtile
;;         mov @#crsrtile,r2
;;         movb r0,sum(r2)        ;always writes no-zero value

;;;*         jsr chkadd
;;         call @#chkadd

;;;*         ldy crsrbyte
;;         movb @#crsrbyte,r0

;;;*         lda (crsrtile),y
;;         add r2,r0

;;;*         eor crsrbit
;;;*         sta (crsrtile),y
;;         movb @#crsrbit,r1
;;         movb @r0,r2
;;         xor r1,r2
;;         movb r2,@r0

;;;*         ldy #sum
;;;*         and crsrbit
;;;*         beq lsp1
;;         bitb r1,r2
;;         beq 79$
;;         
;;;*         jsr inctsum
;;;*lsp2     sta (crsrtile),y  ;always writes no-zero value, so must be AC != 0
;;         mov #1,r2
;;         call @#inctsum

;;;*         lda zoom
;;;*         beq lsp3
;;;*
;;;*         jsr showscnz
;;;*lsp3     jsr infoout
;;;*         jmp cont17u
;;         call @#infoout
;;         br 270$

;;;*lsp1     jsr dectsum
;;;*         bne lsp2
;;79$:     call @#calccells
;;         br 270$

;;170$:    cmpb #'.,r0
;;         bne 171$
.c170:   cmp al,'.'
         jnz .c171

;;;*         jsr crsrclr
;;         call @#crsrclr

;;;*         lda #<tiles+(tilesize*249)
;;;*         sta crsrtile
;;;*         lda #>tiles+(tilesize*249)
;;;*         sta crsrtile+1
;;         mov #tiles+<tilesize*249>,@#crsrtile

;;;*         lda #1
;;;*         sta crsrbyte
;;         mov #1,r1
;;         movb r1,@#crsrbyte
;;;*cont17t  sta crsrbit
;;;*         jsr cont17u
;;272$:    movb r1,@#crsrbit
;;         call @#270$

;;;*         lda zoom
;;;*         beq exit0
;;         tstb @#zoom
;;         beq 100$

;;;*         jsr setviewport
;;;*         jsr showscnz
;;         call @#setviewport
;;         call @#showscnz

;;;*cont17u  jsr crsrset
;;;*         jmp crsrcalc
;;270$:    call @#crsrset
;;         jmp @#crsrcalc

;;171$:    cmpb #12,r0      ;home
;;         bne 172$
.c171:   cmp ax,4700h    ;home
         jnz .c172

;;;*         jsr crsrclr
;;         call @#crsrclr

;;;*         lda #<tiles
;;;*         sta crsrtile
;;;*         lda #>tiles
;;;*         sta crsrtile+1
;;         mov #tiles,@#crsrtile

;;;*         lda #0
;;;*         sta crsrbyte
;;         clrb @#crsrbyte

;;;*         lda #$80
;;;*         bne cont17t
;;         mov #128,r1
;;         br 272$

;;172$:    cmpb #'l,r0
;;         bne 173$
.c172:   cmp al,'l'
         jnz .c173

;;;*         lda zoom
;;;*         pha
;;;*         beq nozoom1
;;         movb @#zoom,r0
;;         push r0
;;         beq 301$

;;;*         jsr zoomout
;;         clrb @#zoom
;;         
;;;*nozoom1  jsr totext
;;;*         jsr loadmenu
;;;*         beq exitload
;;301$:    call @#loadmenu
;;         bcs 302$

;;;*cont17w  jsr loadpat
;;;*         jsr scrnorm
;;303$:    call @#tograph
;;         call @#loadpat

;;;*exitload jsr finish
;;;*         pla
;;;*         bne zoomin
;;302$:    pop r0
;;         movb r0,@#zoom
;;         mov #todata,@#pageport
;;         call @#calccells
;;         jmp @#tograph0

;;173$:     cmpb #'L,r0
;;         bne 174$
.c173:   cmp al,'L'
         jnz .c174

;;;*         lda fnlen
;;;*         bne cont17v
;;         tstb @#fn
;;         bne 317$

;;;*         rts
;;100$:    return

;;;*cont17v  lda zoom
;;;*         pha
;;;*         beq nozoom3
;;;*         jsr zoomout
;;317$:    movb @#zoom,r0
;;         push r0
;;         beq 303$
;;         
;;         call @#319$

;;;*nozoom3  jsr totext
;;;*         lda #147
;;;*         jsr BSOUT
;;;*         jsr curoff
;;;*         jmp cont17w
;;         br 303$

.c174:   cmp al,'+'
         jnz .c175

;;;*zoomin   jsr crsrclr
;;;*         jsr savebl     ;sets YR to 255
;;;*         sty zoom
;;;*         jsr xclrscn
;;;*         jsr setviewport
;;;*         jmp finish
;;         tstb @#zoom
;;         bne 100$

;;         call @#clrscn
;;         incb @#zoom
;;         call @#setviewport
;;271$:    jmp @#tograph0

.c175:   cmp al,'-'
         jnz .c176

;;         tstb @#zoom
;;         beq 100$

;;319$:    clrb @#zoom
;;         br 271$

.c176:   cmp al,'V'
         jnz .c177

;;         jmp @#showcomm

.c177:   cmp al,'v'
         jnz .c178

;;         jmp @#infov

.c178:   cmp al,'Z'
         jnz .c179

;;         call @#totext
;;         call @#chgcolors
;;220$:    jmp @#tograph

.c179:   cmp al,'X'
         jnz .c18

;;         call @#totext
;;         call @#setpalette
;;         br 220$

.c18:    cmp al,'S'
         jnz .c20

;;         call @#boxsz
;;         beq 20$

;;         call @#getsvfn
;;         bcs 220$

;;         call @#savepat
;;         br 220$

;;;*cont20   clc
;;;*         rts
.c20:      retn

;;;*shift    lda $543   ;shift st
;;;*         beq cont20
;;;*
;;;*         lda (crsrtile),y
;;;*         tax
;;;*         iny
;;;*         lda (crsrtile),y
;;;*         dey
;;;*         cmp #>plainbox
;;;*         bne cm4x
;;;*
;;;*         cpx #<plainbox
;;;*         beq cont20
;;;*
;;;*cm4x     sta crsrtile+1
;;;*         stx crsrtile
;;;*         sec
;;;*         rts
;;;*         .bend
;;shift:   tstb @#kbdbuf+1
;;         beq 20$

;;         mov @#crsrtile,r0
;;         add r1,r0
;;         cmp #plainbox,@r0
;;         beq 20$

;;         mov @r0,@#crsrtile
;;20$:     return

benchcalc: call stop_timer
         mov ax,20480    ;=4096*5=TIMERV*5
         mul [timercnt]
         mov cx,59659    ;=1193180/20
         div cx
         inc ax
         shr cx,1
         cmp dx,cx
         jbe .c143

         inc ax
.c143:   push ax
         xor dx,dx
         call todec      ;takes centiseconds in ds:ax
         call totext
         call printstr
         db 'TIME: $'
         call printfloat
         mov ax,[temp2]
         mov cx,10000
         mul cx
         pop si
         or si,si
         jz .c143x

.c143b:  mov cx,ax
         mov ax,dx
         xor dx,dx
         div si
         xchg ax,cx
         div si
         shr si,1
         cmp dx,si
         jb .c143a

         inc ax
.c143a:  mov dx,cx
         call todec
         call printstr
         db 's',0dh,0ah,'SPEED: $'
         call printfloat
.c143x:  call curoff
         jmp getkey

