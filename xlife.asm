;this program doesn't contain code of the original Xlife
;**it is the conversion from 6502 port for Commodore +4 v4
;**from z80 port for Amstrad CPC6128 v2, from K1801VM1 port for BK0011 v1
;written by litwr, 2015
;it is under GNU GPL


         include 'xlife.mac'

         org 100h
         use16

start:   mov ax,0b800h
         mov es,ax
         push cs
         pop ds
         ;call @#copyr
         ;mov #^B10010,@#timerport3    ;start timer
         ;jsr r3,@#printstr
         ;.byte 155,154,0,0   ;cursor off, 32 chars

         ;mov #emptyirq,@#^O100         ;empty timer irq
         ;mov #3,r2
         ;call @#setpalette         ;inits also timer interrupt, sets active video page
         ;incb @#errst
         mov [crsrtile],tiles
         ;call @#tograph
         ;call @#calccells
         ;call @#infoout
         ;mov #crsrirq,@#^O100
         ;call @#help

         mov ax,4    ;set video mode #4 = 320x200x4
         int 10h

     mov si,tiles
     mov [startp],si
     mov word [si+next],1
     mov byte [si+sum],1
     mov byte [si+2],14
     mov [tilecnt],1

crsrflash2: 
         ;;call @#crsrflash
mainloop: 
         ;call dispatcher
         mov al,[mode]
         or al,al
         jz crsrflash2

         cmp al,3
         jnz .c3

         int 20h    ;directly to DOS?

.c3:     cmp [tilecnt],0
         jnz .c4

         mov byte [mode],0
         call incgen
         ;call initxt
         ;call showtopology
         ;call xyout
         call showscn
         ;call showmode
         jmp crsrflash2

.c4:     cmp al,2
         jnz .c5

         call generate     ;hide
         call cleanup
         jmp mainloop

.c5:     call zerocc
         call generate
         call showscn
         call cleanup
         jmp crsrflash2

;         include 'io.s'
;         include 'ramdisk.s'
         include 'video-base.s'
         include 'video.s'
         include 'utils.s'
;          include 'interface.s'

;benchirq0: mov @#saved,r0
;           mov @#timerport2,r1
;           mov r1,@#saved
;           sub r1,r0
;           add r0,@#lowbench
;           adc @#highbench
;           return

;benchirq:  push r0
;           push r1
;           call @#benchirq0
;           pop r1
;           pop r0
;           rti

;crsrflash: return
;           nop

;crsrirq:   cmp @#plainbox+left,#plainbox   ;test memory bank
;           bne emptyirq

;           incb @#crsrticks
;           bitb #15,@#crsrticks
;           bne emptyirq

;           mov #95,@#crsrflash     ;95 = $5f = jmp@#
;           mov #crsrset2,@#crsrflash+2
;           bitb #16,@#crsrticks
;           beq emptyirq

;           mov #crsrclr2,@#crsrflash+2
;emptyirq:  rti

;         include 'rules.s'
         include 'tile.s'

generate:
;;         mov @#startp,r0           ;currp=r0
         mov si,[startp]           ;currp=r0

;;         mov #^B0011111100111111,r3
         mov cx,0c0c0h
;;         mov #^B1100111111001111,r4
         mov dx,3030h
;;         mov #^B1111001111110011,r5
         mov bp,0c0ch
         mov ax,303h
.c30:
         setcount 0
         setcount 2
         setcount 4
         setcount 6

;;         mov next(r0),r0
         mov si,[next+si]
         cmp si,1
         jz .c31        
         jmp .c30
         ;jnz .c30  ;optimize for i8088???

.c31:     
;;         mov @#startp,r0
         mov si,[startp]
.c5:
;;5$:      tstb sum(r0)
         cmp byte [si+sum],0
         jnz .c1
         jmp .lnext        ;opotimize for i8088???

;*cont3
.c1:     xor bx,bx
;;         movb @r0,r1
         or bl,byte [si]            ;top row, later saved at 6502 X

         ;*beq ldown
         jz .c3

;;         mov up(r0),r2       ;adjcell=r2, this line replaces iniadjc call!
         mov di,[si+up]

;;         asl r1
         shl bl,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count7+2(r2)
         add cx,[di+count7+2]
;;         add r4,count7(r2)
         add dx,[di+count7]

;;         add r3,count1+2(r0)
         add [si+count1+2],cx
;;         add r4,count1(r0)
         add [si+count1],dx

;;         add tab2223(r1),count0+2(r0)
         mov ax,[bx+tab2223]
         add [si+count0+2],ax
;;         add tab2021(r1),count0(r0)
         mov ax,[bx+tab2021]
         add [si+count0],ax
         call chkadd

;*ldown:
.c3:
         mov bl,[si+7]            ;top row, later saved at 6502 X
         or bl,bl
         ;*beq lleft
         jz .c4

;;         mov down(r0),r2          ;adjcell=r2
         mov di,[si+down]

         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]

;;         add r3,count0+2(r2)
         add [si+count0+2],cx
;;         add r4,count0(r2)
         add [si+count0],dx

;;         add r3,count6+2(r0)
         add [si+count6+2],cx
;;         add r4,count6(r0)
         add [si+count6],dx

;;         add tab2223(r1),count7+2(r0)
         mov ax,[bx+tab2223]
         add [si+count7+2],ax
;;         add tab2021(r1),count7(r0)
         mov ax,[bx+tab2021]
         add [si+count7],ax
         call chkadd

;*lleft
.c4:
;;         mov left(r0),r2          ;adjcell=r2
         mov di,[si+left]

;;         mov #1024,r4             ;item to add
         mov dx,1024

;;         clr r3     ;change indicator
         xor cx,cx

;;         mov @r0,r1               ;2 rows
         mov bx,[si]
         or bx,bx
         jns .c6

;;         mov r1,r3
         mov cx,bx

;;         add r4,count0+2(r2)
         add [di+count0+2],dx

;;         add r4,count1+2(r2)
         add [di+count1+2],dx

;;         add r4,count2+2(r2)
         add [di+count2+2],dx

.c6:
;;         tstb r1
         or bl,bl
         jns .c7

;;         mov r1,r3
         mov cx,bx

;;         add r4,count0+2(r2)
         add [di+count0+2],dx

;;         add r4,count1+2(r2)
         add [di+count1+2],dx

;;         mov ul(r0),r5          ;adjcell2=r5
         mov bp,[si+ul]

;;         add r4,count7+2(r5)
         add [ds:bp+count7+2],dx
         call chkadd2

.c7:
;;         mov 2(r0),r1               ;2 rows
         mov bx,[si+2]
         or bx,bx
         jns .c8

;;         mov r1,r3
         mov cx,bx

;;         add r4,count2+2(r2)
         add [di+count2+2],dx

;;         add r4,count3+2(r2)
         add [di+count3+2],dx

;;         add r4,count4+2(r2)
         add [di+count4+2],dx

.c8:
;;         tstb r1
         or bl,bl
         jns .c9

;;         mov r1,r3
         mov cx,bx

;;         add r4,count1+2(r2)
         add [di+count1+2],dx

;;         add r4,count2+2(r2)
         add [di+count2+2],dx

;;         add r4,count3+2(r2)
         add [di+count3+2],dx

.c9:
;;         mov 4(r0),r1               ;2 rows
         mov bx,[si+4]
         or bx,bx
         jns .c10

;;         mov r1,r3
         mov cx,bx

;;         add r4,count4+2(r2)
         add [di+count4+2],dx

;;         add r4,count5+2(r2)
         add [di+count5+2],dx

;;         add r4,count6+2(r2)
         add [di+count6+2],dx

.c10:
;;         tstb r1
         or bl,bl
         jns .c11

;;         mov r1,r3
         mov cx,bx

;;         add r4,count3+2(r2)
         add [di+count3+2],dx
;;         add r4,count4+2(r2)
         add [di+count4+2],dx
;;         add r4,count5+2(r2)
         add [di+count5+2],dx

.c11:
;;         mov 6(r0),r1               ;2 rows
         mov bx,[si+6]
         or bx,bx
         jns .c12

;;         mov r1,r3
         mov cx,bx
;;         add r4,count6+2(r2)
         add [di+count6+2],dx
;;         add r4,count7+2(r2)
         add [di+count7+2],dx
;;         mov dl(r0),r5          ;adjcell2=r5
         mov bp,[si+dle]
;;         add r4,count0+2(r5)
         add [ds:bp+count0+2],dx
;;         call @#chkadd2
         call chkadd2
.c12:
;;         tstb r1
         or bl,bl
         jns .c14

;;         mov r1,r3
         mov cx,bx
;;         add r4,count5+2(r2)
         add [di+count5+2],dx
;;         add r4,count6+2(r2)
         add [di+count6+2],dx
;;         add r4,count7+2(r2)
         add [di+count7+2],dx
.c14:    call chkaddt

         ;*ldy #right
;;         mov right(r0),r2          ;adjcell=r2
         mov di,[si+right]
;;         mov #8,r4                ;item to add
         mov dx,8

;;         clr r3
         xor cx,cx

;;         mov @r0,r1               ;2 rows
         mov bx,[si]

;;         asr r1
         shr bx,1
         jnc .c15

         inc cx
;;         mov ur(r0),r5          ;adjcell2=r5
         mov bp,[si]

;;         add r4,count7(r5)
         add [ds:bp+count7],dx
;;         add r4,count0(r2)
         add [di+count0],dx
;;         add r4,count1(r2)
         add [di+count1],dx
         call chkadd2

;*lr1
.c15:
;;         tstb r1
         or bl,bl
         jns .c16

;;         mov r1,r3
         mov cx,bx
;;         add r4,count0(r2)
         add [di+count0],dx
;;         add r4,count1(r2)
         add [di+count1],dx
;;         add r4,count2(r2)
         add [di+count2],dx

;*lr2
.c16:
;;         mov 2(r0),r1               ;2 rows
         mov bx,[si+2]

;;         asr r1
         shr bx,1
         jnc .c17

;;         adc r3
         inc cx
;;         add r4,count1(r2)
         add [di+count1],dx
;;         add r4,count2(r2)
         add [di+count2],dx
;;         add r4,count3(r2)
         add [di+count3],dx

;*lr3
.c17:
;;         tstb r1
         or bl,bl
         jns .c18

;;         mov r1,r3
         mov cx,bx
;;         add r4,count2(r2)
         add [di+count2],dx
;;         add r4,count3(r2)
         add [di+count3],dx
;;         add r4,count4(r2)
         add [di+count4],dx

;*lr4
.c18:
;;         mov 4(r0),r1               ;2 rows
         mov bx,[si+4]

;;         asr r1
         shr bx,1
         jnc .c19

;;         adc r3
         inc cx
;;         add r4,count3(r2)
         add [di+count3],dx
;;         add r4,count4(r2)
         add [di+count4],dx
;;         add r4,count5(r2)
         add [di+count5],dx

;*lr5
.c19:
;;         tstb r1
         or bl,bl
         jns .c20

;;         mov r1,r3
         mov cx,bx
;;         add r4,count4(r2)
         add [di+count4],dx
;;         add r4,count5(r2)
         add [di+count5],dx
;;         add r4,count6(r2)
         add [di+count6],dx

;*lr6
.c20:
;;         mov 6(r0),r1               ;2 rows
         mov bx,[si+6]
;;         asr r1
         shr bx,1
         jnc .c21

;;         adc r3
         inc cx
;;         add r4,count5(r2)
         add [di+count5],dx
;;         add r4,count6(r2)
         add [di+count6],dx
;;         add r4,count7(r2)
         add [di+count7],dx

;*lr7
.c21:
;;         tstb r1
         or bl,bl
         jns .c22

;;         mov r1,r3
         mov cx,bx
;;         add r4,count6(r2)
         add [di+count6],dx
;;         add r4,count7(r2)
         add [di+count7],dx
;;         mov dr(r0),r5          ;adjcell2=r5
         mov bp,[si+dr]
;;         add r4,count0(r5)
         add [ds:bp+count0],dx
         call chkadd2

.c22:    call chkaddt
;;         movb 6(r0),r1
         xor bx,bx
         or bl,[si+6]
         jz .c23

;;         asl r1
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count7+2(r0)
         add [si+count7+2],cx
;;         add r4,count7(r0)
         add [si+count7],dx
;;         add r3,count5+2(r0)
         add [si+count5+2],cx
;;         add r4,count5(r0)
         add [si+count5],dx
;;         add tab2223(r1),count6+2(r0)
         mov ax,[bx+tab2223]
         add [si+count6+2],ax
;;         add tab2021(r1),count6(r0)
         mov ax,[bx+tab2021]
         add [si+count6],ax

;*l2
.c23:
;;         movb 5(r0),r1
         mov bl,[si+5]
         or bl,bl
         jz .c24

;;         asl r1
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count6+2(r0)
         add [si+count6+2],cx
;;         add r4,count6(r0)
         add [si+count6],dx
;;         add r3,count4+2(r0)
         add [si+count4+2],cx
;;         add r4,count4(r0)
         add [si+count4],dx
;;         add tab2223(r1),count5+2(r0)
         mov ax,[bx+tab2223]
         add [si+count5+2],ax
;;         add tab2021(r1),count5(r0)
         mov ax,[bx+tab2021]
         add [si+count5],ax

;*l3
.c24:
;;         movb 4(r0),r1
         mov bl,[si+4]
         or bl,bl
         jz .c25

;;         asl r1
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count5+2(r0)
         add [si+count5+2],cx
;;         add r4,count5(r0)
         add [si+count5],dx
;;         add r3,count3+2(r0)
         add [si+count3+2],cx
;;         add r4,count3(r0)
         add [si+count3],dx
;;         add tab2223(r1),count4+2(r0)
         mov ax,[bx+tab2223]
         add [si+count4+2],ax
;;         add tab2021(r1),count4(r0)
         mov ax,[bx+tab2021]
         add [si+count4],ax

;*l4
.c25:
;;         movb 3(r0),r1
         mov bl,[si+3]
         or bl,bl
         jz .c26

;;         asl r1
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count4+2(r0)
         add [si+count4+2],cx
;;         add r4,count4(r0)
         add [si+count4],dx
;;         add r3,count2+2(r0)
         add [si+count2+2],cx
;;         add r4,count2(r0)
         add [si+count2],dx
;;         add tab2223(r1),count3+2(r0)
         mov ax,[bx+tab2223]
         add [si+count3+2],ax
;;         add tab2021(r1),count3(r0)
         mov ax,[bx+tab2021]
         add [si+count3],ax

;*l5
.c26:
;;         movb 2(r0),r1
         mov bl,[si+2]
         or bl,bl
         jz .c27

;;         asl r1
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count3+2(r0)
         add [si+count3+2],cx
;;         add r4,count3(r0)
         add [si+count3],dx
;;         add r3,count1+2(r0)
         add [si+count1+2],cx
;;         add r4,count1(r0)
         add [si+count1],dx
;;         add tab2223(r1),count2+2(r0)
         mov ax,[bx+tab2223]
         add [si+count2+2],ax
;;         add tab2021(r1),count2(r0)
         mov ax,[bx+tab2021]
         add [si+count2],ax

;*l6
.c27:
;;         movb 1(r0),r1
         mov bl,[si+1]
         or bl,bl
         jz .lnext

;;         asl r1
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count2+2(r0)
         add [si+count2+2],cx
;;         add r4,count2(r0)
         add [si+count2],dx
;;         add r3,count0+2(r0)
         add [si+count0+2],cx
;;         add r4,count0(r0)
         add [si+count0],dx
;;         add tab2223(r1),count1+2(r0)
         mov ax,[bx+tab2223]
         add [si+count1+2],ax
;;         add tab2021(r1),count1(r0)
         mov ax,[bx+tab2021]
         add [si+count1],ax

.lnext:
;;28$:
;;         mov next(r0),r0
         mov si,[si+next]

;;         cmp #1,r0
         cmp si,1
         jz stage2
         jmp .c5

stage2:
;;         mov @#startp,r0
         mov si,[startp]

;*genloop2
.c1:
;;         clrb sum(r0)
         mov byte [si+sum],0
         genmac 0
         genmac 1
         genmac 2
         genmac 3
         genmac 4
         genmac 5
         genmac 6
         genmac 7
;;         mov next(r0),r0
         mov si,[si+next]
;;         cmp #1,r0
         cmp si,1
         jz incgen
         jmp .c1

incgen:   mov bx,gencnt+7
          stc
          incbcd rts2
          incbcd rts2
          incbcd rts2
          incbcd rts2
          incbcd rts2
          incbcd rts2
          incbcd rts2
rts2:     retn

cleanup:  inc [clncnt]
          test [clncnt],15
          jnz rts2

cleanup0:
;;          mov @#startp,r0
          mov bx,[startp]
;;          clr r2        ;mark 1st
          xor si,si
.c1:
;;          tstb sum(r0)
         test byte [bx+sum],0ffh

;*         beq delel
          jz .c2

;;          mov r0,r2     ;save pointer to previous
          mov si,bx
;;          mov next(r0),r0
          mov bx,[bx+next]
;;          cmp #1,r0
          cmp bx,1
          jnz .c1

          retn

;*delel
.c2:     dec [tilecnt]
;;          mov #count0,r1
          mov di,count0
;;          add r0,r1
          add di,bx
          xor ax,ax
          mov cx,16
          rep stosw
;;          mov next(r0),r1
;;          clr next(r0)
;;          mov r1,r0
          xchg ax,[bx+next]
          mov bx,ax
;;          tst r2
          or si,si
          jz .c3

;;         mov r1,next(r2)
         mov [si+next],ax
;;         dec r1
         dec bx
         jnz .c1

.c4:     retn

;*del1st
.c3:
;;         mov r1,@#startp
         mov [startp],bx

;;         tst @#tilecnt
         or [tilecnt],0
         jnz .c1
         retn

         include 'tab12.s'
gentab:
         include 'gentab.s'
;         include 'ramdata.s'

         include 'vistab.s'

         align 2
startp    dw 1
tilecnt   dw 0
viewport  dw 0
crsrtile  dw 0
temp      dw 0
temp2     dw 0
;kbdbuf:     dw 0
saved:      dw 0
;lowbench:   dw 0
;highbench:   dw 0
;tobin:      dw 1,10,100,1000,10000
live     dw 12
born     dw 8

x0        db 0   ;word aligned
y0        db 0
crsrbyte  db 0      ;y%8  word aligned
crsrbit   db 128    ;x bit position
i1        db 0,0
cellcnt   db 0,0,0,0,0
gencnt    db 0,0,0,0,0,0,0
crsrx     db 0      ;[x/8]*8, word aligned!
crsry     db 0      ;[y/8]*8
vptilecx  db 0      ;word aligned!
vptilecy  db 0
xcrsr     db 0,0,0
ycrsr     db 0,0,0
tinfo     db 0,0,0  ;even alignment for BK!
xchgdir   db 0
xdir      db 0      ;linear transformation, word aligned
ydir      db 0
clncnt    db 0
;palette  db 0      ;not word aligned???
pseudoc   db 0
mode      db 1      ;0-stop, 1-run, 2-hide, 3-exit
zoom      db 0
fn        db 0,0,0,0,0,0,0,0,0,0,0,0
density   db 3         ;must follow fn
;;dirname  .TEXT "0:"      ;filename used to access directory
topology  db 0      ;0 - torus
;crsrticks:  db 0
;copyleft: .ascii "CR.TXT"
;errst:     db 0   ;0 - do not print i/o-errors message, 1 - print
ppmode    db 1    ;putpixel mode: 0 - tentative, 1 - active
crsrpgmk  db 1   ;0 - do not draw cursor during showscnz, 1 - draw
svfn      db 0,0,0,0,0,0,0,0,0,0,0,0
;msghide:  .asciz "HIDE"  ;must follow svfn
;msgtore:  .asciz "TORUS"
;msgplan:  .asciz "PLAIN"
;msgrun:   .asciz "RUN "
;msgstop:  .asciz "STOP"
nofnchar db '?%(),./:;<=>[\]|'   ;? - must be the first

;stringbuf: .blkb 19       ;it must be at odd addr!

tab3      db 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4
          db 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
          db 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
          db 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
          db 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
          db 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
          db 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
          db 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
          db 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
          db 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
          db 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
          db 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
          db 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
          db 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
          db 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
          db 4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8

ttab      db 0,1,2,3,3,4,5,6,7,8,8,9,16,17,18,19,19,20
          db 21,22,23,24,24,25,32,33,34,35,35,36
          db 37,38,39,40,40,41,48,49,50,51,51,52
          db 53,54,55,56,56,57,64,65,66,67,67,68
          db 69,70,71,72,72,73,80,81,82,83,83,84
          db 85,86,87,88,88,89,96,97,98,99,99,100
          db 101,102,103,104,104,105,112,113,114,115,115,116
          db 117,118,119,120,120,121,128,129,130,131,131,132
          db 133,134,135,136,136,137,144,145,146,147,147,148
          db 149,150,151,152,152,153

bittab    db 1,2,4,8,16,32,64,128

digifont:   ;8th columns are free
;       db 28,34,50,42,38,34,28, 0 
;       db  8,12, 8, 8, 8, 8,28, 0
;       db 28,34,16, 8, 4, 2,62, 0
;       db 62,32,16,28,32,34,28, 0
;       db 16,24,20,18,62,16,16, 0  ;4
;       db 62, 2,30,32,32,34,28, 0
;       db 24, 4, 2,30,34,34,28, 0
;       db 62,32,16, 8, 4, 4, 4, 0  ;7
;       db 28,34,34,28,34,34,28, 0
;       db 28,34,34,60,32,16,12, 0
;       db  0, 0, 0, 0, 0, 0, 0, 0   ;space
        dw  672,2056,2568,2184,2088,2056, 672, 0
        dw  128, 160, 128, 128, 128, 128, 672, 0
        dw  672,2056, 512, 128,  32,   8,2720, 0
        dw 2720,2048, 512, 672,2048,2056, 672, 0
        dw  512, 640, 544, 520,2720, 512, 512, 0
        dw 2720,   8, 680,2048,2048,2056, 672, 0
        dw  640,  32,   8, 680,2056,2056, 672, 0
        dw 2720,2048, 512, 128,  32,  32,  32, 0
        dw  672,2056,2056, 672,2056,2056, 672, 0
        dw  672,2056,2056,2720,2048, 512, 160, 0
        dw  0, 0, 0, 0, 0, 0, 0, 0   ;space

         align 2
tiles:
         include 'initiles.s'

