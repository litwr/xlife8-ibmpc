;this program doesn't contain code of the original Xlife
;**it is the conversion from 6502 port for Commodore +4 v4
;**from z80 port for Amstrad CPC6128 v2, from K1801VM1 port for BK0011 v1
;written by litwr, 2015
;it is under GNU GPL


         include 'xlife.mac'

         org 100h
         use16

start:   ;push cs   ;??
         ;pop ds
         mov ah,19h   ;get current disk
         int 21h
         add al,'A'
         mov [loadmenu.c80],al
         
         xor bx,bx
.l2:     mov dl,bl
         mov ah,0eh   ;set current drive
         int 21h
         mov ah,19h
         int 21h
         mov [drives+bx],bh
         cmp al,bl
         jnz .l1

         add al,'A'
         cmp al,[loadmenu.c80]
         jnz .l3

         mov [curdrv],bl
.l3:     mov [drives+bx],al
.l1:     inc bx
         cmp bl,26
         jnz .l2

         mov dl,[loadmenu.c80]
         sub dl,'A'
         mov ah,0eh
         int 21h

         mov ax,0b800h
         mov es,ax
         ;;call @#copyr
         ;;mov #3,r2
         ;;call @#setpalette         ;inits also timer interrupt, sets active video page
         ;;incb @#errst
         call help

crsrflash2:
         ;;call @#crsrflash
mainloop:
         call dispatcher
         mov al,[mode]
         or al,al
         jz crsrflash2

         cmp al,3
         jnz .c3

         mov ax,3
         int 10h
         int 20h    ;directly to DOS?

.c3:     cmp [tilecnt],0
         jnz .c4

         mov [mode],0
         call incgen
         call initxt
         call showtopology
         call xyout
         call showscn
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
         include 'interface.s'

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

         include 'rules.s'
         include 'tile.s'

TIMERV          EQU     4096       ;1193180Hz/TIMERV=FREQ OF INTR8, approx 291.304 Hz

start_timer:    CLI                 ;SAVE/SET INTR8 VECTOR
                xor ax,ax
                mov [timercnt],ax
                mov [timercnt+2],ax
                mov ds,ax
                mov ax,[8*4]
                MOV [cs:SAVE8LO],ax
                mov ax,[8*4+2]
                MOV [cs:SAVE8HI],ax
                mov word [8*4],intr8
                mov [8*4+2],cs
                push cs
                pop ds
                MOV     AL,36H          ;SET TIMER 0 HARDWARE
                OUT     43H,AL
                MOV     AL,TIMERV AND 0FFH
                OUT     40H,AL
                MOV     AL,TIMERV SHR 8
                OUT     40H,AL
                STI
                RETN

stop_timer:     CLI                 ;SAVE/SET INTR8 VECTOR
                xor ax,ax
                mov ds,ax
                MOV ax,[cs:SAVE8LO]
                mov [8*4],ax
                MOV ax,[cs:SAVE8HI]
                mov [8*4+2],ax
                push cs
                pop ds
                MOV     AL,36H          ;RESTORE TIMER HARDWARE
                OUT     43H,AL
                XOR     AL,AL
                OUT     40H,AL
                OUT     40H,AL
                STI
                RETN

intr8:   inc [cs:timercnt]
         jnz .c1

         inc [cs:timercnt+2]
.c1:     test [cs:timercnt],TIMERV-1
         jz .c2

         push ax
         MOV AL,20H
         OUT 20H,AL
         pop ax
         iret

.c2:     db  0eah
SAVE8LO  DW      0
SAVE8HI  DW      0

generate:mov si,[startp]           ;currp=si
         mov cx,0c0c0h
         mov dx,3030h
         mov bp,0c0ch
         mov ax,303h
.c30:    setcount 0
         setcount 2
         setcount 4
         setcount 6
         mov si,[next+si]
         cmp si,1
         jz .c31
         jmp .c30
         ;jnz .c30  ;optimize for i8088???

.c31:    mov si,[startp]
.c5:     cmp byte [si+sum],0
         jnz .c1
         jmp .lnext        ;optimize for i8088???

;*cont3
.c1:     xor bx,bx
         or bl,byte [si]            ;top row
         jz .ldown

         mov di,[si+up]    ;adjcell=di, this line replaces iniadjc call!
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count7+2(r2)
         add [di+count7+2],cx
;;         add r4,count7(r2)
         add [di+count7],dx

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

.ldown:
         xor bx,bx
         or bl,[si+7]            ;bottom row
         jz .lleft

;;         mov down(r0),r2          ;adjcell=r2
         mov di,[si+down]

         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]

;;         add r3,count0+2(r2)
         add [di+count0+2],cx
;;         add r4,count0(r2)
         add [di+count0],dx

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

.lleft:
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
         mov bp,[si+ur]

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
         xor bx,bx
         or bl,[si+5]
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
         xor bx,bx
         or bl,[si+4]
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
         xor bx,bx
         or bl,[si+3]
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
         xor bx,bx
         or bl,[si+2]
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
         xor bx,bx
         or bl,[si+1]
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
         jz .delel

;;          mov r0,r2     ;save pointer to previous
          mov si,bx
;;          mov next(r0),r0
          mov bx,[bx+next]
;;          cmp #1,r0
          cmp bx,1
          jnz .c1

          retn

.delel:   dec [tilecnt]
;;          mov #count0,r1
          mov di,count0
;;          add r0,r1
          add di,bx
          xor ax,ax
          mov cx,16
.c2c:
          mov [di],ax
          add di,2
          loop .c2c
;;          mov next(r0),r1
;;          clr next(r0)
;;          mov r1,r0
          xchg ax,[bx+next]
          mov bx,ax
;;          tst r2
          or si,si
          jz .del1st

;;         mov r1,next(r2)
         mov [si+next],ax
;;         dec r1
         dec ax
         jnz .c1

.c4:     retn

.del1st:
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

        align 2
tiles:
         include 'initiles.s'

crsrtab   dw 0,2000h,80,2050h,160,20a0h,240,20f0h
digifont  dw 0a00ah,2828h,0a828h,282ah,2828h,2828h,0a00ah,0  ;8th columns are free
          dw 8002h,8002h,800ah,8002h,8002h,8002h,0a82ah,0
          dw 0a00ah,2828h,2800h,0a000h,0ah,28h,0a82ah,0
          dw 0a00ah,2828h,2800h,0a000h,2800h,2828h,0a00ah,0
          dw 2800h,0a000h,0a802h,280ah,0aa2ah,2800h,2800h,0
          dw 0a82ah,28h,0a02ah,2800h,2800h,2828h,0a00ah,0    ;5
          dw 0a00ah,2828h,28h,0a02ah,2828h,2828h,0a00ah,0
          dw 0a82ah,2828h,0a000h,8002h,8002h,8002h,8002h,0
          dw 0a00ah,2828h,2828h,0a00ah,2828h,2828h,0a00ah,0
          dw 0a00ah,2828h,2828h,0a80ah,2800h,2828h,0a00ah,0
          dw 0,0,0,0,0,0,0                ;space
startp    dw 1
tilecnt   dw 0
viewport  dw tiles
crsrtile  dw tiles
timercnt  dw 0, 0
temp      dw 0
temp2     dw 0
;kbuf:     dw 0
saved     dw 0
tobin     dw 1,10,100,1000,10000
live      dw 12
born      dw 8
x0        db 0   ;word aligned for the speed
y0        db 0
crsrbyte  db 0      ;y%8  word aligned
crsrbit   db 128    ;x bit position
i1        db 0,0
cellcnt   db 0,0,0,0,0
gencnt    db 0,0,0,0,0,0,0
crsrx     db 0      ;[x/8]*8, word aligned
crsry     db 0      ;[y/8]*8
vptilecx  db 0      ;must be word aligned
vptilecy  db 0
xcrsr     db 0,0,0
ycrsr     db 0,0,0  ;must follow xcrsr
tinfo     db 0,0,0
xchgdir   db 0
xdir      db 0      ;linear transformation, word aligned
ydir      db 0
clncnt    db 0
;palette  db 0      ;not word aligned???
pseudoc   db 0
mode      db 0      ;0-stop, 1-run, 2-hide, 3-exit
zoom      db 0
fn        db 0,0,0,0,0,0,0,0,0,0,0,0
density   db 3
;;dirname  .TEXT "0:"      ;filename used to access directory
topology  db 0      ;0 - torus
;crsrticks:  db 0
;copyleft: .ascii "CR.TXT"
;errst:     db 0   ;0 - do not print i/o-errors message, 1 - print
ppmode    db 1    ;putpixel mode: 0 - tentative, 1 - active
crsrpgmk  db 1   ;0 - do not draw cursor during showscnz, 1 - draw
svfn      db 0,0,0,0,0,0,0,0,0,0,0,0
msgtore   db 'TORUS$'
msgplain  db 'PLAIN$'
drives    rb 26
curdrv    db 0
nofnchar db '?,./:;<=>[\]|'
stringbuf rb 19     ;must be after nofnchar

