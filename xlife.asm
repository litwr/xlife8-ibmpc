;this program doesn't contain code of the original Xlife
;**it is the conversion from 6502 port for Commodore +4 v4
;**from z80 port for Amstrad CPC6128 v2, from K1801VM1 port for BK0011 v1
;written by litwr, 2015
;it is under GNU GPL


         include 'xlife.mac'

         org 100h
         use16

start:   mov [iobseg],ds
         add [iobseg],1000h  ;64k/16

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

         call chgdrv.ee1
         mov ax,0b800h
         mov es,ax
         call copyr
         call setcolors
         ;;incb @#errst
         call help

mainloop:
         call crsrflash
.e1:     call dispatcher
         mov al,[mode]
         or al,al
         jz mainloop

         cmp al,3
         jnz .c3

         mov ah,3bh
         mov dx,rootpath
         int 21h

         mov ax,3
         call totext.e1
         int 20h    ;directly to DOS?

.c3:     cmp [tilecnt],0
         jnz .c4

         mov [mode],0
         call incgen
         call tograph
         jmp mainloop

.c4:     cmp al,2
         jnz .c5

         call generate     ;hide
         call cleanup
         jmp .e1

.c5:     call zerocc
         call generate
         call showscn
         call cleanup
         jmp mainloop

         include 'io.s'
         include 'ramdisk.s'
         include 'video-base.s'
         include 'video.s'
         include 'utils.s'
         include 'interface.s'
         include 'rules.s'
         include 'tile.s'
         include 'ramdata.s'

TIMERV          EQU     4096       ;1193180Hz/TIMERV=FREQ OF INTR8, approx 291.304 Hz

start_timer:    cli                 ;SAVE/SET INTR8 VECTOR
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
                MOV     AL,36H          ;SET TIMER 0 HARDWARE
                OUT     43H,AL
                MOV     AL,TIMERV AND 0FFH
                OUT     40H,AL
                MOV     AL,TIMERV SHR 8
                jmp stop_timer.e1

stop_timer:     CLI                 ;SAVE/SET INTR8 VECTOR
                xor ax,ax
                mov ds,ax
                MOV ax,[cs:SAVE8LO]
                mov [8*4],ax
                MOV ax,[cs:SAVE8HI]
                mov [8*4+2],ax
                MOV     AL,36H          ;RESTORE TIMER HARDWARE
                OUT     43H,AL
                XOR     AL,AL
                OUT     40H,AL
.e1:            OUT     40H,AL
                jmp stop_timer2.e1

start_timer2:   cli                 ;SAVE/SET INTR8 VECTOR
                xor ax,ax
                cmp ax,[cs:SAVE8LO2]
                jne stop_timer2.exit

                mov ds,ax
                mov ax,[8*4]
                MOV [cs:SAVE8LO2],ax
                mov ax,[8*4+2]
                MOV [cs:SAVE8HI2],ax
                mov word [8*4],intr82
                mov [8*4+2],cs
                jmp stop_timer2.e1


stop_timer2:    CLI                 ;SAVE/SET INTR8 VECTOR
                xor ax,ax
                cmp ax,[cs:SAVE8LO2]
                je .exit

                mov ds,ax
                XCHG ax,[cs:SAVE8LO2]
                mov [8*4],ax
                MOV ax,[cs:SAVE8HI2]
                mov [8*4+2],ax
.e1:            push cs
                pop ds
.exit:          STI
.e2:            RETN

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

intr82:  inc [cs:crsrticks]
          db  0eah
SAVE8LO2  DW      0
SAVE8HI2  DW      0

crsrflash:
         test [crsrticks],3
         jne stop_timer2.e2

         test [crsrticks],4
         je .l1

         jmp crsrset
.l1:     jmp crsrclr

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

.c1:     xor bx,bx
         or bl,byte [si]            ;top row
         jz .ldown

         mov di,[si+up]    ;adjcell=di, this line replaces iniadjc call!
         shl bx,1
         mov cx,[bx+tab1213]
         mov dx,[bx+tab1011]
         add [di+count7+2],cx
         add [di+count7],dx

         add [si+count1+2],cx
         add [si+count1],dx

         mov ax,[bx+tab2223]
         add [si+count0+2],ax
         mov ax,[bx+tab2021]
         add [si+count0],ax
         call chkadd
.ldown:  xor bx,bx
         or bl,[si+7]            ;bottom row
         jz .lleft

         mov di,[si+down]
         shl bx,1
         mov cx,[bx+tab1213]
         mov dx,[bx+tab1011]
         add [di+count0+2],cx
         add [di+count0],dx

         add [si+count6+2],cx
         add [si+count6],dx

         mov ax,[bx+tab2223]
         add [si+count7+2],ax
         mov ax,[bx+tab2021]
         add [si+count7],ax
         call chkadd
.lleft:  mov di,[si+left]   ;adjcell=di
         mov dx,1024
         xor cx,cx
         mov bx,[si]
         or bx,bx
         jns .c6

         mov cx,bx
         add [di+count0+2],dx
         add [di+count1+2],dx
         add [di+count2+2],dx
.c6:     or bl,bl
         jns .c7

         mov cx,bx
         add [di+count0+2],dx
         add [di+count1+2],dx
         mov bp,[si+ul]   ;adjcell2=bp
         add [ds:bp+count7+2],dx
         call chkadd2
.c7:     mov bx,[si+2]
         or bx,bx
         jns .c8

         mov cx,bx
         add [di+count2+2],dx
         add [di+count3+2],dx
         add [di+count4+2],dx
.c8:     or bl,bl
         jns .c9

         mov cx,bx
         add [di+count1+2],dx
         add [di+count2+2],dx
         add [di+count3+2],dx
.c9:     mov bx,[si+4]     ;2 rows
         or bx,bx
         jns .c10

         mov cx,bx
         add [di+count4+2],dx
         add [di+count5+2],dx
         add [di+count6+2],dx
.c10:    or bl,bl
         jns .c11

         mov cx,bx
         add [di+count3+2],dx
         add [di+count4+2],dx
         add [di+count5+2],dx
.c11:    mov bx,[si+6]
         or bx,bx
         jns .c12

         mov cx,bx
         add [di+count6+2],dx
         add [di+count7+2],dx
         mov bp,[si+dle]
         add [ds:bp+count0+2],dx
         call chkadd2
.c12:
         or bl,bl
         jns .c14

         mov cx,bx
         add [di+count5+2],dx
         add [di+count6+2],dx
         add [di+count7+2],dx
.c14:    call chkaddt

         mov di,[si+right]
         mov dx,8
         xor cx,cx
         mov bx,[si]
         shr bx,1
         jnc .c15

         inc cx
         mov bp,[si+ur]
         add [ds:bp+count7],dx
         add [di+count0],dx
         add [di+count1],dx
         call chkadd2
.c15:    or bl,bl
         jns .c16

         mov cx,bx
         add [di+count0],dx
         add [di+count1],dx
         add [di+count2],dx
.c16:    mov bx,[si+2]
         shr bx,1
         jnc .c17

         inc cx
         add [di+count1],dx
         add [di+count2],dx
         add [di+count3],dx
.c17:    or bl,bl
         jns .c18

         mov cx,bx
         add [di+count2],dx
         add [di+count3],dx
         add [di+count4],dx
.c18:    mov bx,[si+4]
         shr bx,1
         jnc .c19

         inc cx
         add [di+count3],dx
         add [di+count4],dx
         add [di+count5],dx
.c19:    or bl,bl
         jns .c20

         mov cx,bx
         add [di+count4],dx
         add [di+count5],dx
         add [di+count6],dx
.c20:    mov bx,[si+6]
         shr bx,1
         jnc .c21

         inc cx
         add [di+count5],dx
         add [di+count6],dx
         add [di+count7],dx
.c21:    or bl,bl
         jns .c22

         mov cx,bx
         add [di+count6],dx
         add [di+count7],dx
         mov bp,[si+dr]
         add [ds:bp+count0],dx
         call chkadd2
.c22:    call chkaddt
         xor bx,bx
         or bl,[si+6]
         jz .c23

         shl bx,1
         mov cx,[bx+tab1213]
         mov dx,[bx+tab1011]
         add [si+count7+2],cx
         add [si+count7],dx
         add [si+count5+2],cx
         add [si+count5],dx
         mov ax,[bx+tab2223]
         add [si+count6+2],ax
         mov ax,[bx+tab2021]
         add [si+count6],ax
.c23:    xor bx,bx
         or bl,[si+5]
         jz .c24

         shl bx,1
         mov cx,[bx+tab1213]
         mov dx,[bx+tab1011]
         add [si+count6+2],cx
         add [si+count6],dx
         add [si+count4+2],cx
         add [si+count4],dx
         mov ax,[bx+tab2223]
         add [si+count5+2],ax
         mov ax,[bx+tab2021]
         add [si+count5],ax
.c24:    xor bx,bx
         or bl,[si+4]
         jz .c25

         shl bx,1
         mov cx,[bx+tab1213]
         mov dx,[bx+tab1011]
         add [si+count5+2],cx
         add [si+count5],dx
         add [si+count3+2],cx
         add [si+count3],dx
         mov ax,[bx+tab2223]
         add [si+count4+2],ax
         mov ax,[bx+tab2021]
         add [si+count4],ax
.c25:    xor bx,bx
         or bl,[si+3]
         jz .c26

         shl bx,1
         mov cx,[bx+tab1213]
         mov dx,[bx+tab1011]
         add [si+count4+2],cx
         add [si+count4],dx
         add [si+count2+2],cx
         add [si+count2],dx
         mov ax,[bx+tab2223]
         add [si+count3+2],ax
         mov ax,[bx+tab2021]
         add [si+count3],ax
.c26:    xor bx,bx
         or bl,[si+2]
         jz .c27

         shl bx,1
         mov cx,[bx+tab1213]
         mov dx,[bx+tab1011]
         add [si+count3+2],cx
         add [si+count3],dx
         add [si+count1+2],cx
         add [si+count1],dx
         mov ax,[bx+tab2223]
         add [si+count2+2],ax
         mov ax,[bx+tab2021]
         add [si+count2],ax
.c27:    xor bx,bx
         or bl,[si+1]
         jz .lnext

         shl bx,1
         mov cx,[bx+tab1213]
         mov dx,[bx+tab1011]
         add [si+count2+2],cx
         add [si+count2],dx
         add [si+count0+2],cx
         add [si+count0],dx
         mov ax,[bx+tab2223]
         add [si+count1+2],ax
         mov ax,[bx+tab2021]
         add [si+count1],ax

.lnext:  mov si,[si+next]
         cmp si,1
         jz stage2
         jmp .c5

stage2:  mov si,[startp]
.c1:     mov byte [si+sum],0
         genmac 0
         genmac 1
         genmac 2
         genmac 3
         genmac 4
         genmac 5
         genmac 6
         genmac 7
         mov si,[si+next]
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

cleanup0: mov bx,[startp]
          xor si,si
.c1:      test byte [bx+sum],0ffh
          jz .delel

          mov si,bx
          mov bx,[bx+next]
          cmp bx,1
          jnz .c1

          retn

.delel:   dec [tilecnt]
          lea di,[count0+bx]
          xor ax,ax
          mov cx,16
.c2c:     mov [di],ax
          add di,2
          loop .c2c
          xchg ax,[bx+next]
          mov bx,ax
          or si,si
          jz .del1st

         mov [si+next],ax
         dec ax
         jnz .c1

.c4:     retn

.del1st: mov [startp],bx
         cmp [tilecnt],0
         jnz .c1
         retn

ttab:     repeat hormax*vermax/4
          zv = (%-1)*400/hormax/vermax
          db (zv/10)*16 + zv mod 10
          end repeat

bittab    db 1,2,4,8,16,32,64,128

         align 2
         include 'tab12.s'
gentab:
         include 'gentab.s'
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
iobseg    dw 0
filehl    dw 0
filesz    dw 0
tsz       dw 0
saved     dw 0
tobin     dw 1,10,100,1000,10000
x0        db 0   ;word aligned for the speed
y0        db 0
live      dw 12  ;x0,y0,live,born have to go sequently
born      dw 8
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
pseudoc   db 0
mode      db 0      ;0-stop, 1-run, 2-hide, 3-exit
zoom      db 0
fn        db 0,0,0,0,0,0,0,0,0,0,0,0
density   db 3
czbg      db 0
palette   db 0
bgr       db 0ah
bgs       db 0
zbgr      db 20h
zbgs      db 0
zfg       db 3
zfgnc     db 5
topology  db 0      ;0 - torus
crsrticks db 1
;errst:     db 0   ;0 - do not print i/o-errors message, 1 - print
ppmode    db 1    ;putpixel mode: 0 - tentative, 1 - active
crsrpgmk  db 1   ;0 - do not draw cursor during showscnz, 1 - draw
svfn      db 0,0,0,0,0,0,0,0,0,0,0,0
drives    rb 26
curdrv    db 0
patpath   db '\PATTERNS',0
rootpath  db '\',0
cf        db '\COLORS.CFG',0
copyleft  db '\CR.TXT',0
nofnchar  db '?,./:;<=>[\]|'
stringbuf rb 19     ;must be after nofnchar
