maketent:   ;in: si
         lodsw
         sub ax,8
         shr ax,1
         mov [tsz],ax
         mov cx,ax
         push es
         mov es,[iobseg]
         xor di,di
         rep movsw
         pop es
         retn

ramdisk: call printstr
         db ansiclrscn,green,'ENTER FILE# OR HIT '
         db red,'ESC',0dh,10,9,'0',black
         db ' GLIDER GUN',0dh,10,red,9,'1',black
         db ' SMALL FISH',0dh,10,red,9,'2',black
         db ' HEAVYWEIGHT SPACESHIP',0dh,10,red,9,'3',black
         db ' R-PENTOMINO',0dh,10,red,9,'4',black
         db ' BUNNIES',0dh,10,red,9,'5',black
         db ' LIDKA',0dh,10,red,9,'6',black
         db ' BIG GLIDER',0dh,10,red,9,'7',black
         db ' BI-GUN',0dh,10,red,9,'8',black
         db ' ACORN',0dh,10,red,9,'9',black
         db ' SWITCH ENGINE PUFFER',0dh,10,'$'

.c1:     call getkey
         cmp al,27    ;esc
         je puttent.exit

         cmp al,'0'
         jc .c1

         cmp al,'9'+1
         jnc .c1

         sub al,'0'
         xor ah,ah
         shl al,1
         mov si,ax
         mov si,[ramptrs+si]
         lodsw
         mov word [x0],ax   ;geometry
         mov al,[zoom]
         push ax
         mov [zoom],0
         call maketent
         call tograph
         call showrect
         pop ax
         mov [zoom],al
         jc puttent.exit

puttent: mov bp,[tsz]
         xor si,si
.loop:   or bp,bp
         jz .exit

         push es
         mov es,[iobseg]
         lods word [es:si]
         pop es
         mov word [x0],ax
         dec bp
         call putpixel
         jmp .loop
.exit:   retn
