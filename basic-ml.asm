use16
push es
push ds
mov ax,0b800h
mov es,ax
mov ds,ax
mov si,160
xor di,di
cld
mov cx,80*23
rep movsw
mov ah,7
mov cl,80
rep stosw
pop ds
pop es
retf

push es
push ds
mov ax,0b800h
mov es,ax
mov ds,ax
mov si,80*23*2-2
mov di,80*24*2-2
mov cx,80*23
std
rep movsw
mov ah,7
mov cl,80
xor di,di
cld
rep stosw
pop ds
pop es
retf

push ds
xor ax,ax
mov ds,ax
mov ax,[46ch]
add ax,50
l1:cmp ax,[46ch]
jnc l1
pop ds
retf

mov ah,19h
int 21h
mov bp,sp
mov bx,[bp+4]
mov [bx],al
retf 2

push es
mov ax,0b800h
mov es,ax
mov ah,3
xor bx,bx
int 10h
mov al,160
mul dh
mov di,ax
mov bp,sp
mov si,[bp+6]
cld
lodsb
xor cx,cx
mov cl,al
mov bl,80
sub bl,cl
lodsw
mov si,ax
mov ah,7
l3:lodsb
stosw
loop l3
or cl,bl
jz l2
mov al,32
rep stosw
l2:pop es
retf 2
