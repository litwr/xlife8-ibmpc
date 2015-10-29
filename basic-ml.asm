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
