
;
; BOOTSECTOR BRICKS GAME
; author: Meinhard Kissich
; date: Dec 2019
; 

  cpu 8086
  org 0x7c00

;
; DEFINES
;
SCREEN_W:     equ 320
SCREEN_H:     equ 200

BK_ROOF:          equ 30   ; gap top to 1st brick row
BK_BORDER:        equ 10   ; gap left right from wall to brick
BK_GAP:           equ 3    ; gap between bricks
BK_PER_ROW:       equ 10   ; number of bricks in one row
BK_ROWS:          equ 6    ; number of brick-rows

BK_LIVE_COLOR:    equ 0x2a  ; !! unique !! needed for evaluation
BK_MARKING_COLOR: equ 0x10  ; must be dead color at the moment, and diff to bg
BK_DEAD_COLOR:    equ 0x10  ; same as ^^  ^^
COLOR_BG:         equ 0x00
COLOR_BOARD:      equ 0x1f
COLOR_BALL:       equ 0x20
COLOR_FRAME:      equ 0x1f1f  ; double for sides

BD_Y:         equ 180
BD_H:         equ 5
BD_W:         equ 30
BD_X_INIT:    equ 200
BD_SPEED:     equ 12

BALL_INIT_X:  equ 150
BALL_INIT_Y:  equ 100
BALL_SPEED:   equ 3

;
; CALCULATIONS
;
; v v make sure this is element of int!
BRICK_W:    equ (SCREEN_W - 2 * BK_BORDER) / BK_PER_ROW - BK_GAP
BRICK_H:    equ 8
BRICKS_NR:  equ BK_PER_ROW * BK_ROWS 
BG_PIXELS:    equ 0xf9ff - BRICKS_NR

;
; VARIABLES
;
brick_array:  equ 0x0000  ; array of bricks 1 byte color (bk = dead)
bd_x:         equ 0x0140  ; word
bd_x_old:     equ 0x0142  ; word
bd_time:      equ 0x0144  ; byte
ball_x:       equ 0x0146  ; word
ball_y:       equ 0x0148  ; word
ball_x_old:   equ 0x014A  ; word
ball_y_old:   equ 0x014C  ; word
cur_color:    equ 0x014E  ; word
v_y:          equ 0x0150  ; word \ indicator if positive or negative [+1; -1]
v_x:          equ 0x0152  ; word /
v:            equ 0x0154  ; word
is_collision: equ 0x0156  ; byte ; collision detected
is_init_done: equ 0x0158  ; byte

;
; LET THE FUN BEGIN :)
;
boot_game:
  mov ax, 0x0013      ; VGA 320x200 256 color mode (bottom line for vars)
  int 0x10
  mov ax, 0xa000      ; set segments
  mov ds, ax          ; video segment
  mov es, ax
  cld                 ; and some basic adjustments

init_bricks:
  mov cx, BRICKS_NR   ; init this number of bricks  
  mov ax, BK_LIVE_COLOR ; ... with color: living
  xor di, di
  rep stosb

init_bg:              ; di is already in right position
  mov cx, BG_PIXELS   ; init all others to background color
  mov ax, COLOR_BG
  rep stosb
  mov cx, 3 * SCREEN_W  ; now draw the top border
  mov di, 2 * SCREEN_W
  mov ax, COLOR_FRAME
  rep stosb
  dec di
  mov cx, 175         ; now draw left and right border
init_bg_loop:
  stosw
  add di, SCREEN_W - 2
  loop init_bg_loop

; !! do after bg to make sure it wont get overwritten !!
init_vars:
  mov ax, BD_X_INIT
  mov [bd_x], ax
  mov [bd_x_old], ax
  mov byte [bd_time], 0
  mov word [ball_x], BALL_INIT_X
  mov word [ball_y], BALL_INIT_Y
  mov ax, 1
  mov [v_y], ax
  mov [v_x], ax
  mov word [v], BALL_SPEED

game_loop:
  mov ah, 0x00          ; update time
  int 0x1a
  cmp dl, [bd_time]     ; possibly wait for next frame
  je skip_game_loop_
  mov [bd_time], dl

  mov ah, 0x02          ; get key
  int 0x16
  mov bx, [bd_x]
  test al, 0x04         ; CTRL key? left.
  jz test_alt_
  cmp bx, BD_SPEED      ; not over left border after movement?
  sub bx, BD_SPEED      ; this order to save jumps
  jge test_alt_
  xor bx, bx            ; set zero to reach border
  ; no jump here to make game smaller
test_alt_:
  test al, 0x08         ; ALT key? right.
  jz continue_game_loop_
  add bx, BD_SPEED
  cmp bx, SCREEN_W - BD_W - 1 ; not over right border after movemnt?
  jle continue_game_loop_
  mov bx, SCREEN_W - BD_W - 1
  ;
continue_game_loop_:
  mov [bd_x], bx
  ; jmp if no change, needed? would take some space
  call draw_ball
  call remove_ball                ; must be called befor collision detect!
  call update_board
  call draw_bricks
  call collision_position_update
  call draw_ball
skip_game_loop_:
  jmp game_loop


collision_position_update:
  mov bx, [ball_x]          ; load position
  mov dx, [ball_y]
  mov cx, [v]               ; position steps per direction to perform
cp_loop_:
  add bx, [v_x]             ; add one in x direction
  call collision_text       ; check
  test al, al
  jz next_coord
  sub bx, [v_x]             ; would there be a collision?
  neg word [v_x]            ; ... then take step back and invert direction
next_coord:
  add dx, [v_y]             ; same for v direction
  cmp dx, BD_Y-5            ; under board! -> game lost
  jge game_over                           
  call collision_text
  test al, al
  jz test_cond
  sub dx, [v_y]
  neg word [v_y]
test_cond:
  loop cp_loop_             ; check untill all movements done
  mov [ball_x], bx          ; save position
  mov [ball_y], dx
  ret
  
game_over:
  jmp boot_game

; 
; check for a collision
; result returned via al
; 0 ... no collision; 1 ... collision
;
collision_text:
  push dx
  push cx
  push bx
  mov ax, SCREEN_W
  mul dx
  add ax, bx
  mov si, ax
  mov bx, 7                 ; height of ball
col_loop2:
  mov cx, 8                 ; width of ball
col_loop1:
  lodsb
  cmp al, COLOR_BG          ; bg there, cannot be a collision
  je no_col
check_deads:                ; if there is a dead brick -> no collision
  cmp al, BK_DEAD_COLOR
  je no_col
  cmp al, BK_LIVE_COLOR     ; a living brick -> collision + mark as dead
  mov al, 1
  jne return_col_test_
  dec si                    ; back one pixel
  xchg si, di               ; reader position to writers position to ...
  mov al, BK_MARKING_COLOR  ; ... mark as a dead brick
  stosb
  xchg si, di               ; back to original
  jmp return_col_test_
no_col:
  loop col_loop1
  add si, SCREEN_W - 8
  dec bx
  test bx, bx
  jnz col_loop2
  xor ax, ax
return_col_test_:
  pop bx
  pop cx
  pop dx
  ret



remove_ball:
  mov ax, COLOR_BG
  mov [cur_color], ax
  mov ax, [ball_y_old]    ;
  mov bx, SCREEN_W        ; optimize!
  mul bx
  mov di, [ball_x_old]
  jmp common_ball_draw_

draw_ball:
  mov ax, COLOR_BALL
  mov [cur_color], ax
  mov ax, [ball_y]      ;
  mov [ball_y_old], ax
  mov bx, SCREEN_W      ; optimize!
  mul bx
  mov di, [ball_x]
  mov [ball_x_old], di
common_ball_draw_:
  add di, ax
  mov bx, 7
draw_ball_loop_:
  mov cx, 8
draw_byte_loop_:
  mov ax, [cur_color]   ; <-- opt
  stosb
  loop draw_byte_loop_
  add di, SCREEN_W - 8
  dec bx
  test bx, bx
  jnz draw_ball_loop_
  ret





update_board:
  mov bx, BD_Y * SCREEN_W ; remove
  mov di, [bd_x_old]
  add di, bx
  mov al, COLOR_BG
  call draw_board_loaded  ; new
  mov di, [bd_x]
  mov [bd_x_old], di 
  add di, bx
  mov al, COLOR_BOARD
  call draw_board_loaded
  ret

draw_board_loaded:
  mov cx, BD_H
draw_board_loop_:
  push cx
  mov cx, BD_W
  rep stosb
  add di, SCREEN_W - BD_W
  pop cx
  loop draw_board_loop_
  ret




;
; Update all bricks to the game
;
draw_bricks:
  mov di, SCREEN_W*BK_ROOF+BK_BORDER   ; begin of first pixel
  xor si, si          ; first index of brick array
  mov cx, BK_ROWS
draw_brick_row_loop_:               ; - ; draw all rows
  push cx                           ; |
  mov cx, BRICK_H                   ; |
draw_brick_line_loop_:          ; - ; | ; draw all lines for a brick
  call draw_brick_line_and_gap  ; | ; |
  add di, SCREEN_W              ; | ; |
  loop draw_brick_line_loop_    ; - ; |
  add di, SCREEN_W * (BK_GAP)       ; |
  pop cx                            ; |
  add si, BK_PER_ROW                ; |
  loop draw_brick_row_loop_         ; -
  ret
  
  
;
; Draw a pixel line of bricks and gaps
;
draw_brick_line_and_gap:
  push cx
  push di
  push si
  mov bx, BK_PER_ROW
draw_brick_line_and_gap_loop_:
  mov cx, BRICK_W                ; | ; draw BRICK_W pixels for a line of that brick
  ; read each byte if there was a collision 
  push cx
  push si
  push di
  xchg si, di
check_brick_line_:
  lodsb                       ; load pixels in brick
  cmp al, BK_MARKING_COLOR    ; if there is a marking pixel ...
  je remove_this_             ; ... remove this brick
  loop check_brick_line_      ; check all pixels!
  jmp check_done_
remove_this_:
  stosb                       ; write dead brick to array
check_done_:
  pop di
  pop si
  pop cx
  lodsb                             ; | ; load byte for that brick and draw a line of it
  rep stosb             ; /         ; |
  mov cx, BK_GAP        ; \         ; |
  mov ax, COLOR_BG      ; |         ; | ; laoder gap color and draw gap line
  rep stosb             ; /         ; | 
  dec bx
  test bx, bx
  jnz draw_brick_line_and_gap_loop_
  pop si
  pop di
  pop cx
  ret


fill:
  times 510-($-$$) db 0     ; fill to 510 bytes
  dw 0xaa55                 ; make it bootable :D

  
