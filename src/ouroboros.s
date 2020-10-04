.include "constants.inc"
.include "mmc3-constants.inc"
.include "header.inc"

.feature force_range
.linecont +

; famitone2 config
FT_PAL_SUPPORT=0
FT_NTSC_SUPPORT=1
FT_SFX_ENABLE=1
FT_THREAD=1
FT_DPCM_ENABLE=0
FT_SFX_STREAMS=4
FT_DPCM_OFF=$c000

; music/sfx constants
.enum music_track
.endenum

.enum sfx
.endenum

.macro SFX effect, channel
  save_regs
  LDA #sfx::effect
  LDX #.ident ( .concat( "FT_SFX_", .string(channel) ) )
  JSR FamiToneSfxPlay
  restore_regs
.endmacro

.macro PLAY track
.local skip
  save_regs
  LDA #music_track::track
  JSR FamiToneMusicPlay
  restore_regs
.endmacro

.macro PRINT string, ppuaddr
  save_regs
  LDA #<(string)
  STA addr_ptr
  LDA #>(string)
  STA addr_ptr+1
  LDA #<(ppuaddr)
  STA ppu_addr_ptr
  LDA #>(ppuaddr)
  STA ppu_addr_ptr+1
  JSR write_string
  restore_regs
.endmacro

.macro SCREEN_OFF
  LDA #$00
  STA PPUCTRL ; disable NMI
  STA PPUMASK ; disable rendering
.endmacro

.macro SCREEN_ON
  LDA #%10001000  ; turn on NMIs, sprites use second pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK
.endmacro

.macro VERTICAL_PPUADDR
  LDA #%10001100
  STA PPUCTRL
.endmacro

.macro HORIZONTAL_PPUADDR
  LDA #%10001000
  STA PPUCTRL
.endmacro

.macro INX_MOD_16
  .local skip
  INX
  CPX #16
  BNE skip
  LDX #0
skip:  
.endmacro

.macro DEX_MOD_16
  .local skip
  DEX
  BPL skip
  LDX #15
skip:
.endmacro

; game config

STEP_THETA = 2

; debug - macros for NintendulatorDX interaction
.ifdef DEBUG
.macro debugOut str
  sta $4040
  jmp :+
      .byte str, 0
:
.endmacro

.macro debugRegs
  STA debug_a
  STX debug_x
  STY debug_y
.endmacro

.define fHex8( addr ) 1, 0, <(addr), >(addr)
.define fDec8( addr ) 1, 1, <(addr), >(addr)
.define fHex16( addr ) 1, 2, <(addr), >(addr)
.define fDec16( addr ) 1, 3, <(addr), >(addr)
.else
.macro debugOut str
.endmacro
.macro debugRegs
.endmacro
.endif


.segment "ZEROPAGE"
FT_TEMP: .res 3
.segment "FAMITONE"
FT_BASE_ADR: .res 186
.segment "CODE"
.include "famitone2.s"

.segment "OAM"
.struct Sprite
  ycoord .byte
  tile .byte
  flag .byte
  xcoord .byte
.endstruct

oam_sprites:
  .repeat 64
    .tag Sprite
  .endrepeat
.zeropage

.importzp buttons
.importzp last_frame_buttons
.importzp released_buttons
.importzp pressed_buttons
.importzp rng_seed
.importzp rle_ptr

; zp vars
addr_ptr: .res 2 ; generic address pointer
ppu_addr_ptr: .res 2
sprite_ptr: .res 2 ; metasprite pointer

nmis: .res 1
old_nmis: .res 1

sprite_counter: .res 1

; worm queue is unusual:
; it grows at the head
; both head and tail are inclusive
; (so I don't confuse the queue head with the worm's actual head)
worm_theta_queue: .res 16
worm_rho_queue: .res 16
worm_queue_head: .res 1
worm_queue_tail: .res 1

; usual queue:
; grows at the tail
; tail is exclusive
; head == tail means empty queue
enemy_theta_queue: .res 16
enemy_rho_queue: .res 16
enemy_type_queue: .res 16
enemy_queue_head: .res 1
enemy_queue_tail: .res 1

sprite_rho: .res 1
sprite_theta: .res 1
sprite_tile: .res 1
sprite_flag: .res 1

temp_x: .res 1
temp_y: .res 1
temp_rho: .res 1
temp_theta: .res 1

.enum game_states
  waiting_to_start
  playing
  game_over
.endenum

game_state: .res 1

.segment "BSS"
; non-zp RAM goes here

.segment "CODE"

.import reset_handler
.import readjoy
.import rand
.import unrle

.import music_data
.import sfx_data

.macro KIL ; pseudo instruction to kill the program
  .byte $12
.endmacro

.macro VBLANK
  .local vblankwait
vblankwait:
  BIT PPUSTATUS
  BPL vblankwait
.endmacro

.macro save_regs
  PHA
  TXA
  PHA
  TYA
  PHA
.endmacro

.macro restore_regs
  PLA
  TAY
  PLA
  TAX
  PLA
.endmacro

.proc irq_handler
  STA $e000
  save_regs
  LDX #$10
: DEX
  BPL :-
  JSR load_lower_chr
  restore_regs

  RTI
.endproc

.proc nmi_handler
  save_regs
  INC nmis
  LDA game_state
  CMP #game_states::playing
  BNE :+
  JSR load_upper_chr

  ; set irq for bottom half bankswitching
  STA $e000
  LDA #$77
  STA $c000
  STA $c001
  STA $e001
:
  restore_regs
  RTI
.endproc

.export main
.proc main
  SEI         ; ignore IRQs
  CLD         ; disable decimal mode
  LDX #$40
  STX $4017   ; disable APU frame IRQ
  LDX #$ff
  TXS         ; Set up stack
  INX         ; now X = 0
  STX PPUCTRL ; disable NMI
  STX PPUMASK ; disable rendering
  STX $4010   ; disable DMC IRQs

  LDX #0
clear_ram:
  LDA #$00
  STA $0000,X
  STA $0100,X
  STA $0300,X
  STA $0400,X
  STA $0500,X
  STA $0600,X
  STA $0700,X
  LDA #$fe
  STA $0200,X
  INX
  BNE clear_ram

  ; vertical mirroring (fix for everdrive)
  LDA #%00000000
  STA $a000

  SCREEN_ON

  LDX #<music_data
  LDY #>music_data
  LDA #1
  JSR FamiToneInit

  ; init FamiTone SFX
  LDX #<sfx_data
  LDY #>sfx_data
  LDA #1
  JSR FamiToneSfxInit

  ; init rng
  LDA #$a9
  STA rng_seed
  LDA #$73
  STA rng_seed+1

  CLI ; enable interrupts

  JSR go_to_playing ; TODO change to title later

forever:
  LDA nmis
  CMP old_nmis
  BEQ etc
  STA old_nmis
.ifdef DEBUG
  LDA #%01011110  ; red tint
  STA PPUMASK
.endif
  JSR refresh_oam
  ; reset ppuaddr
  BIT PPUSTATUS
  ; reset scroll
  LDA #$00
  STA PPUSCROLL
  STA PPUSCROLL

  ; new frame code
  JSR game_state_handler
.ifdef DEBUG
  LDA #%00011110  ; no tint
  STA PPUMASK
.endif
  JSR FamiToneUpdate
  JSR slow_updates
etc:
  JMP forever
.endproc

.proc refresh_oam
  ; Refresh OAM
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
  RTS
.endproc

.proc load_palettes
  ; cobbles Y
  LDY PPUSTATUS
  LDY #$3f
  STY PPUADDR
  LDY #$00
  STY PPUADDR
:
  LDA palettes,Y
  STA PPUDATA
  INY
  CPY #$20
  BNE :-
  RTS
.endproc

.proc load_default_chr
  ; bg
  LDA #0
  STA BANK_SELECT
  LDA #0
  STA BANK_DATA
  LDA #1
  STA BANK_SELECT
  LDA #2
  STA BANK_DATA

  ; sprites
  LDA #2
  STA BANK_SELECT
  LDA #12
  STA BANK_DATA

  LDA #3
  STA BANK_SELECT
  LDA #13
  STA BANK_DATA

  LDA #4
  STA BANK_SELECT
  LDA #14
  STA BANK_DATA

  LDA #5
  STA BANK_SELECT
  LDA #15
  STA BANK_DATA
  RTS
.endproc

.proc load_upper_chr
  ; bg
  LDA #0
  STA BANK_SELECT
  LDA #4
  STA BANK_DATA
  LDA #1
  STA BANK_SELECT
  LDA #6
  STA BANK_DATA

  ; sprites
  LDA #2
  STA BANK_SELECT
  LDA #12
  STA BANK_DATA

  LDA #3
  STA BANK_SELECT
  LDA #13
  STA BANK_DATA

  LDA #4
  STA BANK_SELECT
  LDA #14
  STA BANK_DATA

  LDA #5
  STA BANK_SELECT
  LDA #15
  STA BANK_DATA
  RTS
.endproc

.proc load_lower_chr
  ; bg
  LDA #0
  STA BANK_SELECT
  LDA #8
  STA BANK_DATA
  LDA #1
  STA BANK_SELECT
  LDA #10
  STA BANK_DATA

  ; sprites
  LDA #2
  STA BANK_SELECT
  LDA #12
  STA BANK_DATA

  LDA #3
  STA BANK_SELECT
  LDA #13
  STA BANK_DATA

  LDA #4
  STA BANK_SELECT
  LDA #14
  STA BANK_DATA

  LDA #5
  STA BANK_SELECT
  LDA #15
  STA BANK_DATA
  RTS
.endproc

.proc game_state_handler
  LDX game_state
  LDA game_state_handlers_h, X
  PHA
  LDA game_state_handlers_l, X
  PHA
  RTS
.endproc

.proc slow_updates
  JSR rand
  RTS
.endproc


.proc go_to_title
  LDA #game_states::waiting_to_start
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  SCREEN_OFF

  JSR load_palettes

  JSR load_default_chr

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_title
  STA rle_ptr
  LDA #>nametable_title
  STA rle_ptr+1
  JSR unrle

  VBLANK

  SCREEN_ON

  RTS
.endproc

.proc go_to_playing
  LDA #game_states::playing
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  SCREEN_OFF

  JSR load_palettes

  JSR load_default_chr

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_main
  STA rle_ptr
  LDA #>nametable_main
  STA rle_ptr+1
  JSR unrle

INITIAL_SIZE=4
  ; game setup
  LDA #$00
  STA worm_queue_tail
  LDA #(INITIAL_SIZE-1)
  STA worm_queue_head

  LDA #$00
  .repeat INITIAL_SIZE, i
    STA worm_rho_queue+i
  .endrepeat

  .repeat INITIAL_SIZE, i
    LDA #(STEP_THETA*i)
    STA worm_theta_queue+i
  .endrepeat

  LDA #0
  STA temp_rho
  STA enemy_queue_head
  STA enemy_queue_tail

  VBLANK

  SCREEN_ON

  RTS
.endproc

.proc go_to_game_over
  LDA #game_states::game_over
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  SCREEN_OFF

  VBLANK

  SCREEN_ON

  RTS
.endproc

.proc waiting_to_start
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_START
  BEQ :+
  JSR go_to_playing
:
  RTS
.endproc

.proc game_over
  JSR readjoy
  LDA pressed_buttons
  BEQ :+
  JSR go_to_title
:
  RTS
.endproc

.proc playing
  JSR readjoy

  LDA pressed_buttons
  AND #BUTTON_LEFT
  BEQ no_left
  LDX worm_queue_head
  LDA worm_rho_queue, X
  CMP #6
  BEQ no_left
  CLC
  ADC #1
  STA temp_rho
no_left:
  LDA pressed_buttons
  AND #BUTTON_RIGHT
  BEQ no_right

  LDX worm_queue_head
  LDA worm_rho_queue, X
  BEQ no_right
  SEC
  SBC #1
  STA temp_rho

no_right:

  LDA nmis
  AND #%111
  BNE no_step

  LDX worm_queue_tail
  INX_MOD_16
  STX worm_queue_tail

  LDX worm_queue_head

  LDY temp_rho
  LDA worm_theta_queue, X
  CLC
  ADC step_theta_per_rho, Y
  STA temp_theta

  INX_MOD_16
  LDA temp_rho
  STA worm_rho_queue, X
  LDA temp_theta
  STA worm_theta_queue, X
  STX worm_queue_head

no_step:

  LDA #0
  STA sprite_counter

  LDX worm_queue_head
render_worm_loop:
  LDA worm_rho_queue, X
  STA sprite_rho
  LDA worm_theta_queue, X
  STA sprite_theta
  CPX worm_queue_head
  BNE @no_head
  LDA sprite_theta
  LSR
  LSR
  LSR
  LSR
  STA sprite_tile
  JMP @set_flag
@no_head:
  CPX worm_queue_tail
  BNE @no_tail
  LDA sprite_theta
  LSR
  LSR
  LSR
  LSR
  CLC
  ADC #$20
  STA sprite_tile
  JMP @set_flag
@no_tail:
  LDA sprite_theta
  LSR
  LSR
  LSR
  LSR
  CLC
  ADC #$10
  STA sprite_tile
@set_flag:
  LDA #0
  STA sprite_flag

  JSR draw_polar_sprite
  CPX worm_queue_tail
  BEQ exit_render_worm_loop
  DEX_MOD_16
  JMP render_worm_loop
exit_render_worm_loop:
  RTS
.endproc

.proc draw_polar_sprite
  ; input: sprite_rho, sprite_theta, sprite_tile, sprite_flag
  save_regs
  LDX sprite_rho
  LDY sprite_theta

  LDA circle_lut_x_ptr_l, X
  STA addr_ptr
  LDA circle_lut_x_ptr_h, X
  STA addr_ptr+1
  
  LDA (addr_ptr), Y
  SEC
  SBC #4
  STA temp_x

  LDA circle_lut_y_ptr_l, X
  STA addr_ptr
  LDA circle_lut_y_ptr_h, X
  STA addr_ptr+1
  
  LDA (addr_ptr), Y
  SEC
  SBC #4
  STA temp_y

  LDX sprite_counter

  LDA temp_x
  STA oam_sprites+Sprite::xcoord, X
  LDA temp_y
  STA oam_sprites+Sprite::ycoord, X
  LDA sprite_tile
  STA oam_sprites+Sprite::tile, X
  LDA sprite_flag
  STA oam_sprites+Sprite::flag, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat

  STX sprite_counter
  restore_regs
  RTS
.endproc

.proc draw_polar_metasprite
  ; input: sprite_rho, sprite_theta, sprite_ptr for metasprite
  save_regs
  LDX sprite_rho
  LDY sprite_theta

  LDA circle_lut_x_ptr_l, X
  STA addr_ptr
  LDA circle_lut_x_ptr_h, X
  STA addr_ptr+1
  
  LDA (addr_ptr), Y
  STA temp_x

  LDA circle_lut_y_ptr_l, X
  STA addr_ptr
  LDA circle_lut_y_ptr_h, X
  STA addr_ptr+1
  
  LDA (addr_ptr), Y
  STA temp_y

  LDX sprite_counter
  LDY #$0

metasprite_loop:
  LDA (sprite_ptr), Y
  INY
  CMP #128
  BEQ exit_metasprite_loop
  CLC
  ADC temp_x
  STA oam_sprites+Sprite::xcoord, X
  
  LDA (sprite_ptr), Y
  INY
  CLC
  ADC temp_y
  STA oam_sprites+Sprite::ycoord, X
  
  LDA (sprite_ptr), Y
  INY
  STA oam_sprites+Sprite::tile, X

  LDA (sprite_ptr), Y
  INY
  STA oam_sprites+Sprite::flag, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat

  JMP metasprite_loop
exit_metasprite_loop:

  STX sprite_counter
  restore_regs
  RTS
.endproc

.proc write_string
  LDA PPUSTATUS
  LDA ppu_addr_ptr+1
  STA PPUADDR
  LDA ppu_addr_ptr
  STA PPUADDR
  LDY #$00
@loop:
  LDA (addr_ptr), Y
  CMP #$ff
  BEQ @exit
  STA PPUDATA
  INY
  JMP @loop
@exit:
  RTS
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"

.define game_state_handlers waiting_to_start-1, playing-1, game_over-1

game_state_handlers_l: .lobytes game_state_handlers
game_state_handlers_h: .hibytes game_state_handlers

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"

nametable_title: .incbin "../assets/nametables/title.rle"
nametable_main: .incbin "../assets/nametables/main.rle"

step_theta_per_rho: .byte 2, 2, 2, 2, 3, 3, 3

; 256 pairs of points x, y in a circle

.include "circle-lut.inc"

.include "../assets/metasprites.inc"

.segment "CHR"
.incbin "../assets/chr/bg-4k-title.chr"
.incbin "../assets/chr/bg-4k-upper-half.chr"
.incbin "../assets/chr/bg-4k-lower-half.chr"
.incbin "../assets/chr/sprites-4k.chr"
.incbin "../assets/chr/sprites-4k.chr"

; 1k blocks
;  0 : bg-title
;  1 : bg-title
;  2 : bg-title
;  3 : bg-title
;  4 : bg-upper
;  5 : bg-upper
;  6 : bg-upper
;  7 : bg-upper
;  8 : bg-lower
;  9 : bg-lower
; 10 : bg-lower
; 11 : bg-lower
; 12 : sp
; 13 : sp
; 14 : sp
; 15 : sp

; banks
; 0 : 2k bg
; 1 : 2k bg
; 2 : 1k sp
; 3 : 1k sp
; 4 : 1k sp
; 5 : 1k sp
