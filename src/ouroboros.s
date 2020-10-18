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
  Borosouro
  ItKeepsHappening
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

.macro INY_MOD_16
  .local skip
  INY
  CPY #16
  BNE skip
  LDY #0
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
SPAWN_PERIOD = 3 ; seconds
MAX_ENEMIES = 15
START_TIME = $0100
COLLISION_THETA = 4
SHAKE_DISTANCE = 3
SHAKE_COUNT = 10

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

screen_shake: .res 1

subsecond_counter: .res 1

sprite_counter: .res 1

step_delay: .res 1
step_counter: .res 1
song_delay: .res 1
song_counter: .res 1

; time = one byte for seconds, then one byte for minutes
remaining_time: .res 2
elapsed_time: .res 2
best_time: .res 2

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
enemy_spawn_timer: .res 1

random_rho_buffer: .res 4
random_rho_buffer_index: .res 1

.enum enemy_types
  small_clock
  large_clock
  hourglass
  virus
  anti_worm
.endenum

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
  CMP #game_states::waiting_to_start
  BEQ :+
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

  ; horizontal mirroring (fix for everdrive)
  LDA #%00000001
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

  LDA #$ff
  STA best_time
  STA best_time+1
  JSR go_to_title

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
  JSR set_scroll

  ; new frame code
  JSR game_state_handler
.ifdef DEBUG
  LDA #%00011110  ; no tint
  STA PPUMASK
.endif
  JSR music_update
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

.proc set_scroll
  LDA screen_shake
  BEQ reset_scroll

  DEC screen_shake
  LDA screen_shake
  AND #%10
  BEQ left_shake

  LDA #SHAKE_DISTANCE
  STA PPUSCROLL
  LDA #$00
  STA PPUSCROLL
  RTS
left_shake:
  LDA #-SHAKE_DISTANCE
  STA PPUSCROLL
  LDA #$00
  STA PPUSCROLL
  RTS
  
reset_scroll:
  LDA #$00
  LDA #$00
  STA PPUSCROLL
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

.proc music_update
  LDA game_state
  CMP #game_states::playing
  BNE play_music ; play the usual way

  DEC song_counter
  BMI play_delayed
  BEQ play_delayed
  JMP skip_music
play_delayed:
  LDA song_delay
  STA song_counter
play_music:
  JSR FamiToneUpdate
skip_music:
  RTS
.endproc

.proc slow_updates
  JSR rand
  RTS
.endproc

.proc go_to_title
  STA $e000

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

  PLAY Borosouro

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
  STA screen_shake
  STA temp_rho
  STA enemy_queue_head
  STA enemy_queue_tail
  LDA #15
  STA step_counter
  STA step_delay
  LDA #6
  STA song_counter
  STA song_delay
  
  LDA #0
  STA subsecond_counter
  STA enemy_spawn_timer
  STA elapsed_time
  STA elapsed_time+1

  JSR fill_random_rho_buffer

  LDA #.lobyte(START_TIME)
  STA remaining_time
  LDA #.hibyte(START_TIME)
  STA remaining_time+1

  BIT PPUSTATUS
  LDA #$21
  STA PPUADDR
  LDA #$b0
  STA PPUADDR
  LDX remaining_time+1
  LDA tens_digit_lt, X
  STA PPUDATA
  LDA ones_digit_lt, X
  STA PPUDATA
  LDA #$fa ; ":"
  STA PPUDATA
  LDX remaining_time
  LDA tens_digit_lt, X
  STA PPUDATA
  LDA ones_digit_lt, X
  STA PPUDATA

  LDA best_time+1
  BMI skip_display_best
  BIT PPUSTATUS
  LDA #$21
  STA PPUADDR
  LDA #$f0
  STA PPUADDR
  LDX best_time+1
  LDA tens_digit_lt, X
  STA PPUDATA
  LDA ones_digit_lt, X
  STA PPUDATA
  LDA #$fa ; ":"
  STA PPUDATA
  LDX best_time
  LDA tens_digit_lt, X
  STA PPUDATA
  LDA ones_digit_lt, X
  STA PPUDATA
skip_display_best:
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL

  VBLANK

  SCREEN_ON

  PLAY ItKeepsHappening

  RTS
.endproc

.proc go_to_game_over
  LDA #game_states::game_over
  STA game_state
  PRINT string_yay, $222c
  PRINT string_start, $226d

  ; update high score
  LDA best_time+1
  BMI set_best_time ; infinity best time, just replace with current time

  ; compare times
  LDA elapsed_time+1
  CMP best_time+1
  BCC set_best_time
  BNE skip_best_time_stuff

  LDA elapsed_time
  CMP best_time
  BCS skip_best_time_stuff

set_best_time:
  LDA elapsed_time
  STA best_time
  LDA elapsed_time+1
  STA best_time+1

skip_best_time_stuff:

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
  AND #(BUTTON_START|BUTTON_A|BUTTON_B)
  BEQ :+
  JSR go_to_title
:
  RTS
.endproc

.proc playing
  JSR time_stuff

  JSR readjoy

  LDA pressed_buttons
  AND #BUTTON_LEFT
  BEQ no_left
  LDX worm_queue_head
  LDA worm_rho_queue, X
  CMP #3
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

  DEC step_counter
  BEQ :+
  BPL no_step
:
  LDA step_delay
  STA step_counter

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

  JSR render_worm
  JSR render_enemies

  JSR check_collisions

  ; erase sprites
  LDX sprite_counter
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  RTS
.endproc

.proc check_collisions
  LDX enemy_queue_head
  CPX enemy_queue_tail
  BNE :+
  RTS
:

loop:
  LDY worm_queue_head
  LDA enemy_rho_queue, X
  CMP worm_rho_queue, Y
  BNE skip

  LDA enemy_theta_queue, X
  SEC
  SBC worm_theta_queue, Y
  BPL :+
  ; negate
  EOR #$ff
  CLC
  ADC #1
:
  CMP #COLLISION_THETA
  BCS skip

  LDY enemy_type_queue, X
  LDA time_delta_per_enemy, Y
  CLC
  ADC remaining_time
  STA remaining_time
  CMP #60
  BCC no_carry
  ; caps time to 1:00
  LDA #1
  STA remaining_time+1
  LDA #0
  STA remaining_time
no_carry:

  ; shake screen
  LDA #SHAKE_COUNT
  STA screen_shake

  ; delete enemy
  CPX enemy_queue_head
  BEQ delete_head
delete_non_head:
  LDY enemy_queue_head
  LDA enemy_type_queue, Y
  STA enemy_type_queue, X
  LDA enemy_rho_queue, Y
  STA enemy_rho_queue, X
  LDA enemy_theta_queue, Y
  STA enemy_theta_queue, X
delete_head:
  LDY enemy_queue_head
  INY_MOD_16
  STY enemy_queue_head
  JMP skip
skip:
  INX_MOD_16
  CPX enemy_queue_tail
  BNE loop
  RTS
.endproc

.proc render_worm
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

.proc render_enemies
  LDX enemy_queue_head
  CPX enemy_queue_tail
  BNE loop
  RTS
loop:
  JSR render_enemy
  INX_MOD_16
  CPX enemy_queue_tail
  BNE loop
  RTS
.endproc

.proc render_enemy
  ; input: X = enemy queue index
  LDY enemy_type_queue, X
  LDA enemy_renders_h, Y
  PHA
  LDA enemy_renders_l, Y
  PHA
  RTS
.endproc

.proc render_small_clock
  LDA enemy_rho_queue, X
  STA sprite_rho
  LDA enemy_theta_queue, X
  STA sprite_theta
  LDA #$38
  STA sprite_tile
  LDA #$01
  STA sprite_flag
  JSR draw_polar_sprite
  RTS
.endproc

.proc render_large_clock
  LDA enemy_rho_queue, X
  STA sprite_rho
  LDA enemy_theta_queue, X
  STA sprite_theta
  LDA #.lobyte(metasprite_4_data)
  STA sprite_ptr
  LDA #.hibyte(metasprite_4_data)
  STA sprite_ptr+1
  JSR draw_polar_metasprite
  RTS
.endproc

.proc render_hourglass
  LDA enemy_rho_queue, X
  STA sprite_rho
  LDA nmis
  AND #%1
  BEQ :+
  INC enemy_theta_queue, X
:
  LDA enemy_theta_queue, X
  STA sprite_theta
  LDA nmis
  AND #%110000
  LSR
  LSR
  LSR
  LSR
  CMP #%01
  BEQ frame_1
  CMP #%10
  BEQ frame_2
  CMP #%11
  BEQ frame_3
frame_0:
  LDA #.lobyte(metasprite_0_data)
  STA sprite_ptr
  LDA #.hibyte(metasprite_0_data)
  STA sprite_ptr+1
  JSR draw_polar_metasprite
  RTS
frame_1:
  LDA #.lobyte(metasprite_1_data)
  STA sprite_ptr
  LDA #.hibyte(metasprite_1_data)
  STA sprite_ptr+1
  JSR draw_polar_metasprite
  RTS
frame_2:
  LDA #.lobyte(metasprite_2_data)
  STA sprite_ptr
  LDA #.hibyte(metasprite_2_data)
  STA sprite_ptr+1
  JSR draw_polar_metasprite
  RTS
frame_3:
  LDA #.lobyte(metasprite_3_data)
  STA sprite_ptr
  LDA #.hibyte(metasprite_3_data)
  STA sprite_ptr+1
  JSR draw_polar_metasprite
  RTS
.endproc

.proc render_virus
  LDA enemy_rho_queue, X
  STA sprite_rho
  LDA nmis
  AND #%1
  BEQ :+
  DEC enemy_theta_queue, X
:
  LDA enemy_theta_queue, X
  STA sprite_theta
  LDA #.lobyte(metasprite_5_data)
  STA sprite_ptr
  LDA #.hibyte(metasprite_5_data)
  STA sprite_ptr+1
  JSR draw_polar_metasprite
  RTS
.endproc

.proc render_anti_worm
  RTS
.endproc

.proc time_stuff
  INC subsecond_counter
  LDA subsecond_counter
  CMP #60
  BEQ :+
  RTS
:
  LDA #0
  STA subsecond_counter

  ; update timers

  ; remaining time
  DEC remaining_time
  BPL no_game_over
  LDA #59
  STA remaining_time
  DEC remaining_time+1
  BPL no_game_over

  JSR go_to_game_over
  RTS
no_game_over:
  INC elapsed_time
  LDA elapsed_time
  CMP #60
  BNE :+
  LDA #0
  STA elapsed_time
  INC elapsed_time+1
:

  ; display timers

  BIT PPUSTATUS
  LDA #$21
  STA PPUADDR
  LDA #$b0
  STA PPUADDR
  LDX remaining_time+1
  LDA tens_digit_lt, X
  STA PPUDATA
  LDA ones_digit_lt, X
  STA PPUDATA
  LDA #$fa ; ":"
  STA PPUDATA
  LDX remaining_time
  LDA tens_digit_lt, X
  STA PPUDATA
  LDA ones_digit_lt, X
  STA PPUDATA

  LDA #$21
  STA PPUADDR
  LDA #$d0
  STA PPUADDR
  LDX elapsed_time+1
  LDA tens_digit_lt, X
  STA PPUDATA
  LDA ones_digit_lt, X
  STA PPUDATA
  LDA #$fa ; ":"
  STA PPUDATA
  LDX elapsed_time
  LDA tens_digit_lt, X
  STA PPUDATA
  LDA ones_digit_lt, X
  STA PPUDATA

  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL

  ; tweak speed depending on timer
  JSR adjust_speed

  ; enemy spawn

  INC enemy_spawn_timer
  LDA enemy_spawn_timer
  CMP #SPAWN_PERIOD
  BCC no_enemy_spawn
  LDA #0
  STA enemy_spawn_timer
  JSR spawn_enemy
no_enemy_spawn:

  RTS
.endproc

.proc adjust_speed
  LDA remaining_time+1
  CMP #2
  BCC less_than_2_minutes
  LDA #10
  STA step_delay
  LDA #6
  STA song_delay
  RTS
less_than_2_minutes:
  CMP #1
  BCC less_than_1_minute
  LDA #8
  STA step_delay
  LDA #5
  STA song_delay
  RTS
less_than_1_minute:
  LDA remaining_time
  CMP #45
  BCC less_than_45_seconds
  LDA #6
  STA step_delay
  LDA #4
  STA song_delay
  RTS
less_than_45_seconds:
  CMP #30
  BCC less_than_30_seconds
  LDA #4
  STA step_delay
  LDA #3
  STA song_delay
  RTS
less_than_30_seconds:
  CMP #15
  BCC less_than_15_seconds
  LDA #3
  STA step_delay
  LDA #2
  STA song_delay
  RTS
less_than_15_seconds:
  LDA #2
  STA step_delay
  LDA #1
  STA song_delay
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

.proc fill_random_rho_buffer
  .repeat 4, i
    LDA #i
    STA random_rho_buffer+i
  .endrepeat
  LDA #0
  STA random_rho_buffer_index

  LDA #4
  STA temp_x
  LDX #3
shuffle_loop:
  JSR rand
  AND #%11
  CMP temp_x
  BCS shuffle_loop
  TAY
  LDA random_rho_buffer, X
  PHA
  LDA random_rho_buffer, Y
  STA random_rho_buffer, X
  PLA
  STA random_rho_buffer, Y
  DEC temp_x
  DEX
  BEQ exit_loop
  JMP shuffle_loop
exit_loop:
  RTS
.endproc

.proc spawn_enemy
  LDX enemy_queue_tail

  ; random enemy (0-3)
  ; TODO: include/implement anti worm
  JSR rand
  AND #%1111
  LDY #0
enemy_weight_loop:
  SEC
  SBC enemy_weights, Y
  BMI exit_enemy_weight_loop
  INY
  JMP enemy_weight_loop
exit_enemy_weight_loop:
  STY enemy_type_queue, X

  ; random rho (0-3)
  LDY random_rho_buffer_index
  LDA random_rho_buffer, Y
  STA enemy_rho_queue, X

  INC random_rho_buffer_index
  LDA random_rho_buffer_index
  CMP #4
  BNE no_fill
  save_regs
  JSR fill_random_rho_buffer
  restore_regs
no_fill:

  ; spawn oposite of player
  LDY worm_queue_head
  LDA worm_theta_queue, Y
  CLC
  ADC #128
  AND #%11111100
  STA enemy_theta_queue, X

  INX_MOD_16
  STX enemy_queue_tail

  TXA
  SEC
  SBC enemy_queue_head
  BPL :+
  CLC
  ADC #$10
:
  CMP #MAX_ENEMIES
  BCC :+
  LDX enemy_queue_head
  INX_MOD_16
  STX enemy_queue_head
:
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

.define enemy_renders render_small_clock-1, \
                      render_large_clock-1, \
                      render_hourglass-1, \
                      render_virus-1, \
                      render_anti_worm-1
enemy_renders_l: .lobytes enemy_renders
enemy_renders_h: .hibytes enemy_renders

time_delta_per_enemy:
  .byte  5 ; small clock
  .byte 10 ; large clock
  .byte 15 ; hourglass
  .byte 20 ; virus
  .byte 30 ; anti worm

; must add up to 16
enemy_weights:
  .byte  7 ; small clock
  .byte  5 ; large clock
  .byte  3 ; hourglass
  .byte  1 ; virus
  .byte  0 ; anti worm

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"

nametable_title: .incbin "../assets/nametables/title.rle"
nametable_main: .incbin "../assets/nametables/main.rle"

step_theta_per_rho: .byte 2, 2, 3, 3

; decimal to tens digit
tens_digit_lt:
  .repeat 60, i
    .byte i / 10 + $f0
  .endrepeat
; decimal to ones digit
ones_digit_lt:
  .repeat 60, i
    .byte i .mod 10 + $f0
  .endrepeat

; 256 pairs of points x, y in a circle

.include "circle-lut.inc"

.include "../assets/metasprites.inc"

; strings
string_yay: .byte $e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ff
string_start: .byte $d0,$d1,$d2,$d3,$d4,$ff


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
