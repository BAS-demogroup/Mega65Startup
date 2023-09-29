.cpu _45gs02
  // MEGA65 platform PRG executable starting in MEGA65 mode.
.file [name="startup.prg", type="prg", segments="Program"]

#import "global_functions.asm"
#import "mega65_io.asm"

.segmentdef Program [segments="Basic, Code, Data"]
.segmentdef Basic [start=$2001]

.segmentdef Code [start=$202a]
.segmentdef Data [startAfter="Code"]
.segment Basic
	basic_stub(main)
	
.segment Code
	.const line_length = 20
	.const line_count =  25
	.const base_char = CHARSET / $40

.print "$" + toHexString(base_char)
	
main: {
    // enable 40MHz
	lda #65
    sta CPU_PORTDDR
		
	// Enable the VIC4
	lda #$47
	sta VIC4_KEY
	lda #$53
	sta VIC4_KEY
	
	// configure PAL/NTSC variation
	lda	VIC4_PALNTSC_ADDR
	and	#VIC4_PALNTSC_MASK
	bne	!+
	
	lda	#$18
	sta	matrix_raster
	lda	#$01
	sta	matrix_raster + 1
	
	lda #VIC4_PALNTSC_MASK
	trb VIC4_PALNTSC_ADDR
	bra	!++
	
!:
	lda	#$e8
	sta	matrix_raster
	lda	#$00
	sta	matrix_raster + 1
	
	lda #VIC4_PALNTSC_MASK
	tsb VIC4_PALNTSC_ADDR

!:
	// disable 640 horizontal width
	lda #VIC3_H640_MASK
	trb VIC3_H640_ADDR

	// disable hot registers
	lda #VIC4_HOTREG_MASK
    trb VIC4_HOTREG_ADDR
	
	// use wide character lookup (i.e. character data anywhere in memory)
    lda #VIC4_CHR16_MASK
    sta VIC4_CHR16_ADDR

	// configure screen row length
    lda #line_length << 1
    sta VIC4_LINESTEPLSB
    lda #line_length
    sta VIC4_CHRCOUNT
    lda #$00
    sta VIC4_LINESTEPMSB
    sta VIC4_CHRCOUNT_ADDR
    lda #line_count
    sta VIC4_Number
	
	// load attributes map
	run_dma_job(fill_attribute_map)
	
	// clear the screen
	run_dma_job(clear_tile_map)
    
	// draw the logo itself
	ldx #base_char + 1
	ldy #$00
!:
	stx $09c4, y
	inx
	iny
	iny
	cpy #$10
	bne !-
	
	ldy #$00
!:
	stx $09ec, y
	inx
	iny
	iny
	cpy #$10
	bne !-
	
	ldy #$00
!:
	stx $0a14, y
	inx
	iny
	iny
	cpy #$10
	bne !-
	
	// load the color palette
    run_dma_job(load_color_palette_dma)
	
	// disable 400 vertical height
	lda #VIC3_V400_MASK
	trb VIC3_V400_ADDR
	
	lda #$02
	sta VIC2_BORDERCOL_ADDR
	sta VIC2_SCREENCOL_ADDR

	lda #VIC2_BLNK_MASK
	trb VIC2_BLNK_ADDR
	
	lda #$2a
	sta drawin_index
	sta drawin_delay
	
delay_loop:
    ldx matrix_raster
    ldy matrix_raster + 1
!:	    
    cpx VIC4_FNRASTERLSB
    bne !-
    cpy VIC4_FN_RASTER_MSB_ADDR
    bne !-
	
	clc
    lda matrix_raster
	adc #$2a
	tax
    lda matrix_raster + 1
	adc #$00
	tay
		
!:	    
    cpx VIC4_FNRASTERLSB
    bne !-
    cpy VIC4_FN_RASTER_MSB_ADDR
    bne !-
	
	dec drawin_delay
	bpl delay_loop

drawin_loop:
    ldx matrix_raster
    ldy matrix_raster + 1
	lda #VIC2_BLNK_MASK
		
!:	    
    cpx VIC4_FNRASTERLSB
    bne !-
    cpy VIC4_FN_RASTER_MSB_ADDR
    bne !-
	
	trb VIC2_BLNK_ADDR
	
	clc
    lda matrix_raster
	adc #$2a
	tax
    lda matrix_raster + 1
	adc #$00
	tay
	lda #VIC2_BLNK_MASK
		
!:	    
    cpx VIC4_FNRASTERLSB
    bne !-
    cpy VIC4_FN_RASTER_MSB_ADDR
    bne !-
	
	tsb VIC2_BLNK_ADDR
	
	clc
	lda matrix_raster
	adc #$01
	sta matrix_raster
	lda matrix_raster + 1
	adc #$00
	sta matrix_raster + 1
	
	dec drawin_index
	bpl drawin_loop
	
	lda #$2a
	sta drawin_delay
	
delay2_loop:
    ldx matrix_raster
    ldy matrix_raster + 1
!:	    
    cpx VIC4_FNRASTERLSB
    bne !-
    cpy VIC4_FN_RASTER_MSB_ADDR
    bne !-
	
	clc
    lda matrix_raster
	adc #$2a
	tax
    lda matrix_raster + 1
	adc #$00
	tay
		
!:	    
    cpx VIC4_FNRASTERLSB
    bne !-
    cpy VIC4_FN_RASTER_MSB_ADDR
    bne !-
	
	dec drawin_delay
	bpl delay_loop2

!:
	bra !-
	
	// drawin should take 0.7 seconds - which conviently works out to round 
	// numbers in both NTSC and PAL.  In NTSC, that is 42 frames, and in PAL,
	// that is 35 frames.  However, the height of the logo is 21 pixels, which
	// means perfect timing is NTSC, so maybe instead we'll just make it slower
	// in PAL.
}

.segment Data
matrix_raster:
	.word $0000
	
drawin_index:
	.byte $00
	
drawin_delay:
	.byte $00
	
load_color_palette_dma:
	dma_copy_job(CLUT, VIC3_PALRED, $300, false, false, false, true)
	
fill_attribute_map:
	.byte $0a
	.byte $81, $ff
	.byte $85, $02
	.byte $00
	.byte $07
	.word line_length * line_count
	.word $08
	.byte $00
	.word $0000
	.byte $08
	.word $0000

	.byte $0a
	.byte $81, $ff
	.byte $85, $02
	.byte $00
	.byte $03
	.word line_length * line_count
	.word $0f
	.byte $00
	.word $0001
	.byte $08
	
clear_tile_map:
	.byte $0a
	.byte $85, $02
	.byte $00
	.byte $07
	.word line_length * line_count
	.word base_char & $ff
	.byte $00
	.word $0800
	.byte $00
	.word $0000

	.byte $0a
	.byte $85, $02
	.byte $00
	.byte $03
	.word line_length * line_count
	.word $0000
	.byte $00
	.word $0801
	.byte $00
	
CLUT:
	.const palette = LoadBinary("../assets/logo.clut")
	.fill palette.getSize(), palette.get(i)

.align $40
CHARSET:
	.const charset = LoadBinary("../assets/logo.chrs")
	.fill charset.getSize(), charset.get(i)
