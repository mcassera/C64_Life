/*

Computer Life v2- written by Michael Cassera for the C64

Original concept by: John Conway

*/

.label 	vcreg3	=	$d412	// sid V3 control register
.label	frelo3	=	$d40e	// sid V3 lo frequency
.label	rndom	=	$d41b	// read sid V3 oscilator

/*	--------------------------------	This is the working memory for the life cycle. Mem1 is working and
										Mem2 is where new data is placed. Once the entire screen is done,
										Mem2 is copies over Mem1 and the life cycle starts over again.
*/

.label	mem1		=	$4000   		// arbitrary start of working memory that can be moved around if needed
.label	mem1calc	=	mem1+$2b	
.label	mem1view	=	mem1calc+$2a
.label	mem1end		=	mem1view+$041a
.label	mem1pad		=	mem1end+$2a

.label	mem2		=	mem1pad+$0166
.label	mem2calc	=	mem2+$2b	
.label	mem2view	=	mem2calc+$2a
.label	mem2end		=	mem2view+$041a
.label	mem2pad		=	mem2end+$2a



* = $0801
BasicUpstart2(start)



start:
			cld							// Clear Decimal Mode
			lda #$00					// set border to black
			sta $d020					// Border register
			lda #$00					// set screen to black
			sta $d021					// screen register
			sta $c6						// Clear the keyboard buffer
			
			jsr ClearMem				// and clear the working memory
			
//	-------------------------- Set Color - This is so all KERNAL revisions will look correct. Black Screen, Yellow cells.		
			
			lda #$07					// set the color to Yellow (7))
			ldx #250					// set x to 250 for the loop and indexing
ColorLoop:
			sta $d800+000,x				// store yellow into the 1000 bytes of
			sta $d800+250,x				// of color memory by indexing 250
			sta $d800+500,x				// memory addresses apart for each pass
			sta $d800+750,x
			dex							// decrement x by 1
			bne ColorLoop				// if not zero, continue to loop
			sta $d800					// got to get that first byte yellow too
		
//	---------------------------- Set oscillator 3 to act as random number generator			
			lda #$ff
			sta frelo3					// Set the frequency to $ffff on SID chip
			sta frelo3+1				// for random number generator
			lda #$80					// $80 is the trigger for noise, but gate closed.
			sta vcreg3					// trigger noise on V3
			
//	---------------------------- Fill memory with random life
SetRandom:
			lda #<mem1view				// set $fb & $fc with view mem to fill
			sta $fb
			lda #>mem1view
			sta $fc
RLoop:		ldy #0
RLoop1:		lda rndom					// load a random number from the noise generator
			and #$0f					// we only want 0 to f
			sta ($fb),y					// and store
			ldx #$0a					// this is a delay to count to 10 because
delay:		nop							// the noise generator needs 17 cycle to change value
			dex							// this adds a lot more than 17 cycles.		
			bne delay
			iny							// next character to choose for
			cpy #40						// have we hit the end of a line?
			bne RLoop1					// no - loop again
			clc							// yes - then add 42 to memory counter so
			lda $fb						// that the space outside the viewable memory
			adc #42						// is filled with zeros							
			sta $fb
			lda $fc
			adc #$00
			sta $fc
			cmp #>mem1end
			bne RLoop
			lda $fb
			cmp #<mem1end
			bcc RLoop



//	------------------------------- Display memory on the screen	
Display:
			lda #<mem1view				// get the start of viewing memory
			sta $fb						// and store in $fb, $fc for indexing
			lda #>mem1view				// with Y
			sta $fc
			lda #$00					// then set the screen mem at $0400
			sta $fd						// and set it up at $fd, $fc for
			lda #$04					// indexing with Y
			sta $fe
DLoop:		ldy #$00					// set Y to zero
DLoop1:		lda ($fb),y					// grab the cell info ($00 to $0f)
			tax							// transfer to X so that we can load
			lda char,x					// the right petscii char to represent the 4 cells
			sta ($fd),y					// and place on the screen
			iny							// increment Y by 1
			cpy #40						// check to see if we finished a line
			bne DLoop1					// no, go to the next character
			clc							// yes, we need to increase screen memory index
			lda $fd						// by 40, but the cell memory index by 42. This				
			adc #40						// is because we calculate for a larger area 
			sta $fd						// then we display so cells can interacted 
			lda $fe						// correctly on the edge of our screen
			adc #$00
			sta $fe
			clc
			lda $fb
			adc #42
			sta $fb
			lda $fc
			adc #$00
			sta $fc
			cmp #>mem1end				// check to see if we've reached the end of display
			bne DLoop					// memory. If not, loop again, if yes. continue.
			lda $fb
			cmp #<mem1end
			bcc DLoop

//	------------------------------- check for key press to reset screen			
			lda 198
			beq LifeCycle
			lda #$00
			sta 198
			lda $0277
			cmp #49						// the "1" character
			beq GlidrGun
			cmp #50						// the "2" character
			beq P51P
			cmp #51						// the "3" character
			beq diamondp
			jmp SetRandom
			
GlidrGun:
			jsr GGun
			jmp Display
			
P51P:
			jsr PP51
			jmp Display
			
diamondp:	
			jsr ddiamond
			jmp Display
/*	------------------------------- Start the life cycle	

									This routine loops for every character on the screen. Each
									character is made up of four life cells. These are labeled by
									a bit number. 
									
									               1   2
									               4   8
									
									Each cell needs to check the 8 cells surrounding
									it and keep a tally. This routine does 32 cell checks for
									the character. To keep things snappy the routine is very
									linear with no looping to slow things down. 
									
									Each cell has it's own 8 checks, labelled UpLeft,
									UpMid, UpRt, Left, Right, BotLeft, BotMid, BotRT.
									
											UL UM UR
											L      R
											BL BM BR
											
									After the cell is checked, the cell itself is checked to see if
									it already alive. 
									
									If the cell was alive and it is surrounded by only 2 or 3
									living cells, the cell survives, otherwise, it dies.
									
									If the cell was not alive and there are exactly 3 living cells around
									it, then the cell will be born.
									
*/		
LifeCycle:
//	------------------------------- Set some zero page memory for indirect indexing with Y
			lda #<mem1calc				// The lo byte for the start of memory that we are calculating life cycle
			sta $fb						// this is outside the screen viewing area so things can track out of view.
			lda #>mem1calc				// The hi byte for the start of memory that we are calculating life cycle
			sta $fc
			lda #<mem1					// The start of memory for mem 1. This is a buffer zone for the calculating
			sta $fd						// process. The screen is 40 x 25, but the calculating zone is 42 x 27. 
			lda #>mem1					// the hi byte for the start of memory
			sta $fe
			lda #<mem2calc				// The lo byte for our memory to write to. We need to keep the main memory
			sta $02						// intact while calculating the life cycle so this is where the result of
			lda #>mem2calc				// all the checks will go.
			sta $03			
//	------------------------------- This is the check for bit 1			
bit1:		ldy #$00					// set the y index to zero
			lda ($fb),y					// to grab the character we are going to examine
			sta $04						// and store in location $04. Zero page is faster and takes less memory.
Uleft1:		ldx #$00					// and set X to zero. X is going to keep a count of live cells
			lda ($fd),y					// Load the char up left of our character
			and #$08					// and check the cell just up left from our test cell
			beq skipUL1					// skip if the cell is off
			inx							// increment X if it is on
skipUL1:	iny							// increment y to move one character to the right
UpMid1:		lda ($fd),y					// load the character just above our character
			and #$04					// and check the cell above our test cell
			beq skipUM1					// skip if the cell is off
			inx							// increment X if it is on
skipUM1:								// do not increment y here because we are still on the same character
UpRt1:		lda ($fd),y					// load character again
			and #$08					// now check the cell up and right of our test cell
			beq skipUR1					// skip if cell is off
			inx							// increment X if it is on
skipUR1:	ldy #42						// Set Y to 42(d) this will set the character to the left of our test character
Left1:		lda ($fd),y					// load the character to the left of our test character
			and #$02					// and check the cell to the left of our test cell
			beq skipL1					// skip if cell is off
			inx							// increment X if it is on
skipL1:									// Do not change Y because the next cell is in the same character
BotLt1:		lda ($fd),y					// reload the character to the left of the test character
			and #$08					// check the cell down and to the left of the test cell
			beq skipBL1					// skip if cell is off
			inx							// increment X if it is on
skipBL1:	iny							// increment Y to the next character for testing
Right1:		lda ($fd),y					// Load the test character
			sta $06						// and store in $06 because we'll be testing 3 cells from this char.
			and #$02					// check the cell to the right of our test cell
			beq skipRT1					// skip if it is off
			inx							// increment X if it is on
skipRT1:
BotMid1:	lda $06						// load the character again from $06
			and #$04					// check the cell below our test cell
			beq skipBM1					// skip if off
			inx							// increment X if it is on
skipBM1:	
BotRt1:		lda $06						// load the character again from $06
			and #$08					// check the cell below and to the right of our test cell
			beq	skipBR1					// skip if test cell is off
			inx							// increment X if it is on
				
skipBR1:
//	------------------------------- This checks for life or death conditions and sets the storage byte
/*
										With all the surrounding cells to our test cell checked, we have a 
										count of how many are alive in X. Now we check if our test cell
										is alive and act accordingly.
*/
			lda $04						// load the character that contains our test cell
			and #$01					// check the test cell
			beq Birth1					// if it is off, go to birth check
			cpx #$02					// if it is on, compare $05 to $02 and $03
			beq	Done1					// if it is equal to either, the cell lives
			cpx #$03					// and do nothing by jumping to Done1
			beq Done1
			lda $04						// if X is less than 2 or more then 3 then the cell
			and #%00001110				// dies and we turn off bit one from the character
			sta $04						// and store in $04 for now
Done1:		jmp bit2					// after we determine cell status, we jump to check bit 2 of the character

Birth1:		cpx #$03					// The cell is off so we check if there are 3 surrounding live cells
			bne bit2					// if not exactly 3, no bith, move to bit 2 check
			lda $04						// if exactly 3, 
			ora #%00000001				// we turn on bit 1 of the character
			sta $04						// and temporarily store in $04 and move to bit 2			
		
//	------------------------------- This is the check for bit 2
/*
									The same process as above is repeated for bits 2, 4, and 8 so the 
									there are no comments there. Every bit surrounding the test bit is 
									checked and the result is stored in X. Once all the bits are checked,
									a life/death check of the test bit is made and then the life rules are 
									applied. $04 contains the current status of all 4 bits as it goes though 
									this process to be saved later. 
*/

bit2:		
Uleft2:		ldx #$00
			ldy #$01
			lda ($fd),y
			and #$04
			beq skipUL2
			inx
skipUL2:	
UpMid2:		lda ($fd),y
			and #$08
			beq skipUM2
			inx
skipUM2:	iny
UpRt2:		lda ($fd),y
			and #$04
			beq skipUR2
			inx
skipUR2:	ldy #43	
Left2:		lda ($fd),y
			sta $06
			and #$01
			beq skipL2
			inx
skipL2:		
BotLt2:		lda $06
			and #$04
			beq skipBL2
			inx
skipBL2:	
BotMid2:	lda $06
			and #$08
			beq skipBM2
			inx
skipBM2:	iny

Right2:		lda ($fd),y
			sta $06	
			and #$01
			beq skipRT2
			inx
skipRT2:
BotRt2:		lda $06
			and #$04
			beq	skipBR2
			inx
skipBR2:

//	------------------------------- This checks for life or death conditions and sets the storage byte
			lda $04
			and #$02
			beq Birth2
			cpx #$02
			beq Done2
			cpx #$03
			beq Done2
			lda $04
			and #%00001101
			sta $04
Done2:		jmp bit4					
	
			
Birth2:		cpx #$03
			bne bit4
			lda $04
			ora #%00000010
			sta $04		
			
//	------------------------------- This is the check for bit 4		
bit4:		
Uleft4:		ldx #$00
			ldy #42
			lda ($fd),y
			sta $06
			and #$02
			beq skipUL4
			inx
skipUL4:
Left4:		lda $06
			and #$08
			beq skipL4
			inx
skipL4:		iny
UpMid4:		lda ($fd),y
			sta $06
			and #$01
			beq skipUM4
			inx
skipUM4:	
UpRt4:		lda $06
			and #$02
			beq skipUR4
			inx
skipUR4:	
Right4:		lda $06	
			and #$08
			beq skipRT4
			inx
skipRT4:	ldy #84
BotLt4:		lda ($fd),y
			and #$02
			beq skipBL4
			inx
skipBL4:	iny

BotMid4:	lda ($fd),y
			sta $06
			and #$01
			beq skipBM4
			inx
skipBM4:	
BotRt4:		lda $06
			and #$02
			beq	skipBR4
			inx
skipBR4:

//	------------------------------- This checks for life or death conditions and sets the storage byte
			lda $04
			and #$04
			beq Birth4
			cpx #$02
			beq Done4
			cpx #$03
			beq Done4
			lda $04
			and #%00001011
			sta $04
Done4:		jmp bit8					

			
Birth4:		cpx #$03
			bne bit8
			lda $04
			ora #%00000100
			sta $04	

//	------------------------------- This is the check for bit 8			
bit8:		
Uleft8:		ldx #$00
			ldy #43
			lda ($fd),y
			sta $06
			and #$01
			beq skipUL8
			inx
skipUL8:
Left8:		lda $06
			and #$04
			beq skipL8
			inx
skipL8:	
UpMid8:		lda $06
			and #$02
			beq skipUM8
			inx
skipUM8:	iny
	
UpRt8:		lda ($fd),y
			sta $06
			and #$01
			beq skipUR8
			inx
skipUR8:	
Right8:		lda $06	
			and #$04
			beq skipRT8
			inx
skipRT8:	ldy #85
BotLt8:		lda ($fd),y
			sta $06
			and #$01
			beq skipBL8
			inx
skipBL8:	

BotMid8:	lda $06
			and #$02
			beq skipBM8
			inx
skipBM8:	iny
BotRt8:		lda ($fd),y
			and #$01
			beq	skipBR8
			inx

skipBR8:

//	------------------------------- This checks for life or death conditions and sets the storage byte
			lda $04
			and #$08
			beq Birth8
			cpx #$02
			beq Done8
			cpx #$03
			beq Done8
			lda $04
			and #%00000111
			sta $04
Done8:		jmp nextbyte					

			
Birth8:		cpx #$03
			bne nextbyte
			lda $04
			ora #%00001000
			sta $04	

//	-------------------------------- This stores the new 4 cell character in new memory		
/*
										Now that all 4 bits of the character have been checked, $04 holds
										the current status for all the bits of the character. That will be
										stored in mem2 area. $02 and $03 were set earlier to address that 
										block of memory and will be used here with Y to store.
*/	
nextbyte:	ldy #$00				// set Y to zero ($02 an d $03 have the exact address for placmement)
			lda $04					// load $04 (the result of all of our testing)
			sta ($02),y				// and store.

//	-------------------------------- This does some adding to the indexing addresses to move to the next byte	
/*
										Now that the memory has been stored, we increment all the base address
										for the index to the next memory location over. We do this until we go 
										through the entire screen plus the buffer space below the screen.
										All three base addresses are incremented here, but only the main one
										is checked for reaching an end point. The others just follow along.
*/		
LoopAdd:	clc
			lda $fb
			adc #$01
			sta $fb
			lda $fc
			adc #$00
			sta $fc
			cmp #>mem1pad			// check to see if we've hit the end of memory we want to check
			bcc bigloop				// it checks the hi byte first, and then the low byte
			lda $fb
			cmp #<mem1pad
			bcc bigloop
			jmp mempass				// we've hit the end of memory and want to copy the new data to working memory
bigloop:	clc
			lda $fd
			adc #$01
			sta $fd
			lda $fe
			adc #$00
			sta $fe
			clc
			lda $02
			adc #$01
			sta $02
			lda $03
			adc #$00
			sta $03

			jmp bit1				// Once everything is incremented we go back and start checking the next character.

//	---------------------------- Once the Life Cycle is done, this copies the result back over the working memory		
mempass:
			lda #<mem2				// quick memory copy of mem2 to mem 1
			sta $fb					// The blocks of memory are padded to be
			lda #>mem2				// 6 x 256 bytes to make use of X as a counter
			sta $fc					// and Y to go through its full range
			lda #<mem1				// before incrementing the high
			sta $fd					// byte of the index base.
			lda #>mem1
			sta $fe
			ldx #$06
memloopy:	ldy #$00				// probably not needed because y loops itself
memloop:
			lda ($fb),y				// get from mem2
			sta ($fd),y				// place in mem1
			iny						// increment y
			bne memloop				// if we haven't looped y, loop again
			clc 					// if we have increment the high byte of both bases
			inc $fc
			inc $fe
			dex						// dec x
			bne memloopy			// if we haven't counted down to zero, loop again
			jmp Display				// memory moved. jump to display to start the cycle again

//	---------------------------- This section transfers the Glider Gun to memory			
GGun:
			jsr ClearMem
			lda #<mem1view
			sta $fb
			lda #>mem1view
			sta $fc
			clc
			lda $fb
			adc #$83
			sta $fb
			lda $fc
			adc #$00
			sta $fc

GloopY:
			ldy #$d2
Gloop:
			lda GliderGun,y
			sta ($fb),y
			dey
			bne Gloop
			rts
			
//	---------------------------- This sectioin transfers Beluchenko's p51 to memory
PP51:
			jsr ClearMem
		

P51loopY:
			ldy #238
P51loop:
			lda P51,y
			sta mem1view+221,y
			lda P51+238,y
			sta mem1view+459,y
			lda P51+476,y
			sta mem1view+697,y
			dey
			bne P51loop
			rts

ddiamond:
			jsr ClearMem
		

ddloopY:
			ldy #237
ddloop:
			lda diamond,y
			sta mem1view+437,y
			dey
			bne ddloop
			rts


ClearMem:
			lda #<mem2				// quick memory clear
			sta $fb					// The blocks of memory are padded to be
			lda #>mem2				// 6 x 256 bytes to make use of X as a counter
			sta $fc					// and Y to go through its full range
			lda #<mem1				// before incrementing the high
			sta $fd					// byte of the index base.
			lda #>mem1
			sta $fe
			ldx #$06
clrloopy:	ldy #$00	
			lda #$00				
clrloop:
			sta ($fb),y				// place 0 in mem2
			sta ($fd),y				// place 0 in mem1
			iny						// increment y
			bne clrloop				// if we haven't looped y, loop again
			clc 					// if we have increment the high byte of both bases
			inc $fc
			inc $fe
			dex						// dec x
			bne clrloopy			// if we haven't counted down to zero, loop again
			rts

//	--------------------------- These are the screen characters to translate the 4 cells into a single character
char:		.byte $20,$7e,$7c,$e2
			.byte $7b,$61,$ff,$ec
			.byte $6c,$7f,$e1,$fb
			.byte $62,$fc,$fe,$a0
			

//  ---------------------------- This is the layout for the Gosper glider gun
*=* "GliderGun"
GliderGun:
			.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$04,$05,$00,$00,$00,$00,$00
			.fill 24,0
			.byte $00,$00,$00,$00,$00,$08,$03,$08,$00,$00,$0f,$00,$00,$00,$00,$00,$00,$0f
			.fill 24,0
			.byte $0f,$00,$00,$00,$00,$05,$00,$04,$0d,$00,$03,$04,$04,$00,$00,$00,$00,$00
			.fill 24,0
			.byte $00,$00,$00,$00,$00,$09,$00,$08,$01,$00,$00,$00,$01,$00,$00,$00,$00,$00
			.fill 24,0
			.byte $00,$00,$00,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
			.fill 48,0
			
//	------------------------------ This is the layout for Beluchenko's p51
*=* "P51"
P51:
			.byte $00,$00,$00,$0f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0a,$05,$00,$00,$00
			.fill 23,0
			.byte $00,$08,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0c,$00,$00
			.fill 23,0
			.byte $00,$02,$01,$00,$08,$0c,$08,$07,$00,$00,$02,$0d,$08,$0c,$00,$00,$03,$00,$00
			.fill 23,0
			.byte $00,$00,$00,$00,$0d,$01,$00,$00,$00,$00,$00,$00,$00,$09,$05,$00,$00,$00,$00
			.fill 23,0
			.byte $00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00
			.fill 23,0
			.byte $00,$00,$00,$08,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0d,$00,$00,$00,$00
			.fill 23,0
			.byte $05,$00,$00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$00,$00,$00,$05
			.fill 23,0
			.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
			.fill 23,0
			.byte $04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$04
			.fill 23,0
			.byte $01,$00,$00,$0a,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0e,$00,$00,$00,$01
			.fill 23,0
			.byte $00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00
			.fill 23,0
			.byte $00,$00,$00,$00,$0d,$00,$00,$00,$00,$00,$00,$00,$00,$08,$05,$00,$00,$00,$00
			.fill 23,0
			.byte $00,$00,$00,$00,$09,$0d,$08,$04,$00,$00,$00,$0c,$08,$0d,$01,$00,$00,$00,$00
			.fill 23,0
			.byte $00,$0a,$05,$00,$00,$00,$00,$03,$00,$00,$02,$01,$00,$00,$00,$00,$0f,$00,$00
			.fill 23,0
			.byte $00,$00,$00,$0c,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08,$04,$00,$00,$00
			.fill 23,0
			.byte $00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$01,$00,$00,$00
			.fill 23,0
			.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
			.fill 23,0
			.fill 23,0
diamond:
			.byte $00,$00,$03,$03,$00,$00
			.fill 36,0
			.byte $00,$03,$03,$03,$03,$00
			.fill 36,0
			.byte $03,$03,$03,$03,$03,$03
			.fill 36,0
			.byte $00,$03,$03,$03,$03,$00
			.fill 36,0
			.byte $00,$00,$03,$03,$00,$00
			.fill 36,0
			.fill 36,0
