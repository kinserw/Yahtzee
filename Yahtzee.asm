;******************************************************************************************
;* Program Name:	Yahtzee.asm
;* Programmer:		William Kinser
;* Class:	 		CSCI 2160-001
;* Lab:   			Homework ?: Yahtzee
;* Date:  			04/15/2020
;* Purpose:			play the game
;
; NOTE: the overall design of the program follows:
; Clear the screen by printing long strings of spaces at each row
; set position of the cursor to the top of the screen 
; display the score card (which consists of labels and values/scores)
; use Random to generate dice roll and display these to the user
;				I display a series of rolls with a pause so there is some visual 
;				effect to the dice roll
; Prompt the user for what to do next
; 		The user gets up to 3 rolls per turn, the first roll: all dice are rolled
;		User gets the option to reroll some or all the die or to score the roll as is
;There is a bit map for which die are to be rolled and which are not to be rolled.
;		When the user selects a die, it toggles that reroll state for that die and
; 				the current state is display by underlining the die to be rerolled
; Once they commit to the roll, the die are updated multiple times for visual effect
; When the turn is over (3 rolls or user elects to stop rolling), 
; 				The dice are analyzed for patterns (3/4/5 of kind, etc) stored in Tallies
;				user specifies where to place the roll (1-D)
; 				if die roll meets the criteria for the specified score card location, do it
;				else ask the user to either put a zero there or specify a different field
; As each field is scored based on the dice rolled, 
;				The upper and lower sections are totaled if completely filled
; If both upper and lower are filled, the game is over and final totals are made
; User is asked to play again or not
;
; some key design choices (for the better or worse):
; multiple arrays are used to track stats, scores, displays, etc
; no good way to clear the screen I just print blank lines on each row) 
;          There built in methods to do fillConsole... but these just basically do the 
;          same thing by printing a space to each cell individually. 
; using WriteConsoleOutputCharacterA allows me to specify the location of a printout 
;		which is helpful in printing the markers for the dice to be rolled
;       but there are drawback; it doesn't handle carriage returns embedded in the string
;       and it doesn't always pickup the colors in setConsoleTextAttributes. 
; 		So, I use SetConsoleCursorPosition to position the cursor and the printString.
; In most cases, the user can enter either lower or upper case letters
; Invalid entries are handled and user is reprompted for something more valid
; No real optimizations are performed in the code, pretty much went for what worked
; All the math is based on unsigned integers
; In some cases, I used shifting to multiple non-EAX registers by 2, 4, etc
; 		while I used MUL in the others. 
; I used the BTC operator to toggle specific bits when user sets which die to reroll
; Lots of private procedures for this program using registers as parameters
; 		(not very maintainable at this stage but it works)
; Used the rollDie proc from last homework without any changes
; Given that the number of players is fixed, the number of dice rolls and the size are 
; score cards are also fixed, I elected not to mess with doing heap allocations. 
; In theory, I could use the heap to create structures for each player and thereby
; simulate a more Object-based approach but it's doesn't seem to me to add enough 
; value to do a pseudo OO solution in a language that pre-dates structured programming let
; alone object oriented. Procedural development seems more of a reasonable compromise,
; though I have to admit I did more of an unstructured solution in the end.
;
; SOUND: 
; I settled on Beep for sound but had tried using ctrl-g (bell) and a user32 library
; procedure that played default Windows tones. I like the beep but I can't figure out
; why it delays and doesn't come out of the speaker until after the dice have finished 
; rolling. (I even moved it around in the code but the delay remained).
;
; AND then there is the AI portion. I shouldn't really call it AI, it's more like a 
; dumb version of a expert system. There is no fuzzy logic or improvements over time, just
; a lot of conditional statements based on various scenarios.
; Here is the basic logic I started with...
	; - if no rolls left uses tallies to determine best fit (highest score for final roll of die)
	    ; - use same logic as for evualting each roll (see below) (just don't roll again)
			; (this covers Yahtzees & straights & some full houses)
			; - if not scored, then continue
		; - any 3 or 4 of a kind with 5s or 6s, score as 4 of a kind, 3 of a kind, upper,
			; - or chance
		; - if full house, score as full house
		; - any 3 or 4 of a kind <5 should go in upper if avail, otherwise 3 of kind, or 
			; 4 of a kind if available
		; - if a pair only and < 3, score in upper
		; - if pair 3 and upper 4 of a kind in 4s, 5s or 6s, score as 3s
		; - if total die is > 20, score in chance
		; - if no score possible, take a zero in 1s in upper, Yahtzee, large straight, 
		  ; 2s in upper, small straight, 3s in upper, full house, 4 of a kind, any upper not met
	
	; - evaluates each roll...
		; - scores large straights immediately if rolled (as large or small)
		; - scores yahtzee immediately as Yahtzee, 4 of a kind (if 5 or 6), as 3 of a kind
			; (if 5 or 6), as upper section, 
		; - if Yahtzee and joke rule in effect then score immediately as large straight, small straight		
		; - if Yahtzee score immediately as full house, as 4 of a kind, as 3 of a kind,
			; , as chance
		; - if small straight (and large straight not scored), then roll non sequential die again
			; otherwise score it immediately as a small straight
		; - if 4 of a kind, roll odd die again
		; - if full house and 3 of a kind met and upper met but full house
			; not met, score immediately as a full house
		; - if 3 of a kind (which includes a full house), roll both odd die again
		; - if 2 pair, roll all but the 2 highest pair
		; - if 1 pair roll all odd die again
		; - roll all but the highest die again
;
; once I got it all working, I tweaked some of the logic paths to give better results
; and avoid some infinite loops. To test it it, I ran 4 computer players simultaneously
; and literally stepped through the logic one line at a time while keeping the dice
; roll for reference to see if it was making reasonable decisions.
; I took note of some rolls that led to odd outcomes and later on, I forced the die
; to be those rolls to revisit and rework the logic in those areas. A number of them
; got resolved along the way but a few led to some improvements.
;
; In the end, I am pleased with the scores the computer gets.		
;******************************************************************************************
	

	.486					;tell the assembler to generate 32-bit code (80486 compatible)
	.model flat				;tell assembler all addresses are within the same segment
	.stack 200h				;allocate 100 hexadecimal bytes to the runtime stack

; these are the aliases I use to be more clear on what is happening in the code
HANDLE TEXTEQU <DWORD>		; used in Win32 API documentation
STD_OUTPUT_HANDLE EQU -11  ; -11 for stdout in Windows

; these are data structures I use in association with window procedure calls
COORD STRUCT
	X WORD ?
	Y WORD ?
COORD ENDS	

SYSTEMTIME STRUCT
	wYear WORD ?
	wMonth WORD ?
	wDayOfWeek WORD ?
	wDay WORD ?
	wHour WORD ?
	wMinute WORD ?
	wSecond WORD ?
	wMilliseconds WORD ?
SYSTEMTIME ENDS	

SMALL_RECT STRUCT
  Left     WORD ?
  Top      WORD ?
  Right    WORD ?
  Bottom   WORD ?
SMALL_RECT ENDS

; I wrote this macro (and a few others I ended up deleting) mostly to get some 
; experience using macros. These seem to do best for small pieces of repeating code
; that it would be too cumbersome to make into a procedure. I did have two that took
; in parameters but ended up deleting them because I only used them in two places
; and the code evolved.
setPlayerColor MACRO 
	PUSH ECX
	MOVZX ECX, bCurrentPlayer   ; get current player number
	DEC ECX
	SHL ECX, 1
	MOV CX, wPlayerColors[ECX]  ; get color for this player
	INVOKE SetConsoleTextAttribute, outHandle, CX ; change color of text	
	POP ECX
ENDM

; these are the prototypes for procedures that are not private to this program

	;prototype statements to tell the assembler "how" to call a function
	;will not be resolved until later on by the linker/loader
	ExitProcess 	PROTO Near32 STDCALL, dwExitCode:DWORD

	GetStdHandle  PROTO	Near32  STDCALL, nStdHandle:DWORD

	GetSystemTime PROTO Near32 STDCALL, lpSystemTime:PTR SYSTEMTIME ; system time

	SetConsoleCursorPosition PROTO Near32 STDCALL,
								hConsoleOutput:HANDLE,		
								dwWriteCoord:COORD		

								
	WriteConsoleOutputCharacterA PROTO Near32 STDCALL,
								hConsoleOutput:DWORD,
								lpBuffer:PTR BYTE,
								dwNumberOfChartsToWrite:DWORD,
								dwWriteCoord:COORD,
								lpNumberOfChartsWritten:PTR DWORD

	SetConsoleTextAttribute 	PROTO Near32 STDCALL,
								hConsoleOutput:HANDLE,		; output handle
								wAttributes:WORD		; color attribute (fixed 6/20/05)

	SetConsoleTitleA			PROTO Near32 STDCALL,
								lpTitle:PTR BYTE
	


	SetConsoleWindowInfo 		PROTO Near32 STDCALL,		
								hConsoleOutput:HANDLE,		; screen buffer handle
								bAbsolute:DWORD,		; coordinate type
								lpConsoleWindow:PTR SMALL_RECT	; ptr to window rectangle
	
	Sleep 						PROTO Near32 STDCALL,	   	; sleeep for n milliseconds
								dwMilliseconds:DWORD
								
	Beep						PROTO Near32 STDCALL,
								 dwFreq:DWORD, dwDuration:DWORD


	GetLastError				PROTO Near32 STDCALL

; from random.obj (uses C style proc calls)
	seed			PROTO Near32 C lpValues:PTR DWORD
	rotl			PROTO Near32 C dState:DWORD, bBitsToShift:BYTE
	next			PROTO Near32 C 

; from my utility.obj (uses STDCALL proc calls)
	stringPrint	  PROTO Near32 STDCALL lpString:PTR BYTE 
	stringInput   PROTO Near32 STDCALL lpString:PTR BYTE, dLength:DWORD
	stringLength  PROTO Near32 STDCALL lpString:PTR BYTE
	stringCompare PROTO Near32 STDCALL lpString1:PTR BYTE, lpString2:PTR BYTE
	stringCopy    PROTO Near32 STDCALL lpSource:PTR BYTE, lpDestination:PTR BYTE
	charInput     PROTO Near32 STDCALL ; returns char in EAX
	itoasc32 	  PROTO Near32 STDCALL lpStringResult:PTR BYTE, dSource:DWORD
	asctoi32 	  PROTO Near32 STDCALL lpStringResult:PTR BYTE

	

	;data segment: holds data for our program (ints, chars, arrays, other primitives)
	.data

strName				BYTE	"   Name: William Kinser",13,10,0 
strClass			BYTE	"  Class: CSCI 2160-001",13,10,0
strDate				BYTE	"   Date: 04/15/2020",13,10,0
strLab				BYTE	"    Homework ? : Yahtzee",13,10,0

							; keep welcome narrow in case cmd screen is small
strWelcome			BYTE	13,10, "Welcome to the game of Yahtzee",0
strPlayers			BYTE 	13,10, "You can choose up to 4 players per game. ",
							13,10, "         Each player will be assigned a color ",
									"(white, red, green, yellow).",
							13,10, "How many players for this game (1-4)?",0
strNotValidCount	BYTE	13,10, "Invalid # of players. Must be 1, 2, 3, or 4!",0
strPlayerName		BYTE	13,10, "Please enter the name for Player (max 10 chars): ",0;
 
strComputerNameL	BYTE	"computer",0
strComputerNameU	BYTE	"Computer",0
strLF				BYTE 	13,10,0 ; use as a line feed between outputs
strCriteriaNotMet	BYTE 	13,10, "Criteria not met for field selected.",
								" Do you want to put a zero here? (Y/N)", 0
strTitle			BYTE 	"Yahtzee",0
strPromptToRoll		BYTE 	"Click any key to roll the dice.",0
					
strRow1				BYTE 	      " 1 => Ones           :",0
strRow2				BYTE 	13,10," 2 => Twos___________:________________________________",
									"__________",0
strRow3				BYTE 	13,10," 3 => Threes         :",0
strRow4				BYTE 	13,10," 4 => Fours__________:________________________________",
									"__________",0
strRow5				BYTE 	13,10," 5 => Fives          :",0
strRow6				BYTE 	13,10," 6 => Sixes__________:________________________________",
									"__________",0
strUpperSubTotal	BYTE	13,10,"Upper Section        :",0 
strUpperBonus		BYTE 	13,10,"Bonus ( upper > 63)  :________________________________",
									"__________",0
strUpperTotal		BYTE 	13,10,"Upper Section Total  :",0

strRow7				BYTE	13,10," 7 => 3 of Kind      :",0
strRow8             BYTE	13,10," 8 => 4 of Kind      :________________________________",
									"__________",0
strRow9				BYTE 	13,10," 9 => Full House     :", 0
strRow10			BYTE	13,10," A => Small Straight :________________________________",
									"__________",0
strRow11			BYTE 	13,10," B => Large Straight :", 0
strRow12			BYTE 	13,10," C => YAHTZEE!       :________________________________",
									"__________",0
strRow13			BYTE 	13,10," D => Chance         :",0
strYahtzeeBonus		BYTE 	13,10,"Yahtzee Bonus        :",0
strLowerTotal		BYTE 	13,10,"Lower Section Total  :________________________________",
									"__________",0
strGameTotal		BYTE 	13,10,"Grand Total          :",0

; pointer to all the above display strings
lpScoreCard			DWORD  strRow1, strRow2, strRow3, strRow4, strRow5, strRow6,
							strUpperSubTotal, strUpperBonus, strUpperTotal,
						strRow7, strRow8, strRow9, strRow10, strRow11, strRow12, strRow13,
						strYahtzeeBonus, strLowerTotal, strGameTotal
upperTotalIndex EQU 24 ; 7th field * DWORD size - DWORD SIZE 
lowerTotalIndex EQU 68 ; 18th field * DWORD size - DWORD SIZE 

; these two arrays track the numeric scores based on user selections. They match the 
; values stored in the field strings listed above in lpScoreCard

sScores				WORD 19 dup(0) ; array of scores for each field in card
sScores2			WORD 19 dup(0) ; array of scores for each field in card
sScores3			WORD 19 dup(0) ; array of scores for each field in card
sScores4			WORD 19 dup(0) ; array of scores for each field in card

; the following offsets, allow the code to look and feel more like I used structs 
; instead of arrays. This allows me to find the element I need without having to do
; the math every time.
upperTotalOffset EQU 12 			
upperBonusOffset EQU 14
upperGrandOffset EQU 16
lowerStartOffset EQU 18
fullHouseOffset EQU 22
smallStrtOffset EQU 24
largeStrtOffset EQU 26
yahtzeeOffset EQU 28
chanceOffset EQU 30
lowerBonuseOffset EQU 32
lowerTotalOffset EQU 34
grandTotalOffset EQU 36

; this array allows me to avoid "if player = 1 then set lpCurrentScores = sScores...."
; I can just take the bCurrentPlayer, manipulate the value into a DWORD and index into
; this array to get the right score card. Kind of acts like a reference pointer to 
; a collection of score cards
lpPlayerScores		DWORD sScores, sScores2, sScores3, sScores4
lpCurrentScores		DWORD ?  ; this is a handle/reference to whichever card maps to the 
							 ; current player. THis way I only have to look it up once
							 ; at the start of a turn. (taking advantage of global vars)
bComputerPlayers	BYTE 4 dup (0) ; keeps track of which players use the "AI" logic
bAIPlayer			BYTE 0			; flag indicating the current player is "AI" or not

bCurrentPlayer		BYTE 1; tracks which player is currently playing
bMaxPlayers			BYTE 1; tracks the total number of players in the current game (<=4)

strPlayer1			BYTE 10 dup(20h),0,0	; player 1 name (max of 10 characters)
strPlayer2			BYTE 10 dup(20h),0,0	; player 2 name (max of 10 characters)
strPlayer3			BYTE 10 dup(20h),0,0	; player 3 name (max of 10 characters)
strPlayer4			BYTE 10 dup(20h),0,0	; player 4 name (max of 10 characters)

; this is a lookup array. I can use the bCurrentPlayer to index into this to get the 
; current player's name.
lpPlayerNames		DWORD strPlayer1, strPlayer2, strPlayer3, strPlayer4

; this is a lookup array. I can use the bCurrentPlayer to index into this to get the 
; current player's color. THese colors are fixed to the player number always.
wPlayerColors		WORD 01, ; blue on black
                         04, ; red on black
						 02, ; green on black
						 06  ; yellow on black
; text color is two bytes, first is background, second is foreground (text)
; 0 = Black
; 1 = Blue
; 2 = Green
; 3 = Aqua
; 4 = Red
; 5 = Purple
; 6 = Yellow
; 7 = White
; 8 = Gray,
; 9 = Light Blue.

; these strings are used for the lower section fields with predefined scores
strFullHouseScore 	BYTE "25",0	; preDefined string for full house score
strSmallScore 		BYTE "30",0	; preDefined string for small straight score
strLargeScore 		BYTE "40",0	; preDefined string for large straight score
strYahtzeeScore 	BYTE "50",0	; preDefined string for Yahtzee score

; array of predefined string for certain lower field scores
lpPredefineValues 	DWORD strFullHouseScore, strSmallScore, strLargeScore, strYahtzeeScore
sPredefinedScores	BYTE 25, 30, 40, 50 ; used to calculate the numeric scores for these

; strings used for prompts and error messages associated with those prompts
strRollPrompt		BYTE 	13,10,"What do want to roll again? (Y/N)",0
strRollPrompt2		BYTE 	13,10,"Enter the number of the die you want to roll again ",
							"then ENTER to re-roll",0					
strFieldPrompt		BYTE	13,10,"Enter the row to place this roll (1-D) : ",0
strInvalidField		BYTE	13,10,"That was an invalid field number. ",
									" Must be 1,2...9,A,B,C,D",0
strFieldAlreadyUsed BYTE 	13,10,"That field has already been used. ",0
strPlayAgain		BYTE	13,10,"Do you want to play again? (Y/N)",0
strInvalidYN		BYTE 13,10,"Invalid entry, must be either Y or N",0

; these variables are associated with the random number generation and dice rolls
; dSeed defaults to the homework values but get overridden at the start of the game
; with seeds based on the system time (thus less predictable)
dSeed				DWORD 10203040h, 50607080h, 90A0B0C0h, 0D0E0F000h
lpDice				DWORD 5 dup(?)   ; current value of each die
lp6Dice				DWORD 5 dup(6)   ; used in testing (to override lpDice)
bDiceToRoll			BYTE 0 	; bits 1-5 are set if that die is to be rolled (skips 0)
strDisplayDieValue	BYTE 500 dup(0)	; used to hold the entire die display, all 5 die
strMarker			BYTE 11 DUP (0DCh),20h,0 ; mark which die are to be rerolled
strClearMarker		BYTE 12 DUP (20h), 0 ; clear the marker
bRollNumber			BYTE 1			; track how many times the die have been rolled 
strRingTheBell		BYTE 07,0			; rings the bell (NOT USED)

; generic variables used in various locations of the code
strInput1			BYTE	40 dup (0) ; input string for user values
strInput2			BYTE	40 dup (0) ; input string for user values
strBlankValue		BYTE 	"    ",0   ; used to right justify scores

strBlankLine		BYTE 132 dup (20h),0 ; used in clearing the screen or specific lines

stCurTime 			SYSTEMTIME <> ; this structure is used to generate seeds for random

; tallies are variables that are used in analyzing the dice rolls for a given turn
; tallies are used two ways: first, to validate choices made by human players, 
; secondly, to give options to the AI player.
bStartOfTallies		BYTE 0 ; reserve space just prior to tallies as starting point 
							; for indexing since we use counters most of the time which
							; always start at 1
bFaceTally 			BYTE 6 dup (0) ; tally for each face value
b3KindTally			BYTE ? ; tally indicating a 3 of a kind (this MUST be after bFaceTally)
b4KindTally			BYTE ? ; tally indicating a 4 of a kind
bFullTally			BYTE ? ; tally indicating a full house
bSmall				BYTE ? ; tally indicating a small straight
bLarge				BYTE ? ; tally indicating a large straight
b5KindTally			BYTE ? ; tally indicating a 5 of a kind
bChanceTally		BYTE 1 ; tally indicating chance is valid
bPairFlag			BYTE ? ; count of how many #s have 2 or more matches
bRunningTotal 		BYTE ? ; keep a running total of all 5 die

; values for MS display procedures
outHandle    		HANDLE ? 	; console handle
dCellsWritten 		DWORD ?		; return value from console procedures 
topCornerCoord 		COORD <0,0> ; top left corner of screen
genCoord 			COORD <0,0> ; generic coordinates used in printing at specifc locations
wDefaultAttr		WORD 07h	; holds default text attributes (usually white on black)
genRect				SMALL_RECT <>; rectangle that defines size of the console window


	;code segment: holds instructions for our program
	.code
_start:						;entry point for the program
	MOV EAX, 0				;for debugging purposes

main PROC

	; Get the Console standard output handle: 
	INVOKE GetStdHandle,STD_OUTPUT_HANDLE 
	mov outHandle,eax ; Set the colors of adjacent cells: 
	MOV EAX, 0
	
	; make a few calls to set up the console window the way that looks best for game
	INVOKE SetConsoleTitleA, ADDR strTitle ; set title of window to Yahtzee
	MOV EAX,0
	MOV genRect.Left,AX	 	; top left corner is 0,0
	MOV genRect.Top,AX	 	; top left corner is 0,0
	MOV EAX, 50h
	MOV genRect.Right,AX		; 80 columns
	MOV EAX, 29h
	MOV genRect.Bottom, AX		; 40 rows
	INVOKE SetConsoleWindowInfo, outHandle, 1, ADDR genRect	
	INVOKE SetConsoleTextAttribute, outHandle, wDefaultAttr		; 15 for white on black, 
	
	CALL clearScreen; clear the screen (and sets the cursor in the top left corner)

; for each string in the header, 
	INVOKE stringPrint, ADDR strName 
	INVOKE stringPrint, ADDR strClass 
	INVOKE stringPrint, ADDR strDate 
	INVOKE stringPrint, ADDR strLab 
	INVOKE stringPrint, ADDR strWelcome 

; set seed based on current system clock time
	MOV ECX, 4 			; generate 4 seed dword values
genSeed:
	PUSH ECX			; save counter
	INVOKE GetSystemTime, ADDR stCurTime   ; get current system time
	POP ECX				; retrieve counter
	MOV AX, stCurTime.wMilliseconds ; extract system time's milliseconds 
	SHL EAX, 16			; move milliseconds to high word
	MOV AX, stCurTime.wMilliseconds ; extract system time's milliseconds 
	NEG AX				; flip bits for lower word
	MOV EBX, ECX		; save counter into EBX for offset into seed array
	DEC EBX				; decrement offset by 1 since we start counting at 0
	MOV dSeed[EBX*4], EAX ; save seed value in array (in reverse order)
	loop genSeed

	; store in dSeed (array of 4 seed values) into the random data area  
	PUSH OFFSET dSeed
	CALL seed
	ADD ESP, 4 ; restore stack

startGame: ; this label used so user can play multiple games in same program run

	CALL clearScoreCard		; clears the values from the score card fields

getPlayerCount:
	INVOKE stringPrint, ADDR strPlayers ; Prompt for # of players this game
	INVOKE charInput		; get a # between 1 and 4; returned in EAX
	SUB EAX, 30h			; convert to a digit
	CMP EAX, 4				; if not between 1 and 4, error msg and reprompt
	JG notValidCount	
	CMP EAX, 1				; test lower bound
	JGE getPlayerNames
notValidCount:
	INVOKE stringPrint, ADDR strNotValidCount
	JMP getPlayerCount

getPlayerNames:
	MOV bMaxPlayers, AL		; save maximum number of players for this game
	MOV ECX, 0 ; loop counter for getting names
	SHL EAX,1  ; using ECX as index into word array so double EAX in order to compare them
	MOV EDX, OFFSET strPlayer1
getNextName:
	PUSHAD
	MOV CX, wPlayerColors[ECX]  ; get color for this player
	INVOKE SetConsoleTextAttribute, outHandle, CX ; change color of text	
	INVOKE stringPrint, ADDR strPlayerName ; prompt for name`
	POPAD
	INVOKE stringInput, ADDR strInput1, 40 ; get name 
	JNC getNextName ; if unable to read name, try again
	CALL trimName 	; trims strInput1 down to max of 10 chars, copies to string at EDX

	PUSH EAX ; preserve EAX since next set of calls uses it for return values
	INVOKE stringCompare, EDX, ADDR strComputerNameL
	CMP EAX, 0
	JE isAComputerPlayer
	INVOKE stringCompare, EDX, ADDR strComputerNameU
	CMP EAX, 0
	JNE advanceToNextPlayerName
isAComputerPlayer:	
	PUSH ECX    ; save ECX since I need to temporarily manipulate it
	SHR ECX, 1	; get single byte index based on current player being entered
	MOV bComputerPlayers[ECX], 1 ; set flag that this player is a computer player
	POP ECX
advanceToNextPlayerName:	
	POP EAX 	; restore EAX as my index
	INC ECX
	INC ECX 	; advance to next player
	ADD EDX, SIZEOF strPlayer1 ; advance to next name string
	CMP ECX, EAX
	JNE getNextName

; ok, ready to start playing...
	MOV bCurrentPlayer, 1
	CALL displayScoreCard	; display the score card to user
takeTurn:
	MOV bAIPlayer, 0
	; get start of scores for the current player and save it for processing during turn
	; basically, I'm setting my globals that change each turn
	MOV AL, bCurrentPlayer	; get current player`
	MOVZX EAX, AL			; expand player number to a dword
	DEC EAX					; need index to start at 0 not 1
	MOV CL, bComputerPlayers[EAX] ; is this player a computer player?
	CMP CL, 0
	JE currentPlayerNotAI
	MOV bAIPlayer, 1
currentPlayerNotAI:
	SHL EAX,2				; multiply by 4 so it indexs into a dword array
	MOV EAX, lpPlayerScores[EAX] ; get scorecard for this player
	MOV lpCurrentScores, EAX
	
	CALL displayScores ; refreshes scores (redudant the first turn only)
	setPlayerColor ; macro
	; clear lines below the die
	MOV CX, 20		; set row to start printing blank lines
	MOV DX, 42		; set which row to stop printing on
	CALL clearLines ; clears specific lines (bottom of screen)
	CMP bAIPlayer, 1
	JE skipPromptToRoll ; if current player is AI then no need to ask it to roll
	INVOKE stringPrint, ADDR strPromptToRoll ; is user ready to roll (hehe)
	INVOKE charInput ; don't care what they clicked, just that they clicked

skipPromptToRoll:
	; intialize to roll all 5 dice
	MOV AL, 3Eh				; set all 5 bits to indicate all 5 to be rolled
	MOV bDiceToRoll, AL 	; copy dice number into memory

	MOV AL, 1				; first roll
	MOV bRollNumber, AL 	; set the roll number for this turn to be the first roll

	; kept the alternate sound effect just for reference
	;INVOKE stringPrint, ADDR strRingTheBell        ; ring the bell (plays a chime)
	INVOKE Beep, 750, 10		; plays a beep (freq,duration) trying
	INVOKE Beep, 750, 15		; plays a beep (freq,duration) trying
	INVOKE Beep, 750, 25		; plays a beep (freq,duration) trying
	INVOKE Beep, 750, 15		; plays a beep (freq,duration) trying

; now, roll and display the die 5 times with a slight pause in between for visual 
; effect. NOTE: only the die being rolled will appear to change
rollAgain:
	MOV ECX, 5				; roll each die 5 times before proceeding
rollAgain2:

	CALL rollDice			; roll the dice specified in bDiceToRoll, store in lpDice
; TEST CODE: in order to test the joker rule and yahtzee bonuses, comment out the line 
; above "Call rollDice" and uncomment the 5 lines below so that you always roll 6s
	 ; MOV lpDice, 6 
	 ; MOV lpDice+4, 6
	 ; MOV lpDice+8, 6
	 ; MOV lpDice+12, 6
	 ; MOV lpDice+16, 6
	
	; code to display dice
	MOV EAX, 5
	PUSH EAX					; we need to display 5 dice so put 5 on stack as 3rd param
	PUSH OFFSET lpDice			; push dice value array on stack as 2nd param
	PUSH OFFSET strDisplayDieValue		; push string to put display into 1st param
	CALL drawDice				; format string based on 2 dice rolls
	ADD ESP, 12    				; restore stack


	setPlayerColor			; macro to set text attributes for current player's color
	MOV genCoord.X, 0
	MOV genCoord.Y, 20
	PUSH ECX				; system calls stomp on ECX which I care about so preserve it
	INVOKE Sleep, 100		; wait 100 milliseconds before displaying next roll
	INVOKE SetConsoleCursorPosition, outHandle, genCoord  
	INVOKE stringPrint, ADDR strDisplayDieValue ; display formatted dice
	POP ECX
	DEC ECX					; count down # of rolls to be displayed
	CMP ECX, 0
	JNE rollAgain2		    ; keep going till done
	
	INC bRollNumber			; advance the roll number
	MOV AL, bRollNumber		; get current roll number
	CMP AL, 4				; if third roll is over, score it
	JE scoreTheRoll 

; clear lines below the die
	MOV CX, 27		; set row to start printing blank lines
	MOV DX, 42		; set which row to stop printing on
	CALL clearLines

; ask user if they want to roll again (no need to do this if player is AI)
rollPrompt:	
	CMP bAIPlayer,1
	JNE getHumanRollAgain 	; if player is human go to the prompt

							; otherwise, call the AI logic to determine which die to roll
							; again (which will be set in the same bitmap the human uses
							; prior to returning if the AI decides to roll again)
	CALL willAIRollAgain ; EAX holds single Char answer (Y/N)
	JMP rollAgainOrNot
	
getHumanRollAgain:
	INVOKE stringPrint, ADDR strRollPrompt ; ask user if they want to roll again
	INVOKE charInput			; get Y or N
rollAgainOrNot:

; test for upper and lower case responses, anything else is an error so re-prompt
	CMP EAX, 'Y'
	JE pickDice
	CMP EAX, 'y'
	JE pickDice
	CMP EAX, 'N'
	JE scoreTheRoll
	CMP EAX, 'n'
	JE scoreTheRoll

	; re-prompt
	INVOKE stringPrint, ADDR strInvalidYN 
	JMP rollPrompt

; now, let the use toggle which die to re-roll. The AI players skip all this.
pickDice:
	MOV AL, bAIPlayer
	CMP AL, 1
	JE rollAgain ; "dice to roll" already set if this is an AI player
	
	; initialize "dice to roll" to be empty (all zeros) so we don't get bleed thru
	MOV AL, 0				; 0 clears all the bits
	MOV bDiceToRoll, al 	; clear all the bits in memory indicating no die to reroll
	
	INVOKE stringPrint, ADDR strRollPrompt2 ; ask user which dice to reroll

getDiceNumber:
	INVOKE charInput		; get dice # or <enter>  (# is 1-5)
	CMP EAX, 13				; user pressed <enter> meaning they are ready to roll
	JE rollAgain			; jump out of this area and start the roll
	CMP EAX, '5'			; '5' is the biggest single char we care about
	JG getDiceNumber		; ignore anything above '5'
	CMP EAX, 1				; '1' is the lowest single char we care about
	JL	getDiceNumber		; ignore anything  below '1'
	; at this point, EAX contains a # between '1' and '5' (inclusively), 
	; and ECX is our index into the array, 

	; now, toggle the bit representing the die selected and set/clear the marker
	
	SUB EAX, 30h			; convert single char to numberic equivalent (will be in AL)
	MOV DX, AX				; copy index into DL so we can use AL for math
	DEC AX					; to use as index, must start at zero, not 1
	MOV CL, 12
	MUL CL					; multiple AL by 12 to get the position of the die
	INC AX					; add 1 so we have a leading space
	MOV genCoord.X, AX
	MOV genCoord.Y, 27		; just below die display
	MOV AL, bDiceToRoll		; get the value currently in memory
	BTC AX, DX				; save the current bit state for this die in CF, then toggle it
	MOV bDiceToRoll, AL		; save it back to memory
	; now need to mark it or clear it on screen
	JC clearTheMarker ; if carry set then we need to clear the marker on the screen
	INVOKE SetConsoleCursorPosition, outHandle, genCoord  
	setPlayerColor
	INVOKE stringPrint, ADDR strMarker
	JMP getDiceNumber		; keep getting #s until the user hits <enter>
clearTheMarker:
	INVOKE SetConsoleCursorPosition, outHandle, genCoord  
	INVOKE stringPrint, ADDR strClearMarker
	JMP getDiceNumber		; keep getting #s until the user hits <enter>

alreadyUsed: ; this gets called from below if user picks a field already scored
	INVOKE stringPrint, ADDR strFieldAlreadyUsed
scoreTheRoll:	; now ask the user where to put the final roll
	; clear lines below the die
	MOV CX, 27		; set row to start printing blank lines
	MOV DX, 42		; set which row to stop printing on
	CALL clearLines

	; move cursor to just below the die
	MOV genCoord.X, 0
	MOV genCoord.Y, 28
	INVOKE SetConsoleCursorPosition, outHandle, genCoord  

	INVOKE stringPrint, ADDR strFieldPrompt ; ask user which field to put the roll in
	MOV AL, bAIPlayer			; no need to wait for a response if player is an AI
	CMP AL, 1
	JE fieldSelectedByAI
	INVOKE charInput						; get single char input in EAX
	JMP convertFieldToIndex
fieldSelectedByAI:
	CALL getFieldForAIScore ; returns field number (1-13) in EAX
convertFieldToIndex:
	SUB AL, 30h				; convert char to numeric equivalent
	JZ invalidFieldNumber	; zero is not a valid field number
	CMP AL, 9				; additional error checking if field # > 9
	JLE validFieldNumber	; so skip error checking if <= 9
	SUB AL, 11h 			; upper case starts at 41h, we already sub 30h so only sub 11h
	ADD AL, 10				; now shift so A = 10, B=11, etc
	CMP AL, 9				; if # is <= 9 then invalid (we already checked for <=9 above)
	JLE invalidFieldNumber
	CMP AL, 14				; if # is >= 14 then invalid, else is valid
	JL validFieldNumber
	SUB AL, 20h				; might be lower case
	CMP AL, 14				; if # is >= 14 then invalid, else is valid
	JL validFieldNumber
	
		; fall through to invalidFieldNumber

invalidFieldNumber:
	INVOKE stringPrint, ADDR strInvalidField
	JMP scoreTheRoll

validFieldNumber:
; check to make sure the selected field is blank before proceeding
; if not blank then print error message and jmp scoreTheRoll
	MOV ECX, EAX			; get copy of index (it's 1-13 at this point)
	DEC ECX					; so sub one so we can index into an array
	SHL ECX, 1				; multiply by 2 to since array elements are word sized
	CMP ECX, 14				; if for the upper level, no adjustment needed
	JL noAdjustmentToIndex
	ADD ECX, 6				; if for the lower index, skip the upper totals (3 of them)
noAdjustmentToIndex:
	ADD ECX, lpCurrentScores
	MOV DX, [ECX]	; get score for this field
	CMP DX, -1				; compare to -1 (which means no score since 0 is a valid score)
	JNE alreadyUsed			; if not zero then it's been used this game, get new field

; so, field value in AL is valid and the field hasn't been used yet. So score it.
	PUSH EAX					; preserve field index in AL
	CALL processDice			; process dice into tallies
	POP EAX						; restore field index
	CMP AL,7					; am I in upper or lower sections of the card
	JGE scoreLower
	CALL scoreUpperSection		; must be valid and in upper section
	JMP anotherTurn
scoreLower:
	CALL scoreLowerSection		; set carry flag if dice don't meet criteria for field
	JC scoreTheRoll				; score it again if field chosen doesn't work
anotherTurn:
	; advance to the next player
	MOV AL, bCurrentPlayer	; get current player`
	INC AL					; go to next one`
	CMP AL, bMaxPlayers		; compare to the last player
	JG cycleBackToFirst		; if we've advanced past the last player, cycleBackToFirst
	MOV bCurrentPlayer, AL	; else save new player as current
	JMP takeTurn			; and let them take a turn
	
; check total in upper and lower sections of last player, if -1 then not done, keep playing
cycleBackToFirst:
	MOV bCurrentPlayer, 1		; make player 1 the current player in case game not over
	MOVZX EDX, bMaxPlayers		; get last player # as an index
	DEC EDX						; adjust to start at zero`
	SHL EDX, 2					; multiply by 4 so we can index into dword array
	MOV EBX, lpPlayerScores[EDX]; get score array for last player
	MOV AX, [EBX+upperGrandOffset]			; get upper score total
	CMP AX, -1					; if -1 then it hasn't been totaled
	JE takeTurn					; and we can keep playing
	MOV AX, [EBX+lowerTotalOffset]      		; get lower section total 
	CMP AX, -1					; if -1 then it hasn't been totaled
	JE takeTurn					; and we can keep playing
	
; now, we add the yahtzee bonus and upper total and lower total to get grand total
; for each player, compute totals for top and bottom
	MOV EDX, 0					; index into each player
getTotalsForPlayer:
	MOV EBX, lpPlayerScores[EDX]; get score array for player
	MOV AX, [EBX+upperGrandOffset]			; get upper score total
	MOV CX, [EBX+lowerTotalOffset]      		; get lower section total 
	
	ADD AX, CX					; add these together
	MOV CX, [EBX+lowerBonuseOffset]		; Yahtzee bonus is -1 if not used 
	CMP CX, -1
	JE skipYahtzeeBonus
	ADD AX, CX					; add bonus to section totals
skipYahtzeeBonus:
	; now add score into grand total line
	MOV [EBX+grandTotalOffset], AX	; grand total goes in 6th total line ((6-1)*2)

	; continue computing by advancing to next player
	MOVZX EAX, bMaxPlayers
	DEC EAX
	MOV CL, 4		; used MUL instead of shifting just for the fun and experience
	MUL CL
	ADD EDX,4		; move to next player
	CMP EDX, EAX
	JLE getTotalsForPlayer
	
	CALL displayScoreCard	; refresh the display now that totals are computed

gameOver:
	INVOKE stringPrint, ADDR strPlayAgain
	INVOKE charInput			; get Y or N
	CMP EAX, 'Y'
	JE startGame
	CMP EAX, 'y'
	JE startGame
	CMP EAX, 'N'
	JE done
	CMP EAX, 'n'
	JE done
	
	INVOKE stringPrint, ADDR strInvalidYN
	JMP gameOver
	
errorExit:	
 	INVOKE stringPrint, ADDR strLF             ; advance to a new line
done:	
	INVOKE SetConsoleTextAttribute, outHandle, wDefaultAttr		; 15 for white on black, 
	INVOKE ExitProcess, 0	;tell the OS everything terminating normally
PUBLIC _start				;so linker knows the entry point for the program
main ENDP




COMMENT %
******************************************************************************************
* Proc Name:	displayScores 
* Programmer:   William Kinser
* Purpose:	private proc that iterates across all the players, converts each score in 
*			their scorecard to a string and displays it on the screen (right justified).   
*
* Date created: 04 15  2020
* Date last modified: 04 16 2020 
*
* @param 
; PRECONDITIONS: none
; POSTCONDITIONS: none
* @return  
******************************************************************************************
%
displayScores PROC C PRIVATE
LOCAL color:WORD
; iterate across all players
	MOV ECX, 0		; set counter for player number
computeColumnToDisplayOn:
	PUSHAD				; save off all the registers since we call system procedures
	SHL ECX, 1			; convert counter into an index for a word array
	MOV CX, wPlayerColors[ECX]  ; get color for this player
	MOV color, CX				; store locally so we don't have to look it up again
	POPAD
	MOV AX, 12				; calculate the column for this player
	MUL CL					; Player # * 8
	ADD AX, 24				; then add 24 (for the scorecard already printed)
	MOV genCoord.X, AX 		; set column to print to
	MOV genCoord.Y, 0		; first row for player names
	
; for each player
	PUSH ECX				; preserve counter
	SHL ECX, 2				; convert counter into an index for a dword array
	MOV EBX, lpPlayerNames[ECX]
	PUSHAD					; preserve all since these procs stomp on things
	INVOKE SetConsoleCursorPosition, outHandle, genCoord  
	INVOKE SetConsoleTextAttribute, outHandle, color ; change color of text	
	INVOKE stringPrint, EBX
	POPAD					; restore all registers
	MOV EBX,  lpPlayerScores[ECX]
	POP ECX					; restore the counter now that we're done using as an index
	MOV EDX,0				; iterate across upper scores
convertScoreToString:
	INC genCoord.Y
    MOV AX, [EBX+EDX] 		; get score at this position
	CMP AX,-1				; if -1, no score so nothing to display
	JE noScore
	MOVZX EAX, AX
	INVOKE itoasc32, ADDR strInput1,EAX ; convert value of EAX into a string
	INVOKE stringLength, ADDR strInput1 ; returns length of numeric string into EAX
	INVOKE stringCopy, ADDR strBlankValue, ADDR strInput2; fill strInput2 with 4 blanks
	; right justify string representation of score
	NEG EAX					; turn length into a negative...
	ADD EAX, 5				; and add 4 this gives us how far into... 
	ADD EAX, OFFSET strInput2 ; strInput2 to place value held in strInput1
	INVOKE stringCopy, ADDR strInput1, EAX ; copy numeric string into substring of EAX
	PUSHAD
	INVOKE SetConsoleCursorPosition, outHandle, genCoord  
	INVOKE SetConsoleTextAttribute, outHandle, color ; change color of text	
	INVOKE stringPrint, ADDR strInput2	
	POPAD
noScore:
	INC EDX			; advance to next score...
	INC EDX			; which is a word so add 2
	CMP EDX, 36		; see if we are at end of scores for this player
	JLE convertScoreToString
	INC ECX 		; go to next player
	CMP CL, bMaxPlayers ; see if we reached the last player
	JL  computeColumnToDisplayOn
	RET
displayScores ENDP


COMMENT %
******************************************************************************************
* Proc Name:	drawDice(lpString:PTR BYTE, dValues:PTR DWORD, dNumDice:DWORD):void 
* Programmer:   William Kinser
* Purpose:	Format string passed in based on array of dice rolls.   
*
* Date created: 04 08  2020
* Date last modified: 04 08 2020 
*
* @param lpString:PTR BYTE, dValues:PTR DWORD, dNumDice:DWORD
* @return  
******************************************************************************************
%
drawDice PROC C

; prologue to preserve stack and find parameters
	PUSH EBP
	MOV EBP, ESP
	
lpString        EQU [EBP+8]		; this is address of the string to format
dValues 		EQU [EBP+12]	; this is address of the array of dword values
dNumDice		EQU [EBP+16]	; this is the number of dice in dValues array

; prologue continued, save locally used registers
	PUSH EAX
	PUSH EBX
	PUSH ECX
	PUSH EDX

; make room on stack for local variables
	SUB ESP, 8 ; (PUSHAD = 4*4 = 16; +8 for dCount =24)
dCount EQU [EBP-24] ; current die being drawn; is a DWORD because dNumDice is a DWORD

; using these labels allows me to more easily change the look and feel
bSpace EQU 20h 			; empty space on face of die 
wSpace EQU 2020h 		; 2 empty spaces
dSpace EQU 20202020h 	; 4 empty spaces
bDot   EQU 4fh 			; the dot to show face value of die
	
bTop    EQU 0DCh ; Top side of die
bLeft   EQU 0DDh ; Left side of die
bRight  EQU 0DEh ; Right side of die
bBottom EQU 0DFh ; Bottom side of die
	
bCF 	EQU 13 ; carriage return
bLF     EQU 10 ; Line Feed

;each die is 12 chars wide * 7 high * 2 die + 2*7 CF/LF
;NOTE on design: I took a line by line approach. So I draw the top line of 
; each die in turn. At the end of each line, I place CF/LF to go to next line
; on the display. 
; There are only 3 lines that depend on the die values (first, middle, third).
; And the third line is just the reverse of the the first line, or a mirror images
; (since I show a six as two horizontal lines): reference images below
;  _________   _________   _________   _________   _________   _________ 
; |         | |         | |         | |         | |         | |         | 
; |  * * *  | |  *   *  | |  *   *  | |  *      | |  *      | |         | 
; |         | |    *    | |         | |    *    | |         | |    *    | 
; |  * * *  | |  *   *  | |  *   *  | |      *  | |      *  | |         | 
; |_________| |_________| |_________| |_________| |_________| |_________| 
;
; Notice that only odd numbered values have a dot in the middle row and there is 
; only one dot there ever.
; Also, all even numbered values have a dot in the upper left corner
; These observations dictate the logic  below
  
; draw top of all the die
	LEA EAX, dCount 		; load EAX with address of dCount
	MOV DWORD PTR [EAX], 0 	; initialize to zero for first die
	MOV EBX, lpString 		; get string address
top2:
	MOV BYTE PTR [EBX], bSpace ; all die faces start with a space
	MOV ECX, 11				   ; top line has 11 bars in a row
top:
	INC EBX					   ; advance to next char position
	MOV BYTE PTR [EBX], bTop   ; place a bar at this position
	LOOP top				   ; loop back until done

	INC DWORD PTR [EAX]		; increment the number of die drawn`
	INC EBX					; advance to the next char position
	MOV ECX, [EAX]			; get the count of die built
	CMP ECX, [dNumDice]		; compare to the number to be built`
	JNE top2			    ; keep going if more to be built0

; end this line of drawing	
	CALL endCurrentLine 	; places a CF/LF at end of this line
	
	CALL drawBlankLineOnDice ; draws a blank line face of each die
	
; end this line of drawing	
	CALL endCurrentLine		 ; ends the blank line with a CF/LF
	
; draw first line of dots
	MOV DWORD PTR [EAX], 0 	; initialize to zero for first die
	MOV EDX, dValues 		; get values for die
	; THIS PUSH IS IMPORTANT TO REMEMBER FOR LATER (needed for mirror image on third line)
	PUSH EBX  				; remember start of first line, we'll use it in the third line
firstLine:
	MOV ECX, [EDX] 			; get value for this die
	CMP ECX, 1     			; all values > 1 have a dot in top left corner of first row
	JNE firstLine2
	CALL drawBlankLineOnDie ; just on this die
	JMP nextDieFirstLine
firstLine2:
	MOV BYTE PTR [EBX], bSpace ; each line starts with a space
	INC EBX					   ; advance to next char position
	MOV BYTE PTR [EBX], bLeft  ; draw vertical bar on left side of face
	INC EBX					   ; advance to next char position
	MOV BYTE PTR [EBX], bSpace ; draw a space at this position
	INC EBX					   ; advance to next char position
	MOV BYTE PTR [EBX], bSpace ; draw a space at this position
	INC EBX					   ; advance to next char position
	; already took care of case where die equals 1, all others get a dot in the top left
	MOV BYTE PTR [EBX], bDot   ; put a dot in this position
	INC EBX
	MOV BYTE PTR [EBX], bSpace ; draw a space at this position
	INC EBX					   ; advance to next char position
	; now, middle dot on first line is only present for value of 6
	CMP ECX, 6				   ; is this a six
	JNE not6OnFirst
	MOV BYTE PTR [EBX], bDot   ; put a dot in this position
	INC EBX					   ; advance to next char position
	JMP contFirst1
not6OnFirst:
	MOV BYTE PTR [EBX], bSpace ; draw a space at this position
	INC EBX					   ; advance to next char position
	
contFirst1:
	MOV BYTE PTR [EBX], bSpace ; draw a space at this position
	INC EBX					   ; advance to next char position
	
	;now, top right is only set if die value is > = 4
	CMP ECX, 4					; is it less than 4?
	JL noTROnFirst
	MOV BYTE PTR [EBX], bDot   ; put a dot in this position
	INC EBX					   ; advance to next char position
	JMP finishFirstLine
noTROnFirst:
	MOV BYTE PTR [EBX], bSpace ; draw a space at this position
	INC EBX					   ; advance to next char position
finishFirstLine:
	MOV BYTE PTR [EBX], bSpace ; draw a space at this position
	INC EBX					   ; advance to next char position

	MOV BYTE PTR [EBX], bSpace ; draw a space at this position
	INC EBX					   ; advance to next char position
	MOV BYTE PTR [EBX], bRight ; put a vertical bar on the right side of the face

nextDieFirstLine:
	INC DWORD PTR [EAX]			; increment the number of die drawn`
	INC EBX						; advance to next char position
	ADD EDX, 4 					; advance to next value in the array
	MOV ECX, [EAX]				; get the count of die built
	CMP ECX, [dNumDice]			; compare to the number to be built`
	JNE firstLine	
	

; end this line of drawing	
	CALL endCurrentLine			; places a CF/LF at end of the line

; now do the middle line, only has a dot if value is odd (and only in the middle)
	MOV DWORD PTR [EAX], 0 		; initialize to zero for first die
	MOV EDX, dValues 			; get values for die
middleLine: 					; at this point we know it's odd
	MOV ECX, [EDX] 				; get value for this die
	AND ECX, 1     				; if even # just draw a blank line and move on
	JNZ middleLine2
	CALL drawBlankLineOnDie 	; just on this die
	JMP nextDieMiddleLine
middleLine2:
	MOV BYTE PTR [EBX], bSpace ; each line starts with a space
	INC EBX					   ; advance to next char position
	MOV BYTE PTR [EBX], bLeft  ; draw vertical bar on left side of face
	INC EBX					   ; advance to next char position
	MOV DWORD PTR [EBX], dSpace; put 4 spaces in a row here
	ADD EBX, 4					; advance forward 4 positions
	MOV BYTE PTR [EBX], bDot   ; draw a dot at this position (center of middle line)
	INC EBX					   ; advance to next char position
	MOV DWORD PTR [EBX], dSpace; put 4 spaces in a row here
	ADD EBX, 4				   ; advance forward 4 positions

	MOV BYTE PTR [EBX], bRight ; put a vertical bar on right side of face

nextDieMiddleLine:
	INC DWORD PTR [EAX]			; increment count of how many die drawn
	INC EBX					    ; advance to next char position
	ADD EDX, 4 					; advance to next value in the array
	MOV ECX, [EAX]				; get count of die drawn
	CMP ECX, [dNumDice]			; compare to how many to draw
	JNE middleLine				; jump back if more to do
	

; end this line of drawing	
	CALL endCurrentLine			; place a CF/LF at end of this line

; now do the third line which is just the inverse face of each die's first line
	MOV DWORD PTR [EAX], 0 ; initialize to zero for first die
	POP EDX ; we pushed EBX at the start of the first line, now we pull that as EDX
	; so, EBX is the start of the third line, EDX is the start of the first line
thirdLine:
	MOV BYTE PTR [EBX], bSpace  ; each line starts with a space
	INC EBX					    ; advance to next char position
	MOV BYTE PTR [EBX], bLeft   ; draw vertical bar on left side of face
	INC EBX					    ; advance to next char position
	MOV BYTE PTR [EBX], bSpace  ; draw a space at this position
	INC EBX					    ; advance to next char position
	MOV BYTE PTR [EBX], bSpace  ; draw a space at this position
	INC EBX					    ; advance to next char position
	ADD EDX,8 					; move EDX to end of the first line for this die
	MOV CL,5					; do mirror of next 5 chars
reverseFace:
	MOV CH, [EDX] 				; get char on first line (pulling in reverse order)
	MOV [EBX], CH 				; put it on the third line (placing in proper order)
	DEC CL						; count down how many chars to mirror
	DEC EDX						; move position on first line back one
	INC EBX						; move position on third line forward one
	CMP CL, 0					; see if we're done
	JNE reverseFace				; if not, copy another char
	
	MOV WORD PTR [EBX], wSpace  ; draw 2 spaces 
	ADD EBX, 2					; advance forward 2 positions
	MOV BYTE PTR [EBX], bRight  ; draw vertical bar on right side of face
	ADD EDX, 9 					; move position for first line to next die

; advance to the next die
	INC DWORD PTR [EAX]			; increment how many die have been drawn
	INC EBX					    ; advance to next char position
	MOV ECX, [EAX]				; get count of how many die drawn
	CMP ECX, [dNumDice]			; compare to how many to be drawn
	JNE thirdLine				; loop back if more to draw for this line

; end this line of drawing	
	CALL endCurrentLine   		; place a CF/LF at end of this line

	CALL drawBlankLineOnDice    ; draw a blank line for face of each die to be drawn

; end this line of drawing	
	CALL endCurrentLine			; place a CF/LF at end of the blank line

; draw final row
	MOV DWORD PTR [EAX], 0 		; initialize to zero for first die
bottom2:
	MOV BYTE PTR [EBX], bSpace  ; each line starts with a space
	MOV ECX, 11					; final line has 11 horizontal bars
bottom:
	INC EBX					    ; advance to next char position
	MOV BYTE PTR [EBX], bBottom ; draw horizontal line along bottom
	LOOP bottom

; advance to the next die
	INC DWORD PTR [EAX]			; increment how many die have been drawn
	INC EBX					    ; advance to next char position
	MOV ECX, [EAX]				; get count of how many die drawn
	CMP ECX, [dNumDice]			; compare to how many to be drawn
	JNE bottom2					; loop back if more to draw for this line

; end this line of drawing	
	CALL endCurrentLine			; place CF/LF at end of this line

	
	MOV BYTE PTR [EBX], 0 ; null terminator
	
	ADD ESP, 8 ; remove locals from stack

	; prologue
	POP EDX
	POP ECX
	POP EBX
	POP EAX
	
	POP EBP 
	
	RET
	
drawDice ENDP

COMMENT %
******************************************************************************************
* Proc Name:	endCurrentLine():void 
* Programmer:   William Kinser
* Purpose:	basically an inlined method to place CF/LF at end of a line. could be a macro   
*
* Date created: 04 08  2020
* Date last modified: 04 08 2020 
*
* @param assumes EBX points to the string being built
* @return  
******************************************************************************************
%
endCurrentLine PROC C PRIVATE
; end this line of drawing	
	MOV BYTE PTR [EBX], bCF		; places carriage return at current position
	INC EBX						; advances to next char position
	MOV BYTE PTR [EBX], bLF
	INC EBX						; advances to next char position
	RET
endCurrentLine ENDP

COMMENT %
******************************************************************************************
* Proc Name:	drawBlankLineOnDice():void 
* Programmer:   William Kinser
* Purpose:	basically an inlined method to draw blank lines on face of all the 
* 			die to be drawn. could be a macro   
*
* Date created: 04 08  2020
* Date last modified: 04 08 2020 
*
* @param assumes EBX points to the string being built, EAX points to count of 
* how many die have been drawn
* @return  
******************************************************************************************
%
drawBlankLineOnDice PROC C PRIVATE

; draw blank line
	MOV DWORD PTR [EAX], 0 ; initialize to zero for first die
blank2:
	call drawBlankLineOnDie 	; draw a blank line for face of this die
	
	INC DWORD PTR [EAX]			; increment count of how many die drawn
	INC EBX						; advances to next char position
	MOV ECX, [EAX]				; get count`
	CMP ECX, [dNumDice]			; compare to how many die to be drawn
	JNE blank2					; loop back if more to draw

	RET
drawBlankLineOnDice ENDP

COMMENT %
******************************************************************************************
* Proc Name:	drawBlankLineOnDie():void 
* Programmer:   William Kinser
* Purpose:	basically an inlined method to draw blank lines on face of current die.
*          could be a macro   
*
* Date created: 04 08  2020
* Date last modified: 04 08 2020 
*
* @param assumes EBX points to the string being built
* @return  
******************************************************************************************
%
drawBlankLineOnDie PROC C PRIVATE
	MOV BYTE PTR [EBX], bSpace ; each line starts with a space
	INC EBX						; advances to next char position
	MOV BYTE PTR [EBX], bLeft  ; draw vertical bar on left side of face
	MOV ECX, 10					; need 10 spaces
blank:
	INC EBX						; advances to next char position
	MOV BYTE PTR [EBX], bSpace ; each line starts with a space
	LOOP blank

	MOV BYTE PTR [EBX], bRight  ; draw vertical bar on right side of face
	RET
drawBlankLineOnDie ENDP

COMMENT %
******************************************************************************************
* Proc Name:	rollDie():void 
* Programmer:   William Kinser
* Purpose:	basically an inlined method to roll a die, store in [EBX]
*
*          could be a macro   
*
* Date created: 04 08  2020
* Date last modified: 04 08 2020 
*
* @param EBX points to location to store the DWORD value (range 1-6)
* @return  
******************************************************************************************
%
rollDie PROC C PRIVATE
	CALL next ; get randomNum into EAX
C1:
	MOV ECX,7 					; create mask for 3 bits (min needed for values 1-6)
	SHR EAX,1					; shift EAX value 1 (doing it here to simplify looping)
	AND ECX, EAX				; apply mask; gives us a value of 0-7 inclusive
	CMP ECX, 7					; see if result > 6
	JE C1						; if so, then shift again
	CMP ECX, 0					; see if result < 1
	JE C1						; if so, then shift again
	MOV [EBX], ECX			 	; store number (1-6)in array
	RET
rollDie ENDP



COMMENT %
******************************************************************************************
* Proc Name:	clearScoreCard 
* Programmer:   William Kinser
* Purpose:	clears all the values in the score card fields
*
* Date created: 04 16  2020
* Date last modified: 04 17 2020 
*
* @param 
* @return  
******************************************************************************************
%
clearScoreCard PROC C PRIVATE
	MOV ECX, 0			;  loop counter 
nextScoreField:
	MOV sScores[ECX], -1 ; mark all the fields as having no score (zero is a valid score)
	MOV sScores2[ECX], -1;
	MOV sScores3[ECX], -1;
	MOV sScores4[ECX], -1;
	INC ECX ;advance index to next word
	INC ECX
	CMP ECX, 38		; keep going until card is full
	JL nextScoreField
	MOV ECX, 1		; reset these to represent the start of a new game
	MOV bCurrentPlayer, CL
	MOV bMaxPlayers, CL	
	RET
clearScoreCard ENDP


COMMENT %
******************************************************************************************
* Proc Name:	addValue 
* Programmer:   William Kinser
* Purpose:	adds the string value in EBX into the score field indexed by ECX 
*			(inserts it at the end)
*
* Date created: 04 16  2020
* Date last modified: 04 17 2020 
*
* @param PRECONDITIONS: uses ECX to offset into the lpScoreCard to add value in EBX
* @return  
******************************************************************************************
%
addValue PROC C PRIVATE

	INVOKE stringLength, EBX 	; make sure value string pointed to by EBX is <= 4 chars
	CMP EAX, 5 					; 4 chars plus null
	JG doneAddValue
	MOV EDX, EAX				; save value length in EDX
	MOV EAX, lpScoreCard[ECX]	; get address of field (storing in register for debugging)
	INVOKE stringLength, EAX 	; now EAX has length of field string in it
	SUB EAX, EDX				; count backwards from field end so we can add value
	ADD EAX, lpScoreCard[ECX]	; Index into field string by field.length - value.length
	INVOKE stringCopy, EBX, EAX 	; copy value string over end of field string
doneAddValue:
	RET
addValue ENDP

COMMENT %
******************************************************************************************
* Proc Name:	rollDice 
* Programmer:   William Kinser
* Purpose:	roll the dice specified in bDiceToRoll, store in lpDice
*
* Date created: 04 16  2020
* Date last modified: 04 17 2020 
*
* @param 
* @return  
******************************************************************************************
%
rollDice PROC C PRIVATE
	PUSHAD
; set bit mask to bit #1
	MOV DL, 2		; set bit #1 to true, all others to false
; set index to 0 for lpDice (where the values of each die is stored)
	MOV ECX, 0		; use ECX as index into lpDice

checkIfDieIsToBeRolled:
; if die at bit mask location is set then roll that die
	MOV DH, bDiceToRoll	; get value from memory
	AND DH, DL			; apply mask to value
	CMP DH, 0			; if not set then skip rolling the die
	JE findNextDieToRoll

; and save value in lpDice
	MOV EBX, OFFSET lpDice	; move array address into EBX
	ADD EBX, ECX	; offset EBX (array address) by the computed index	
	PUSH ECX
	PUSH EDX
	CALL rollDie	; expect EBX to point to location to store dword value for die
	POP EDX 
	POP ECX
	
findNextDieToRoll:	
; increment index into lpDice
	ADD ECX, 4		; increase index of which die to roll
; shift bit mask to the left 1 position
	SHL DL,1		; shift mask left 
; loop back
	CMP DL, 40h		; if the 6th bit is set in mask then we are done (5 is the max)
	JNE checkIfDieIsToBeRolled
	POPAD
	RET
rollDice ENDP

COMMENT %
******************************************************************************************
* Proc Name:	clearScreen
* Programmer:   William Kinser
* Purpose:	clear screen of previous display and sets cursor at top left corner   
*
* Date created: 04 18  2020
* Date last modified: 04 18 2020 
*
* @param 
* @return  
******************************************************************************************
%
clearScreen PROC C PRIVATE 
; print blanks lines at each row in the display
	MOV CX, 0    	; make the word sized row zero
	MOV DX, 42H		; # of lines to clear
	CALL clearLines ; this clears the lines 0-42 and puts cursor at 0,0
	INVOKE SetConsoleTextAttribute, outHandle, 07; change color of text to white on black
	RET
clearScreen ENDP


COMMENT %
******************************************************************************************
* Proc Name:	clearLines
* Programmer:   William Kinser
* Purpose:	clear lines in the range specified by CX & DX, and puts cursor at 0,CX  
*
* Date created: 04 18  2020
* Date last modified: 04 18 2020 
*
* @param 
* PRECONDITIONS: CX has starting row, DX has the end row #
* POSTCONDITION: cursor is placed at 0,CX when done
* @return  
******************************************************************************************
%
clearLines PROC C PRIVATE
	MOV genCoord.X, 0 ; set column to zero
	PUSH ECX 			; save starting row
clearAnotherLine:
	MOV genCoord.Y, CX ; set row to CX 
	PUSHAD
	; print a really long string of blanks at the start of the line
	INVOKE WriteConsoleOutputCharacterA, outHandle, ADDR strBlankLine, 
					SIZEOF strBlankLine,
					genCoord, ADDR dCellsWritten
	POPAD
	INC CX			; go to next row/line
	CMP CX,DX		;	the answer to the ultimate question should work for this
	JNE clearAnotherLine
	POP ECX
	MOV genCoord.Y, CX
; set cursor to the place we starting printing blank lines
	INVOKE SetConsoleCursorPosition, outHandle, genCoord  

	RET
clearLines ENDP



COMMENT %
******************************************************************************************
* Proc Name:	displayScoreCard 
* Programmer:   William Kinser
* Purpose:	Clears the screen, displays all the score card strings (including values)
* 			and sets the text colors accordingly.   
*
* Date created: 04 15  2020
* Date last modified: 04 16 2020 
*
* @param 
* PRECONDITIONS: lpScoreCard is populated with current field values
* @return  
* POSTCONDITION: 
******************************************************************************************
%
displayScoreCard PROC C PRIVATE

; set cursor at top left corner of screen
	CALL clearScreen
	INVOKE stringPrint,  ADDR strLF 

	MOV ECX, 19 	; 19 lines in card (6 + 3 on upper, 7 + 3 on lower)
	MOV EDX, 0		; index into the array of card fields
printCardField:
	PUSH ECX		; preserve register before calling window procs
	PUSH EDX		; preserve register before calling window procs
	CMP ECX, 14		; Pick colors of text based on rows being displayed
	JGE defaultColor
	CMP ECX, 11
	JGE highlightedColor
	CMP ECX, 3
	JLE highlightedColor
defaultColor:
	INVOKE SetConsoleTextAttribute, outHandle, wDefaultAttr		; 15 for white on black, 
	JMP displayLine
highlightedColor:
	INVOKE SetConsoleTextAttribute, outHandle, CX		
displayLine:
	POP EDX 		; restore register so we can advance to next row properly
	POP ECX 		; restore register so we can advance to next row properly
	
	INVOKE stringPrint,  lpScoreCard[EDX]  ; print the label for this row
	ADD EDX, 4		; advance index into scorecard's next row
	loop printCardField
	CALL displayScores ; labels printed, now print all the scores for each player
	INVOKE SetConsoleTextAttribute, outHandle, wDefaultAttr		; 15 for white on black, 
	INVOKE stringPrint, ADDR strLF ; print blank line
	RET
displayScoreCard ENDP


COMMENT %
******************************************************************************************
* Proc Name:	processDice 
* Programmer:   William Kinser
* Purpose:	Looks at the current values in the 5 dice and computes the tallies for each
*			possible field. These tallies are used later to determine if the field the
*			user selects has its criteria met or not.   
*
* Date created: 04 15  2020
* Date last modified: 04 16 2020 
*
* @param 
* PRECONDITIONS: lpDice is populated with current dice roll values
* @return  
* POSTCONDITION: all the Tallies are computed (starting at bStartOfTallies)
******************************************************************************************
%
processDice PROC C PRIVATE 
; clear all tallies
	MOV ECX, 16						; number of byte sized tallies
	MOV EAX, OFFSET bStartOfTallies ; get address of start of tallies
clearTallies:
	MOV BYTE PTR [EAX], 0			; clear the tally 
	INC EAX							; move to next tally
	loop clearTallies   			; keep going till all clear
; iterate through the dice	
	MOV ECX, 5 				; use as index into dice values
nextDiceToProcess:
	; get value of die
	MOV EDX, ECX			; take counter
	DEC EDX					; decrement by 1
	SHL EDX,2				; and multiply by 4 to get index into array of dice values
	MOV EBX, lpDice[EDX] 	; get dice value based on counter
	; increment count of # equal to value of the die by indexing into byte array
	INC bStartOfTallies[EBX]
	loop nextDiceToProcess
	
; clear flag to indicate how many die #s have 2 or more (pairs)
	MOV bPairFlag, CH ; AH is always zero at this point because EAX is was index up to 5
; iterate through byte array of values 
	MOV ECX, 6 ; set counter to # of dice face values available
nextTallyToProcess:
	; if any have a count of 3 or greater, save that # in the 3ofKind tally
	MOV EBX, OFFSET bStartOfTallies	; get address of first face tally-1 
	ADD EBX, ECX			; increment EBX by counter (going backwards is ok)
	MOV DL, [EBX]			; get tally for this die face #
	MOV AL, DL				; copy tally into multiplacand
	MUL CL					; multiply by die face #
	ADD bRunningTotal, AL	; add to running total
	CMP DL, 2				; do we have a pair of these?
	JL keepTallying
	INC bPairFlag			; increment # of pairs we've found
keepTallying:
	CMP DL, 3				; do we have at least 3 of these
	JL doneWithThisTally
	MOV b3KindTally, CL	; this is the # that we have at least 3 of a kind with
		; if that same # has a count of 4 or greater, save that # in the 4ofKind tally
	CMP DL, 4
	JL doneWithThisTally
	MOV b4KindTally, CL ; this is the # we have at least 4 of a kind with
			; if that same # has a count of 5 then save in Yahtzee & full house tallies
	CMP DL, 5
	JL doneWithThisTally
	MOV b5KindTally, CL  ; this is the # that we have a yahtzee with
doneWithThisTally:
	loop nextTallyToProcess

; if flag showing pairs is <= 1 then set small straight tally
	MOV DL, b3KindTally ; since a 3 of a kind shows as a pair too, check for this case
	CMP DL, 1
	JGE almostDoneProcessingDice ; if there's 3+ of akind, there CAN NOT be a straight
	MOV DL, bPairFlag	; get count of how many # have 2 or more matches
	CMP DL, 1
	JG almostDoneProcessingDice
	
; at this point, there is only zero or one pairs, other tallies are either 0 or 1.
; that means that b3KindTally is zero at this point so we can use that as a terminator.
; so we need to check for a gap in the sequence 
; (ie  1 2 3 4 * 6, 1 * 3 4 5 6, or * 2 3 4 5 ? (?>=2), or ? 2 3 4 5 * (?<=5) 
; COULD be a series like 123*56 (no pairs but no straight) or
; a series like 1 2 2 * 4 5 *!
	; DL still holds the number of pair so jump if one pair
	CMP DL, 1
	JE onePairIsItAStraight
	; knowing there are NO pairs, means we have a small straight if there is a zero
	; at either the #2 or #5 die (making either the 1 or the 6 an outlyer)	
	; OR we have a large if there is a zero at the #1 or the #6 die

	MOV AL, bFaceTally+1 ; it's a 1 if there are no 2s 
	CMP AL, 0
	JE itsASmallStraight
	MOV AL, bFaceTally+4 ; it's a 6 if there are no 5s 
	CMP AL, 0
	JE itsASmallStraight
	MOV AL, bFaceTally   ; it's a large starting at 2 if no 1s
	CMP AL, 0
	JE itsALargeStraight
	MOV AL, bFaceTally+5   ; it's a large starting at 1 if no 6s
	CMP AL, 0
	JE itsALargeStraight
	; anything else is not a straight
	JMP almostDoneProcessingDice

itsALargeStraight:
	MOV bLarge, 1 ; if its a large, it's also a small so fall thru to next line
itsASmallStraight:
	MOV bSmall, 1
	JMP almostDoneProcessingDice

onePairIsItAStraight:
	; so, we know there are no 3s of a kind, and we have 1 and only 1 pair, which means
	; there's a straight if die following these patterns:
	; SMALL: **3456 1234** *2345*
	; LARGE: NONE (no way to have a large if you have a pair)
	MOV DL, bFaceTally
	ADD DL, bFaceTally+1
	CMP DL, 0
	JE itsASmallStraight
	MOV DL, bFaceTally
	ADD DL, bFaceTally+5
	CMP DL, 0
	JE itsASmallStraight
	MOV DL, bFaceTally+4
	ADD DL, bFaceTally+5
	CMP DL, 0
	JE itsASmallStraight
	
	; now, do the chance and full house fields`
almostDoneProcessingDice:
	MOV AL, 1				; by definition, Chance is always set to true`
	MOV bChanceTally, AL	; save tally`
	MOV AL, b5KindTally		; if 5 of a kind is true then so is full house
	CMP AL, 0				; 
	JG isAFullHouse			; yes, full house
	MOV AL, b3KindTally		; to determine if full house is valid, need one 3 of a kind
	CMP AL, 0				; if no 3 of kind,... 
	JE notFullHouse			; no full house
	MOV AL, bPairFlag		; also need at least 1 pair in addition to the 3 of a kind
	CMP AL, 2				; the 3 of a kind counts as a pair so we need 2 pairs
	JNE notFullHouse		; if not exactly 2 pair plus a 3 of a kind, then no full house
isAFullHouse:
	MOV AL, 1
	MOV bFullTally, AL		; full house criteria met
notFullHouse:
	; ok. so, at this point, we've tallied all the fields based on the dice roll. Now
	; we have to evaluate the Joker rule...
	; If this roll is a yahtzee (b5KindTally is >= 1) AND 
	; the Yahtzee field (C) is filled (either 0 or 50) AND
	; the upper section field corresponding to this Yahtzee roll has been filled/used
	; THEN the JOKE RULE is in effect and the Full House, Small Straight and Large
	; Straight can be scored even though the criteria is not met by the dice.
	MOVZX EDX, b5KindTally ; was this roll a yahtzee ? 
	CMP EDX, 1			 ; the tally value = the # on the dice face for the yahtzee
	JL noJoker
	
	; get start of scores for the current player so we can see what they have already
	MOV EAX, lpCurrentScores ; get scorecard for this player
	
	DEC EDX				; use the tally as an index so make it start at 0 instead of 1
	SHL EDX, 1			; multiple by 2 since we are indexing into a word array
	MOV CX, [EAX+EDX]   ; get score for the upper section corresponding to this yahtzee
	CMP CX, -1			; -1 indicates this field hasn't been used or scored yet
	JE noJoker			; if not used then joker rule NOT in effect
	MOV CX, [EAX+yahtzeeOffset]   ; get score for yahtzee field
	CMP CX, 50			; must be scored and must be scored as 50
	JNE noJoker			; Joker rule not in effect
	; at this point, all three criteria are met for the Joker rule
	MOV AL, 1
	MOV bFullTally, AL	; make it possible for user to score this turn as a full house
	MOV bSmall, AL		; make it possible for user to score this turn as a small straight
	MOV bLarge, AL      ; make it possible for user to score this turn as a large straight	

noJoker:
	RET
processDice ENDP


COMMENT %
******************************************************************************************
* Proc Name:	scoreUpperSection 
* Programmer:   William Kinser
* Purpose:	Uses the statistics in the Tally variables to calculate and validate the 
*           score for the specified field in the upper section of the score card. 
*			If the upper section is full, it calculates the total for that section.   
*
* Date created: 04 15  2020
* Date last modified: 04 16 2020 
*
* @param 
; PRECONDITIONS: AL contain index into the field (1-6) and tallies are calculated
* @return  
******************************************************************************************
%
scoreUpperSection PROC C PRIVATE
; calculate score by summing all the die with the same face value as the index
	; get start of scores for the current player so we can see what they have already
	MOV EBX, lpCurrentScores ; get scorecard for this player	

	MOVZX ECX, AL					; convert index to DWORD in size
	MOV EBX, OFFSET bStartOfTallies	; start just before first tally since index starts @ 1
	ADD EBX, ECX					; add index to address to find correct face tally
	MOV AH, [EBX]					; get tally
	MUL AH							; multiply tally by dice # to get score
	MOVZX EAX, AL					; convert score into DWORD

	PUSH ECX						; preserve dword index 
	DEC ECX							; since index is 1-6, need to sub 1 to index into array
	SHL ECX,1						; multiply by 2 since array elements are word sized
	ADD ECX, lpCurrentScores
	MOV [ECX],AX				; store score in array of scores
	POP ECX							; restore dword index

	; now that the score is recorded correctly (above), check to see if this was a 
	; yahtzee role and whether a bonus is applicable: 
	; If a Yahtzee was already scored with 50 points add 100 points to yahtzee bonus
	MOV DL, b5KindTally ; find out if this roll is a Yahtzee or not
	CMP DL, 1			; any number >= 1 indicates a yahtzee was rolled with that #
	JL noUpperYahtzee  ; skip over the bonus logic and continue processing upper roll
	MOV ECX, 28		   ; offset into score array for Yahtzee 
	ADD ECX, lpCurrentScores   ; add to current score array
	MOV DX, [ECX]; get current score for the Yahtzee field
	CMP DX, 50			; has it been scored yet?
	JNE noUpperYahtzee  ; if not scored as 50, then we don't get a bonus
	; so at this point we get the Yahtzee bonus
	MOV ECX, 32		   ; offset to total score array is word size to Yahtzee bonus line
	ADD ECX, lpCurrentScores
	MOV DX, [ECX]; get current score for the Yahtzee BONUS field
	CMP DX, -1			; see if a bonus has already been scored
	JNE add100			; if so, just keep the value
	INC DX				; if not, then add 1 since -1 represents "not scored"
add100:
	ADD DX, 100			; add another 100 to the bonus
	MOV [ECX], DX ; save the bonus back into the score array 
	
noUpperYahtzee:

	; now check to see if all 6 fields are scored, if so, fill in the totals
	MOV ECX, 6		; there are 6 fields in the upper section to check
	MOV EDX, lpCurrentScores; get address of array of scores
	MOV EBX, 0		; use BX for running total 
checkUpper:
	MOV AX, [EDX] ; get score for this field
	CMP AX, -1		 ; if score for this field is < 0, we're done (-1 means no score)
	JE doneScoringUpper
	ADD BX, AX		; add to running total
	INC EDX			; since values are stored as a word...
	INC EDX			; advance 2 times
	LOOP checkUpper

	; can only get here if all 6 fields are scored so do totals
	MOV ECX, lpCurrentScores
	ADD ECX, upperTotalOffset ; 
	MOV [ECX], BX	; upper total goes in first total line
	ADD ECX, 4		; skip 1 line, go to next
	MOV [ECX], BX  ; save total in upper section total too in case no bonus
	MOV AX, 0				; initialize with zero (no bonus) as the default
	SUB ECX, 2		; move back one line
	MOV [ECX], AX	; put a zero in the bonus score ('cuz initialized to -1)

	; now check if bonus criteria is met
	CMP BX, 63				; total must be over 63 to get bonus
	JLE showUpperTotals 	; no bonus
	MOV AX, 35				; bonus
	MOV [ECX], AX  ; save bonus in array
	ADD BX, AX				; add bonus to total and save it
	ADD ECX, 2
	MOV [ECX], BX  ; save total in upper section total too in case no bonus
showUpperTotals:

doneScoringUpper:
	RET
scoreUpperSection ENDP

COMMENT %
******************************************************************************************
* Proc Name:	scoreLowerSection 
* Programmer:   William Kinser
* Purpose:	Uses the statistics in the Tally variables to calculate and validate the 
*           score for the specified field in the lower section of the score card. 
*			If the lower section is full, it calculates the total for that section.   
*
* Date created: 04 15  2020
* Date last modified: 04 16 2020 
*
* @param 
* PRECONDITIONS: AL contain index into the field (7-13) and tallies are calculated
* @return  
* POSTCONDITION: Carry flag set if error and score not done
******************************************************************************************
%
scoreLowerSection PROC C PRIVATE
LOCAL customStack:DWORD
	; basically, we need a switch statement 
	; switch (field#)
	; case (3 of a kind)
	;    if 3 of a kind tally is non zero then save running total in field
	; case (4 of a kind)
	; 	if 4 of a kind tally is non zero then save running total in field
	; case (full house)
	; 	 if 3 of a kind tally is non zero and pair tally is exactly 2 then 25 in field 
	; case (small straight)
	; 	if small tally is non zero then put 30 in field
	; case (large straight)
	;   if large tally is non zero then put 40 in field
	; case (yahtzee)
	; 	if 5 of a kind tally is non zero then put 50 in field
	; case (chance)
	; put running tally in field


	; get start of scores for the current player so we can see what they have already
	MOV EBX, lpCurrentScores ; get scorecard for this player	

	; so, to simulate the switch statement, first check to see if the tally is nonzero
	MOVZX ECX, AL					; convert index to DWORD in size
	MOV EBX, OFFSET bStartOfTallies	; start just before first tally since index starts @ 1
	ADD EBX, ECX					; add index to address to find correct face tally
	MOV AH, [EBX]					; get tally
	CMP AH, 0						; is it non-zero
	JNE keepScoringLower

criteriaNotMet:
; ask user if they want to proceed with this field. if so, put a zero in it 
; if zeroed then it's valid and we should process a zero else set carry and return
	MOV customStack,EAX
	CMP bAIPlayer, 1
	JE putAZeroInIt ; if this is a computer player, no one to ask, so proceed with zero

	INVOKE stringPrint, ADDR strCriteriaNotMet ; print error message
	INVOKE charInput			; get Y or N
	CMP EAX, 'Y'				; allow for upper case Y
	JE putAZeroInIt
	CMP EAX, 'y'				; or lower case y
	JE putAZeroInIt
	CMP EAX, 'N'				; allow for upper case N
	JE reselectField	
	CMP EAX, 'n'				; or lower case n
	JE reselectField
	; if control falls to here, they didn't pick Y or N so error message and try again
	INVOKE stringPrint, ADDR strInvalidYN ; display error message
	JMP criteriaNotMet ; allow user to answer the question again
reselectField:	; 
	STC			; set the carry flag indicating to the caller no scoring took place
	RET

; user has elected to put a zero in this field
putAZeroInIt:
	MOV BX, 0
	MOV EAX, customStack		; preserved index to custom stack (so we can forget about 
								; it if we exit or branch else where; can't do that with
								; the real stack)
	MOVZX ECX, AL				; convert index to DWORD in size
	MOV EDX, lpCurrentScores	; get addr of 3 of a Kind - 7 words
	ADD EDX, 4
	ADD EDX, ECX				; add index to address twice ...
	ADD EDX, ECX				; since it is a word size element
	MOV [EDX], BX				; save the numberic score
	JMP saveLowerScore

keepScoringLower:
	; need to pause processing the lower at this point so we can determine if a 
	; yahtzee bonus has been earned. If we wait till later, it will not be clear if
	; the yahtzee score was already earned or just earned in this roll.
	PUSHAD ; preserve AL which we need for scoring later
	
	; If a Yahtzee was already scored with 50 points add 100 points to yahtzee bonus
	MOV DL, b5KindTally ; find out if this roll is a Yahtzee or not
	CMP DL, 1			; any number >= 1 indicates a yahtzee was rolled with that #
	JL noLowerYahtzee  ; skip over the bonus logic and continue processing upper roll
	MOV ECX, lpCurrentScores   ; load current score array	
	ADD ECX, 28		    		; offset into Yahtzee field
	MOV DX, [ECX]; get current score for the Yahtzee field
	CMP DX, 50			; has it been scored yet?
	JNE noLowerYahtzee  ; if not scored as 50, then we don't get a bonus
	; so at this point we get the bonus
	MOV ECX, lpCurrentScores ; offset  total score array is word size to Yahtzee bonus line
	ADD ECX, 32
	MOV DX, [ECX]; get current score for the Yahtzee BONUS field
	CMP DX, -1			; see if a bonus has already been scored
	JNE add100			; if so, just keep the value
	INC DX				; if not, then add 1 since -1 represents "not scored"
add100:
	ADD DX, 100			; add another 100 to the bonus
	MOV [ECX], DX ; save the bonus back into the score array 

noLowerYahtzee:	
	POPAD

    ; so, at this point tally for field is non-zero
	; which means, if field is 7 or 8 or 13 we save the running total in the field
	; if it is any other field then we save predefined values
	
	CMP AL, 8					; if field index is for 3 or 4 of a kind
	JLE scoreRunningTotal		; save score as running total
	CMP AL, 13					; if field is not chance
	JNE usePredefinedValues		; then use predefined values otherwise fall thru

; there are 3 fields in the lower section that use the die total; 3&4 kind and chance
scoreRunningTotal:
	MOVZX EBX, bRunningTotal	; get running total
	MOV EDX, lpCurrentScores	; set EDX to 3 of kind line - 7 words
	ADD EDX, 4
	ADD EDX, ECX				; add index to address twice ...
	ADD EDX, ECX				; since it is a word size element
	MOV [EDX], BX				; save the numberic score
	JMP saveLowerScore

; there are 4 fields in the lower section that have predefined scores if criteria met
usePredefinedValues:
	; at this point, AL is an index into the fields is >= 9 but <= 12 
	; and criteria has been met
	; and ECX is dword copy of index
	PUSH ECX					; preserve dword copy of index
	SUB ECX, 9					; so adjust index down
	MOV BL, sPredefinedScores[ECX]  ; get the score and save it to score array
	MOVZX BX, BL				; expand to word size
	MOV EDX,  lpCurrentScores 	; set EDX to start of score array
	ADD EDX, 22					; offset to 9th element
	ADD EDX, ECX				; add index to address twice ...
	ADD EDX, ECX				; since it is a word size element
	MOV [EDX], BX				; save the numberic score

	POP ECX							; restore index


saveLowerScore:
	
	; now check to see if all lower fields are scored, if so, fill in the totals
	MOV ECX, 7		; there are 7 fields in the lower section to check
	MOV EDX, lpCurrentScores ; get address of array of scores 
	ADD EDX, 18				; offset to where lower scores start
	MOV EBX, 0		; use BX for running total 

; now check to see if all of the lower section is complete; if so, total it
checkLower:
	MOV AX, [EDX] 		; get score for this field
	CMP AX, -1		 	; if score for this field is < 0, we're done (-1 means no score)
	JE doneScoringLower
	ADD BX, AX			; add to running total
	INC EDX				; advance index to next address...
	INC EDX				; which is a word so we need to do it twice
	LOOP checkLower

	; can only get here if all 7 fields are scored so do totals
	MOV EDX, lpCurrentScores ; get address of score array
	ADD EDX, 34			; offset into lower total
	MOV [EDX], BX	; lower total goes in next to last line

doneScoringLower:
	CLC					; clearing CF indicates successful scoring of lower section
	RET
scoreLowerSection ENDP


COMMENT %
******************************************************************************************
* Proc Name:	willAIRollAgain
* Programmer:   William Kinser
* Purpose:	this procedure looks at the tallies for the current roll, then decides 
* which die to roll again, returning "Y" in EAX. 
* If no die selected, it returns "N" in EAX.  
* If die are selected to be rolled again, they are set in the bDiceToRoll data item.
*
* NOTE: this procedure does NOT place the roll in the scorecard even though it does
* look at current scores for this player in deciding whether to roll again or not.
*
* Date created: 04 18  2020
* Date last modified: 04 18 2020 
*
* @param 
* PRECONDITIONS: lpDice contains the current roll. lpCurrentScores & bCurrentPlayer are set
* POSTCONDITION: EAX holds single char answer Y or N
* @return  
******************************************************************************************
%
willAIRollAgain PROC C PRIVATE

	CALL processDice			; process dice into tallies

	MOV AL, 0				; set  bits to indicate none of the die to be rolled
	MOV bDiceToRoll, AL 	; copy dice number into memory
	MOV EBX, lpCurrentScores

	; - scores large straights immediately if rolled (as large or small)
	MOV DL, bLarge				  ; did we roll a large straight?
	CMP DL, 0
	JE notALargeAI
	MOV AX, [EBX+largeStrtOffset] ; do we already have a large straight
	CMP AX, -1
	JE doNotRollAgainAI  		  ; we rolled a large strt and we don't have one
	; we fall through here even if we rolled a large because a large straight also
	; means we have a small straight (which we check after checking for a yahtzee)

notALargeAI:
		; - scores yahtzee immediately  
	MOV DL, b5KindTally			; did we roll a yahtzee?
	CMP DL, 0					; DL holds the die value (1-6) if yahtzee rolled
	JNE doNotRollAgainAI

notAYahtzeeAI:
	MOV DL, bSmall				; did we roll a small straight?
	CMP DL, 0
	JE	notASmallAI
	MOV AX, [EBX+largeStrtOffset] ; do we already have a large straight?
	CMP AX, -1
	JNE yesALargeExistsAI
; - if small straight (and large straight not scored), then roll non sequential die again
; find non-sequential die (it's either the pair or an outlier)
; if we have a pair, pick one of those
	MOV DL, bPairFlag
	CMP DL, 0
	JE findOutlyerAI

; find first pair and pick one of the die in that pair at random
	MOV ECX, 6
findNextPairInStraight:
	MOV AL, bStartOfTallies[ECX] ; get # of die with this face value
	CMP AL, 2					 ; if its a pair...
	JGE reRollSingleDieAI		; reroll this die
	LOOP findNextPairInStraight
	JMP errorLogic	; should never get here but put a jump here just in case

reRollSingleDieAI:
	; pair face value is in ECX
	CALL findFirstDieWithValue ; returns the die position with the face value ECX, in EAX
	MOVZX DX, bDiceToRoll		; get current bit map for dice to reroll
	MOVZX AX, AL				; expand dice position to a word size
	BTC DX, AX 					; flips bit in mask (ignore the carry)
	MOV bDiceToRoll, DL			; save the updated mask back into data item
	JMP rollAgainAI				; jump to end (setting EAX to Y when we do)

findOutlyerAI: 		;(either 1 or 6)	
	MOV AL, bFaceTally+1 ; it's a 1 if there are no 2s 
	CMP AL, 0
	JNE mustBeASix
	MOV ECX, 1			; the 1 is the face value that is an outlier
	JMP reRollSingleDieAI
mustBeASix:
	MOV ECX, 6			; the 6 is the face value that is an outlier
	JMP reRollSingleDieAI

errorLogic: ; this code should never get called but is here in case there is a gap in logic
	; intialize to roll all 5 dice
	MOV AL, 3Eh				; set all 5 bits to indicate all 5 to be rolled
	MOV bDiceToRoll, AL 	; copy dice number into memory
	JMP rollAgainAI
	
yesALargeExistsAI:
	MOV AX, [EBX+smallStrtOffset]  ; do we have a small already need to roll again
	CMP AX, -1                     ; otherwise score it immediately as a small straight
	JE doNotRollAgainAI				; if we have a large and not a small, no need to roll 

notASmallAI:	; if we didn't roll a small, start checking multiples 
	; - if 4 of a kind, roll odd die again
	MOV DL, b4KindTally		; did we roll a 4 of a kind?
	CMP DL, 0				; DL will hold the face value (1-6) of what we have 4 of
	JE notA4KindAI
; find odd die out
	MOV ECX, 6
findOddDieOut:
	MOV AL, bStartOfTallies[ECX] ; how many of this face value do we have
	CMP AL, 1	; if 4 of kind, odd die out has a value of 1
	JE reRollSingleDieAI
	loop findOddDieOut
	JMP errorLogic
	
notA4KindAI: ; didn't roll any straights, yahtzees or 4 of a kind, so....
	; - if full house and 3 of a kind met and upper met but full house
		; not met, score immediately as a full house
	MOV AL, bFullTally		; did we roll a full house
	CMP AL, 0				; will be a 1 if full house rolled
	JE notAFullHouseAI
	; so we know we rolled a full house at this stage
	MOV AX, [EBX+fullHouseOffset]	; do we already have a full house in the scorecard
	CMP AX, -1						; -1 means this field not scored
	JNE haveA3GoFor4; rolled a full house but already have a full house so go for 4kind
	MOV AX, [EBX+lowerStartOffset]	; havent' scored a full house, check 3 of a kind too
	CMP AX, -1						; -1 means no 3 of kind scored yet
	JNE continueWithFullHouse; full house not used yet and no 3kind avail so check upper 
	; 3 of a kind avail. if running total so far < 25 (full house) keep full house
	MOV AL, bRunningTotal	; this holds the total of all die rolled
	CMP AL, 25
	JL continueWithFullHouse ; if total < 25 stay with full house
	MOV AX, [EBX+chanceOffset] ; have we used our chance field yet?
	CMP AX, -1					; -1 means no
	JE haveA3GoFor4 ; chance is open so even if we can't get 4kind, we can put it in chance

	; fall through to continueWithFullHouse
	
continueWithFullHouse:
	; rolled a full house & full house not used yet
	
	MOV AL, b3KindTally ; this tells us the die value we have 3 of 
	MOVZX EAX, AL	    ; expand AL to dword so we can use it as an index
	; EAX has the face value of the 3 of a kind in it at this point
	; we know we rolled a full house, don't have a full house but do have a 3 of a kind.
	; so can put this in upper or full house. 
	; check upper
	DEC EAX				; adjust face value to be an index into the scorecard
	SHL EAX, 1			; multiply by 2 since it is a word array
	MOV AX, [EBX+ EAX] ; get the value in the upper section for which we have a 3kind
	CMP AX, -1			; has the upper section field for this die been filled?
	JNE doNotRollAgainAI ; keep the full house since upper already filled
	
	; so we have a full house, and the 3kind is used and upper is avail 
	; if the thing we have 3 of is < 4 then stay with full house
	MOV AL, b3KindTally ; this tells us the die value we have 3 of 
	CMP AL, 4
	JL doNotRollAgainAI
	; else fall through to haveA3GoFor4

haveA3GoFor4:	
	; go for more of the die we have 3 of
	MOV AL, b3KindTally ; this tells us the die value we have 3 of 
	
	MOVZX EAX, AL			; expand AL to be a dword in size so we can use it as an index
	MOV ECX, 0				; start at the beginning
	MOV DL, bDiceToRoll		; get the current mask for dice to reroll
	MOVZX DX,DL				; expand to word so we can do bit operations on it
findThe3KindAI:
	CMP lpDice[ECX],EAX		; if this die's face value matches the 3 of a kind
	JE nextDieIn3KindAI 	; skip the die that match our 3 of a kind
	PUSH ECX				; else save ECX so we can temporarily manipulate it
	SHR CX,2				; div by 2 and ...
	INC CX					; add 1 to get the "bit" # that matches the die position
	BTC DX,CX			; set bit for the die that doesn't have the 3 of a kind
	MOV bDiceToRoll, DL		; save the updated mask back into the data item
	POP ECX					; restore ECX so we can continue looping
nextDieIn3KindAI:
	ADD ECX, 4				; advance to next position
	CMP ECX, SIZEOF lpDice	; are we at the end of the dice rolled?
	JNE findThe3KindAI
	JMP rollAgainAI			; mask set so ready to roll again
	
notAFullHouseAI:	
	MOV AL, b3KindTally ; not a full house but is it a 3 of a kind
	CMP AL, 0			; AL holds the face value we have 3 of if any 
	JNE haveA3GoFor4	; go for more if we have 3 of something
	
		; - if 2 pair, roll all but the 2 highest pair
	MOV AL, bPairFlag	; how many pairs do we have
	CMP AL, 2			; if not 2 pair, ...
	JNE goForThePairAI	; then keep processing as if we have 1 pair
	
	; so we have two pair, find the highest pair
	; (possible improvement: could we start at 6 and work backwards, keeping the first
	;  pair found?)
	MOV ECX, 1			; initialize index (also represents die face)
findFirstPairAI:
	MOV DL, bStartOfTallies[ECX] ; get count of die with this face
	CMP DL, 2			; do we have 2 of this die
	MOV EDX, ECX ; a move doesn't affect flags so we can do this after the CMP
	JE findNextPairAI
	INC ECX
	JMP findFirstPairAI
findNextPairAI:
	; ECX now holds the face value of the first pair
	INC EDX
	MOV CH, bStartOfTallies[EDX]; only need to keep CL for first pair, so ok to use CH
	CMP CH, 2				; do we have 2 of this die?
	JNE findNextPairAI
	; now DL has the 2nd pair and CL has the first, pick the highest
	CMP DL, CL
	JG pickDL
	MOVZX EAX, CL			; expand die face in CL to dword & put in EAX to process
	JMP rollAllButThePair
pickDL:
	MOVZX EAX, DL			; expand die face in DL to dword, & put in EAX to process
rollAllButThePair:
	; pick all the die that don't match the face value in EAX
	MOV ECX, 0
	MOV DL, bDiceToRoll		; get current bit mask for die to reroll
	MOVZX DX, DL			; expand to word size so we can do bit operations on it
findThePairAI:
	CMP lpDice[ECX],EAX		; does this match the high pair?
	JE nextDieInPairAI ; skip the die that match our pair in EAX
	PUSH ECX			;preserve counter so we can temporarily manipulate it as an index
	SHR CX,2			; div by 2...
	INC CX				; and add 1 to get position in mask
	BTC DX,CX			; set bit for the die that isn't in our pair
	MOV bDiceToRoll, DL	; save mask back to data item
	POP ECX				; restore counter/index
nextDieInPairAI:
	ADD ECX, 4			; advance to next die rolled
	CMP ECX, SIZEOF lpDice ; are we done?
	JNE findThePairAI
	JMP rollAgainAI		; mask updated, ready to roll again

goForThePairAI:	
	; - if 1 pair roll all odd die again
	MOV AL, bPairFlag 	; get the face value we have a pair of 
	CMP AL, 0			; if no pair then...
	JE rollAllButHighestAI ; roll all but the highest die we have
	
	; find the pair and make sure it isn't a pair of 1s or pair of 2s
	MOV ECX, 1			; initialize the index/counter
findAPairAI:
	MOV DL, bStartOfTallies[ECX] ; how many of this die do we have
	CMP DL, 2					; if a pair, then skip it
	MOV EDX, ECX ; a move doesn't affect flags so we can do this after the CMP
	JGE foundAPairAI
	INC ECX			; keep going till we find the pair
	JMP findAPairAI
foundAPairAI:
	; face value of the pair is in ECX
	CMP ECX, 2 ; if we only have 1 pair and its 2s or 1s, roll all but highes die
	JLE rollAllButHighestAI
	MOV EAX, ECX
	JMP rollAllButThePair

rollAllButHighestAI:
		; - roll all but the highest die again (only get here if no multiples and 
		; no straights, which means a disconnected series. highest die will either be
		; a 5 or a 6 since all die are different values.
	MOV EAX, 6 ; initialize index/counter to go in reverse (highest first)
findHighestAI:
	MOV DL, bStartOfTallies[EAX] ; do we have any of this face value
	CMP DL, 1					; if so, keep it and roll everything else
	JE foundHighestAI
	DEC EAX						; otherwise keep looking
	JMP findHighestAI			; no need for conditional check, we jump on either 5 or 6
foundHighestAI:
	; face value of the highest value die is in ECX
	JMP rollAllButThePair ; logic for selection is same as if we had a pair

; we clear the EAX register two different ways below, just for fun and potentially see
; which one is faster or more asthetically pleasing to the reader
rollAgainAI:
	XOR EAX, EAX ; clear the register
	MOV AL, 'Y'  ; save char 'Y' to indicate we want to roll again
	RET
doNotRollAgainAI:
	MOV AL, 'N'	; save char 'N' to indicate we want to roll again
	MOVZX EAX, AL ; expand to full dword in size 
	RET
willAIRollAgain ENDP

COMMENT %
******************************************************************************************
* Proc Name:	getFieldForAIScore
* Programmer:   William Kinser
* Purpose:	the AI player is done rolling and ready to pick the best place for the roll
* on the scorecard.   
*
* Date created: 04 18  2020
* Date last modified: 04 18 2020 
*
* @param 
* PRECONDITIONS: lpDice contains the current roll. lpCurrentScores & bCurrentPlayer are set
* POSTCONDITION: returns score card field number (1-13) in EAX
* @return  
******************************************************************************************
%
getFieldForAIScore PROC C PRIVATE

; local data items to temporarily hold the field we may choose, whether we have about
; low score (total of all die), and which upper section field we might use
LOCAL fieldToScore:BYTE, lowScore:BYTE, upperIndex:DWORD

	PUSHAD						; preserve all the registers
	CALL processDice			; process dice into tallies

	MOV lowScore, 0 ; initialize to false
	MOV upperIndex, 0; initialize to zero
	MOV DL, bRunningTotal ; if die total is < 20 (1,2,3, 4s) then its low score = true
	CMP DL, 20			  ; this helps later to decide between choices of scoring
	JG continueAIScoring
	MOV lowScore, 1		  ; local flag indicating we have a low score
continueAIScoring:

	MOV EBX, lpCurrentScores ; get address of score card for this AI player

	; - use some similar logic as for evaluating each roll (see willAIRollAgain)
	; (this covers Yahtzees & straights & some full houses)
	MOV DL, bLarge   			  ; was a large straight rolled?
	CMP DL, 0					  ; DL will be 1 if yes
	JE checkSmallAI
	MOV AX, [EBX+largeStrtOffset] ; do we already have a large straight
	CMP AX, -1					  ; -1 mean no
	JNE haveASmallStrtAI  		  ; we rolled a large strt and we have one, so move on
	MOV fieldToScore, 'B' 		  ; otherwise, set field to Large Straight
	JMP doneScoringAI			  ; and be done

checkSmallAI:
	MOV DL, bSmall   				; was a small straight rolled?
	CMP DL, 0						; 1 means yes
	JE checkYahtzeeAI				; if not, move on
haveASmallStrtAI: ; rolled small straight, do we already have a small straight?
	MOV AX, [EBX+smallStrtOffset] ; get current score for small straight
	CMP AX, -1						; -1 means no
	JNE dumpABadScoreAI 			; find the least damaging place to put this roll
	MOV fieldToScore, 'A' 			; otherwise, set field to small straight
	JMP doneScoringAI				; and be done

checkYahtzeeAI:	
	MOV DL, b5KindTally 			; did we roll a yahtzee?
	CMP DL, 0						; 1 means yes
	JE check4KindAI					; if not, move on
	; we did roll a yahtzee so find best place to put it
	MOV AX, [EBX+yahtzeeOffset]		; did we already score one?
	CMP AX, -1 						; -1 means no
	JNE findNextBestPlace4Yahtzee	; if yes then find next best place to put it
	MOV fieldToScore, 'C'			; otherwise, set field to yahtzee
	JMP doneScoringAI				; and be done
	
findNextBestPlace4Yahtzee:
	; test for joker rule (rolled a Yahtzee, already used one and filled upper field)
	MOV DL, b5KindTally   ; DL now has the die value we have 5 of
	MOV fieldToScore, DL  ; save this just in case we're done
	ADD fieldToScore, 30h ; convert digit to char (= 1-6; or field # in upper section)
	MOVZX EDX, DL		  ; expand to dword size...
	DEC EDX				  ; and adjust to be...
	SHL EDX,1			  ; and index into the upper section of the score card
	MOV upperIndex, EDX		; store in local variable so we don't compute twice
	MOV AX, [EBX+EDX]	  ; see if we already have this upper slot filled
	CMP AX, -1			  ; -1 means no
	JE upperNotUsedYahtzeeAI ; if no, then it means no joker rule in effect

	MOV AX, [EBX+yahtzeeOffset]  ; get score in the yahtzee field  (either 0 or 50)
	CMP AX, 50					; must be 50 for the joker rule
	JNE noJokerRuleInEffect		; if not 50, then the joker rule is not in effect
	JMP jokerRuleInEffect		; if yes then it is
	
upperNotUsedYahtzeeAI:
	CMP lowScore, 1			; if die total is < 20 (1,2,3, 4s) then done
	JE doneScoringAI		; because we had a low scoring yahtzee, put it in upper section
	JMP noJokerRuleInEffect ; otherwise keep processing
	
jokerRuleInEffect:
	; Yahtzee can be placed in lg strt, small strt, 4kind, 3kind or full house
	MOV AX, [EBX+largeStrtOffset] ; is large straight used ?
	CMP AX, -1					  ; -1 means no
	JNE JRIE2					  ; if yes, keep looking
	MOV fieldToScore, 'B'		  ; otherwise, set field to large straight 
	JMP doneScoringAI			  ; and call it done
JRIE2:							
	MOV AX, [EBX+smallStrtOffset] ; is small straight used ?
	CMP AX, -1					  ; -1 means no
	JNE JRIE3					  ; if yes, keep looking
	MOV fieldToScore, 'A'		  ; otherwise, set field to small straight
	JMP doneScoringAI			  ; and call it done
JRIE3:
	MOV AX, [EBX+lowerStartOffset+2] 	; is 4 of a kind used ?
	CMP AX, -1							; -1 means no
	JNE JRIE4							; if yes, keep looking
	MOV fieldToScore, '8'				; otherwise, set field to 4 of a kind
	JMP doneScoringAI					; and call it done
JRIE4:
	MOV AX, [EBX+lowerStartOffset]		; is 3 of a kind used ?
	CMP AX, -1							; -1 means no
	JNE JRIE5							; if yes, keep looking
	MOV fieldToScore, '7'				; otherwise, set field to 3 of a kind
	JMP doneScoringAI					; and call it done
JRIE5:
	MOV ECX, upperIndex					; use upper section at this point if available
	MOV AX, [EBX+ECX] 					; is the upper section for this die used?
	CMP AX, -1							; -1 means no
	JNE JRIE6							; if yes, then keep looking
	JMP doneScoringAI					; otherwise, call it done (field already set)
JRIE6:
	MOV AX, [EBX+fullHouseOffset] 		; is full house already used?
	CMP AX, -1							; -1 means no
	JNE JRIE7							; if yes, then keep looking
	MOV fieldToScore, '9'				; otherwise, set field to full house
	JMP doneScoringAI					; and call it done
JRIE7:
	MOV AX, [EBX+chanceOffset] 			; is the chance used?
	CMP AX, -1							; -1 means no
	JNE JRIE8							; if yes, then keep looking
	MOV fieldToScore, 'D'				; otherwise, set field to Chance
	JMP doneScoringAI					; and call it done
JRIE8:
; we've tried all of the lower section. and the upper field so
; only place left is first available upper field (which will be a zero)
	JMP firstAvailableUpper
	
noJokerRuleInEffect:
	; so, to get here, we rolled a yahtzee but can't put it in that field (it's either
	; zeroed out or has 50 in it already).
	; NOTE: if the total of all DIE <= 20, we've already put it in upper section & exited
	
	; for now, let's just fall through to logic for 4 of a kind and see how it processes
	; since the tallies mark all possible scoring positions

check4KindAI:
	; - any 3 or 4 of a kind with 5s or 6s, score as 4 of a kind, 3 of a kind, upper,
		; - or chance
	; - any 3 or 4 of a kind <5 should go in upper if avail, otherwise 3 of kind, or 
		; 4 of a kind if available
	MOV DL, b4KindTally
	CMP DL, 0
	JE check3KindAI	
	CMP lowScore, 1; if die total is < 20 (1,2,3, 4s) then try upper first
	JNE putIn4KindIfAvailAI
	; DL has the face value we have four of, so turn it into a dword index for scorecard
	MOV fieldToScore, DL 	; save this off just in case we can put it in upper
	ADD fieldToScore, 30h 	; convert digit to char
	DEC DL					; modify DL to be and index into scorecard...
	MOVZX EDX, DL			; by decrementing and expanding to dword size
	SHL EDX, 1				; multiply by 2 to index into word array
	MOV AX, [EBX+EDX] 		; is upper position available
	CMP AX, -1				; -1 means no
	JE doneScoringAI		; if no, then we're done (field is already set)

putIn4KindIfAvailAI:
	MOV AX, [EBX+lowerStartOffset+2] 	; see if 4 of a kind if avail
	CMP AX, -1							; -1 means no
	JNE check3KindAI 					; if yes, maybe in 3 of a kind 
	MOV fieldToScore, '8'				; otherwise, set field to 4 of a kind
	JMP doneScoringAI					; call it done

check3KindAI:
	MOV DL, b3KindTally			; DL holds die face we have 3 of (1-6)
	CMP DL, 0					; if 0, we don't have 3 of a kind
	JE check2KindAI				; if none then keep looking
	CMP lowScore, 1				; if die total is < 20 (1,2,3, 4s) then try upper first
	JNE putIn3KindIfAvailAI 	; if total >= 20 then try 3 of a kind first

	; - if 3 of a kind, may be a full house so if full house, score as full house 
	; since we know the die total is low
	MOV AL, bFullTally			; did we roll a full house?
	CMP AL, 0					; 1 means yes
	JE try3InUpperAI			; if no, then try upper section for low scoring 3 of a kind
	MOV AX, [EBX+fullHouseOffset] ; we rolled a full house but did we use it already?
	CMP AX, -1					; -1 means no
	JNE try3InUpperAI			; if yes, then put low scoring 3 of a kind in upper
	MOV fieldToScore, '9'		; otherewise set field to full house
	JMP doneScoringAI			; and call it done
	
putIn3KindIfAvailAI:
	MOV AX, [EBX + lowerStartOffset] ; is the 3 of a kind used?
	CMP AX, -1						 ; -1 means no
	JNE try3InUpperAI 				 ; if yes, try upper section
	MOV fieldToScore, '7'			 ; otherwise, set field to 3 of a kind
	JMP doneScoringAI				 ; call it done
	
try3InUpperAI:	
	; DL has the face value we have 3 of, so turn it into a dword index for scorecard
	MOV fieldToScore, DL ; save this off just in case we can put it in upper
	ADD fieldToScore, 30h ; convert digit to char
	DEC DL				 ; manipulate into an index by subtracting 1...
	MOVZX EDX, DL		 ; expanding to dword size...
	SHL EDX, 1			 ; and multiply by 2 to index index into word array
	MOV AX, [EBX+EDX] 	 ; is upper position available
	CMP AX, -1			 ; -1 means no
	JE doneScoringAI	 ; if no, call it done (field already set)

	MOV AX, [EBX + lowerStartOffset] 	; do we already have 3 of a kind used?
	CMP AX, -1							; -1 means no
	JNE check2KindAI 					; if yes, keep looking
	MOV fieldToScore, '7'				; otherwise, set field to 3 of a kind
	JMP doneScoringAI					; and call it done


check2KindAI:
	; it's possible to get here with a full house if it scored lower than 3 of a kind
	; but 3 of a kind was full and upper was full so check full house now
	MOV AL, bFullTally				; did we roll a full house
	CMP AL, 0						; 1 means yes
	JE continueChecking2KindAI		; if no, then keep looking
	MOV AX, [EBX+fullHouseOffset]	; otherwise, check if full house used
	CMP AX, -1					    ; -1 means no
	JNE continueChecking2KindAI 	; full house is full so keep checking
	MOV fieldToScore, '9'			; otherwise, set field to full house
	JMP doneScoringAI				; and call it done
	
dumpABadScoreAI: ; has a straight and couldn't use it as a straight (same as a pair or less
continueChecking2KindAI: 
	CMP lowScore, 1		 		; do we have a low die total score
	JE CC2KAI1 					; if yes, then keep looking
	MOV AX, [EBX+chanceOffset]	; otherwise, is chance used?
	CMP AX,-1					; -1 means no
	JNE CC2KAI1					; if yes, keep looking 
	MOV fieldToScore, 'D'		; otherwise, set field to Chance
	JMP doneScoringAI			; and call it done
	
CC2KAI1: ; this is where we start picking the least worst place to put the roll
	MOV DL, bFaceTally 			; if at least 1 one, put it in upper
	CMP DL, 0					; 1 or more means yes
	JE CC2KAI2					; if no, move on to next least worst
	MOV AX, [EBX]				; has the 1s field been used
	CMP AX, -1					; -1 means no
	JNE CC2KAI2					; if yes, then move on to next least worst
	MOV fieldToScore, '1'		; otherwise, set field to 1 
	JMP doneScoringAI			; and call it done
	
CC2KAI2:
	MOV DL, bFaceTally+1 		; if at least two 2s, put in upper
	CMP DL, 2					; 2 or more means yes
	JL CC2KAI3					; if no, move on to next least worst
	MOV AX, [EBX+2]				; has the 2s field been used
	CMP AX, -1					; -1 means no
	JNE CC2KAI3					; if yes, then move on to next least worst
	MOV fieldToScore, '2'		; otherwise, set field to 2
	JMP doneScoringAI			; and call it done
CC2KAI3:
	MOV DL, bFaceTally+2 ; if at least two 3s, put in upper
	CMP DL, 2					; 2 or more means yes
	JL CC2KAI4					; if no, move on to next least worst
	MOV AX, [EBX+4]				; has the 3s field been used
	CMP AX, -1					; -1 means no
	JNE CC2KAI4					; if yes, then move on to next least worst
	MOV fieldToScore, '3'		; otherwise, set field to 3
	JMP doneScoringAI			; and call it done
	
CC2KAI4:
	; if current upper score is > 63, fill first available field in upper starting with l
	
	; now get current total for upper section (some fields may be -1, it's ok)
	MOV ECX, 0	; use this for total
	ADD CX, [EBX] ; get total of 1s
	ADD CX, [EBX+2] ; get total of 2s
	ADD CX, [EBX+4] ; get total of 3s
	ADD CX, [EBX+6] ; get total of 4s
	ADD CX, [EBX+8] ; get total of 5s
	ADD CX, [EBX+10] ; get total of 6s
	CMP CX, 63
	JGE firstAvailableUpper ; if upper section bonus achieve, just put this roll there

	CMP lowScore, 1			; if upper bonus not met, do we have a low score?
	JE firstAvailableUpper	; if yes, then put it upper anyway
	; - if total die is > 20, score in chance if avail
	MOV AX, [EBX+chanceOffset]  ; has chance been used?
	CMP AX, -1					; -1 means no
	JNE firstAvailableLower		; if yes, then find any other slot in lower section
	MOV fieldToScore, 'D'		; otherwise, set field to Chance
	JMP doneScoringAI			; and call it done
	
firstAvailableLower:
	; take any available field in lower section starting with chance & working backwards

	MOV ECX, chanceOffset		; set index to chance field
	MOV DL, 'D'					; keep DL set to field value in score card
CC2KAI7:
	MOV AX, [EBX+ECX] 			; get score for this field 
	CMP AX, -1					; -1 means no score
	JNE CC2KAI8					; if scored, then continue looking
	MOV fieldToScore, DL		; otherwise, set field to DL
	JMP doneScoringAI			; and call it done
CC2KAI8:						; 
	SUB ECX, 2  				; retreat to previous score card field
	DEC EDX						; decrement field number
	CMP DL, 40h					; exit loop if we get past 'A', small straight
	JNE CC2KAI7
; now do the lower fields with numeric index, in reverse order
	MOV DL, '9'					; set DL to mark the full house field in lower section
CC2KAI9:
	MOV AX, [EBX+ECX] 			; get score for this field
	CMP AX, -1					; -1 means no score
	JNE CC2KAI10				; if scored, keep looking
	MOV fieldToScore, DL		; otherwise, set field to DL
	JMP doneScoringAI			; and call it done
CC2KAI10: ; adjust index and field number to next one in reverse order
	SUB ECX, 2			; subtract 2 from index since we are using a word array
	DEC EDX				; subtract 1 from field number
	CMP DL, '6'			; is field number outside the lower section?
	JNE CC2KAI9			; if no, then keep looping
	;; fall through to firstAvailableUpper

firstAvailableUpper:
	; take any available field in upper section starting with lowest die value

	MOV ECX, 0				; set index to start at beginning of upper section
	MOV EDX, 31h ; '1'		; set field number (DL) to be the 1s field
CC2KAI5:
	MOV AX, [EBX+ECX] 		; get score for this field
	CMP AX, -1				; -1 means no score
	JNE CC2KAI6				; if scored, then keep looking
	MOV fieldToScore, DL	; otherwise, set field to DL
	JMP doneScoringAI		; and call it done
CC2KAI6:
	ADD ECX, 2 				; advance to next score card field
	INC EDX					; advance field number
	CMP DL, '7' ; stop if we get to the lower section (it's gets processed differently)
	JNE CC2KAI5
	JMP firstAvailableLower ; no spot in upper so try lower
;NOTE: firstAvailableUpper jumps to firstAvailableLower if no slot found
;   and firstAvailableLower jumpts to firstAvailableUpper if no slot found
; because there has to be at least 1 slot somewhere or we would not have entered this
; procedure (The game would be over). We start looking in upper or lower based on
; the total die roll
	
doneScoringAI: ; EAX has field to place score in (1-13)
	POPAD		; now, restore all the registers we preserved when we started
	MOVZX EAX, fieldToScore ; get field number and put it in EAX as our return value
	RET
getFieldForAIScore ENDP

COMMENT %
******************************************************************************************
* Proc Name:	findFirstDieWithValue 
* Programmer:   William Kinser
* Purpose:	Helper procedure to find the first die in the current roll that matches
*				the face value passed in using ECX register. The matching die #
*				is returned in EAX   
*
* Date created: 04 15  2020
* Date last modified: 04 16 2020 
*
* @param 
* PRECONDITIONS: ECX contains the die value (1-6) we are looking for
* @return  
* POSTCONDITION: EAX will hold the first die # (1-5) that matches ECX
******************************************************************************************
%
findFirstDieWithValue PROC C PRIVATE
	PUSH EDX 			; preserve EDX since we use it temporarily
	MOV EAX, 0			; set index to the beginning
compareToNextDie:
	MOV EDX, lpDice[EAX] ; get first die value
	CMP EDX, ECX		 ; compare it to what we are looking for
	JE foundIt			 ; done if they match
	ADD EAX, 4			 ; otherwise, advance index (by 4 since its a dword array)
	CMP EAX, SIZEOF lpDice ; are we past the end
	JNE compareToNextDie	; keep looping if not
	; should never get here since we only call this proc if we know we rolled a ECX value
	MOV EAX, 1			; assume the first die if we go off the end for some reason
	JMP doneFindFirstDieWithValue
foundIt:
	SHR EAX,2			; adjust index to be the die # by dividing by 2...
	ADD EAX,1			; and adding 1
doneFindFirstDieWithValue:
	POP EDX				; restore register used temporarily
	RET
findFirstDieWithValue ENDP


COMMENT %
******************************************************************************************
* Proc Name:	trimName 
* Programmer:   William Kinser
* Purpose:	Helper procedure to trim player name to max of 10 chars.   
*
* Date created: 04 30  2020
* Date last modified: 04 30 2020 
*
* @param 
* PRECONDITIONS: strInput1 has source string, EDX points to destination string
* @return  
* POSTCONDITION: string pointed to by EDX will be updated.
******************************************************************************************
%
trimName PROC C PRIVATE
	PUSHAD
	MOV ECX, 10 ; trim name to just 10 chars max Plus null terminator
	MOV EBX, 0  ; index into each string
trimName1:
	MOV AL, strInput1[EBX]   ; get char from first string
	MOV [EDX+EBX], AL			 ; put it in second string
	CMP AL, 0				; if null, then we are done
	JE trimNameDone
	CMP EBX, 9				; if we've moved 10 chars, put a null in second string & done
	JE trimMaxedOut
	INC EBX
	loop trimName1
trimMaxedOut:
	INC EBX					; max # chars reached, advance index 1 
	MOV AL, 0
	MOV [EDX+EBX],AL		; and add null terminator
trimNameDone:
	POPAD
	RET
trimName ENDP

END							;the assembler ignores all instructions beyond END
