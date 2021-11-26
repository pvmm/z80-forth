----

decimal 2 capacity 1- thru

----
only forth also
vocabulary Z80

\ save the current context
context @ avoc !

\ z/code is Z80 version of CODE
: z/code  create hide here dup 2- ! context @ avoc ! z80 ;
----
\ set Z80 context
z80 definitions hex

\ old sum word renamed
: (+) + ;

\ Set FORTH context
: [forth]  forth context ! ;

\ Restore previous context
: [prev]  avoc @ context ! ;

\ Define Z80 version of END-CODE
: z/end-code  avoc @ context ! reveal ;
----
\ Undoing C, and , when errors are detected
: undoC,  dp @ 1- dp ! ;
: undo,   dp @ 2- dp ! ;

\ Split integer into two 8-bit numbers ( n -- c c )
: splitc  dup 8RSHIFT swap 0FF and ;

\ Join 8-bit numbers into integer
: join  dup FF00 and if abort" 8-bit value expected #1" then
    swap dup FF00 and if abort" 8-bit value expected #2" then
    8LSHIFT (+) ;
----
\ operand stack and functions
variable opr.p0 2 allot         \ maximum of 3 places
variable opr.p opr.p0 opr.p !   \ point opr.p to operand stack
: 'opr  opr.p @ ;               \ top operand stack position
: opr@  opr.p @ 2- @ ;          \ get top operand stack value
: opr!  opr.p ! ;               \ store on operand stack
: opr#  'opr opr.p0 - 2 / ;     \ get operand stack size

\ empty operand stack
: oprs>nul  opr.p0 opr! ;

\ store register
: >opr
  'opr opr.p0 - 4 > if
    oprs>nul abort" Operand stack overflow" then
  'opr ! 'opr 2+ opr! ;
----
\ operand stack and functions (cont)
\ restore register
: opr>
  dup 0fff0 - 0 < abort" Not a placeholder"
  'opr opr.p0 - 0= abort" Operand stack underflow"
  drop opr@ 'opr 2- opr! ;

\ drop single register from register stack
: opr>nul  opr> drop ;

\ is register stack not empty?
: ?opr  'opr opr.p0 - 0> ;

\ stack placeholder for register, address and index
0fff1 constant >R<   0fff3 constant >8R<   0fff5 constant >16R<
0fff2 constant >A<   0fff4 constant >I<
----
variable (DEBUG) 1 (DEBUG) !
variable LCOUNT -1 LCOUNT !
: ><
 LCOUNT @ 1+ LCOUNT !
 (DEBUG) @ if cr LCOUNT @ 3 u.r space .S ." / " opr# 1 u.r then ;
----
variable start( 0 start( !
variable address 0 address !
: (  address @ if abort" Nesting ( not allowed" then
     1 address !  depth start( ! ;
: )  depth start( <= if 0 address ! abort" Missing address" then
     address @ if 0 address ! >opr >A<
     else abort" Matching ) is missing" then ;

\ First parameter position in the operand stack
variable firstparam -1 firstparam !
\ Locate first parameter in the operand stack
: >loc<  depth firstparam ! ;
\ Reset parameter position when done
: !reset  -1 firstparam ! ;
----
\ 8 and 16 bit register identifiers
000 constant reg.B      (S 8BIT OPCODE+00 )
001 constant reg.C      (S 8BIT OPCODE+01 )
002 constant reg.D      (S 8BIT OPCODE+02 )
003 constant reg.E      (S 8BIT OPCODE+03 )
004 constant reg.H      (S 8BIT OPCODE+04 )
005 constant reg.L      (S 8BIT OPCODE+05 )
006 constant reg.(HL)   (S 8BIT OPCODE+06 )
007 constant reg.A      (S 8BIT OPCODE+07 )
000 constant reg.BC     (S 16BIT OPCODE+00 )
002 constant reg.DE     (S 16BIT OPCODE+02 )
004 constant reg.HL     (S 16BIT OPCODE+04 )
006 constant reg.SP     (S 16BIT OPCODE+06 )
008 constant reg.AF     (S 16BIT OPCODE+08 )
0DD constant reg.IX     (S 0DD, HL OPCODE )
0FD constant reg.IY     (S 0FD, HL OPCODE )
----
\ 8 and 16 bit register identifiers (cont)
\ 030 constant reg.(w)    (S OPCODE+30 )
\ 200 constant reg.(BC)   (S OPCODE+00 )
\ 210 constant reg.(DE)   (S OPCODE+10 )
----
\ registers as CODE parameters
: A     >loc< reg.A        >opr >8R< ;
: B     >loc< reg.B        >opr >8R< ;
: C     >loc< reg.C        >opr >8R< ;
: D     >loc< reg.D        >opr >8R< ;
: E     >loc< reg.E        >opr >8R< ;
: H     >loc< reg.H        >opr >8R< ;
: L     >loc< reg.L        >opr >8R< ;
: (HL)  >loc< reg.(HL)     >opr >8R< ;
: BC    >loc< reg.BC       >opr >16R< ;
: DE    >loc< reg.DE       >opr >16R< ;
: HL    >loc< reg.HL       >opr >16R< ;
----
\ register as CODE parameters (cont)
: SP    >loc< reg.SP       >opr >16R< ;
: AF    >loc< reg.AF       >opr >16R< ;
: IX    >loc< reg.IX       >opr >16R< ;
: IX+   >loc< reg.IX join  >opr >16R< ;
: IY    >loc< reg.IY       >opr >16R< ;
: IY+   >loc< reg.IY join  >opr >16R< ;
: (BC)  >loc< reg.BC       >opr >16R< ;
: (DE)  >loc< reg.DE       >opr >16R< ;
----
\ operand stack checkings
: ?r  ?opr if dup >R< and 0fff1 = else 0 then ;     \ register?
: ?8r                                         \ 8-bit register?
  ?r if dup >8R< = else 0 then ;
: ?16r                                       \ 16-bit register?
  ?r if dup >16R< = else 0 then ;
: ?idx  ?opr if dup >I< = else 0 then ;        \ indexed ix/iy?
: ?ixy  ?r if opr@ 0FD and 0DD >= else 0 then ;     \ ix or iy?
: ?adr  ?opr if dup >A< = else 0 then ;              \ address?
----
\ Consume and move operand stack register into stack
: opr>r  opr> dup 8 = if drop 0 then ; (S >R< -- r )
\ Consume and convert stack register into rr+index
: opr>idx  opr> splitc ;

\ Check if signed 8-bit value
: ?8s  dup dup 7F <= swap -80 >= and ;

\ Sum operator used on IX + i or i + IY
: + (S param1 param2 -- >R< ) 
 ?ixy not if
    ?8s not if abort" Signed 8-bit overflow" then
    swap then
 ?ixy not if abort" IX/IY register expected" then
 opr>r join >opr >I< ;
----
\ opcode type (inherited from 8080 and extended)
: 1MI  create C, does> C@ C, !reset ;
: 2MI  create C, does> C@ (+) C, !reset ;
: 3MI  create C, does> C@ swap 8* (+) C, !reset ;
: 4MI  create C, does> C@ C, C, !reset ;
: 5MI  create C, does> C@ C, , !reset ;
: 6MI  create C, does> C@ (+) C, C, !reset ;
: 7MI  create C, does> C@ (+) C, , !reset ;
: 8MI  create C, does> C@ swap 8* (+) C, C, C, !reset ;
: 9MI  create C, does> C@ swap 8* (+) C, , !reset ;
----
\ opcodes 1/2
\ r: 8 bit register
\ rr: 16 bit register
\ b: 8 bit value
\ w: 16 bit value
078 2MI (LDA,r)       03E 4MI (LDA,b)      00A 3MI (LDA,(rr))
07E 4MI (LDA,(IX+i))  040 2MI (LDB,r)      006 4MI (LDB,b)
046 4MI (LDB,(IX+i))  048 2MI (LDC,r)      00E 4MI (LDC,b)
04E 4MI (LDC,(IX+i))  050 2MI (LDD,r)      016 4MI (LDD,b)
056 4MI (LDD,(IX+i))  058 2MI (LDE,r)      01E 4MI (LDE,b)
05E 4MI (LDE,(IX+i))  060 2MI (LDH,r)      026 4MI (LDH,b) 
066 4MI (LDH,(IX+i))  068 2MI (LDL,r)      02E 4MI (LDL,b)
06E 4MI (LDL,(IX+i))  070 2MI (LD(HL),r)   036 4MI (LD(HL),b)
----
\ opcodes 2/2
021 5MI (LDHL,w)      02A 5MI (LDHL,(w))   0F9 1MI (LDSP,HL)
070 6MI (LD(IX+i),r)  076 7MI (LD(IX+i),b) 
002 3MI (LD(rr),A)    03A 5MI (LDA,(w))    001 9MI (LDrr,w)
04B 9MI (LDrr,(w))    043 9MI (LD(w),rr)   022 5MI (LD(w),HL)
003 3MI (INCrr)       004 3MI (INCr)       034 4MI (INC(IX+i))
00B 3MI (DECrr)       005 3MI (DECr)       035 4MI (DEC(IX+i))
080 2MI (ADDA,r)      0C6 4MI (ADDA,b)     009 3MI (ADDHL,BC)
000 1MI (NOP)         0C1 2MI (POP)        0C5 2MI (PUSH)
0C3 4MI JP
----
: (LDA,*)  ?8r if opr>r (LDA,r) exit then
   opr@ reg.HL = if opr>nul reg.(HL) (LDA,r) exit then
   ?idx if opr>idx C, (LDA,(IX+i)) exit then
   ?16r if opr>r (LDA,(rr)) exit then
   ?adr if opr> (LDA,(w)) else (LDA,b) then ;
: (LDB,*)  ?8r if opr>r (LDB,r) exit then
   opr@ reg.HL = if opr>nul reg.(HL) (LDB,r) exit then
   opr@ reg.(HL) = if opr>nul reg.(HL) (LDB,r) exit then
   ?idx if opr>idx C, (LDB,(IX+i)) else (LDB,b) then ;
: (LDC,*)  ?8r if opr>r (LDC,r) exit then
   opr@ reg.HL = if opr>nul reg.(HL) (LDC,r) exit then
   opr@ reg.(HL) = if opr>nul reg.(HL) (LDC,r) exit then
   ?idx if opr>idx C, (LDC,(IX+i)) else (LDC,b) then ;
: (LDD,*)  ?8r if opr>r (LDD,r) exit then
   opr@ reg.HL = if opr>nul reg.(HL) (LDD,r) exit then
   opr@ reg.(HL) = if opr>nul reg.(HL) (LDD,r) exit then
----
   ?idx if opr>idx C, (LDD,(IX+i)) else (LDD,b) then ;
: (LDE,*)  ?8r if opr>r (LDE,r) exit then
   opr@ reg.HL = if opr>nul reg.(HL) (LDE,r) exit then
   opr@ reg.(HL) = if opr>nul reg.(HL) (LDE,r) exit then
   ?idx if opr>idx C, (LDE,(IX+i)) else (LDE,b) then ;
: (LDH,*)  ?8r if opr>r (LDH,r) exit then
   opr@ reg.HL = if opr>nul reg.(HL) (LDH,r) exit then
   opr@ reg.(HL) = if opr>nul reg.(HL) (LDH,r) exit then
   ?idx if opr>idx C, (LDH,(IX+i)) else (LDH,b) then ;
: (LDL,*)  ?8r if opr>r (LDL,r) exit then
   opr@ reg.HL = if opr>nul reg.(HL) (LDL,r) exit then
   opr@ reg.(HL) = if opr>nul reg.(HL) (LDL,r) exit then
   ?idx if opr>idx C, (LDL,(IX+i)) else (LDL,b) then ;
: (LD(HL),*)  ?8r if opr>r (LD(HL),r) exit then (LD(HL),b) ;
----
\ Load into 8-bit register
: (LDr,*)
 opr@ reg.A    = if opr>nul (LDA,*) exit then
 opr@ reg.B    = if opr>nul (LDB,*) exit then
 opr@ reg.C    = if opr>nul (LDC,*) exit then
 opr@ reg.D    = if opr>nul (LDD,*) exit then
 opr@ reg.E    = if opr>nul (LDE,*) exit then
 opr@ reg.H    = if opr>nul (LDH,*) exit then
 opr@ reg.L    = if opr>nul (LDL,*) exit then
 opr@ reg.(HL) = if opr>nul (LD(HL),*) then ;
----
: (LD(rr),A)'
 dup reg.SP = if abort" Invalid parameter" else (LD(rr),A) then ;
: (LDrr,(w))'
 dup reg.HL = if drop (LDHL,(w)) exit then
 dup reg.IX = over reg.IY = or if C, (LDHL,(w)) exit then
 0ED C, (LDrr,(w)) ; \ TODO: make it simple
: (LDrr,w)'
 dup reg.IX = over reg.IY = or if C, (LDHL,w) exit then
 (LDrr,w) ;
: (LDSP,*)
 ?opr if ?ixy if opr> C, (LDSP,HL) exit then
    opr@ reg.HL = if opr>nul (LDSP,HL) exit then then
 ?adr if opr> reg.SP (LDrr,(w))' exit then
 reg.SP (LDrr,w) ;
----
: (LDrr,*)
 opr@ reg.SP = if opr>nul (LDSP,*) exit then
 opr>r >R
 opr@ reg.A = if opr>nul R> (LD(rr),A)' exit then
 ?r if R> .S abort" Invalid 2nd parameter" then
 ?adr if opr> R> (LDrr,(w))' exit then
 R> (LDrr,w)' ;
: (LD(w),rr)'
 >R \ store address again
 opr@ reg.HL = if opr>nul R> (LD(w),HL) exit then
 ?ixy if opr>r R> swap C, (LD(w),HL) exit then
 opr>r R> swap 0ED C, (LD(w),rr) ;
----
\ Load second parameter into indexed register
: (LD(IX+i),*)
 opr> >R
 ?8r if R> splitc C, swap opr>r (LD(IX+i),r) exit then
 ?8s if R> splitc C, reg.(HL) (LD(IX+i),r) C, exit then
 abort" 8-bit parameter expected" ;
: (LDw,*)
 opr> >R
 opr@ reg.A = if opr>nul R> 30 (LD(rr),A)' exit then
 ?16r if R> (LD(w),rr)' exit then
 R> abort" Invalid parameter" ;
----
\ detect type of LD by its parameters
: LD (S reg|addr reg|addr|value -- )
 ?8r  if (LDr,*) exit then
 ?idx if (LD(IX+i),*) exit then
 ?16r if (LDrr,*) exit else (LDw,*) then ;
----
: INC (S reg -- )
 ?idx if opr>idx C, (INC(IX+i)) exit then
 ?8r  if opr>r (INCr) exit then
 ?ixy if opr>r C, reg.HL (INCrr) exit then
 ?16r if opr>r (INCrr) else abort" Invalid parameter" then ;
: DEC (S reg -- )
 ?idx if opr>idx C, (DEC(IX+i)) exit then
 ?8r  if opr>r (DECr) exit then
 ?ixy if opr>r C, reg.HL (DECrr) exit then
 ?16r if opr>r (DECrr) else abort" Invalid parameter" then ;
----
: (ADDA,*)
 ?8r  if opr>r (ADDA,r) exit then
 opr@ reg.HL = if opr>nul reg.(HL) (ADDA,r) exit then
 ?idx if opr>idx C, reg.(HL) (ADDA,r) C, exit then
 ?8s not abort" Invalid parameter" 40 (ADDA,r) ;
: (ADDHL,*)
 ?16r not abort" Invalid parameter" opr>r (ADDHL,BC) ;
: (ADDIX,*) ." =>"
 >R ?16r not abort" Invalid parameter"
 ?ixy if opr>r R> over <> abort" Invalid parameter" C, reg.HL
         (ADDHL,BC) else R> C, opr>r (ADDHL,BC) then ;
: ADD (S reg|idx -- )
 ?r if opr@ reg.A  = if opr>nul (ADDA,*) exit then
       opr@ reg.HL = if opr>nul (ADDHL,*) exit then
       ?ixy if opr>r (ADDIX,*) exit then then
 true abort" Invalid parameter" ;
----
: next  >next JP ;
----
\ Revert to FORTH context
FORTH DEFINITIONS
Z80 ASSEMBLER
