      SUBROUTINE KWICK (LFLAG, BRANCH)
C-----------------------------------------------------------------------
C! verbs:  math, assignment, comparison, looping, branching, proc calls
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000-2001, 2004-2005, 2007-2008
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   KWICK contains the most frequently used interpreter codes.
C   Inputs:
C      BRANCH     I      verb number:
C                         1 => "Omega" operator, end a command
C                         2 => "(" no-op
C                         3 => ")" get arguments in proc call
C                         4 => = store operation
C                         5 => + addition
C                         6 => - subtraction
C                         7 => * multiplication
C                         8 => / division
C                         9 => ** exponentiation
C                        10 => >  logical comparison, greater than
C                        11 => >  logical comparison, less than
C                        12 => "+" unary plus;  no-op
C                        13 => "-" unary minus
C                        14 => ^ logical not
C                        15 => : or TO  loop upper limit
C                        16 => BY loop increment
C                        17 => = logical comparison, equal
C                        18 => ! logical OR
C                        19 => & logical AND
C                        20 => ; end of line, no-op
C                        21 => FOR  loop from a to b by c
C                        22 => END  end loop
C                        23 => READ read adverb values
C                        24 => TYPE or PRINT display adverb values
C                        25 => RETURN end of proc, return function value
C                        26 => LENGTH length of string to last non blank
C                        27 => reserved for vector assignments ("=")
C                        28 => reserved to execute procedures
C                        29 => reserved for partial vector assignments
C                        30 => RANDOM
C                        31 => LOG base-10 logarithm
C                        32 => LN  base-e logarithm
C                        33 => MOD modulo  x1 - int(x1/x2)*x2
C                        34 => MODULUS  sqrt(x1*x1 + x2*x2)
C                        35 => ATAN2
C                        36 => SIN
C                        37 => COS
C                        38 => TAN
C                        39 => ATAN
C                        40 => SQRT
C                        41 => DUMP print part of the K array
C                        42 => <= logical comparison, less or equal
C                        43 => >= logical comparison, greater or equal
C                        44 => <> logical comparison, not equal
C                        45 => EXP exponential
C                        46 => SUBSTR converts piece of string to substr
C                        47 => !! concatanate strings/substrings
C                        48 => CHAR converts f.p. value to string
C                        49 => VALUE converts string to f.p. value
C                        50 => ASIN arc-sine of a value
C                        51 => ACOS arc-cosine of a value
C                       106 => ELSE (If-Then-Else) set pointers
C                       107 => THEN (If-Then-Else) set pointers
C                       109 => WHILE loop, set pointer when false
C                        74 => CLRTEMP clear KKT area
C                        91 => CEIL returns next higher integer
C                        92 => FLOOR returns next lower integer
C                        93 => ABS absolute value
C                        94 => MAX return max value of 2 args
C                        95 => MIN return min value of 2 args
C                        96 => reserved to end If-Then-Else (no-op)
C                        97 => reserved to init WHILE loop
C                        98 => reserved for subscript evaluation
C                        >1000 => procedure code to run
C   Outputs:
C      LFLAG      I      opcode for INTRPR to use:
C                        1 => advance L by 2, then execute K(L)
C                             "program chunk begins"
C                        2 => advance L to next location, then execute
C                             K(L) "step thru compiled line"
C                        3 => continue at pgm location L
C                        4 => decr SP, STACK(newSP)=0, save XX at V(SP)
C                        5 => save XX at V(SP)
C                             then 4 and 5 as 2
C                        6 => exit interpreter (BRANCH = 1 only)
C-----------------------------------------------------------------------
      INTEGER   BRANCH, LFLAG
C                                         Parameterize the stack max
      CHARACTER STRING*132, PRGNAM*6, TMPSTR*132
      INTEGER   COUNT, DISC, IVAL, LOFF, LOWBND, N, NODIM, POTERR, I,
     *   START, IDIM, IK, ISP, ITDUM, IX, IXL, IY, J, KARLM1, KP, M, MI,
     *   MJ, NCH, NSIZE, FRMT, IOFF, LSAVE, NCWORD, IROUND, MXCORE, IM,
     *   MM, JM, KKT(10), NC, NP, JTRIM, LSIZE
      REAL      PIR, EPSILN, TEMP, XI, XL
      DOUBLE PRECISION    DBLX
      LOGICAL   EQUAL
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DIO.INC'
      EQUIVALENCE (KKT(1), K(51))
      DATA KARLM1 /64/
      DATA EPSILN /1.0E-8/
      DATA PRGNAM /'KWICK '/
C-----------------------------------------------------------------------
      PIR = 3.141592654 / 180.0
      LFLAG = 2
C                                        test for arrays where single
C                                        variables are required
      IF (BRANCH.LT.5) GO TO 5
      IF ((BRANCH.GE.15) .AND. (BRANCH.LE.17)) GO TO 5
      IF ((BRANCH.GT.19) .AND. (BRANCH.LE.30)) GO TO 5
      IF (BRANCH.EQ.44) GO TO 5
      IF ((BRANCH.GT.45) .AND. (BRANCH.LE.49)) GO TO 5
      IF (BRANCH.GT.51) GO TO 5
         POTERR = 8
         IF (SP.LT.1) GO TO 5
         IF (STACK(SP).EQ.2) GO TO 980
         IF (SP.LT.2) GO TO 5
         IF (STACK(SP-1).NE.2) GO TO 5
            IF ((BRANCH.GE.12) .AND. (BRANCH.LE.14)) GO TO 5
            IF ((BRANCH.EQ.31) .OR. (BRANCH.EQ.32)) GO TO 5
            IF ((BRANCH.GE.36) .AND. (BRANCH.LE.40)) GO TO 5
            IF ((BRANCH.GE.50) .AND. (BRANCH.LE.51)) GO TO 5
            IF (BRANCH.NE.45) GO TO 980
 5    POTERR = 7
      IF (BRANCH.LE.51) GO TO
     *   ( 10, 999,  30,  40,  50,  60,  70,  80,  90, 100,
     *    110, 999, 130, 140, 150, 160, 170, 180, 190, 999,
     *    210, 220, 230, 270, 320, 650, 360, 370, 380, 390,
     *    400, 410, 420, 430, 440, 450, 460, 470, 480, 490,
     *    500, 510, 520, 530, 540, 630, 640, 660, 670, 740,
     *    750), BRANCH
      IF (BRANCH.EQ.106) GO TO 550
      IF (BRANCH.EQ.107) GO TO 560
      IF (BRANCH.EQ.109) GO TO 590
      IF (BRANCH.EQ.96) GO TO 600
      IF (BRANCH.EQ.97) GO TO 610
      IF (BRANCH.EQ.98) GO TO 620
      IF (BRANCH.EQ.91) GO TO 680
      IF (BRANCH.EQ.92) GO TO 690
      IF (BRANCH.EQ.93) GO TO 700
      IF ((BRANCH.EQ.94) .OR. (BRANCH.EQ.95)) GO TO 710
      IF (BRANCH.EQ.74) GO TO 720
      IF (BRANCH.EQ.90) GO TO 730
      IF (BRANCH.GT.999) GO TO 375
      GO TO 999
C-----------------------------------------------------------------------
C                  O M E G A   O P E R A T O R                    1
C-----------------------------------------------------------------------
 10   L = LINK
      LINK = K(L)
      LFLAG = 1
      IF (L.EQ.0) LFLAG = 6
      GO TO 999
C-----------------------------------------------------------------------
C                  G E T   A R G U M E N T
C                                        pick up arguments in proc call
C-----------------------------------------------------------------------
 30   POTERR = 12
      IF (CP.LT.1) GO TO 980
      IF (CSTACK(CP).NE.-28) GO TO 980
      I = 2*SP0 - SP - 1
      IF (I.LE.0) GO TO 980
      J = SP0
      POTERR = 8
 34   IF ((I.GE.SP0) .OR. (J.GT.SP)) GO TO 38
C                                        reals
         IF ((STACK(J+3).EQ.2) .AND. (J+3.LE.SP)) GO TO 36
            M = STACK(J)
            IF (STACK(I).EQ.2) GO TO 980
            IF (M.LE.K(KXORG+7-1)) GO TO 980
C                                        Check for array bounds
            MXCORE = (MPAGE - LPAGE) * 256
            IF ((M.LT.1) .OR. (M.GT.MXCORE)) THEN
               POTERR = 6
               WRITE (MSGTXT,1034) M, MXCORE
               CALL MSGWRT(6)
               GO TO 980
               END IF
C                                        Check for array bounds
            IF ((I.LT.1) .OR. (I.GT.SLIM)) THEN
               POTERR = 7
               WRITE (MSGTXT,1035) I, SLIM
               CALL MSGWRT(6)
               GO TO 980
               END IF
            C(M) = V(I)
            I = I + 1
            J = J + 1
            GO TO 34
C                                        arrays/strings
 36      IF ((STACK(I+3).NE.2) .OR. (I+3.GE.SP0)) GO TO 980
            POTERR = 7
            IF (SP.GT.SLIM-8) GO TO 980
            POTERR = 8
            ISP = SP
            SP = SP + 8
            CALL COPY (4, STACK(J), STACK(ISP+1))
            CALL COPY (4, STACK(I), STACK(ISP+5))
            CALL ASSGN
            IF (ERRNUM.NE.0) GO TO 980
            SP = ISP
            I = I + 4
            J = J + 4
            GO TO 34
 38   CP = CP + 2
      CSTACK(CP-1) = SP0
      CSTACK(CP) = -3
      SP0 = SP + 1
      GO TO 999
C-----------------------------------------------------------------------
C                  =    S T O R E   O P E R A T O R               4
C-----------------------------------------------------------------------
 40   CALL ASSGN
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                  +   O P E R A T O R                            5
C-----------------------------------------------------------------------
 50   IF (SP.LT.2) GO TO 980
      XX = V(SP-1) + V(SP)
      LFLAG = 4
      GO TO 999
C-----------------------------------------------------------------------
C                  -   O P E R A T O R                            6
C-----------------------------------------------------------------------
 60   IF (SP.LT.2) GO TO 980
      XX = V(SP-1) - V(SP)
      LFLAG = 4
      GO TO 999
C-----------------------------------------------------------------------
C                  *   O P E R A T O R                            7
C-----------------------------------------------------------------------
 70   IF (SP.LT.2) GO TO 980
      XX = V(SP-1) * V(SP)
      LFLAG = 4
      GO TO 999
C-----------------------------------------------------------------------
C                  /   O P E R A T O R                            8
C-----------------------------------------------------------------------
 80   IF (SP.LT.2) GO TO 980
      POTERR = 20
      IF (V(SP).EQ.0) GO TO 980
      XX = V(SP-1) / V(SP)
      LFLAG = 4
      GO TO 999
C-----------------------------------------------------------------------
C                  * *    O P E R A T O R                         9
C-----------------------------------------------------------------------
 90   IF (SP.LT.2) GO TO 980
      LFLAG = 4
      IF (V(SP-1).LT.0.0) GO TO 95
         XX = V(SP-1) ** V(SP)
         GO TO 999
 95   I = IROUND (V(SP))
      POTERR = 62
      IF (ABS(V(SP)-I).GT.0.01) GO TO 980
         XX = V(SP-1) ** I
         GO TO 999
C-----------------------------------------------------------------------
C                  >   C O M P A R I S O N   O P E R A T O R       10
C-----------------------------------------------------------------------
 100  IF (SP.LT.2) GO TO 980
      SP = SP-1
      TEMP = C(FALSE)
      IF (V(SP).GT.V(SP+1)) TEMP = C(TRUE)
      V(SP) = TEMP
      GO TO 999
C-----------------------------------------------------------------------
C                  <   C O M P A R I S O N   O P E R A T O R       11
C-----------------------------------------------------------------------
 110  IF (SP.LT.2) GO TO 980
      SP = SP - 1
      TEMP = C(FALSE)
      IF (V(SP).LT.V(SP+1)) TEMP = C(TRUE)
      V(SP) = TEMP
      GO TO 999
C-----------------------------------------------------------------------
C                  U N A R Y   ( - )   O P E R A T O R           13
C-----------------------------------------------------------------------
 130  IF (SP.LT.1) GO TO 980
      XX = -V(SP)
      LFLAG = 5
      GO TO 999
C-----------------------------------------------------------------------
C                  ^   O P E R A T O R                           14
C-----------------------------------------------------------------------
 140  IF (SP.LT.1) GO TO 980
      TEMP = 0.0
      IF (V(SP).GT.0) TEMP = C(FALSE)
      IF (V(SP).LE.0) TEMP = C(TRUE)
      V(SP) = TEMP
      GO TO 999
C-----------------------------------------------------------------------
C                  T O   O P E R A T O R                         15
C-----------------------------------------------------------------------
C                                        test for arrays !
 150  POTERR = 8
      IF (SP.LT.3) GO TO 980
      IF ((STACK(SP).EQ.2) .OR. (STACK(SP-1).EQ.2) .OR. (STACK(SP-2)
     *   .EQ.2))  GO TO 980
      CSTACK(CP+1) = SP
      CSTACK(CP+2) = SP0
      CSTACK(CP+3) = -15
      CP = CP+3
      SP0 = SP0+3
      V(SP+2) = V(SP-1)
      STACK(SP+1) = STACK(SP-2)
      STACK(SP+2) = STACK(SP-1)
      V(SP-1) = V(SP)
      V(SP) = 1.0
      SP = SP + 2
      GO TO 999
C-----------------------------------------------------------------------
C                  B Y   O P E R A T O R                         16
C-----------------------------------------------------------------------
 160  IF (SP.LT.4) GO TO 980
      POTERR = 15
      IF (CSTACK(CP).NE.-15) GO TO 980
      POTERR = 8
      IF (STACK(SP).EQ.2) GO TO 980
      V(SP-3) = V(SP)
      SP = SP - 1
      GO TO 999
C-----------------------------------------------------------------------
C                  =   E Q U A L S   O P E R A T O R             17
C-----------------------------------------------------------------------
 170  CALL EQUIV (EQUAL)
      IF (ERRNUM.NE.0) GO TO 980
      TEMP = C(FALSE)
      IF (EQUAL) TEMP = C(TRUE)
      SP = SP + 1
      V(SP) = TEMP
      GO TO 999
C-----------------------------------------------------------------------
C                  |    ' O R '    O P E R A T O R               18
C-----------------------------------------------------------------------
 180  IF (SP.LT.2) GO TO 980
      SP = SP - 1
      TEMP = C(TRUE)
      IF ((V(SP).LE.0) .AND. (V(SP+1).LE.0)) TEMP = C(FALSE)
      V(SP) = TEMP
      GO TO 999
C-----------------------------------------------------------------------
C                  &   ' A N D '    O P E R A T O R              19
C-----------------------------------------------------------------------
 190  IF (SP.LT.2) GO TO 980
      SP = SP - 1
      TEMP = C(FALSE)
      IF ((V(SP).GT.0) .AND. (V(SP+1).GT.0)) TEMP = C(TRUE)
      V(SP) = TEMP
      GO TO 999
C-----------------------------------------------------------------------
C                  F O R   O P E R A T O R                       21
C-----------------------------------------------------------------------
 210  POTERR = 15
      IF (CSTACK(CP).NE.-15) GO TO 980
      CSTACK(CP) = L + 1
      CSTACK(CP+1) = LINK
      CP = CP+2
      CSTACK(CP) = -21
      GO TO 999
C-----------------------------------------------------------------------
C                  E N D    O P E R A T O R                      22
C-----------------------------------------------------------------------
 220  IF (CSTACK(CP).EQ.-21) THEN
         MI = CSTACK(CP-4)
         XI = V(MI)
         XL = V(MI-1)
         M = STACK(MI-2)
         POTERR = 18
         IF (ABS(XI).LT.EPSILN) GO TO 980
         C(M) = C(M) + XI
         IF ((C(M)-XL)/XI.LT.0.0001) THEN
            LINK = CSTACK(CP-1)
            L = CSTACK(CP-2)
            LFLAG = 3
         ELSE
            CP = CP - 5
            SP = CSTACK(CP+1) - 3
            SP0 = CSTACK(CP+2)
            END IF
      ELSE IF (CSTACK(CP).EQ.-61) THEN
         L = CSTACK(CP-2)
         LINK = CSTACK(CP-1)
         LFLAG = 3
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                  R E A D    O P E R A T O R                    23
C                                        may have trouble when number
C                                        hits edge of screen
C-----------------------------------------------------------------------
 230  I = SP0
 235  CALL PREAD (KARBUF)
         KP = 1
         IF (ERRNUM.NE.0) GO TO 980
         N = NBYTES
         IF (N.EQ.0) GO TO 265
 240     IF (I.GT.SP) GO TO 265
            IF ((I+3.LE.SP) .AND. (STACK(I+3).EQ.2) .AND.
     *         (STACK(I).LT.15)) GO TO 245
               M = STACK(I)
               CALL GETNUM (KARBUF, KARLIM, KP, DBLX)
               IF (ERRNUM.NE.0) GO TO 980
               POTERR = 25
               IF (M.LE.K(KXORG+7-1)) GO TO 980
               C(M) = DBLX
               IF ((KP.GT.KARLIM) .AND. (C(M).EQ.0.0)) GO TO 235
               I = I + 1
               GO TO 240
 245        SYTYPE = STACK(I)
            NSIZE = STACK(I+1)
            TAG = STACK(I+2)
            POTERR = 25
            IF (TAG.LE.K(KXORG+7-1)) GO TO 980
            IF (SYTYPE.NE.2) GO TO 260
               NSIZE = K(NSIZE)
               LAST = TAG + NSIZE - 1
               DO 255 J = TAG,LAST
 250              CALL GETNUM (KARBUF, KARLIM, KP, DBLX)
                  IF (ERRNUM.NE.0) GO TO 980
                  C(J) = DBLX
                  IF ((KP.LE.KARLIM) .OR. (C(J).NE.0.0)) GO TO 255
                     CALL PREAD (KARBUF)
                     IF (ERRNUM.NE.0) GO TO 980
                     N = NBYTES
                     IF (N.EQ.0) GO TO 265
                     KP = 1
                     GO TO 250
 255              CONTINUE
               I = I+4
               GO TO 240
C                                        character strings
 260        CONTINUE
               POTERR = 23
               IF ((SYTYPE.LT.7) .OR. (SYTYPE.GT.9)) GO TO 980
               IDIM = 1
               IOFF = 1
               IF (SYTYPE.EQ.9) IOFF = NSIZE / 1024
               IF (SYTYPE.EQ.9) NSIZE = MOD (NSIZE, 1024) - IOFF + 1
               NCWORD = (NSIZE + 3) / 4
               IF (SYTYPE.NE.7) GO TO 261
                  IDIM = K(NSIZE)
                  NSIZE = K(NSIZE+3)
                  POTERR = 13
                  IF (NSIZE.LE.0) GO TO 980
                  NCWORD = (NSIZE + 3) / 4
                  IDIM = IDIM / NCWORD
 261           DO 264 J = 1,IDIM
 262              CALL GETSTR (KARBUF, KARLIM, NSIZE, KP, STRING, NCH)
                  IF (ERRNUM.NE.0) GO TO 980
                  IF (NCH.GT.0) GO TO 263
                     CALL PREAD (KARBUF)
                     IF (ERRNUM.NE.0) GO TO 980
                     N = NBYTES
                     IF (N.EQ.0) GO TO 265
                     KP = 1
                     GO TO 262
 263              CALL CHR2H (NSIZE, STRING, IOFF, CH(TAG))
                  TAG = TAG + NCWORD
 264              CONTINUE
               I = I + 4
               GO TO 240
 265  SP = SP0 - 1
      GO TO 999
C-----------------------------------------------------------------------
C                  P R I N T   O P E R A T O R                   24
C-----------------------------------------------------------------------
 270  I = SP0
      NC = 14
      NP = 7
 274  KP = 1
      MSGTXT = ' '
C                                        loop thru list
 275  IF (I.GT.SP) GO TO 315
C                                        scalars
         IF ((I+3.LE.SP) .AND. (STACK(I+3).EQ.2) .AND.
     *      (STACK(I).LT.15)) GO TO 285
C                                        KP set to (multiple of 6) + 1
            KP = ((KP+NC-2)/NC) * NC + 1
            IF (KP.LE.KARLM1-NC) GO TO 280
               CALL MSGWRT (5)
               KP = 1
               MSGTXT = ' '
 280        ITDUM = FRMT (NC, NP, V(I), STRING)
            MSGTXT(KP:KP+NC-1) = STRING(:NC)
            KP = KP + NC
            I = I + 1
            GO TO 275
C                                        vectors
 285     CONTINUE
            SYTYPE = STACK(I)
            NSIZE = STACK(I+1)
            START = STACK(I+1)
            TAG = STACK(I+2)
            I = I + 4
C                                        reals
            IF ((SYTYPE.NE.2) .AND. (SYTYPE.NE.12)) GO TO 300
C                                        Start new line if necessary.
               IF (KP.NE.1) CALL MSGWRT (5)
               KP = 1
               MSGTXT = ' '
               IF (SYTYPE.EQ.2) THEN
                  NODIM = K(START + 1)
                  NSIZE = K(START)
               ELSE
                  NODIM = 1
                  END IF
               LAST = TAG + NSIZE - 1
C
               DO 295 J = TAG,LAST
                  IF (KP.GT.KARLM1-NC) THEN
                     CALL MSGWRT (5)
                     KP = 1
                     MSGTXT = ' '
                     END IF
C                                       Print array subscript numbers.
                  IF (KP.EQ.1) THEN
                     IF (SYTYPE.EQ.2) THEN
                        DISC = START + 2
                        COUNT = J - TAG
                        DO 287 IK = 1,NODIM
                           LOWBND = K(DISC)
                           N = K(DISC+1)
                           DISC = DISC + 2
                           IVAL = MOD (COUNT, N) + LOWBND
                           COUNT = COUNT / N
                           IF (KP.EQ.1) WRITE (STRING,1287) IVAL
                           IF (KP.NE.1) WRITE (STRING,1288) IVAL
                           MSGTXT(KP:KP+3) = STRING(1:4)
                           KP = KP + 4
 287                       CONTINUE
                     ELSE
                        IVAL = J - TAG + 1
                        WRITE (STRING,1287) IVAL
                        END IF
                     MSGTXT(KP:KP) = ')'
                     KP = KP + 3
                     END IF
C
                  ITDUM = FRMT (NC, NP, C(J), STRING)
                  MSGTXT(KP:KP+NC-1) = STRING(1:NC)
                  KP = KP + NC
 295              CONTINUE
               CALL MSGWRT (5)
               GO TO 274
C                                        character strings
 300        CONTINUE
               IDIM = 1
               IOFF = 1
               IF (SYTYPE.EQ.9) IOFF = NSIZE / 1024
               IF (SYTYPE.EQ.9) NSIZE = MOD (NSIZE, 1024) - IOFF + 1
               NCWORD = (NSIZE + 3) / 4
               IF (SYTYPE.EQ.7) THEN
                  IDIM = K(NSIZE)
                  NSIZE = K(NSIZE+3)
                  POTERR = 13
                  IF (NSIZE.LE.0) GO TO 980
                  NCWORD = (NSIZE + 3) / 4
                  IDIM = IDIM / NCWORD
                  END IF
               IF ((IDIM.NE.1) .AND. (KP.NE.1)) THEN
                  CALL MSGWRT (5)
                  KP = 1
                  MSGTXT = ' '
                  END IF
               DO 310 J = 1,IDIM
                  IF (KP.GT.1) KP = KP + 3
                  IF (NSIZE.GT.KARLM1-KP) THEN
                     IF (MSGTXT.NE.' ') CALL MSGWRT (5)
                     KP = 1
                     MSGTXT = ' '
                     END IF
C                                        Print array subscript numbers.
                  IF ((KP.EQ.1) .AND. (IDIM.NE.1)) THEN
                     COUNT = J - 1
                     DISC = START + 4
                     NODIM = K(START+1) - 1
                     DO 305 IK = 1,NODIM
                        LOWBND = K(DISC)
                        N = K(DISC+1)
                        DISC = DISC + 2
                        IVAL = MOD (COUNT, N) + LOWBND
                        COUNT = COUNT / N
                        IF (KP.EQ.1) WRITE (STRING,1287) IVAL
                        IF (KP.NE.1) WRITE (STRING,1288) IVAL
                        MSGTXT(KP:KP+3) = STRING(1:4)
                        KP = KP + 4
C
 305                    CONTINUE
                     MSGTXT(KP:KP) = ')'
                     KP = KP + 4
                     END IF
C
                  CALL H2CHR (NSIZE, IOFF, C(TAG), TMPSTR)
                  LSIZE = JTRIM (TMPSTR)
                  LSIZE = MAX (1, LSIZE)
                  IF (LSIZE+KP-3.LE.KARLM1) THEN
                     MSGTXT(KP:KP) = ''''
                     KP = KP + 1
                     MSGTXT(KP:KP+LSIZE-1) = TMPSTR(:LSIZE)
                     KP = KP + LSIZE
                     MSGTXT(KP:KP) = ''''
                  ELSE
                     IM = MIN (LSIZE, 132)
                     MSGTXT(KP:KP) = ''''
                     JM = 1
 307                 MM = MIN (IM, KARLM1 - KP + JM - 1)
                        MSGTXT(KP+1:) = TMPSTR(JM:MM)
                        IF (MM.LT.IM) THEN
                           CALL MSGWRT (5)
                           MSGTXT = ' '
                           JM = MM + 1
                           KP = KP - 1
                           GO TO 307
                        ELSE
                           KP = KP + 1 + MM - JM + 1
                           MSGTXT(KP:KP) = ''''
                           IF (IDIM.EQ.1) THEN
                              CALL MSGWRT (5)
                              GO TO 274
                              END IF
                           END IF
                     END IF
                  TAG = TAG + NCWORD
 310              CONTINUE
               IF (IDIM.EQ.1) THEN
                  IF (KP.NE.1) KP = KP + 3
                  GO TO 275
                  END IF
               CALL MSGWRT (5)
               GO TO 274
C                                        done
 315  IF (KP.GT.1) CALL MSGWRT (5)
      SP = SP0 - 1
      GO TO 999
C-----------------------------------------------------------------------
C                  R E T U R N                                   25
C-----------------------------------------------------------------------
 320  CONTINUE
         IXL = K(KXORG+7-1) + 1
         IF (CSTACK(CP).NE.-3) GO TO 335
            I = CSTACK(CP-1)
            N = SP0 - I
            J = I - N
            MI = I
 325        IF ((J.GE.MI) .OR. (I.GE.SP0)) GO TO 330
C                                        reals
               IF ((STACK(I+3).EQ.2) .AND. (I+3.LT.SP0)) GO TO 327
                  MJ = STACK(J)
                  M = STACK(I)
                  IF (MJ.GT.IXL) C(MJ) = C(M)
                  I = I + 1
                  J = J + 1
                  GO TO 325
C                                        arrays/strings
 327           CONTINUE
C                                        no return to temp strings
                  IF (STACK(J).EQ.14) GO TO 328
                     POTERR = 7
                     IF (SP.GT.SLIM-8) GO TO 980
                     ISP = SP
                     SP = SP + 8
                     CALL COPY (4, STACK(J), STACK(ISP+1))
                     CALL COPY (4, STACK(I), STACK(ISP+5))
                     CALL ASSGN
                     IF (ERRNUM.NE.0) GO TO 980
                     SP = ISP
 328              I = I + 4
                  J = J + 4
                  GO TO 325
 330        SP0 = MI
            IF ((STACK(MI+N+3).EQ.2) .AND. (MI+N+3.LE.SP)) GO TO 332
               STACK(MI) = STACK(MI+N)
               V(MI) = V(MI+N)
               GO TO 334
C                                        array
 332        CONTINUE
               CALL COPY (4, STACK(MI+N), STACK(MI))
 334        SP = SP - N
            CP = CP - 2
 335     IF (CSTACK(CP).EQ.-28) GO TO 345
            IF (CSTACK(CP).NE.-21) GO TO 340
               CP = CP - 6
               GO TO 320
 340        CONTINUE
               POTERR = 15
               IF (CSTACK(CP).NE.-61) GO TO 980
               CP = CP - 3
               GO TO 320
 345  IF (CSTACK(CP+2).NE.-3) GO TO 350
         MJ = SP0 - N
         IF ((STACK(MJ+N+3).EQ.2) .AND. (MJ+N+3.LE.SP)) GO TO 347
            STACK(MJ) = STACK(MJ+N)
            V(MJ) = V(MJ+N)
            GO TO 349
C                                        array
 347     CONTINUE
            CALL COPY (4, STACK(MJ+N), STACK(MJ))
 349     SP = SP - N
 350  L = CSTACK(CP-3)
      LINK = CSTACK(CP-2)
      SP0 = CSTACK(CP-1)
      CP = CP - 4
      GO TO 999
C-----------------------------------------------------------------------
C                    V E C T O R   A S S I G N M E N T             27
C-----------------------------------------------------------------------
 360  CALL MASSGN
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                    R U N    O P E R A T O R                      28
C-----------------------------------------------------------------------
 370  GO TO 999
 375  POTERR = 11
      IF (BRANCH.GT.KXORG) GO TO 980
      CP = CP + 4
      CSTACK(CP+2) = 0
      CSTACK(CP-3) = L
      CSTACK(CP-2) = LINK
      CSTACK(CP-1) = SP0
      CSTACK(CP) = -28
      L = BRANCH
      LINK = K(L)
      SP0 = SP + 1
      LFLAG = 1
      GO TO 999
C-----------------------------------------------------------------------
C         P A R T I A L   V E C T O R   A S S I G N M E N T        29
C-----------------------------------------------------------------------
 380  CALL PASSGN
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                  U N U S E D                                     30
C-----------------------------------------------------------------------
 390  GO TO 999
C-----------------------------------------------------------------------
C                  L  O  G   and   L  N
C-----------------------------------------------------------------------
 400  IF (SP.LT.1) GO TO 980
      LFLAG = 5
      POTERR = 29
      IF (V(SP).LE.0.0) GO TO 980
      XX = LOG10 (V(SP))
      GO TO 999
 410  IF (SP.LT.1) GO TO 980
      LFLAG = 5
      POTERR = 29
      IF (V(SP).LE.0.0) GO TO 980
      XX = LOG (V(SP))
      GO TO 999
C-----------------------------------------------------------------------
C                  M  O  D  u l o
C-----------------------------------------------------------------------
 420  IF (SP.LT.2) GO TO 980
      POTERR = 20
      IF (V(SP).EQ.0.) GO TO 980
      SP = SP - 1
      V(SP) = MOD (V(SP), V(SP+1))
      GO TO 999
C-----------------------------------------------------------------------
C                  M  O  D  U  L  U  S   and   A  T  A  N  2
C-----------------------------------------------------------------------
 430  IF (SP.LT.2) GO TO 980
      SP = SP - 1
      V(SP) = SQRT (V(SP)*V(SP) + V(SP+1)*V(SP+1))
      GO TO 999
 440  IF (SP.LT.2) GO TO 980
      SP = SP - 1
      POTERR = 20
      IF ((V(SP).EQ.0.0) .AND. (V(SP+1).EQ.0.0)) GO TO 980
      V(SP) = ATAN2 (V(SP), V(SP+1)) / PIR
      GO TO 999
C-----------------------------------------------------------------------
C                  SIN, COS, TAN, ATAN, SQRT      (36---40)
C-----------------------------------------------------------------------
 450  IF (SP.LT.1) GO TO 980
      LFLAG = 5
      XX = SIN(V(SP)*PIR)
      GO TO 999
 460  IF (SP.LT.1) GO TO 980
      LFLAG = 5
      XX = COS(V(SP)*PIR)
      GO TO 999
 470  IF (SP.LT.1) GO TO 980
      LFLAG = 5
      XX = TAN(V(SP)*PIR)
      GO TO 999
 480  IF (SP.LT.1) GO TO 980
      LFLAG = 5
      XX = ATAN(V(SP)) / PIR
      GO TO 999
 490  IF (SP.LT.1) GO TO 980
      LFLAG = 5
      POTERR = 26
      IF (V(SP).LT.0.0) GO TO 980
      XX = SQRT(V(SP))
      GO TO 999
C-----------------------------------------------------------------------
C                                       DUMP
C                                       print part of K array
C-----------------------------------------------------------------------
 500  IF (SP.LT.2) GO TO 980
      IX = V(SP-1)
      IY = V(SP) + IX - 1
      SP = SP-2
      POTERR = 24
      IF (IY.LT.IX) GO TO 980
      CALL ZKDUMP (IX, IY, K, C)
      GO TO 999
C-----------------------------------------------------------------------
C                 <=   C O M P A R I S O N   O P E R A T O R       42
C-----------------------------------------------------------------------
 510  IF (SP.LT.2) GO TO 980
      SP = SP - 1
      TEMP = C(FALSE)
      IF (V(SP).LE.V(SP+1)) TEMP = C(TRUE)
      V(SP) = TEMP
      GO TO 999
C-----------------------------------------------------------------------
C                 >=   C O M P A R I S O N   O P E R A T O R       43
C-----------------------------------------------------------------------
 520  IF (SP.LT.2) GO TO 980
      SP = SP - 1
      TEMP = C(FALSE)
      IF (V(SP).GE.V(SP+1)) TEMP = C(TRUE)
      V(SP) = TEMP
      GO TO 999
C-----------------------------------------------------------------------
C                 <>   C O M P A R I S O N   O P E R A T O R       44
C-----------------------------------------------------------------------
 530  CALL EQUIV (EQUAL)
      IF (ERRNUM.NE.0) GO TO 980
      TEMP = C(FALSE)
      IF (.NOT.EQUAL) TEMP = C(TRUE)
      SP = SP + 1
      V(SP) = TEMP
      GO TO 999
C-----------------------------------------------------------------------
C                  E   X   P
C-----------------------------------------------------------------------
 540  IF (SP.LT.1) GO TO 980
      LFLAG = 5
      XX = EXP (V(SP))
      GO TO 999
C-----------------------------------------------------------------------
C                  E L S E    O P E R A T O R                    53
C-----------------------------------------------------------------------
 550  IF (SP.LT.2) GO TO 980
      SP = SP - 2
      L = STACK(SP+1)
      LINK = STACK(SP+2)
      LFLAG = 3
      GO TO 999
C-----------------------------------------------------------------------
C                  T H E N    O P E R A T O R                    54
C-----------------------------------------------------------------------
 560  IF (SP.LT.3) GO TO 980
      SP = SP - 3
      IF (V(SP+1).GT.0) GO TO 999
         L = STACK(SP+2)
         LINK = STACK(SP+3)
         LFLAG = 3
         GO TO 999
C-----------------------------------------------------------------------
C                  W  H  I  L  E                                 59
C-----------------------------------------------------------------------
 590  IF (SP.LT.3) GO TO 980
      SP = SP - 3
      IF (V(SP+1).GT.0) GO TO 999
         L = STACK(SP+2)
         LINK = STACK(SP+3)
         CP = CP - 3
         LFLAG = 3
         GO TO 999
C-----------------------------------------------------------------------
C                  D U M M Y   E N D   (IF-THEN-ELSE)            96
C-----------------------------------------------------------------------
 600  GO TO 999
C-----------------------------------------------------------------------
C                  W H I L E    S E T - U P                      97
C-----------------------------------------------------------------------
 610  CP = CP+3
      CSTACK(CP) = -61
      CSTACK(CP-1) = LINK
      CSTACK(CP-2) = L + 1
      GO TO 999
C-----------------------------------------------------------------------
C                  S U B S   O P E R A T O R                     98
C                                       subscript evaluation
C-----------------------------------------------------------------------
 620  CALL SUBS
      IF (ERRNUM.EQ.0) GO TO 999
      GO TO 980
C-----------------------------------------------------------------------
C                  S U B S T R   O P E R A T O R                 46
C                                       string -> substring
C-----------------------------------------------------------------------
 630  POTERR = 7
      IF (SP.LT.6) GO TO 980
      POTERR = 8
      IF ((STACK(SP).EQ.2) .OR. (STACK(SP-1).EQ.2) .OR. (STACK(SP-2)
     *   .NE.2)) GO TO 980
      SYTYPE = STACK(SP-5)
      NSIZE = STACK(SP-4)
      IF (((SYTYPE.LT.7) .OR. (SYTYPE.GT.9)) .AND. (SYTYPE.NE.14))
     *   GO TO 980
      LOFF = 0
      IF (SYTYPE.EQ.9) LOFF = NSIZE/1024 - 1
      IF (SYTYPE.EQ.9) NSIZE = MOD (NSIZE, 1024) - LOFF
      IF ((SYTYPE.EQ.7) .AND. (K(NSIZE+1).GT.1)) GO TO 980
      IF (SYTYPE.EQ.7) NSIZE = K(NSIZE+3)
      NCH = V(SP) + 0.01
      J = V(SP-1) + 0.01
      NCH = MIN (NCH, NSIZE)
      IF ((J.GT.NCH) .OR. (J.LE.0) .OR. (NCH.LE.0)) GO TO 980
      STACK(SP-5) = 9
      STACK(SP-4) = 1024 * (J + LOFF)  +  (NCH + LOFF)
      SP = SP - 2
      GO TO 999
C-----------------------------------------------------------------------
C                  !!  O P E R A T O R                          47
C                                       concatanate operator
C-----------------------------------------------------------------------
 640  CALL CONCAT
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                  L E N    O P E R A T O R                      26
C                                       string length operator
C-----------------------------------------------------------------------
 650  POTERR = 7
      IF (SP.LT.4) GO TO 980
      POTERR = 8
      SYTYPE = STACK(SP-3)
      IF (STACK(SP).NE.2) GO TO 980
      IF (((SYTYPE.LT.7) .OR. (SYTYPE.GT.9)) .AND. (SYTYPE.NE.14))
     *   GO TO 980
      NSIZE = STACK(SP-2)
      IOFF = 1
      IF (SYTYPE.EQ.9) IOFF = NSIZE / 1024
      COUNT = NSIZE
      IF (SYTYPE.EQ.7) COUNT = K(NSIZE+3)
      IF (SYTYPE.EQ.9) COUNT = MOD (NSIZE, 1024)
      TAG = STACK(SP-1)
      CALL SPFIL (CH(TAG), COUNT, LOFF)
      LOFF = LOFF - IOFF + 1
      IF (LOFF.LT.0) LOFF = 0
      SP = SP - 3
      V(SP) = LOFF
      STACK(SP) = 1
      GO TO 999
C-----------------------------------------------------------------------
C                  C H A R   O P E R A T O R                   48
C                                       value to string
C-----------------------------------------------------------------------
 660  POTERR = 7
      IF ((SP.LT.1) .OR. (SP.GT.SLIM-3)) GO TO 980
      POTERR = 8
      IF (STACK(SP).EQ.2) GO TO 980
      I = FRMT (9, 4, V(SP), STRING)
      CALL CHTRIM (STRING, 9, STRING, NCH)
      LX = (NCH + 3) / 4
      I = 4 * LX
      CALL CHR2H (I, STRING, 1, X)
      SYTYPE = 14
      LSAVE = L
      CALL LTSTOR
      L = LSAVE
      IF (ERRNUM.EQ.1) ERRNUM = 75
      IF (ERRNUM.NE.0) GO TO 980
      K(LOCSYM+1) = SYTYPE + 16 * NCH
      STACK(SP) = 14
      STACK(SP+1) = NCH
      STACK(SP+2) = TAG
      STACK(SP+3) = 2
      SP = SP + 3
      GO TO 999
C-----------------------------------------------------------------------
C                  V A L U E   O P E R A T O R                  49
C                                       string to number
C-----------------------------------------------------------------------
 670  POTERR = 7
      IF (SP.LT.4) GO TO 980
      POTERR = 8
      IF (STACK(SP).NE.2) GO TO 980
      NSIZE = STACK(SP-2)
      SYTYPE = STACK(SP-3)
      IF (((SYTYPE.LT.7) .OR. (SYTYPE.GT.9)) .AND. (SYTYPE.NE.14))
     *   GO TO 980
      IOFF = 1
      IF (SYTYPE.EQ.9) IOFF = NSIZE / 1024
      COUNT = NSIZE
      IF (SYTYPE.EQ.7) COUNT = K(NSIZE+3)
      IF (SYTYPE.EQ.9) COUNT = MOD (NSIZE, 1024) - IOFF + 1
      IF (COUNT.LE.0) GO TO 980
      CALL H2CHR (COUNT, IOFF, CH(STACK(SP-1)), KARBUF)
      I = 1
      CALL GETNUM (KARBUF, COUNT, I, DBLX)
      SP = SP - 3
      IF (ERRNUM.NE.0) GO TO 980
      V(SP) = DBLX
      STACK(SP) = 1
      GO TO 999
C-----------------------------------------------------------------------
C                  C E I L   O P E R A T O R                    91
C                                       return next higher integer
C-----------------------------------------------------------------------
 680  IF (SP.LT.1) GO TO 980
      POTERR = 8
      IF (STACK(SP).EQ.2) GO TO 980
      LFLAG = 5
      TEMP = ABS(V(SP))
      TEMP = MOD (TEMP, 1.0)
      IF ((V(SP).GT.0.0) .AND. (TEMP.GT.0.0)) TEMP = 1.0 - TEMP
      XX = V(SP) + TEMP
      GO TO 999
C-----------------------------------------------------------------------
C                  F L O O R   O P E R A T O R                  92
C                                       return next lower integer
C-----------------------------------------------------------------------
 690  IF (SP.LT.1) GO TO 980
      POTERR = 8
      IF (STACK(SP).EQ.2) GO TO 980
      LFLAG = 5
      TEMP = ABS (V(SP))
      TEMP = MOD (TEMP, 1.0)
      IF ((V(SP).LT.0.0) .AND. (TEMP.GT.0.0)) TEMP = 1.0 - TEMP
      XX = V(SP) - TEMP
      GO TO 999
C-----------------------------------------------------------------------
C                  A B S   O P E R A T O R                  93
C                                       Return absolute value
C-----------------------------------------------------------------------
 700  IF (SP.LT.1) GO TO 980
      POTERR = 8
      IF (STACK(SP).EQ.2) GO TO 980
      LFLAG = 5
      XX = ABS (V(SP))
      GO TO 999
C-----------------------------------------------------------------------
C                  M A X   O P E R A T O R                  94
C                                       Return max value
C                  M I N   O P E R A T O R                  95
C                                       Return min value
C-----------------------------------------------------------------------
 710  LFLAG = 5
C                                       2nd or only argument
      IF (SP.LT.1) GO TO 980
C                                       single scalar
      IF (SP.EQ.1) THEN
         XX = V(SP)
         SP = SP - 1
C                                       scalar
      ELSE IF (STACK(SP).NE.2) THEN
         XX = V(SP)
         SP = SP - 1
C                                       array
      ELSE
         IF (SP.LT.4) GO TO 980
         SYTYPE = STACK(SP-3)
         POTERR = 8
         IF ((SYTYPE.NE.2) .AND. (SYTYPE.NE.12)) GO TO 980
         NSIZE = STACK(SP-2)
         IF (SYTYPE.EQ.2) NSIZE = K(NSIZE)
         IOFF = STACK(SP-1) - 1
         SP = SP - 4
         XX = C(IOFF+1)
         DO 712 I = 1,NSIZE
            IF (BRANCH.EQ.94) XX = MAX (XX, C(IOFF+I))
            IF (BRANCH.EQ.95) XX = MIN (XX, C(IOFF+I))
 712        CONTINUE
         END IF
C                                       only 1 argument
 715  IF (SP.EQ.0) THEN
         SP = 1
C                                       2nd argument
      ELSE
         IF (STACK(SP).NE.2) THEN
            IF (BRANCH.EQ.94) XX = MAX (V(SP), XX)
            IF (BRANCH.EQ.95) XX = MIN (V(SP), XX)
         ELSE
            POTERR = 7
            IF (SP.LT.4) GO TO 980
            SYTYPE = STACK(SP-3)
            POTERR = 8
            IF ((SYTYPE.NE.2) .AND. (SYTYPE.NE.12)) GO TO 980
            NSIZE = STACK(SP-2)
            IF (SYTYPE.EQ.2) NSIZE = K(NSIZE)
            IOFF = STACK(SP-1) - 1
            SP = SP - 3
            STACK(SP) = IOFF+1
            DO 717 I = 1,NSIZE
               IF (BRANCH.EQ.94) XX = MAX (XX, C(IOFF+I))
               IF (BRANCH.EQ.95) XX = MIN (XX, C(IOFF+I))
 717           CONTINUE
            END IF
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                  C L R T E M P   V E R B                  93
C                                       Clear KKT area down to basics
C-----------------------------------------------------------------------
 720  IF ((KKT(3).GE.KKT(7)) .AND. (KKT(7).GT.0)) THEN
         KKT(3) = KKT(7)
         KKT(10) = MAX (11, KKT(6))
         GO TO 999
      ELSE
         ERRNUM = 7
         END IF
      GO TO 980
C-----------------------------------------------------------------------
C                  R   A   N   D   O   M                           90
C-----------------------------------------------------------------------
 730  LFLAG = 5
      CALL RANDUM (XX)
      GO TO 999
C-----------------------------------------------------------------------
C                  ASIN, ACOS                          (50---51)
C-----------------------------------------------------------------------
 740  IF (SP.LT.1) GO TO 980
      POTERR = 27
      IF (ABS(V(SP)).GT.1.0) GO TO 980
      LFLAG = 5
      XX = ASIN(V(SP)) / PIR
      GO TO 999
 750  IF (SP.LT.1) GO TO 980
      POTERR = 27
      IF (ABS(V(SP)).GT.1.0) GO TO 980
      LFLAG = 5
      XX = ACOS(V(SP)) / PIR
      GO TO 999
C-----------------------------------------------------------------------
C                                        error return
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1034 FORMAT ('KWICK: CORE  ERROR, INDEX',I12,' NOT BETWEEN 1 AND',I6)
 1035 FORMAT ('KWICK: STACK ERROR, INDEX',I12,' NOT BETWEEN 1 AND',I4)
 1287 FORMAT ('(',I3)
 1288 FORMAT (',',I3)
      END
