      SUBROUTINE GCHDRW (X0, Y0, CS, TXT, IC, GBUFF, IERR)
C-----------------------------------------------------------------------
C! writes characters by line drawing to plot file
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2006
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
C   GCHDRW draws with vector command characters to a plot file
C   Inputs:
C      Y0      R       Y position of bLC of character
C      CS      R       X size of character
C      TXT     C*(*)   Character string to draw - trailing blanks count
C      IC      I       > 0 => use 3-color vectors
C   In/Out:
C      X0      R       BLC X position of first character
C                      BLC X position of next character returned
C      GBUFF   I(*)    Plot buffer
C   Outputs:
C      IERR    I       Error code
C   Can draw numbers, -, ., and blank for now
C-----------------------------------------------------------------------
      INTEGER   IC, GBUFF(256), IERR
      REAL      X0, Y0, CS
      CHARACTER TXT*(*)
C
      INTEGER   NC, LC, IT, NL(43), NLS(7), NLN(10), NLA(26), I, INUM,
     *   IALPHA
      REAL      YS, XL(2,16,43), X, Y, XLS(2,16,7), XLN(2,16,10),
     *   XLA(2,16,26)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (XL(1,1,1), XLA), (XL(1,1,27), XLN),
     *   (XL(1,1,37), XLS)
      EQUIVALENCE (NL(1), NLA), (NL(27), NLN), (NL(37), NLS)
C                                       special symbols
      DATA NLS /5, 2, 6, 6, 4, 6, 4/
C                                       period
      DATA XLS /0.27,0.04, 0.33,0.04, 0.33,.10, 0.27,0.1, 0.27,0.04,
     *   22*0.0,
C                                       minus
     *   0.20,0.50, 0.80,0.50, 28*0.0,
C                                       open paren
     *   0.60,1.00, 0.50,0.90, 0.40,0.70, 0.40,0.30, 0.50,0.10,
     *   0.60,0.00, 20*0.0,
C                                       close paren
     *   0.40,1.00, 0.50,0.90, 0.60,0.70, 0.60,0.30, 0.50,0.10,
     *   0.40,0.00, 20*0.0,
C                                       equals
     *   0.20,0.35, 0.80,0.35, -0.20,0.65, 0.80,0.65, 24*0.0,
C                                       asterisk
     *   0.50,0.10, 0.50,0.90, -0.22,0.22, 0.78,0.78, -0.22,0.78,
     *   0.78,0.22, 20*0.0,
C                                       plus
     *   0.20,0.50, 0.80,0.50, -0.50,0.20, 0.50,0.80, 24*0.0/
C                                       Numbers
      DATA NLN /9, 5, 6, 13, 4, 11, 11, 3, 16, 10/
C                                       0
      DATA XLN /0.85,0.00, 0.15,0.00, 0.00,0.15, 0.00,0.85, 0.15,1.00,
     *   0.85,1.00, 1.00,0.85, 1.00,0.15, 0.85,0.00, 14*0.0,
C                                       1
     *   0.30,0.70, 0.50,1.00, 0.50,0.00, 0.90,0.00, 0.10,0.00,
     *   22*0.0,
C                                       2
     *   0.00,0.85, 0.15,1.00, 0.85,1.00, 1.00,0.85, 0.00,0.00,
     *   1.00,0.00, 20*0.0,
C                                       3
     *   0.00,0.85, 0.15,1.00, 0.85,1.00, 1.00,0.85, 1.00,0.65,
     *   0.85,0.50, 0.50,0.50, 0.85,0.50, 1.00,0.35, 1.00,0.15,
     *   0.85,0.00, 0.15,0.00, 0.00,0.15, 6*0.0,
C                                       4
     *   1.00,0.50, 0.00,0.50, 0.95,1.00, 0.95,0.00, 24*0.0,
C                                       5
     *   1.00,1.00, 0.00,1.00, 0.00,0.40, 0.15,0.50, 0.85,0.50,
     *   1.00,0.35, 1.00,0.15, 0.85,0.00, 0.15,0.00, 0.00,0.15,
     *   12*0.0,
C                                       6
     *   1.00,0.85, 0.85,1.00, 0.20,1.00, 0.00,0.80, 0.00,0.15,
     *   0.15,0.00, 0.85,0.00, 1.00,0.15, 1.00,0.35, 0.85,0.50,
     *   0.15,0.50, 0.00,0.35, 8*0.0,
C                                       7
     *   0.00,1.00, 1.00,1.00, 0.35,0.00, 26*0.0,
C                                       8
     *   0.15,0.50, 0.00,0.65, 0.00,0.85, 0.15,1.00, 0.85,1.00,
     *   1.00,0.85, 1.00,0.65, 0.85,0.50, 0.15,0.50, 0.00,0.35,
     *   0.00,0.15, 0.15,0.00, 0.85,0.00, 1.00,0.15, 1.00,0.35,
     *   0.85,0.50,
C                                       9
     *   1.00,0.65, 0.85,0.50, 0.15,0.50, 0.00,0.65, 0.00,0.85,
     *   0.15,1.00, 0.85,1.00, 1.00,0.85, 1.00,0.35, 0.50,0.00,
     *   12*0.0/
C                                       Letters
      DATA NLA /5, 11, 8, 7, 7, 6, 10, 6, 6, 7, 5, 3, 5, 4,
     *   9, 7, 11, 9, 10, 4, 6, 3, 5, 4, 5, 4/
C                                       A
      DATA XLA /0.00,0.00, 0.50,1.00, 1.00,0.00, 0.80,0.40, 0.20,0.40,
     *   22*0.0,
C                                       B
     *   0.00,0.50, 0.85,0.50, 1.00,0.65, 1.00,0.85, 0.85,1.00,
     *   0.00,1.00, 0.00,0.00, 0.85,0.00, 1.00,0.15, 1.00,0.45,
     *   0.85,0.50, 10*0.0,
C                                       C
     *   1.00,0.85, 0.85,1.00, 0.15,1.00, 0.00,0.85, 0.00,0.15,
     *   0.15,0.00, 0.85,0.00, 1.00,0.15, 16*0.0,
C                                       D
     *   0.00,0.00, 0.85,0.00, 1.00,0.15, 1.00,0.85, 0.85,1.00,
     *   0.00,1.00, 0.00,0.00, 18*0.0,
C                                       E
     *   1.00,1.00, 0.00,1.00, 0.00,0.50, 0.60,0.50, 0.00,0.50,
     *   0.00,0.00, 1.00,0.00, 18*0.0,
C                                       F
     *   0.00,0.00, 0.00,0.50, 0.60,0.50, 0.00,0.50, 0.00,1.00,
     *   1.00,1.00, 20*0.0,
C                                       G
     *   1.00,0.85, 0.85,1.00, 0.15,1.00, 0.00,0.85, 0.00,0.15,
     *   0.15,0.00, 0.85,0.00, 1.00,0.15, 1.00,0.50, 0.70,0.50,
     *   12*0.0,
C                                       H
     *   0.00,0.00, 0.00,1.00, 0.00,0.50, 1.00,0.50, 1.00,1.00,
     *   1.00,0.00, 20*0.0,
C                                       I
     *   0.20,0.00, 0.80,0.00, 0.50,0.00, 0.50,1.00, 0.20,1.00,
     *   0.80,1.00, 20*0.0,
C                                       J
     *   0.20,1.00, 0.80,1.00, 0.60,1.00, 0.60,0.15, 0.45,0.00,
     *   0.25,0.00, 0.10,0.15, 18*0.0,
C                                       K
     *   0.00,1.00, 0.00,0.00, -0.85,1.00, 0.00,0.50, 1.00,0.00,
     *   22*0.0,
C                                       L
     *   0.00,1.00, 0.00,0.00, 1.00,0.00, 26*0.0,
C                                       M
     *   0.00,0.00, 0.00,1.00, 0.50,0.6, 1.00,1.00, 1.00,0.00,
     *   22*0.0,
C                                       N
     *   0.00,0.00, 0.00,1.00, 1.00,0.00, 1.00,1.00, 24*0.0,
C                                       O
     *   0.85,0.00, 0.15,0.00, 0.00,0.15, 0.00,0.85, 0.15,1.00,
     *   0.85,1.00, 1.00,0.85, 1.00,0.15, 0.85,0.00, 14*0.0,
C                                       P
     *   0.00,0.00, 0.00,1.00, 0.85,1.00, 1.00,0.85, 1.00,0.65,
     *   0.85,0.50, 0.00,0.50, 18*0.0,
C                                       Q
     *   0.85,0.00, 0.15,0.00, 0.00,0.15, 0.00,0.85, 0.15,1.00,
     *   0.85,1.00, 1.00,0.85, 1.00,0.15, 0.85,0.00, -0.70,0.30,
     *   1.00,0.00, 10*0.0,
C                                       R
     *   0.00,0.00, 0.00,1.00, 0.85,1.00, 1.00,0.85, 1.00,0.65,
     *   0.85,0.50, 0.00,0.50, -0.70,0.50, 1.00,0.00, 14*0.0,
C                                       S
     *   1.00,0.85, 0.85,1.00, 0.15,1.00, 0.00,0.85, 0.00,0.65,
     *   1.00,0.35, 1.00,0.15, 0.85,0.00, 0.15,0.00, 0.00,0.15,
     *   12*0.0,
C                                       T
     *   0.00,1.00, 1.00,1.00, 0.50,1.00, 0.50,0.00, 24*0.0,
C                                       U
     *   0.00,1.00, 0.00,0.15, 0.15,0.00, 0.85,0.00, 1.00,0.15,
     *   1.00,1.00, 20*0.0,
C                                       V
     *   0.00,1.00, 0.50,0.00, 1.00,1.00, 26*0.0,
C                                       W
     *   0.00,1.00, 0.25,0.00, 0.50,0.50, 0.75,0.00, 1.00,1.00,
     *   22*0.0,
C                                       X
     *   0.00,0.00, 1.00,1.00, -1.00,0.00, 0.00,1.00, 24*0.0,
C                                       Y
     *   0.00,1.00, 0.50,0.55, 1.00,1.00, 0.50,0.55, 0.50,0.00,
     *   22*0.0,
C                                       Z
     *   0.00,1.00, 1.00,1.00, 0.00,0.00, 1.00,0.00, 24*0.0/
C-----------------------------------------------------------------------
C                                       decide scale
      IERR = 0
      NC = LEN (TXT)
      YS = 1.4 * CS
      DO 100 LC = 1,NC
         INUM = ICHAR (TXT(LC:LC)) - ICHAR ('0')
         IALPHA = ICHAR (TXT(LC:LC)) - ICHAR ('A')
         IF ((IALPHA.GE.0) .AND. (IALPHA.LE.25)) THEN
            IT = IALPHA + 1
         ELSE IF ((INUM.GE.0) .AND. (INUM.LE.9)) THEN
            IT = INUM + 27
         ELSE IF (TXT(LC:LC).EQ.'.') THEN
            IT = 37
         ELSE IF (TXT(LC:LC).EQ.'-') THEN
            IT = 38
         ELSE IF (TXT(LC:LC).EQ.'(') THEN
            IT = 39
         ELSE IF (TXT(LC:LC).EQ.')') THEN
            IT = 40
         ELSE IF (TXT(LC:LC).EQ.'=') THEN
            IT = 41
         ELSE IF (TXT(LC:LC).EQ.'*') THEN
            IT = 42
         ELSE IF (TXT(LC:LC).EQ.'+') THEN
            IT = 43
         ELSE
            IT = 0
            X0 = X0 +  1.1 * CS
            GO TO 100
            END IF
         DO 20 I = 1,NL(IT)
            X = X0 + CS * ABS (XL(1,I,IT))
            Y = Y0 + YS * XL(2,I,IT)
            IF ((I.EQ.1) .OR. (XL(1,I,IT).LT.0.0)) THEN
               CALL GPOS (X, Y, GBUFF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) 'GPOS', IERR
                  GO TO 990
                  END IF
            ELSE IF (IC.GT.0) THEN
               CALL G3VEC (X, Y, GBUFF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) 'G3VEC', IERR
                  GO TO 990
                  END IF
            ELSE
               CALL GVEC (X, Y, GBUFF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) 'GVEC', IERR
                  GO TO 990
                  END IF
               END IF
 20         CONTINUE
         IF (IT.EQ.37) THEN
            X0 = X0 + 0.85*CS
         ELSE
            X0 = X0 + 1.4*CS
            END IF
 100     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GCHDRW: ',A,' RETURNS ERROR CODE',I4)
      END
