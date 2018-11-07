      SUBROUTINE KEYIN (KEYS, VALUES, VALCHR, N, MRKEND, MODE, LUN,
     *   FIND, IERR)
C-----------------------------------------------------------------------
C! AIPS version of CIT parsing routine
C# Parsing
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2007, 2012, 2017
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
C   Standard Fortran version of the CIT VLBI Keyin subroutines.
C   These subroutines read keyed parameters on cards images.
C   The text file should be opened via a call to ZTXOPN before the
C   first call to KEYIN and closed via a call to ZTXCLS after the last
C   call.  (HINT: use LUN = 10)
C   Note: in this version time like entries in the form hh:mm:ss
C   will be returned in hours.
C  Inputs:
C     KEYS(N)    C*8  Array of parameter names .
C                     Array names should have the last characters
C                     indicate the element number.  Should all be
C                     in upper case characters.
C     VALUES(N)  D    array to receive numeric values or defaults,
C                     each value corresponds to a KEY.
C     VALCHR(N)  C*8  array to receive character values or defaults,
C                     each value corresponds to a KEY.
C     N          I    number of parameters (dimension of keys, values)
C     MRKEND     C*?  special keyword to indicate end of input
C     MODE       I    1 = turn on reflection, 0 = turn off
C                     2 = interactive mode (prompts for input,
C                         no reflection, no limit on errors)
C                     3 = Pass values until ENDMRK, File should not
C                         contain keywords.
C                     4 = As for mode 3, but with reflection
C                     Note: currently only reads from a file.
C     LUN        I    LUN to read from (used in call to ZTXOPN)
C     FIND       I    FTAB pointer for input. (from ZTXOPN)
C  Outputs:
C     VALUES(N)  D    Values or input defaults.
C     N          I    (MODE=3 or 4 only) number of values found
C     IERR       I    error code, 0=>OK, 1=>EOF found, 2=>Error
C-----------------------------------------------------------------------
      CHARACTER VALCHR(*)*(*), KEYS(*)*(*), ENDMRK*16, LINE*2560,
     *   WORD*16, DELIM*1, MRKEND*(*), CWORD*16, CBLANK*16
      INTEGER   MODE, LUN, FIND, IERR, ERRCNT, N, NK, JERR, NCHAR, I,
     *   MAXREC, I1, NLETS, K, NW, ITEMP, IVALUE, I1T, NKT, JTRIM,
     *   NCOMP, LNGCNT
      LOGICAL   REFLEC, ACTIVE, EQUAL
      DOUBLE PRECISION    VALUES(*), XVALUE, ZVALUE
      CHARACTER TAB, CR
      CHARACTER ZSCHAR
      EXTERNAL  ZSCHAR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CBLANK /' '/
C-----------------------------------------------------------------------
      REFLEC = ((MODE.EQ.1).OR.(MODE.EQ.4))
      ACTIVE = .FALSE.
      ERRCNT = 0
      LNGCNT = 0
      IERR = 0
      NK = 1
      TAB = ZSCHAR ('tab')
      CR  = ZSCHAR ('cr')
      ENDMRK = MRKEND
      NCOMP = LEN(KEYS(1))
C                                       Read next record.
 100  CALL ZTXIO ('READ', LUN,  FIND,  LINE, JERR)
      IF (JERR.EQ.2) GO TO 340
      IF (JERR.NE.0) GO TO 999
C                                       Reflect line ?
         IF (REFLEC) THEN
            MSGTXT = LINE(:64)
            CALL MSGWRT (3)
            END IF
C                                       skip simple comment
         I = 1
         IF (LINE(I:I).EQ.'!') GO TO 100
C                                       'MAXREC' points to last
C                                       non-blank character in record)
         MAXREC = JTRIM (LINE)
 120     IF (ACTIVE) GO TO 270
C                                       Look for keyword;
 130     CALL CITSKP (LINE, MAXREC, I)
         IF (I.GT.MAXREC) GO TO 100
         IF (LINE(I:I).EQ.'!') GO TO 100
         I1 = I
         NLETS = 1
 140     I = I + 1
         IF (I.LE.MAXREC) THEN
            IF ((LINE(I:I).EQ.' ') .OR. (LINE(I:I).EQ.'=') .OR.
     *          (LINE(I:I).EQ.'(') .OR. (LINE(I:I).EQ.TAB) .OR.
     *          (LINE(I:I).EQ.CR)) GO TO 150
               NLETS = NLETS + 1
               GO TO 140
            END IF
C                                       Copy into WORD
 150     IF (NLETS.GT.16) THEN
            I = MIN (NLETS, 50) + I1 - 1
            WRITE (MSGTXT,1150) LINE(I1:I)
            IERR = NLETS
            LNGCNT = LNGCNT + 1
            GO TO 330
            END IF
         WORD = LINE(I1:I1+NLETS-1)
C                                       Convert to upper case.
         CALL CHLTOU (16, WORD)
C                                       Scan 'KEYS' for 'WORD';
C                                       error if not found
C                                       (check first that 'WORD'
C                                       is not 'ENDMRK'
C                                       or a special command-word)
         CALL CITCPR (WORD, ENDMRK, NCOMP, '?', EQUAL)
         IF (EQUAL) GO TO 340
C                                       Check for mode=3 or 4
         IF ((MODE.EQ.3).OR.(MODE.EQ.4)) I = I1
         IF ((MODE.EQ.3).OR.(MODE.EQ.4)) GO TO 200
         DO 160 K = 1,N
            IF ((MODE.NE.3).AND.(MODE.NE.4)) NK = K
            CWORD = KEYS(K)
            CALL CITCPR (WORD, CWORD, NCOMP, '?', EQUAL)
            IF (EQUAL) GO TO 170
 160        CONTINUE
C                                       Error: unknown parameter
         IERR = 4
         GO TO 330
C                                       Look for (subscript) to keyword
 170     IF (LINE(I:I).NE.'(') GO TO 190
            I = I + 1
            CALL CITC2I (LINE, MAXREC, I, IVALUE)
            NK = NK - 1 + IVALUE
C                                       subscript error?
            IERR = 1
            IF (LINE(I:I).NE.')') GO TO 330
            I = I+1
 180        IF (NK.LT.1) GO TO 330
C                                       too many values?
            IERR = 2
            IF (NK.GT.N) GO TO 330
            CWORD = KEYS(K)
            CALL CITCPR (WORD, CWORD, NCOMP, '?', EQUAL)
            IF (.NOT.EQUAL) GO TO 330
C                                       Look for value
C                                       after optional ' = '
 190     VALUES(NK) = 0.0
         CALL CITSKP (LINE, MAXREC, I)
         IF (I.GT.MAXREC) GO TO 100
         IF (LINE(I:I).NE.'=') GO TO 200
            I = I + 1
            CALL CITSKP (LINE, MAXREC, I)
C                                       missing value after ' = '?
            IERR = 5
            IF (I.GT.MAXREC) GO TO 330
C                                       Numerical value
 200     IF ((LINE(I:I).EQ.'''') .OR. (LINE(I:I).EQ.'"')) GO TO 210
         IF (LINE(I:I).EQ.'(') GO TO 205
            CALL CITC2R (LINE, MAXREC, I, XVALUE)
            VALUES(NK) = XVALUE
            GO TO 260
C                                       Arithmetic expression
 205        CALL CITEXP (LINE, MAXREC, I, ZVALUE, IERR)
            VALUES(NK) = ZVALUE
            IF (IERR.NE.0) GO TO 330
            GO TO 260
C                                       Textual value - 'NW' words
C                                       of 8 characters
C                                       Find number of blank keywords
 210     NW = 1
         VALCHR(NK) = '        '
 220     IF (NK+NW.GT.N) GO TO 230
         CWORD = KEYS(NK+NW)
         CALL CITCPR (CWORD, CBLANK, NCOMP, '?', EQUAL)
         IF (.NOT.EQUAL) GO TO 230
            VALCHR(NK+NW) = '        '
            NW = NW + 1
            GO TO 220
 230     DELIM = LINE(I:I)
         I1 = I + 1
         NLETS = 0
 240     I = I + 1
C                                       missing quote?
         IERR = 3
         IF (I.GT.MAXREC) GO TO 330
         IF (LINE(I:I).EQ.DELIM) GO TO 250
            NLETS = NLETS + 1
            GO TO 240
 250     ITEMP = 8 * NW
         NCHAR = MIN (ITEMP, NLETS)
         NKT = NK
         I1T = I1
C                                       Loop filling words
 255        ITEMP = MIN (8, NCHAR)
            VALCHR(NKT) = LINE(I1T:I1T+ITEMP-1)
            NCHAR = NCHAR - 8
            I1T = I1T + 8
            NKT = NKT + 1
            IF (NCHAR.GT.0) GO TO 255
         I = I + 1
         NK = NK + NW - 1
C                                       Look for another value,
C                                       if allowed
 260     IF ((MODE.EQ.3).OR.(MODE.EQ.4)) GO TO 280
         CALL CITSKP (LINE, MAXREC, I)
         IF (I.GT.MAXREC) GO TO 100
         IF (LINE(I:I).NE.',') GO TO 130
         I = I + 1
         ACTIVE = .TRUE.
         CALL CITSKP (LINE, MAXREC, I)
         IF (I.GT.MAXREC) GO TO 100
 270     ACTIVE = .FALSE.
         NK = NK + 1
         GO TO 180
 280     NK = NK + 1
         CALL CITSKP (LINE, MAXREC, I)
         IF (I.GT.MAXREC) GO TO 100
         GO TO 130
C-----------------------------------------------------------------------
C                                       Error report
 330  IF (IERR.EQ.1)  WRITE (MSGTXT,1331) WORD
      IF (IERR.EQ.2)  WRITE (MSGTXT,1332) WORD
      IF (IERR.EQ.3)  WRITE (MSGTXT,1333)
      IF (IERR.EQ.4)  WRITE (MSGTXT,1334) WORD
      IF (IERR.EQ.5)  WRITE (MSGTXT,1335) WORD
      IF (IERR.EQ.11) WRITE (MSGTXT,1336)
      IF (IERR.EQ.12) WRITE (MSGTXT,1337)
      IF (IERR.EQ.13) WRITE (MSGTXT,1338)
      IF (IERR.EQ.14) WRITE (MSGTXT,1339)
      IF ((IERR.GT.0) .AND. (IERR.LT.16)) ERRCNT = ERRCNT + 1
C                                       Report error
      IF (IERR.NE.0) CALL MSGWRT (8)
      IF (ERRCNT.LT.10) GO TO 100
C                                       Finished
 340  IERR = 0
      IF (JERR.EQ.2) IERR = 1
      IF (ERRCNT.GT.0) IERR = 2
      IF  ((MODE.EQ.3).OR.(MODE.EQ.4)) N = NK - 1
      IF (LNGCNT.GT.0) THEN
         WRITE (MSGTXT,1340) LNGCNT
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT ('KEYIN: PARAMETER TOO LONG ''',A,'''')
 1331 FORMAT ('KEYIN: ERROR IN SUBSCRIPT OF ',A)
 1332 FORMAT ('KEYIN: ERROR, TOO MANY VALUES FOR ',A)
 1333 FORMAT ('KEYIN: ERROR, UNMATCHED QUOTE ')
 1334 FORMAT ('KEYIN: ERROR, UNKNOWN PARAMETER ',A)
 1335 FORMAT ('KEYIN: ERROR IN VALUE OF ',A)
 1336 FORMAT ('KEYIN: ERROR, TOO MANY PARENTHESES')
 1337 FORMAT ('KEYIN: ERROR, EXPECTING '')'' ')
 1338 FORMAT ('KEYIN: ERROR, VALUE EXPECTED AFTER OPERATOR')
 1339 FORMAT ('KEYIN: ERROR, ATTEMPT TO DIVIDE BY ZERO')
 1340 FORMAT ('KEYIN: IGNORED',I5,' INPUT LINES DUE TO LONG SYMBOLS')
      END
