      SUBROUTINE CHGRIP (ITYP, LUN, IND, GH, CB, STR, IERR)
C-----------------------------------------------------------------------
C! reads and writes strings to the Gripe file
C# Gripes
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C   CHGRIP reads or writes a string to the Gripe file.  When writing,
C   it looks for the strings '_END' and '_end' and replaces the
C   characters {, }, and \ with [, ], and |, respectively.  Then it
C   moves the string with a prefix and suffix to the file, writing
C   and expanding as needed.  When reading it returns the next
C   string in the file removing the prefixes and suffixes and appending
C   trailing blanks to fill the string.
C   Inputs: ITYP  I         0 -> bl { // STR // } bl (write 1 line)
C                           1 -> bl { // STR // crlf (write 1st of n)
C                           2 -> bl bl // STR // crlf (write m of n)
C                           unless end, then ... // }
C                           3 -> read next string
C           LUN   I(2)      Open file logical unit number, 2nd e-mail
C           IND   I(2)      FTAB pointer for open file
C   In/out: GH    I(256)    GH(1) : current file size in blocks
C                             (3) : current record number
C                             (4) : current character position
C           CB    H(256)    Character buffer (1024 chars)
C           STR   C*(*)     String (<= 1000 chars)
C   Output: IERR  I         Error code: 0 => ok and normal
C                              99 => _END found (type 1,2)
C                              -1 => _FORGET found (type 1,2)
C                             100 => string not done (reads)
C                             else serious error (from ZFIO,...)
C-----------------------------------------------------------------------
      INTEGER   ITYP, LUN(2), IND(2), GH(256), IERR
      HOLLERITH CB(256)
      CHARACTER STR*(*)
C
      CHARACTER TSTR*1024, PHNAME*48, IEND*4, LEND*4, IFORG*7, LFORG*7,
     *   IBSL*2, IT*1
      INTEGER   CPOS, CSIZ, CHLIM, JTRIM, IL, ILP, I2, J1, J2, I, JL,
     *   ICR, ILF, NREC, CREC
      LOGICAL   EQUAL
      INCLUDE 'INCS:DDCH.INC'
      DATA IEND, LEND /'_END','_end'/
      DATA IFORG, LFORG /'_FORGET','_forget'/
      DATA ICR, ILF /13, 10/
      DATA IBSL /'\\'/
C-----------------------------------------------------------------------
C                                       Check ITYP
      IERR = 2
      IF ((ITYP.LT.0) .OR. (ITYP.GT.3)) GO TO 999
      IERR = 0
C                                       Parms
      CREC = GH(3)
      CPOS = GH(4)
      CSIZ = GH(1)
      CHLIM = 1024
C                                       WRITE
      IF (ITYP.EQ.3) GO TO 100
C                                       Get actual length of STR
         IL = JTRIM (STR)
C                                       Check for _forget
         ILP = INDEX (STR(1:IL), IFORG)
         IF (ILP.EQ.0) ILP = INDEX (STR(1:IL), LFORG)
         IF (ILP.GT.0) THEN
            IERR = -1
            GO TO 999
            END IF
C                                       Check for _end
         ILP = INDEX (STR(1:IL), IEND)
         IF (ILP.EQ.0) ILP = INDEX (STR(1:IL), LEND)
         IF (ILP.GT.0) IL = ILP - 1
         DO 20 I = 1,IL
            IF (STR(I:I).EQ.IBSL(1:1)) STR(I:I) = '|'
            IF (STR(I:I).EQ.'{') STR(I:I) = '['
            IF (STR(I:I).EQ.'}') STR(I:I) = ']'
 20         CONTINUE
C                                       Build copy string
 30      TSTR = ' '
         IF (ITYP.NE.2) TSTR(2:2) = '{'
         IF (IL.GT.0) THEN
            TSTR(3:2+IL) = STR(1:IL)
         ELSE
            IL = 1
            TSTR(3:3) = ' '
            END IF
         IL = IL + 2
C                                       End with Cr Lf
         IF ((ITYP.NE.0) .AND. (ILP.LE.0)) THEN
            JL = IL
            IL = IL + 1
            TSTR(IL:IL) = CHAR (ICR)
            IL = IL + 1
            TSTR(IL:IL) = CHAR (ILF)
         ELSE
C                                       End with bracket
            I2 = IL + 1
            TSTR(I2:I2+1) = '} '
            JL = I2
            IL = IL + 2
            END IF
C                                       Write it out
         IF (LUN(2).GT.0) THEN
            CALL ZTXIO ('WRIT', LUN(2), IND(2), TSTR(2:JL), IERR)
            IF (IERR.NE.0) THEN
               CALL ZTXCLS (LUN(2), IND(2), IERR)
               LUN(2) = 0
               END IF
            END IF
         I = IL + CPOS - 1
         J1 = MIN (I, CHLIM) - CPOS + 1
         J2 = I - CHLIM
         CALL CHR2H (J1, TSTR, CPOS, CB)
         CPOS = CPOS + J1
         IF (CPOS.GT.CHLIM) THEN
            CALL ZFIO ('WRIT', LUN, IND, CREC, CB, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Blank fill record
            CALL RFILL (256, HBLANK, CB)
            CREC = CREC + 1
            CPOS = 1
            J1 = J1 + 1
            CALL CHR2H (J2, TSTR(J1:), CPOS, CB)
            CPOS = CPOS + J2
C                                       Expand file to hold record
            IF (CREC.GT.CSIZ) THEN
               CALL ZPHFIL ('GR', 1, 0, 0, PHNAME, IERR)
               NREC = 1
               CALL ZEXPND (LUN, 1, PHNAME, NREC, IERR)
               IF (IERR.NE.0) GO TO 999
               CSIZ = CSIZ + NREC
               END IF
            END IF
         GH(3) = CREC
         GH(4) = CPOS
         GH(1) = CSIZ
C                                       Was an _end or _END found?
         IF (ILP.GT.0) IERR = 99
         GO TO 999
C                                       READ
 100  CONTINUE
         IL = 0
         ILP = LEN(STR)
         STR = ' '
 105     J1 = CPOS
         J2 = CHLIM
         DO 110 I = J1,J2
            CALL H2CHR (1, I, CB, IT)
            IF ((IL.EQ.0) .AND. ((IT.EQ.' ') .OR. (IT.EQ.'{')))
     *         GO TO 110
            IF (IT.EQ.'}') GO TO 120
            IF (IT.EQ.CHAR(ICR)) GO TO 125
            IF (IL.GE.ILP) GO TO 130
            IL = IL + 1
            STR(IL:IL) = IT
 110        CONTINUE
         CREC = CREC + 1
         CPOS = 1
         CALL ZFIO ('READ', LUN, IND, CREC, CB, IERR)
         IF (IERR.NE.0) GO TO 999
         GO TO 105
C                                       Got it with a }
 120     CONTINUE
            EQUAL = .FALSE.
            CPOS = I + 1
            GO TO 135
C                                       With a Cr Lf
 125     CONTINUE
            EQUAL = .TRUE.
            CPOS = I + 2
            GO TO 135
C                                       Overran output STR
 130     CONTINUE
            EQUAL = .TRUE.
            CPOS = I
C                                       Read next rec if needed
 135     IF (CPOS.LE.CHLIM) GO TO 140
            CREC = CREC + 1
            CALL ZFIO ('READ', LUN, IND, CREC, CB, IERR)
            IF (IERR.NE.0) GO TO 999
            CPOS = CPOS - CHLIM
C                                       Return pointers
 140     GH(3) = CREC
         GH(4) = CPOS
         IF (EQUAL) IERR = 100
C
 999  RETURN
      END
