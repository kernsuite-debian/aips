      PROGRAM NLINE
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
C-----------------------------------------------------------------------
C     Kluge program to print out the first \n-line (network address) in
C     each group of blank-line-delimited lines in a file.
C     Built to produce a network address distribution list for CPs with
C     E-mail addresses in WHOGETS who have not returned a survey form,
C     and who therefore do not have the Q87 attribute set (use SORT1 to
C     produce a "CP -COM -BA -Q87" file).
C     DCW, NRAO-CV, 11Jan88.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER     L1, L2, L5, L6, ITRIM, BCOUNT, NCOUNT
      CHARACTER   LINE*100, INP*30, FNAME*60
      LOGICAL     NSEEN
C
      L1 = 1
      L2 = 2
      L5 = 5
      L6 = 6
C                                       Open files:
 110  CONTINUE
      WRITE (L6, 1110)
      READ  (L5, 1115, END=999, ERR=110) INP
      FNAME = INP(:ITRIM(INP)) // '.ADR'
      OPEN (UNIT=L1, FILE=FNAME, STATUS='OLD', ERR=120)
         GO TO 130
 120  CONTINUE
         WRITE (L6, 1120) FNAME(1:ITRIM(FNAME))
         GO TO 110
 130  CONTINUE
      WRITE (L6, 1130) 1, FNAME
      FNAME = INP(:ITRIM(INP)) // '.DIS'
      WRITE (L6, 1130) 2, FNAME
      OPEN (UNIT=L2, FILE=FNAME, STATUS='NEW',
     *      CARRIAGECONTROL='LIST', ERR=140)
         GO TO 150
 140  CONTINUE
         WRITE (L6, 1120) FNAME(1:ITRIM(FNAME))
         GO TO 110
 150  CONTINUE
C
      NSEEN = .FALSE.
      BCOUNT = 0
      NCOUNT = 0
C
 200  READ (L1, '(A)', END=900) LINE
         IF (LINE.EQ.' ') THEN
               NSEEN = .FALSE.
               BCOUNT = BCOUNT + 1
         END IF
         IF (.NOT.NSEEN) THEN
            IF (LINE(1:3).EQ.'\n ') THEN
               WRITE (L2, '(A)') LINE(4:ITRIM(LINE))
               NCOUNT = NCOUNT + 1
               NSEEN = .TRUE.
            END IF
         END IF
         GO TO 200
C
 900  CONTINUE
      CLOSE (UNIT = L1)
      CLOSE (UNIT = L2)
      WRITE (L6, '(''BCOUNT='', I4, '', NCOUNT='', I4)') BCOUNT, NCOUNT
 999  CONTINUE
C-----------------------------------------------------------------------
 1110 FORMAT (' Enter input address-list-file (w/o .ADR):', $)
 1115 FORMAT (A30)
 1120 FORMAT (' Cannot open file ', A)
 1130 FORMAT (' L', I1, '=', A)
      END
      INTEGER FUNCTION ITRIM (STRING)
C-----------------------------------------------------------------------
C   Function to determine length of a string. I.e., it trims trailing
C   blanks. Use with calls like:
C         TRIMMED = GROSS(1:ITRIM(GROSS))
C   DCW, NRAO-CV, 30Nov82.
C-----------------------------------------------------------------------
      IMPLICIT  NONE
      CHARACTER STRING*(*)
C
      ITRIM = LEN (STRING) + 1
 10   CONTINUE
         ITRIM = ITRIM - 1
         IF (ITRIM.LT.1) GO TO 999
         IF (STRING(ITRIM:ITRIM).EQ.' ') GO TO 10
 999  RETURN
      END
