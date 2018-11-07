      SUBROUTINE ZTAP2 (OP, FCB, IERR)
C-----------------------------------------------------------------------
C! position (forward/back record/file), write EOF, etc. for tapes
C# Z2 Tape
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
C   Performs standard tape manipulating functions.
C   Inputs:
C      OP      C*4   Operation to be performed:
C                    'REWI' = rewind the tape on unit LUN
C                    'ADVF' = advance file marks
C                    'BAKF' = backspace file marks.
C                    'ADVR' = advance records
C                    'BAKR' = backspace records.
C                    'WEOF' = write end of file on unit LUN
C                    'BEGW' = mark to begin write (bakf/weof..)
C                    'AEOI' = advance to end-of-information (eg 2 EOFs)
C      FCB     I(*)  File control block for open tape device
C   Outputs:
C      IERR  I       Error return: 0 => ok
C                       3 = I/O error.
C                       4 = End Of File
C                       5 = Beginning Of Medium
C                       6 = End Of Medium
C   VMS version
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   FCB(*), IERR
C
      INTEGER   FUNC, P1, JERR, SYS$QIOW, ITEMP(2), I, ZAND
      INTEGER*2 I2TEMP(4)
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE '($IODEF)'
      INCLUDE '($SSDEF)'
      INCLUDE '($MTDEF)'
      EQUIVALENCE (I2TEMP, ITEMP)
C-----------------------------------------------------------------------
C                                       Check/set opcodes
      IF (OP.EQ.'REWI') THEN
         FUNC = IO$_REWIND
         P1 = 0
      ELSE IF (OP.EQ.'ADVF') THEN
         FUNC = IO$_SKIPFILE
         P1 = 1
      ELSE IF (OP.EQ.'ADVR') THEN
         FUNC = IO$_SKIPRECORD
	 P1 = 1
      ELSE IF (OP.EQ.'BAKF') THEN
         FUNC = IO$_SKIPFILE
         P1 = -1
      ELSE IF (OP.EQ.'BAKR') THEN
         FUNC = IO$_SKIPRECORD
         P1 = -1
      ELSE IF (OP.EQ.'WEOF') THEN
         FUNC = IO$_WRITEOF
         P1 = 0
      ELSE
         IERR = 2
         GO TO 999
         END IF
C                                       Do the QIO
      IF (P1.NE.0) THEN
         JERR = SYS$QIOW (%VAL(FCB(1+FCBSP1)), %VAL(FCB(1+FCBFD)),
     *      %VAL(FUNC), FCB(1+FCBSP2), , , %VAL(P1), , , , , )
      ELSE
         JERR = SYS$QIOW (%VAL(FCB(1+FCBSP1)), %VAL(FCB(1+FCBFD)),
     *      %VAL(FUNC), FCB(1+FCBSP2), , , , , , , , )
         END IF
C                                       error testing
      ITEMP(1) = FCB(1+FCBSP2)
      ITEMP(2) = FCB(2+FCBSP2)
      IF (JERR.NE.1) THEN
         IERR = 3
         FCB(1+FCBERR) = JERR
      ELSE
         JERR = I2TEMP(1)
         IF (JERR.NE.1) THEN
            FCB(1+FCBERR) = JERR
            IERR = 3
            IF (JERR.EQ.SS$_ENDOFFILE) THEN
               IERR = 4
C                                       test for BOM, EOM
               IF (ZAND(MT$M_BOT,ITEMP(2)).NE.0) IERR = 5
               IF (ZAND(MT$M_EOT,ITEMP(2)).NE.0) IERR = 6
C                                       EOM test
            ELSE IF (JERR.EQ.SS$_ENDOFTAPE) THEN
               IERR = 6
C                                       EOV should have another code
            ELSE IF (JERR.EQ.SS$_ENDOFVOLUME) THEN
               IERR = 4
C                                       Test for BOT for Kennedy drives
            ELSE IF (ZAND(MT$M_BOT,ITEMP(2)).NE.0) THEN
               IERR = 5
               END IF
         ELSE
            IERR = 0
            FCB(1+FCBERR) = 0
C                                       test for BOM
            IF (ZAND(MT$M_BOT,ITEMP(2)).NE.0) IERR = 5
            IF (ZAND(MT$M_EOT,ITEMP(2)).NE.0) IERR = 6
            END IF
         END IF
C
 999  RETURN
      END
