      SUBROUTINE ZM70M2 (FCB, IERR)
C-----------------------------------------------------------------------
C! issues a master clear to an IIS Model 70 TV
C# Z2 IO-TV
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
C   Perform a master clear on an IIS model 70 TV device - second level z
C   Inputs:
C      FCB      I(*)        File control block in FTAB for TV device
C   VMS version
C-----------------------------------------------------------------------
      INTEGER   FCB(*), IERR
C
      INTEGER   SYS$QIOW, JERR, ITEMP, FCODE, BUFF
      INTEGER*2 I2TEMP(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE '($IODEF)'
      INCLUDE '($SSDEF)'
      EQUIVALENCE (I2TEMP, ITEMP)
C-----------------------------------------------------------------------
C                                       Read
      FCB(1+FCBREQ) = 0
      FCODE = IO$_REWIND
      FCB(1+FCBSP2) = 1
C                                       Do it
      JERR = SYS$QIOW (%VAL(FCB(1+FCBSP1)), %VAL(FCB(1+FCBFD)),
     *   %VAL(FCODE), FCB(1+FCBSP2), , , BUFF, %VAL(FCB(1+FCBREQ)),
     *   , , , )
C                                       error testing
      ITEMP = FCB(1+FCBSP2)
      IF (JERR.NE.1) THEN
         IERR = 3
         FCB(1+FCBERR) = JERR
         FCB(1+FCBXFR) = 0
      ELSE
         FCB(1+FCBXFR) = I2TEMP(2)
         IF (I2TEMP(1).NE.1) THEN
            FCB(1+FCBERR) = I2TEMP(1)
            IERR = 3
            IF (I2TEMP(1).EQ.SS$_ENDOFFILE) IERR = 4
         ELSE
            IERR = 0
            FCB(1+FCBERR) = 0
            END IF
         END IF
C
 999  RETURN
      END
