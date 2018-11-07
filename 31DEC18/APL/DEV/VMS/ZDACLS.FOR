      SUBROUTINE ZDACLS (FCB, MAP, IERR)
C-----------------------------------------------------------------------
C! close a disk file
C# Z2 IO-basic
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
C--------------------------------------------------------------------
C   Close a disk file.
C   Inputs:
C      FCB    I(*)   File control block for open disk file
C      MAP    I      I/O type: 0 => non-map (single buffered)
C                              1 => map (double buffered)
C   Output:
C      IERR   I      Error return code: 0 => ok
C                       1 => error
C                       2 => deassign error
C   Must not call MSGWRT.
C   VMS version.
C-----------------------------------------------------------------------
      INTEGER   FCB(*), MAP, IERR
C
      INTEGER   JERR, SYS$QIOW, SYS$DASSGN, FCODE
      INTEGER*2 IOSB2(4)
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE '($IODEF)'
C-----------------------------------------------------------------------
      IERR = 0
      FCODE = IO$_DEACCESS
      JERR = SYS$QIOW ( , %VAL(FCB(1+FCBFD)), %VAL(FCODE), IOSB2, , , ,
     *   , , , , )
      IF (JERR.EQ.1) THEN
         FCB(1+FCBERR) = IOSB2(1)
         IF (MAP) FCB(1+MFCB+FCBERR) = IOSB2(1)
      ELSE
         IERR = 1
         FCB(1+FCBERR) = JERR
         IF (MAP) FCB(1+MFCB+FCBERR) = JERR
         END IF
C                                       Deassign device
      JERR = SYS$DASSGN (%VAL(FCB(1+FCBFD)))
      IF (JERR.NE.1) THEN
         IERR = 2
         FCB(1+FCBERR) = JERR
         IF (MAP) FCB(1+MFCB+FCBERR) = JERR
         END IF
      FCB(1+FCBFD) = 0
      IF (MAP) FCB(1+MFCB+FCBFD) = 0
C
 999  RETURN
      END

