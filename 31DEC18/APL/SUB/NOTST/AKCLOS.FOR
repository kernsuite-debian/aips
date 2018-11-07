      INTEGER FUNCTION AKCLOS (FIL, BUFFER)
C-----------------------------------------------------------------------
C! *TESS routine to close files
C# IO-util
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
C   AKCLOS flushes the buffer if writing using (MDISK) and closes
C   a scratch file using ZCLOSE
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER   FIL, LUN, IARG, IERR
      REAL      BUFFER(*)
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      LUN = BUFNUM(FIL) + 15
      IARG = BUFNUM(FIL)
      AKCLOS = 0
      IF (ACCTYP(FIL).NE.'WRIT') GO TO 100
         CALL MDISK ('FINI', LUN, FIND(IARG), BUFFER, BIND(IARG), IERR)
         IF (IERR.EQ.0) GO TO 100
            WRITE (MSGTXT,1000) IERR,FIL
            CALL MSGWRT (8)
            AKCLOS = IERR
            GO TO 999
 100     CALL ZCLOSE (LUN, FIND(IARG), IERR)
         IF (IERR.EQ.0) GO TO 999
            WRITE (MSGTXT,1010) IERR,FIL
            CALL MSGWRT (8)
            AKCLOS = IERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I3,' FINISHING WRITE IN FILE ',I2,
     *        ' IN CLOSE ROUTINE:')
 1010 FORMAT ('ERROR ',I3,' CLOSING FILE ',I2,' IN CLOSE ROUTINE:')
      END
