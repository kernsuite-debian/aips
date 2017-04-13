      SUBROUTINE STCLOS (IRNO, IVOL, ISLOT, OUVER, BUF, IERR)
C-----------------------------------------------------------------------
C! Close an open Star table
C# EXT-util Map-util Utility
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
C STCLOS Will Close an open star table.
C   INPUTS:
C     IRNO   I   Number of STars in Table
C     IVOL   I   Disk number containing the ST table.
C     ISLOT  I   Catalog number
C     OUVER  I   Output ST version number
C     BUF    I   I/O Buffer
C   OUTPUT
C     IERR   I   I/O error number. 0=> OK
C-----------------------------------------------------------------------
      INTEGER IRNO, IVOL, ISLOT, OUVER, BUF(*), IERR
      CHARACTER HILINE*72, ATIME*8, ADATE*12, SBUF*80
      REAL      DUMMY
      INTEGER  IWBUFF(256), HLUN, IT(3), ID(3), IER
      LOGICAL   T
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA HLUN /28/
      DATA T/.TRUE./
C-----------------------------------------------------------------------
C                                       Normal EOF
 200  CALL TABIO ('CLOS', 0, IRNO, DUMMY, BUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1150) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      WRITE (MSGTXT,1210) IRNO, OUVER
      CALL MSGWRT (4)
C                                       Add to history file
      CALL HIINIT (3)
      CALL HIOPEN (HLUN, IVOL, ISLOT, IWBUFF, IER)
      IF (IER.NE.0) GO TO 999
C                                       Prepare text and add to file
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (SBUF,1211,ERR=215) TSKNAM, OUVER, IRNO, ADATE, ATIME
 215  HILINE = SBUF
      CALL HIADD (HLUN, HILINE, IWBUFF, IER)
      CALL HICLOS (HLUN, T, IWBUFF, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT ('STCLOS: ERROR',I4,' WRITING ST FILE')
 1210 FORMAT ('STCLOS: Wrote',I6,' lines in ST file version',I4)
 1211 FORMAT (A6,' Version=',I3,' Lines=',I5,
     *   '  / Star file created ',A,A)
      END
