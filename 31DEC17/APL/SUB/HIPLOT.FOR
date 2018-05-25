      SUBROUTINE HIPLOT (IVOL, ICNO, IVER, IBUF, IERR)
C-----------------------------------------------------------------------
C! places a record in the history file concerning a plot file creation
C# History Plot-util
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
C   HIPLOT places one record in a file's HI extension file reporting
C   that a plot extension was created by the present program.
C   Inputs:
C      IVOL   I         Disk volume
C      ICNO   I         Catalog number
C      IVER   I         Plot file version number created
C   Output:
C      IBUF   I(256)    Scratch
C      IERR   I         Error code: 0 => ok - don't quit on error
C                               1 => no HI file
C                               2,3,4 => open, add, close error
C   Uses LUN=29 and inits the HI common
C-----------------------------------------------------------------------
      INTEGER   IVOL, ICNO, IVER, IBUF(256), IERR
C
      CHARACTER HILINE*72, ATIME*8, ADATE*12
      INTEGER   IT(3), ID(3), IHLUN, IER
      LOGICAL   SAVE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA IHLUN, SAVE /29, .TRUE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Initialize HI common
      CALL HIINIT (3)
C                                       Open HI file
      CALL HIOPEN (IHLUN, IVOL, ICNO, IBUF, IER)
      IF (IER.EQ.0) GO TO 20
         IF (IER.NE.5) GO TO 10
            IERR = 1
            WRITE (MSGTXT,1000)
            GO TO 990
 10      CONTINUE
            IERR = 2
            WRITE (MSGTXT,1010) IER
            GO TO 990
C                                       Prepare text and add to file
 20   CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (HILINE,1020,ERR=25) TSKNAM, IVER, ADATE, ATIME
 25   CALL HIADD (IHLUN, HILINE, IBUF, IER)
      IF (IER.EQ.0) GO TO 30
         IERR = 3
         WRITE (MSGTXT,1025) IER
         GO TO 990
C                                       Close file updating it
 30   CALL HICLOS (IHLUN, SAVE, IBUF, IER)
      IF (IER.EQ.0) GO TO 999
         IERR = 4
         WRITE (MSGTXT,1030) IER
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('HIPLOT: HISTORY FILE DOES NOT EXIST')
 1010 FORMAT ('HIPLOT: ERROR',I5,' OPENING THE HISTORY FILE')
 1020 FORMAT (A6,' VERSION=',I3,'  / plot file created ',A12,A8)
 1025 FORMAT ('HIPLOT: ERROR',I5,' ADDING TO THE HISTORY FILE')
 1030 FORMAT ('HIPLOT: ERROR',I5,' CLOSING/UPDATING THE HISTORY FILE')
      END
