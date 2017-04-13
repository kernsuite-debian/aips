      SUBROUTINE CLSORT (DISKIN, CNOIN, GUSE, IERR)
C-----------------------------------------------------------------------
C! Sort CL table.
C# Table Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2011
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
C   Sorts the CL table to time-ant order, ignoring the sort order
C   asserted in the table header.
C   Inputs:
C      DISKIN   I   Volume number
C      CNOIN    I   File catalogue number
C      GUSE     I   Version number of CL table to use
C   Output:
C      IERR     I   Return code, 0=>OK, otherwise CL table exists but
C                   cannot be read.
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, GUSE, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER COLHED(5)*24
      INTEGER   J,  NKEY, NREC, NCOL, DATP(128,2), IPOINT, KOLS(5),
     *   KEY(2,2), ICLUN, ICLRNO, CLBUFF(512), NCLINR, CLKOLS(MAXCLC),
     *   CLNUMV(MAXCLC), KEYSUB(2,2)
      LOGICAL   T
      REAL      FKEY(2,2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA COLHED /'TIME', 'TIME INTERVAL', 'SOURCE ID',
     *   'ANTENNA NO.', 'DOPPOFF' /
      DATA T /.TRUE./
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB/4*1/
C-----------------------------------------------------------------------
C                                       Open Calibration table
      NKEY = 0
      NREC = 0
      NCOL = 0
      ICLRNO = 1
      ICLUN = 29
      CALL TABINI ('READ', 'CL', DISKIN, CNOIN, GUSE, CATBLK, ICLUN,
     *   NKEY, NREC, NCOL, DATP, CLBUFF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CL', GUSE
         GO TO 990
         END IF
C                                       Get number of scans
 150  NCLINR = CLBUFF(5)
C                                       Check if empty
      IF (NCLINR.LE.0) THEN
         WRITE (MSGTXT,1010)
         IERR = 1
         GO TO 990
         END IF
C                                       Get column pointers
      NKEY = 5
      CALL FNDCOL (NKEY, COLHED, 24, T, CLBUFF, KOLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
      IERR = 0
      CALL FILL (NKEY, 0, CLKOLS)
      CALL FILL (NKEY, 0, CLNUMV)
      DO 160 J = 1,NKEY
         IPOINT = KOLS(J)
         IF (IPOINT.NE.0) THEN
            CLKOLS(J) = DATP(IPOINT,1)
            CLNUMV(J) = DATP(IPOINT,2) / 10
            END IF
 160     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, ICLRNO, CLBUFF, CLBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Sort to time-ant if necessary
      KEY(1,1) = KOLS(1)
      KEY(2,1) = 0
      KEY(1,2) = KOLS(4)
      KEY(2,2) = 0
C                                       Sort
      CALL TABSRT (DISKIN, CNOIN, 'CL', GUSE, GUSE, KEY, KEYSUB, FKEY,
     *   CLBUFF, CATBLK, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLSORT: ERROR',I3,' OPENING ',A2,' TABLE, VERSION',I5)
 1010 FORMAT ('CLSORT: EMPTY CL TABLE')
 1020 FORMAT ('CLSORT: ERROR',I3,' CLOSING CL TABLE')
      END
