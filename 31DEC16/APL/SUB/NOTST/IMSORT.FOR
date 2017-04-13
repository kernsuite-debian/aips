      SUBROUTINE IMSORT (DISKIN, CNOIN, GUSE, CATBLK, IERR)
C-----------------------------------------------------------------------
C! Sorts the IM table to time-antenna order.
C# EXT-appl VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998, 2000, 2011
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
C   Sorts the IM table to time-ant order, ignoring the sort order
C   asserted in the table header.
C   Inputs:
C      DISKIN   I   Volume number
C      CNOIN    I   File catalogue number
C      GUSE     I   Version number of IM table to use
C   Output:
C      IERR     I   Return code, 0=>OK, otherwise IM table exists but
C                   cannot be read.
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, GUSE, CATBLK(256), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER COLHED(2)*24
      INTEGER   J,  KOLS(2), KEY(2,2), IMLUN, IMBUFF(512), NIMINR,
     *   KEYSUB(2,2)
      LOGICAL   T
      REAL      FKEY(2,2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DIMV.INC'
      DATA COLHED /'TIME', 'ANTENNA_NO.' /
      DATA T /.TRUE./
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       Open Calibration table
      NKEY = 0
      NREC = 0
      NCOL = 0
      IIMRNO = 1
      IMLUN = 29
      CALL TABINI ('READ', 'IM', DISKIN, CNOIN, GUSE, CATBLK, IMLUN,
     *   NKEY, NREC, NCOL, DATP, IMBUFF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CL', GUSE
         GO TO 990
         END IF
C                                       Get number of scans
 150  NIMINR = IMBUFF(5)
C                                       Check if empty
      IF (NIMINR.LE.0) THEN
         WRITE (MSGTXT,1010)
         IERR = 1
         GO TO 990
         END IF
C                                       Get column pointers
      NKEY = 2
      CALL FNDCOL (NKEY, COLHED, 8, T, IMBUFF, KOLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
      IERR = 0
      CALL FILL (NKEY, 0, IMKOLS)
      CALL FILL (NKEY, 0, IMNUMV)
      DO 160 J = 1,NKEY
         IPOINT = KOLS(J)
         IF (IPOINT.NE.0) THEN
            IMKOLS(J) = DATP(IPOINT,1)
            IMNUMV(J) = DATP(IPOINT,2) / 10
            END IF
 160     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, IIMRNO, IMBUFF, IMBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Sort to time-ant in any case
      KEY(1,1) = KOLS(1)
      KEY(2,1) = 0
      KEY(1,2) = KOLS(2)
      KEY(2,2) = 0
C                                       Sort
      CALL TABSRT (DISKIN, CNOIN, 'IM', GUSE, GUSE, KEY, KEYSUB, FKEY,
     *   IMBUFF, CATBLK, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMSORT: ERROR',I3,' OPENING ',A2,' TABLE, VERSION',I5)
 1010 FORMAT ('IMSORT: EMPTY IM TABLE')
 1020 FORMAT ('IMSORT: ERROR',I3,' CLOSING IM TABLE')
      END

