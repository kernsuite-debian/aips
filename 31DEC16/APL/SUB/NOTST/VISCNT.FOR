      SUBROUTINE VISCNT (IERR)
C-----------------------------------------------------------------------
C! Determines number of visibility records requested of UVGET
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   Counts the number of visibility records for the sources selected
C   with the given time range in /SELCAL/.
C   The INDEX file should already be open.
C   Inputs from common /SELCAL/
C      TSTART       R    Start time (days)
C      TEND         R    End time (days)
C      INXRNO       I    Current INDEX file record number.
C                        If .LT. 0 then there is no index file.
C      NINDEX       I    Number of entries in the index table
C      NXBUFF(*)    I    TABIO buffer for INDEX table.
C      NSOUWD       I    Number of sources specified.
C      DOSWNT       L    If true sources specified are included
C                        else excluded.
C      SOUWAN(30)   I    List or source numbers from source file.
C      SUBARR       I    Selected subarray, 0=>all
C      FRQSEL       I    Selected freq id, 0=>all
C   Output:
C      IERR         I    Return code, 0=>OK, 1=>no data, 2=>error.
C   Output to common /SELCAL/:
C      CATBLK(KIGCN)  I    Visibility count. (CATBLK)
C-----------------------------------------------------------------------
      INTEGER   IERR, JERR, NXIDSO, NXSUBA, J
      LOGICAL   MATCH
      INTEGER   I, NXSTA, NXEND, COUNT, INDX, FREQID
      REAL      NXTIME, NXDTIM
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      IERR = 0
      COUNT = 0
C                                       See if NX file exists
      IF (INXRNO.LT.1) GO TO 999
C                                       Check if empty
      IF (NINDEX.LE.0) GO TO 999
C                                       Locate first selected scan.
C                                       Loop through records
      DO 80 I = 1,NINDEX
         INDX = I
C                                       Read record.
         CALL TABNDX ('READ', NXBUFF, INDX, NXKOLS, NXNUMV, NXTIME,
     *      NXDTIM, NXIDSO, NXSUBA, NXSTA, NXEND, FREQID, JERR)
         IF (JERR.LE.0) GO TO 30
            IERR = 2
            WRITE (MSGTXT,1020) JERR
            GO TO 990
C                                       Check time range.
 30      IF ((TEND.LT.(NXTIME-NXDTIM*0.5)) .OR.
     *      (TSTART.GT.(NXTIME+NXDTIM*0.5))) GO TO 80
C                                       Check subarray
         IF ((NXSUBA.NE.SUBARR) .AND. (NXSUBA.GT.0) .AND. (SUBARR.GT.0))
     *      GO TO 80
C                                       Check freq id
         IF ((FREQID.NE.FRQSEL) .AND. (FREQID.GT.0) .AND. (FRQSEL.GT.0))
     *      GO TO 80
C                                       See if all sources selected.
         IF (NSOUWD.LE.0) GO TO 60
C                                       Search source lists
         DO 40 J = 1,NSOUWD
            MATCH = NXIDSO .EQ. SOUWAN(J)
            IF (MATCH) GO TO 50
 40         CONTINUE
C                                       See if found match
 50      IF ((MATCH.AND.DOSWNT) .OR. ((.NOT.MATCH).AND.(.NOT.DOSWNT)))
     *      GO TO 60
            GO TO 80
C                                       Count visibilities
 60         COUNT = COUNT + NXEND - NXSTA + 1
 80      CONTINUE
 100  CATBLK(KIGCN) = COUNT
      IF (COUNT.GT.0) GO TO 999
C                                       No valid data
      IERR = 1
      WRITE (MSGTXT,1060) CURSOU, SUBARR
      GO TO 990
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('VISCNT: ERROR',I3,' READING INDEX TABLE')
 1060 FORMAT ('VISCNT: NO DATA FOUND FOR SOURCE ',I6, ' SUBARRAY=',I4)
      END
