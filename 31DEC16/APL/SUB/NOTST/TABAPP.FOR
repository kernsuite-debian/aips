      SUBROUTINE TABAPP (TYPE, INVER, OUTVER, LUN1, LUN2, VOL1, VOL2,
     *   CNO1, CNO2, CATBLK, BUFF1, BUFF2, RECORD, IRET)
C-----------------------------------------------------------------------
C! Appends one table to the end of a similar table.
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2015
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
C   TABAPP appends one table onto the end of a similar table
C   Inputs:
C      TYPE     C*2      Extension table type (e.g. 'CC','AN')
C      INVER    I        Version number to append, 0 => highest.
C      OUTVER   I        Version number on output file, 0=>create new
C      LUN1     I        LUN for first file (input)
C      LUN2     I        LUN for second file (output)
C      VOL1     I        Disk number for first file.
C      VOL2     I        Disk number for second file.
C      CNO1     I        Catalog slot number for first file
C      CNO2     I        Catalog slot number for second file
C   In/out:
C      CATBLK   I(256)   Catalog header for the first file.
C   Output:
C      BUFF1    I(512)   Work buffer
C      BUFF2    I(512)   Work buffer
C      RECORD   I(*)     Buffer = length of one row
C      IRET     I        Return error code  0 => ok
C                           1 => files the same, no copy.
C                           2 => no input files exist
C                           3 => failed
C                           4 => no output files created.
C                           5 => failed to update CATBLK
C-----------------------------------------------------------------------
      CHARACTER TYPE*2
      INTEGER   INVER, OUTVER, LUN1, LUN2, VOL1, VOL2, CNO1, CNO2,
     *   BUFF1(512), BUFF2(512), CATBLK(256), RECORD(*), IRET
C
      INTEGER   NKEY, NREC, NCOL, DATP(2,128,2), IER, I, NRECIN, NRECOU,
     *   INREC, OUTREC, LOOP
      LOGICAL   NEW
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Get CATBLK for first file in
C                                       BUFF2.
      CALL CATIO ('READ', VOL1, CNO1, BUFF2, 'REST', BUFF1, IER)
      IF ((IER.GT.0) .AND. (IER.LT.5)) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IER
         GO TO 990
         END IF
C                                       Open first (input) file
      MSGSUP = 32000
      IRET = 0
      CALL TABINI ('READ', TYPE, VOL1, CNO1, INVER, BUFF2, LUN1, NKEY,
     *   NREC, NCOL, DATP(1,1,1), BUFF1, IER)
      MSGSUP = 0
C                                       If not there - quit
      IF (IER.EQ.2) GO TO 999
      IF (IER.GT.0) THEN
         IRET = 2
         WRITE (MSGTXT,1010) IER
         GO TO 990
         END IF
C                                       Open second (output) file
      CALL TABINI ('WRIT', TYPE, VOL2, CNO2, OUTVER, CATBLK, LUN2, NKEY,
     *   NREC, NCOL, DATP(1,1,2), BUFF2, IER)
      NEW = IER.LT.0
      IF (IER.GT.0) THEN
         IRET = 3
         WRITE (MSGTXT,1020) IER
         GO TO 990
         END IF
C                                       Check table data.
      DO 50 I = 1,NCOL
C                                       Tables don't match
         IF (DATP(2,I,2).NE.DATP(2,I,1)) THEN
            IRET = 5
            WRITE (MSGTXT,1060)
            GO TO 990
            END IF
 50      CONTINUE
C                                       Get number of records in the
C                                       files.
      NRECIN = BUFF1(5)
      NRECOU = BUFF2(5)
      OUTREC = NRECOU + 1
C                                       Copy
      IRET = 6
      DO 200 LOOP = 1,NRECIN
         INREC = LOOP
         CALL TABIO ('READ', 0, INREC, RECORD, BUFF1, IER)
         IF (IER.GT.0) THEN
            WRITE (MSGTXT,1070) IER, 'READ', TYPE
            GO TO 990
         ELSE IF (IER.EQ.0) THEN
            CALL TABIO ('WRIT', 0, OUTREC, RECORD, BUFF2, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1070) IER, 'WRIT', TYPE
               GO TO 990
               END IF
            OUTREC = OUTREC + 1
            END IF
 200     CONTINUE
C                                       Close tables.
      CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
         GO TO 990
         END IF
      CALL TABIO ('CLOS', 0, OUTREC, RECORD, BUFF2, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
         GO TO 990
         END IF
      IRET = 0
      IF (NEW) THEN
         CALL CATIO ('UPDT', VOL2, CNO2, CATBLK, 'REST', BUFF1, IER)
         IF (IER.NE.0) IRET = 5
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABAPP: ERROR',I5,' READING CATBLK')
 1010 FORMAT ('TABAPP: ERROR',I5,' OPENING OUTPUT TABLE')
 1020 FORMAT ('TABAPP: ERROR',I5,' OPENING INPUT TABLE')
 1060 FORMAT ('TABAPP: INPUT TABLES DO NOT MATCH')
 1070 FORMAT ('TABAPP: ERROR ',I3,2X,A4,'ING ',A2,' TABLE')
      END
