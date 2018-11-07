      SUBROUTINE TABOTT (OPCODE, BUFFER, IERR)
C-----------------------------------------------------------------------
C! Does I/O to Index (OT) table opened by OTTINI
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2011, 2013
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
C   Does I/O to INDEX extention tables. Usually used after setup by
C   OTTINI.
C    Inputs:
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open. Should have been returned by OTTINI or
C                      TABINI.
C    Output:
C     IERR         I   Error code, 0=>OK else TABIO error.
C                      Note: -1=> read but record deselected.
C    Input/output written to or read from DOTTV.INC, COMMON /OTVALS/:
C     TIME8        D   Center time of the scan (Days)
C     DT           R   Duration of scan (days).
C     JDSOUR       I   Source ID as defined in then SOURCE table.
C     IANT         I   antenna number
C     OTT          L   Over the Top code.
C    Input/output written to or read from DOTTV.INC, COMMON /OTTABV/:
C     IOTRNO       I   Next scan number to read or write.
C     OTKOLS(MAXNXC)  I   The column pointer array in order, TIME,
C                         TIME INTERVAL, SOURCE ID, ANTENNA-NUMBER,
C                         OVER-THE-TOP
C     OTNUMV(MAXNXC)  I   Element count in each column, set by OTTINI.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DOTTV.INC'
      CHARACTER OPCODE*4
      DOUBLE PRECISION RECD(6)
      INTEGER   BUFFER(512), IERR, TIMKOL, INTKOL,
     *          SOUKOL, ANTKOL, OTTKOL, RECI(24)
      LOGICAL   DOREAD, DOCLOS, RECL(48)
      REAL      RECORD(12)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECD, RECORD, RECI, RECL)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       Set pointers
      TIMKOL = OTKOLS(1)
      INTKOL = OTKOLS(2)
      SOUKOL = OTKOLS(3)
      ANTKOL = OTKOLS(4)
      OTTKOL = OTKOLS(5)
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
         IF (TDUBL) THEN
            RECD(TIMKOL) = TIME8
         ELSE
            RECORD(TIMKOL) = TIME8
            END IF
         RECORD(INTKOL) = DT
         RECI(SOUKOL)   = JDSOUR
         RECI(ANTKOL)   = IANT
         RECL(OTTKOL)   = OTT
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, IOTRNO, RECORD, BUFFER, IERR)
      IOTRNO = IOTRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
         IF (TDUBL) THEN
            TIME8  = RECD(TIMKOL)
         ELSE
            TIME8 = RECORD(TIMKOL)
            END IF
         DT     = RECORD(INTKOL)
         JDSOUR = RECI(SOUKOL)
         IANT   = RECI(ANTKOL)
         OTT    = RECL(OTTKOL)
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IOTRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABOTT: TABIO ERROR',I3)
      END
