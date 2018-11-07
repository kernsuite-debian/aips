      SUBROUTINE TABCD (OPCODE, BUFFER, ICDRNO, CDKOLS, CDNUMV, NUMPOL,
     *   NUMIF, ANTNO, SUBA, FREQID, TCAL, IERR)
C-----------------------------------------------------------------------
C! Does I/O to EVLA CalDevice (CD) table opened by CDINI
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2010, 2017
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
C   Does I/O to CALIBRATION extention tables. Usually used after setup
C   by CALINI.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open. Should have been returned by
C                        TABINI or CALINI.
C      ICDRNO   I        Next record number to read or write.
C      CDKOLS   I(5)     The column pointer array in order defined by
C                        the parameters in PCLTAB.INC.
C      CDNUMV   I(5)     Element count in each column.
C      NUMPOL   I        Number of polarizations per IF.
C      NUMIF    I        Number of IFs.
C   Input/output: (written to or read from CD file)
C      ANTNO    I        Antenna number as defined in Antenna table
C      SUBA     I        Subarray number.
C      FREQID   I        Freqid # as defined in FQ table.
C      TCAL     R(4,*)   Tcal (pol/solar,if)
C   Output:
C      ICDRNO   I        Next record number.
C      IERR     I        Error code, 0=>OK else TABIO error.
C                           Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXCDC, XCDRSZ
      PARAMETER (MAXCDC = 7)
      PARAMETER (XCDRSZ = 2 + 4*MAXIF)
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), ICDRNO, CDKOLS(MAXCDC), CDNUMV(MAXCDC),
     *   NUMPOL, NUMIF, ANTNO, SUBA, FREQID, IERR, LOOP
      REAL      TCAL(4,*)
C
      INTEGER   RECI(XCDRSZ), TKOL
      LOGICAL   DOREAD, DOCLOS
      REAL      RECORD(XCDRSZ)
      DOUBLE PRECISION RECD(XCDRSZ/2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RECD, RECORD, RECI)
      DATA RECI /XCDRSZ*0/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
         RECI(CDKOLS(1)) = ANTNO
         RECI(CDKOLS(2)) = SUBA
         IF (CDKOLS(3).GT.0) RECI(CDKOLS(3)) = FREQID
C                                       Setup for looping over IF
         TKOL = CDKOLS(4) - 1
C                                       First polarization
         DO 10 LOOP = 1,NUMIF
            RECORD(TKOL+LOOP) = TCAL(1,LOOP)
 10         CONTINUE
C                                       solar
         IF ((CDKOLS(5).GT.0) .AND. (CDNUMV(5).GT.0)) THEN
            TKOL = CDKOLS(5) - 1
            DO 15 LOOP = 1,NUMIF
               RECORD(TKOL+LOOP) = TCAL(3,LOOP)
 15            CONTINUE
            END IF
C                                       Two polarizations
         IF (NUMPOL.GE.2) THEN
            TKOL = CDKOLS(6) - 1
C                                       First polarization
            DO 20 LOOP = 1,NUMIF
               RECORD(TKOL+LOOP) = TCAL(2,LOOP)
 20            CONTINUE
C                                       solar
            IF ((CDKOLS(5).GT.0) .AND. (CDNUMV(5).GT.0)) THEN
               TKOL = CDKOLS(7) - 1
               DO 25 LOOP = 1,NUMIF
                  RECORD(TKOL+LOOP) = TCAL(4,LOOP)
 25               CONTINUE
               END IF
            END IF
         END IF
C                                       Process record.
      CALL TABIO (OPCODE, 0, ICDRNO, RECORD, BUFFER, IERR)
      ICDRNO = ICDRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
         ANTNO = RECI(CDKOLS(1))
         SUBA = RECI(CDKOLS(2))
         FREQID = 1
         IF (CDKOLS(3).GT.0) FREQID = RECI(CDKOLS(3))
C                                       Setup for looping over IF
         TKOL = CDKOLS(4) - 1
C                                       First polarization
         DO 60 LOOP = 1,NUMIF
            TCAL(1,LOOP) = RECORD(TKOL+LOOP)
 60         CONTINUE
C                                       solar
         IF ((CDKOLS(5).GT.0) .AND. (CDNUMV(5).GT.0)) THEN
            TKOL = CDKOLS(5) - 1
            DO 65 LOOP = 1,NUMIF
               TCAL(3,LOOP) = RECORD(TKOL+LOOP)
               IF (TCAL(3,LOOP).EQ.1.0) TCAL(3,LOOP) = FBLANK
 65            CONTINUE
         ELSE
            DO 66 LOOP = 1,NUMIF
               TCAL(3,LOOP) = FBLANK
 66            CONTINUE
            END IF
C                                       Two polarizations
         IF (NUMPOL.GE.2) THEN
            TKOL = CDKOLS(6) - 1
C                                       second polarization
            DO 70 LOOP = 1,NUMIF
               TCAL(2,LOOP) = RECORD(TKOL+LOOP)
 70            CONTINUE
C                                       solar
            IF ((CDKOLS(5).GT.0) .AND. (CDNUMV(5).GT.0)) THEN
               TKOL = CDKOLS(7) - 1
               DO 75 LOOP = 1,NUMIF
                  TCAL(4,LOOP) = RECORD(TKOL+LOOP)
                  IF (TCAL(4,LOOP).EQ.1.0) TCAL(4,LOOP) = FBLANK
 75               CONTINUE
            ELSE
               DO 76 LOOP = 1,NUMIF
                  TCAL(4,LOOP) = FBLANK
 76               CONTINUE
               END IF
            END IF
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, ICDRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABCD: TABIO ERROR',I3)
      END
