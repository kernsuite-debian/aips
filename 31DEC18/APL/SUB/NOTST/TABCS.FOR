      SUBROUTINE TABCS (OPCODE, BUFFER, ICSRNO, CSKOLS, CSNUMV,
     *   NUMPOL, TIME, RAAPP, DECAPP, BEMNO, SUBA,
     *   CSFACT, CSOFF, CSRAOF, CSDCOF, IERR)
C-----------------------------------------------------------------------
C! Does I/O to single dish calibration (CS) table opened by CSINI
C# EXT-util UV Calibration Sdish
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2013
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
C   Does I/O to single dish calibration (CS) extention tables.
C   Usually used after setup by CSINI.
C    Inputs:
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open. Should have been returned by TABINI or
C                      TABINI.
C     ICSRNO       I   Next scan number to read or write.
C     CSKOLS(13)   I   The column pointer array in order,
C                      TIME, RA, DEC, BEAM NO., SUBARRAY,
C                      FACTOR 1, OFFSET 1, RAOFF 1, DECOFF 1,
C                      Following used if 2 polarizations per IF
C                      FACTOR 2, OFFSET 2, RAOFF 2, DECOFF 2.
C     CSNUMV(13)   I   Element count in each column.
C     NUMPOL       I   Number of polarizations per IF.
C    Input/output: (written to or read from solution file)
C     TIME         R   Center time of solution record (Days)
C     RAAPP        R   Apparent RA (deg)
C     DECAPP       R   Apparent Declination (deg)
C     BEMNO        I   Antenna number.
C     SUBA         I   Subarray number.
C     CSFACT(2,*)  R   Calibration factor, 1 /Poln/IF
C     CSOFF(2,*)   R   Calibration offset, 1 /Poln/IF
C     CSRAOF(2,*)  R   RA correction, 1 /Poln/IF
C     CSDCOF(2,*)  R   Dec. correction, 1 /Poln/IF
C    Output:
C     ICSRNO       I   Next solution number.
C     IERR         I   Error code, 0=>OK else TABIO error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), CSKOLS(13), CSNUMV(13), NUMPOL, IERR,
     *   BEMNO, SUBA,
     *   RECI(1), KOLS(13), NUMIF, ICSRNO, LOOP,
     *   TIMKOL, RAKOL, DECKOL, BEMKOL, SUBKOL,
     *   F1KOL, O1KOL, RF1KOL, DF1KOL,
     *   F2KOL, O2KOL, RF2KOL, DF2KOL
      LOGICAL   DOREAD, DOCLOS
      REAL      TIME, RAAPP, DECAPP,
     *   RECORD(5+8*MAXIF),
     *   CSFACT(2,MAXIF), CSOFF(2,MAXIF), CSRAOF(2,MAXIF),
     *   CSDCOF(2,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (KOLS(1), TIMKOL),
     *   (KOLS(2),RAKOL), (KOLS(3), DECKOL),
     *   (KOLS(4),BEMKOL), (KOLS(5), SUBKOL),
     *   (KOLS(6), F1KOL), (KOLS(7),O1KOL),
     *   (KOLS(8), RF1KOL), (KOLS(9), DF1KOL),
     *   (KOLS(10), F2KOL), (KOLS(11),O2KOL),
     *   (KOLS(12), RF2KOL), (KOLS(13), DF2KOL)
      EQUIVALENCE (RECORD, RECI)
C-----------------------------------------------------------------------
      NUMIF = CSNUMV(6)
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       Set pointers
      CALL COPY (13, CSKOLS, KOLS)
C                                       If write fill RECORD
      IF (DOREAD) GO TO 50
         RECORD(TIMKOL) = TIME
         RECI(BEMKOL) = BEMNO
         RECI(SUBKOL) = SUBA
         RECORD(RAKOL) = RAAPP
         RECORD(DECKOL) = DECAPP
C                                       First polarization
         DO 10 LOOP = 1,NUMIF
            RECORD(F1KOL) = CSFACT(1,LOOP)
            RECORD(O1KOL) = CSOFF(1,LOOP)
            RECORD(RF1KOL) = CSRAOF(1,LOOP)
            RECORD(DF1KOL) = CSDCOF(1,LOOP)
            IF (LOOP.GE.NUMIF) GO TO 10
               F1KOL = F1KOL + 1
               O1KOL = O1KOL + 1
               DF1KOL = DF1KOL + 1
               RF1KOL = RF1KOL + 1
 10         CONTINUE
         IF (NUMPOL.LT.2) GO TO 50
C                                       Two polarizations
            DO 20 LOOP = 1,NUMIF
               RECORD(F2KOL) = CSFACT(2,LOOP)
               RECORD(O2KOL) = CSOFF(2,LOOP)
               RECORD(RF2KOL) = CSRAOF(2,LOOP)
               RECORD(DF2KOL) = CSDCOF(2,LOOP)
               IF (LOOP.GE.NUMIF) GO TO 20
                  F2KOL = F2KOL + 1
                  O2KOL = O2KOL + 1
                  DF2KOL = DF2KOL + 1
                  RF2KOL = RF2KOL + 1
 20         CONTINUE
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, ICSRNO, RECORD, BUFFER, IERR)
      ICSRNO = ICSRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
         TIME = RECORD(TIMKOL)
         BEMNO = RECI(BEMKOL)
         SUBA = RECI(SUBKOL)
         RAAPP = RECORD(RAKOL)
         DECAPP = RECORD(DECKOL)
C                                       First polarization
         DO 60 LOOP = 1,NUMIF
            CSFACT(1,LOOP) = RECORD(F1KOL)
            CSOFF(1,LOOP) = RECORD(O1KOL)
            CSRAOF(1,LOOP) = RECORD(RF1KOL)
            CSDCOF(1,LOOP) = RECORD(DF1KOL)
            IF (LOOP.GE.NUMIF) GO TO 60
               F1KOL = F1KOL + 1
               O1KOL = O1KOL + 1
               DF1KOL = DF1KOL + 1
               RF1KOL = RF1KOL + 1
 60         CONTINUE
C                                       Two polarizations
         IF (NUMPOL.GE.2) THEN
            DO 80 LOOP = 1,NUMIF
               CSFACT(2,LOOP) = RECORD(F2KOL)
               CSOFF(2,LOOP) = RECORD(O2KOL)
               CSRAOF(2,LOOP) = RECORD(RF2KOL)
               CSDCOF(2,LOOP) = RECORD(DF2KOL)
               IF (LOOP.GE.NUMIF) GO TO 80
                  F2KOL = F2KOL + 1
                  O2KOL = O2KOL + 1
                  DF2KOL = DF2KOL + 1
                  RF2KOL = RF2KOL + 1
 80         CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, ICSRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABCS: TABIO ERROR',I3)
      END
