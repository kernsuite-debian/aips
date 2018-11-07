      SUBROUTINE CMTAB (OPCODE, BUFFER, IERR)
C-----------------------------------------------------------------------
C! I/O to CALC model tables (following init. by CMINI)
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2009
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
C   Does I/O to CALC model tables. Usually used after setup by
C   CMINI.
C   Inputs:
C      OPCODE   C*4        Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      BUFFER   I(512)     I/O buffer and related storage, also defines
C                          file if open. Should have been returned by
C                          CMINI.
C   Inputs via common:
C      ICMRNO   I          Next scan number to read or write.
C      CMKOLS   I(MAXCMC)  The column pointer array in order:
C                          TIME, SOURCE ID, ANTENNA NO.,
C                          ARRAY, FREQID, ATMOS, DATMOS,
C                          GDELAY, CLOCK1, DCLOCK1, DISP1, DDISP1,
C                          the following are only present if NOPOLZ = 2
C                          CLOCK2, DCLOCK2, DISP2, DDISP2
C      CMNUMV   I(MAXCMC)  Element count in each column.
C      NOPOLZ   I          # polzns in table
C      NPOLY    I          Order of polynomial for geometric model
C   Input/output: (written to or read from model file)
C      TIME     D          Time of centre of interval since 0hr on
C                          reference day (days)
C      ISRC     I          Source ID number
C      NOSTA    I          Station number
C      IARRAY   I          Array number
C      IFQID    I          Freq ID number
C      ATMOS    R          Atmospheric group delay
C      DATMOS   R          Atmospheric group delay rate
C      GDELAY   D(MAXPOL)  Total group delay (seconds) at ref. time
C      CLOCK1   R          The 'clock' delay, i.e. any electronically
C                          induced array, for polzn 1
C      DCLK1    R          Clock delay rate for polzn 1
C      DISP1    R          Dispersive delay for polzn 1 - sec/m^2
C                          scales to other frequencies as lambda squared
C      DDISP1   R          Dispersive delay rate sec/sec/m^2
C    The following are only used if NOPOLZ = 2.
C      CLOCK2   R          The 'clock' delay, i.e. any electronically
C                          induced array, for polzn 2
C      DCLK2    R          Clock delay rate for polzn 2
C      DISP2    R          Dispersive delay for polzn 2 - sec/m^2
C                          scales to other frequencies as lambda squared
C      DDISP2   R          Dispersive delay rate  sec/sec/m^2
C   Output:
C      ICMRNO   I          Next row number.
C      IERR     I          Error code, 0=>OK else TABIO error.
C                          Note: -1=> read but record deselected.
C   Usage NOTE: use the include 'DCMV.INC' for the declarations in
C   CMINI and CMTAB.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IERR
C
      LOGICAL   DOREAD, DOCLOS
      INTEGER   I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCMV.INC'
      INTEGER   RECI(MAXREC)
      REAL      RECORD(MAXREC)
      HOLLERITH RECH(MAXREC)
      DOUBLE PRECISION RECD(MAXRCD)
      EQUIVALENCE (RECD, RECORD, RECI, RECH)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
C                                       Time
         RECD(CMKOLS(KTIM)) = TIME
C                                       Source ID
         RECI(CMKOLS(KSOU)) = ISRC
C                                       Antenna number
         RECI(CMKOLS(KANO)) = NOSTA
C                                       Array number
         RECI(CMKOLS(KARR)) = IARRAY
C                                       Freqid number
         RECI(CMKOLS(KFRE)) = IFQID
C                                       ATMOS
         RECORD(CMKOLS(KATM)) = ATMOS
C                                       DATMOS
         RECORD(CMKOLS(KDATM)) = DATMOS
C                                       CLOCK 1
         RECORD(CMKOLS(KCLK1)) = CLOCK1
C                                       DCLOCK 1
         RECORD(CMKOLS(KDCLK1)) = DCLK1
C                                       DISP 1
         RECORD(CMKOLS(KDSP1)) = DISP1
C                                       DDISP 1
         RECORD(CMKOLS(KDDSP1)) = DDISP1
C                                       Dual polzn?
         IF (NOPOLZ.EQ.2) THEN
C                                       CLOCK 2
            RECORD(CMKOLS(KCLK2)) = CLOCK2
C                                       DCLOCK 2
            RECORD(CMKOLS(KDCLK2)) = DCLK2
C                                       DISP 2
            RECORD(CMKOLS(KDSP2)) = DISP2
C                                       DDISP 2
            RECORD(CMKOLS(KDDSP2)) = DDISP2
            END IF
C                                       Variables that vary with
C                                       NPOLY
         DO 100 I = 1, NPOLY
C                                       Group delay
            RECD(CMKOLS(KGDL)+I-1) = GDELAY(I)
 100        CONTINUE
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, ICMRNO, RECORD, BUFFER, IERR)
      ICMRNO = ICMRNO + 1
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABIO', 'CMTAB', IERR)
         GO TO 990
         END IF
      IF (IERR.LT.0) GO TO 50
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
C                                       Time
         TIME = RECD(CMKOLS(KTIM))
C                                       Source ID
         ISRC = RECI(CMKOLS(KSOU))
C                                       Antenna number
         NOSTA = RECI(CMKOLS(KANO))
C                                       Array number
         IARRAY = RECI(CMKOLS(KARR))
C                                       Freqid number
         IFQID = RECI(CMKOLS(KFRE))
C                                       ATMOS
         ATMOS = RECORD(CMKOLS(KATM))
C                                       DATMOS
         DATMOS = RECORD(CMKOLS(KDATM))
C                                       CLOCK 1
         CLOCK1 = RECORD(CMKOLS(KCLK1))
C                                       DCLOCK 1
         DCLK1 = RECORD(CMKOLS(KDCLK1))
C                                       DISP 1
         DISP1 = RECORD(CMKOLS(KDSP1))
C                                       DDISP 1
         DDISP1 = RECORD(CMKOLS(KDDSP1))
C                                       Dual polzn?
         IF (NOPOLZ.EQ.2) THEN
C                                       CLOCK 2
            CLOCK2 = RECORD(CMKOLS(KCLK2))
C                                       DCLOCK 2
            DCLK2 = RECORD(CMKOLS(KDCLK2))
C                                       DISP 2
            DISP2 = RECORD(CMKOLS(KDSP2))
C                                       DDISP 2
            DDISP2 = RECORD(CMKOLS(KDDSP2))
            END IF
C                                       Variables that vary with
C                                       NPOLY
         DO 200 I = 1, NPOLY
C                                       Group delay
            GDELAY(I) = RECD(CMKOLS(KGDL)+I-1)
 200        CONTINUE
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, ICMRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CMTAB', IERR)
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('CMTAB: TABIO ERROR',I5)
      END
