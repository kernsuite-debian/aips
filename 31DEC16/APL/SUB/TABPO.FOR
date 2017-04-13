      SUBROUTINE TABPO (OPCODE, POBUFF, IPORNO, POKOLS, PONUMV, TIME,
     *   SOUID, SOURA, SOUDEC, SOUDST, IERR)
C----------------------------------------------------------------------
C! I/O to planetary position table (following init. by POINI)
C# EXT-appl
C----------------------------------------------------------------------
C;  Copyright (C) 2001, 2013
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
C----------------------------------------------------------------------
C   Does I/O to a planetary position table.  Used after setup by POINI.
C   Inputs:
C      OPCODE       C*4         Operation code:
C                               'READ' = read entry from table
C                               'WRIT' = write entry in table
C                               'CLOS' = close file, flush on write
C      POBUFF       I(*)        I/O buffer and related storage, also
C                               defines file if open. Should have
C                               been returned by POINI.
C   Input/Output:
C      IPORNO       I           Next row to read or write
C      POKOLS       I(MAXPOC)   The column pointer array in order:
C                               TIME, SOURCE, RA, DEC, DISTANCE
C      PONUMV       I(MAXPOC)   Element count in each column.
C      TIME         D           Time (days wrt reference day)
C      SOUID        I           Source ID
C      SOURA        D           Source RA (radians)
C      SOUDEC       D           Source DEC (radians)
C      SOUDST       D           Source distance (AU)
C   Output:
C      IERR         I           Return code (0=>ok; else error)
C                               Note: -1 => read by not selected
C----------------------------------------------------------------------
      INCLUDE 'INCS:DPOV.INC'
      CHARACTER OPCODE*4
      INTEGER   POBUFF(512), IPORNO, POKOLS(MAXPOC), PONUMV(MAXPOC),
     *   SOUID, IERR
      DOUBLE PRECISION TIME, SOURA, SOUDEC, SOUDST
C
      REAL      RECORD(MXSPPO)
      INTEGER   RECI(MXSPPO)
      DOUBLE PRECISION RECD(MXDPPO)
      EQUIVALENCE (RECD, RECORD, RECI)
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IERR = 0
C                                       Close
 800  IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IPORNO, RECORD, POBUFF, IERR)
         IF (IERR.GT.0) CALL TABERR ('CLOS', 'TABIO', 'TABPO', IERR)
C                                       Do I/O
      ELSE
C                                       If write fill RECORD
         IF (OPCODE.NE.'READ') THEN
C                                       Time
            RECD(POKOLS(PODTIM)) = TIME
C                                       Source ID
            RECI(POKOLS(POISID)) = SOUID
C                                       Source RA
            RECD(POKOLS(PODRA)) = SOURA
C                                       Source DEC
            RECD(POKOLS(PODDEC)) = SOUDEC
C                                       Source distance
            RECD(POKOLS(PODDST)) = SOUDST
            END IF
C                                       Process record
 200     CALL TABIO (OPCODE, 0, IPORNO, RECORD, POBUFF, IERR)
         IPORNO = IPORNO + 1
         IF (IERR.GT.0) THEN
            CALL TABERR (OPCODE, 'TABIO', 'TABPO', IERR)
C                                       If READ pick data from record
         ELSE IF (OPCODE.EQ.'READ') THEN
C                                       Time
            TIME = RECD(POKOLS(PODTIM))
C                                       Source ID
            SOUID = RECI(POKOLS(POISID))
C                                       Source RA
            SOURA = RECD(POKOLS(PODRA))
C                                       Source DEC
            SOUDEC = RECD(POKOLS(PODDEC))
C                                       Source distance
            SOUDST = RECD(POKOLS(PODDST))
            END IF
         END IF
C
 999  RETURN
      END
