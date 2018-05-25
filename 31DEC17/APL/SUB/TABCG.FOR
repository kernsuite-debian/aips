      SUBROUTINE TABCG (OPCODE, CGBUFF, ICGRNO, CGKOLS, CGNUMV, FREQ,
     *   BMAJ, BMIN, BPA, IERR)
C----------------------------------------------------------------------
C! I/O to Clean beam info table (following init. by CGINI)
C# EXT-appl
C----------------------------------------------------------------------
C;  Copyright (C) 2010
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
C   Does I/O to a Clean beam table.  Used after setup by CGINI.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                           'READ' = read entry from table
C                           'WRIT' = write entry in table
C                           'CLOS' = close file, flush on write
C      CGBUFF   I(512)   I/O buffer and related storage, also defines
C                        file if open. Returned by CGINI.
C   Input/Output:
C      ICGRNO   I        Next row to read or write
C      CGKOLS   I(4)     The column pointer array in order:
C                           FREQ, BMAJ, BMIN, BPA
C      CGNUMV   I(4)     Element count in each column.
C      FREQ     D        Frequency (Hz)
C      BMAJ     R        Clean beam major axis (deg)
C      BMIN     R        Clean beam minor axis (deg)
C      BPA      R        Clean beam position angle (deg)
C   Output:
C      IERR     I        Return code (0=>ok; else error)
C                           Note: -1 => read okay but row flagged
C----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   CGBUFF(512), ICGRNO, CGKOLS(4), CGNUMV(4), IERR
      DOUBLE PRECISION FREQ
      REAL      BMAJ, BMIN, BPA
C
      DOUBLE PRECISION RECD(3)
      REAL      RECR(5)
      INTEGER   RECI(5)

      EQUIVALENCE (RECD, RECR, RECI)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IERR = 0
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, ICGRNO, RECI, CGBUFF, IERR)
         IF (IERR.GT.0) CALL TABERR ('CLOS', 'TABIO', 'TABCG', IERR)
C                                       Do I/O
      ELSE
C                                       If write fill RECORD
         IF (OPCODE.NE.'READ') THEN
C                                       Frequency
            IF ((CGKOLS(1).GT.0) .AND. (CGNUMV(1).GT.0))
     *         RECD(CGKOLS(1)) = FREQ
C                                       Major axis
            IF ((CGKOLS(2).GT.0) .AND. (CGNUMV(2).GT.0))
     *         RECR(CGKOLS(2)) = BMAJ
C                                       Minor axis
            IF ((CGKOLS(3).GT.0) .AND. (CGNUMV(3).GT.0))
     *         RECR(CGKOLS(3)) = BMIN
C                                       Position angle
            IF ((CGKOLS(4).GT.0) .AND. (CGNUMV(4).GT.0))
     *         RECR(CGKOLS(4)) = BPA
            END IF
C                                       Process record
         CALL TABIO (OPCODE, 0, ICGRNO, RECR, CGBUFF, IERR)
         ICGRNO = ICGRNO + 1
         IF (IERR.GT.0) THEN
            CALL TABERR (OPCODE, 'TABIO', 'TABCG', IERR)
            GO TO 999
C                                       If READ pick data from record
         ELSE IF (OPCODE.EQ.'READ') THEN
C                                       Time
            FREQ = 0.0D0
            IF ((CGKOLS(1).GT.0) .AND. (CGNUMV(1).GT.0)) THEN
               FREQ = RECD(CGKOLS(1))
            ELSE
               FREQ = DBLANK
               END IF
C                                       Major axis
            IF ((CGKOLS(2).GT.0) .AND. (CGNUMV(2).GT.0)) THEN
               BMAJ = RECR(CGKOLS(2))
            ELSE
               BMAJ = FBLANK
               END IF
C                                       Minor axis
            IF ((CGKOLS(3).GT.0) .AND. (CGNUMV(3).GT.0)) THEN
               BMIN = RECR(CGKOLS(3))
            ELSE
               BMIN = FBLANK
               END IF
C                                       Position angle
            IF ((CGKOLS(4).GT.0) .AND. (CGNUMV(4).GT.0)) THEN
               BPA = RECR(CGKOLS(4))
            ELSE
               BPA = FBLANK
               END IF
            END IF
         END IF
C
 999  RETURN
      END
