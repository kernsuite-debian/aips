      SUBROUTINE TABGC (OPCODE, BUFFER, IGCRNO, GCKOLS, GCNUMV,
     *   NPOLGC, NTABGC, IANTGC, ISUBGC, IFQDGC, ITPGC, NTGC,
     *   IXTGC, IYTGC, XVALGC, YVALGC, GAINGC, SENSGC, IERR)
C-----------------------------------------------------------------------
C! I/O to Gain curve tables (following init. by GCINI)
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2007, 2013
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
C   Does I/O to gain curve tables. Usually used after setup by
C   GCINI.
C   Inputs:
C      OPCODE   C*4        Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      BUFFER   I(*)       I/O buffer and related storage, also defines
C                          file if open. Should have been returned by
C                          GCINI.
C      IGCRNO   I          Next scan number to read or write.
C      GCKOLS(MAXGCC) I    The column pointer array in order:
C                             ANTENNA NO, SUBARRAY, FREQ ID,
C                             CURVE TYPE 1, NO ENTRIES 1, X TYPE 1,
C                             Y TYPE 1, X VALUE 1, Y VALUES 1,
C                             GAIN VALUES 1, SENSITIVITY 1
C                          the following are only present if NPOLGC = 2
C                             CURVE TYPE 2, NO ENTRIES 2, X TYPE 2,
C                             Y TYPE 2, X VALUE 2, Y VALUES 2,
C                             GAIN VALUES 2, SENSITIVITY 2,
C      GCNUMV   I(MAXGCC)  Element count in each column.
C      NPOLGC   I          # polzns in table
C      NTABGC   I          # tabulated values per row
C   Input/output: (written to or read from gain curve file)
C      IANTGC   I          Antenna number
C      ISUBGC   I          Subarray number
C      IFQDGC   I          Freq. ID
C      ITPGC    I(2,*)     Gain curve type: (1 /Polzn/IF)
C                            1 => tabulated values
C                            2 => polynomial
C                            3 => spherical harmonic
C      NTGC     I(2,*)     No. of terms or entries (1 /Polzn/IF)
C      IXTGC    I(2,*)     Gain curve X-absicssa code type: (1/Polzn/IF)
C                            0 => none
C                            1 => elevation (degrees)
C                            2 => zenith angle (degrees)
C                            3 => hour angle (degrees)
C                            4 => declination (degrees)
C                            5 => 90-declination (degrees)
C      IYTGC    I(2,*)     Gain curve Y-absicssa code type: (1/Polzn/IF)
C                            1 => elevation (degrees)
C                            2 => zenith angle (degrees)
C                            3 => hour angle (degrees)
C                            4 => declination (degrees)
C                            5 => 90-declination (degrees)
C      XVALGC   R(2,*)     Value of X abscissa (1/Polzn/IF)
C      YVALGC   R(2,*,*)   Values of Y abscissa (1/Polzn/IF/sample)
C      GAINGC   R(2,*,*)   Corresponding values of relative
C                          gain (if tabulated); or coefficients
C                          of approximating function. (1/Polzn/IF/sample)
C      SENSGC   R(2,*)     Point source sensitivity (K/Jy) (1/Polzn/IF)
C   Output:
C      IGCRNO   I          Next row number.
C      IERR     I          Error code, 0=>OK else TABIO error.
C                          Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PGCV.INC'
      CHARACTER OPCODE*4
      INTEGER NTABGC
      REAL XVALGC(2,MAXIF), YVALGC(2,MAXIF,NTABGC),
     *   GAINGC(2,MAXIF,NTABGC), SENSGC(2,MAXIF)
      INTEGER BUFFER(*), IGCRNO, GCKOLS(MAXGCC), GCNUMV(MAXGCC),
     *   NPOLGC, IANTGC, ISUBGC, IFQDGC, ITPGC(2,MAXIF),
     *   NTGC(2,MAXIF), IXTGC(2,MAXIF), IYTGC(2,MAXIF), IERR
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL   DOREAD, DOCLOS
      REAL RECORD(MXSPGC)
      INTEGER RECI(MXSPGC), I, NUMIF, JIF, K
      EQUIVALENCE (RECORD, RECI)
C-----------------------------------------------------------------------
      NUMIF = GCNUMV(GCITP1)
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
C                                       Antenna number
         RECI(GCKOLS(GCIANT)) = IANTGC
C                                       Subarray
         RECI(GCKOLS(GCISUB)) = ISUBGC
C                                       Frequency ID.
         RECI(GCKOLS(GCIFQD)) = IFQDGC
C                                       Loop over IF
         DO 100 JIF = 1, NUMIF
C                                       Gain curve type.
            RECI(GCKOLS(GCITP1)+JIF-1) = ITPGC(1,JIF)
C                                       No. of terms or entries
            RECI(GCKOLS(GCINT1)+JIF-1) = NTGC(1,JIF)
C                                       X-abscissa type
            RECI(GCKOLS(GCIXT1)+JIF-1) = IXTGC(1,JIF)
C                                       Y-abscissa type
            RECI(GCKOLS(GCIYT1)+JIF-1) = IYTGC(1,JIF)
C                                       X-abscissa value.
            RECORD(GCKOLS(GCRXV1)+JIF-1) = XVALGC(1,JIF)
            DO 80 I = 1, NTABGC
               K = (JIF - 1) * NTABGC + (I - 1)
               IF (I.LE.NTGC(1,JIF)) THEN
C                                       Y-abscissa values
                  RECORD(GCKOLS(GCRYV1)+K) = YVALGC(1,JIF,I)
C                                       Gain values or gain coefficients
                  RECORD(GCKOLS(GCRGA1)+K) = GAINGC(1,JIF,I)
               ELSE
                  RECORD(GCKOLS(GCRYV1)+K) = FBLANK
                  RECORD(GCKOLS(GCRGA1)+K) = FBLANK
                  END IF
 80            CONTINUE
C                                       Point source sensitivity
            RECORD(GCKOLS(GCRSE1)+JIF-1) = SENSGC(1,JIF)
100         CONTINUE
C
C                                       Dual polzn?
         IF (NPOLGC.EQ.2) THEN
C                                       Loop over IF
            DO 200 JIF = 1, NUMIF
C                                       Gain curve type.
               RECI(GCKOLS(GCITP2)+JIF-1) = ITPGC(2,JIF)
C                                       No. of terms or entries
               RECI(GCKOLS(GCINT2)+JIF-1) = NTGC(2,JIF)
C                                       X-abscissa type
               RECI(GCKOLS(GCIXT2)+JIF-1) = IXTGC(2,JIF)
C                                       Y-abscissa type
               RECI(GCKOLS(GCIYT2)+JIF-1) = IYTGC(2,JIF)
C                                       X-abscissa value.
               RECORD(GCKOLS(GCRXV2)+JIF-1) = XVALGC(2,JIF)
               DO 180 I = 1, NTABGC
                  K = (JIF - 1) * NTABGC + (I - 1)
                  IF (I.LE.NTGC(2,JIF)) THEN
C                                       Y-abscissa values
                     RECORD(GCKOLS(GCRYV2)+K) = YVALGC(2,JIF,I)
C                                       Gain values or gain coefficients
                     RECORD(GCKOLS(GCRGA2)+K) = GAINGC(2,JIF,I)
                  ELSE
                     RECORD(GCKOLS(GCRYV2)+K) = FBLANK
                     RECORD(GCKOLS(GCRGA2)+K) = FBLANK
                     END IF
 180              CONTINUE
C                                       Point source sensitivity
               RECORD(GCKOLS(GCRSE2)+JIF-1) = SENSGC(2,JIF)
200            CONTINUE
            END IF
C
         END IF
C                                       Process record.
220   CALL TABIO (OPCODE, 0, IGCRNO, RECORD, BUFFER, IERR)
      IGCRNO = IGCRNO + 1
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABIO', 'TABGC', IERR)
         GO TO 999
         END IF
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
C                                       Antenna number
         IANTGC = RECI(GCKOLS(GCIANT))
C                                       Subarray
         ISUBGC = RECI(GCKOLS(GCISUB))
C                                       Freq. ID
         IFQDGC = RECI(GCKOLS(GCIFQD))
C                                       Loop over IF
         DO 300 JIF = 1, NUMIF
C                                       Gain curve type
            ITPGC(1,JIF) = RECI(GCKOLS(GCITP1)+JIF-1)
C                                       No. of terms or entries
            NTGC(1,JIF) = RECI(GCKOLS(GCINT1)+JIF-1)
C                                       X-abscissa type
            IXTGC(1,JIF) = RECI(GCKOLS(GCIXT1)+JIF-1)
C                                       Y-abscissa type
            IYTGC(1,JIF) = RECI(GCKOLS(GCIYT1)+JIF-1)
C                                       X-abscissa value
            XVALGC(1,JIF) = RECORD(GCKOLS(GCRXV1)+JIF-1)
            DO 280 I = 1, NTGC(1,JIF)
               K = (JIF - 1) * NTABGC + (I - 1)
C                                       Y-abscissa values
               YVALGC(1,JIF,I) = RECORD(GCKOLS(GCRYV1)+K)
C                                       Gain values
               GAINGC(1,JIF,I) = RECORD(GCKOLS(GCRGA1)+K)
280            CONTINUE
C                                       Point source sensitivity
            SENSGC(1,JIF) = RECORD(GCKOLS(GCRSE1)+JIF-1)
300         CONTINUE
C                                       Dual polarization ?
         IF (NPOLGC.EQ.2) THEN
C                                       Loop over IF
            DO 400 JIF = 1, NUMIF
C                                       Gain curve type
               ITPGC(2,JIF) = RECI(GCKOLS(GCITP2)+JIF-1)
C                                       No. of terms or entries
               NTGC(2,JIF) = RECI(GCKOLS(GCINT2)+JIF-1)
C                                       X-abscissa type
               IXTGC(2,JIF) = RECI(GCKOLS(GCIXT2)+JIF-1)
C                                       Y-abscissa type
               IYTGC(2,JIF) = RECI(GCKOLS(GCIYT2)+JIF-1)
C                                       X-abscissa value
               XVALGC(2,JIF) = RECORD(GCKOLS(GCRXV2)+JIF-1)
               DO 380 I = 1, NTGC(2,JIF)
                  K = (JIF - 1) * NTABGC + (I - 1)
C                                       Y-abscissa values
                  YVALGC(2,JIF,I) = RECORD(GCKOLS(GCRYV2)+K)
C                                       Gain values or coefficients.
                  GAINGC(2,JIF,I) = RECORD(GCKOLS(GCRGA2)+K)
380               CONTINUE
C                                       Point source sensititivity
               SENSGC(2,JIF) = RECORD(GCKOLS(GCRSE2)+JIF-1)
400            CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IGCRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'TABGC', IERR)
         GO TO 999
         END IF
      GO TO 999
C
 999  RETURN
      END
