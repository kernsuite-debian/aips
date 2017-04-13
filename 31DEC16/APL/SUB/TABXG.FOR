      SUBROUTINE TABXG (OPCODE, BUFFER, IXGRNO, XGKOLS, XGNUMV, YZPIX,
     *   NGA, VPEAK, RESULT, IRET)
C-----------------------------------------------------------------------
C! Table IO for XGAUS results
C# Map Spectral Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 2013
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
C   Does I/O to XGAUS Gaussian fit (XG) extention tables.
C   Usually used after setup by XGINI.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'READ' = read entry from table.
C                        'WRIT' = write entry in table.
C                        'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open. Should have been returned by
C                        XGINI or TABINI.
C      IXGRNO   I        Next entry number to read or write.
C      XGKOLS   I(MAXXGC) The column pointer array in order,
C                        pixel(2), #gauss, peak value, baseline(2),
C                        baseline error(2), amp(4), center(4), width(4),
C                        amp error(4), center error(4), width error(4)
C      XGNUMV   I(MAXXGC) Element count in each column.
C   Input/output: (written to or read from baseline file)
C      YZPIX    I(2)     Y , Z pixel number
C      NGA      I        actual number Gaussians fit
C      VPEAK    R        Peak flux in this spectrum
C      RESULT   R(14,2)  Baseline offset, slope, Gauss 1 peak, center,
C                        width, Gauss 2, Gauss 3, Gauss 4, Errors
C   Output:
C      IXGRNO   I        Next solution number.
C      IRET     I        Error code, 0=>OK else TABIO error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXXGC, XXGRSZ, MAXGAU, MAXPRM
      PARAMETER (MAXXGC = 11)
      PARAMETER (MAXGAU=8)
      PARAMETER (MAXPRM=2+3*MAXGAU)
      PARAMETER (XXGRSZ=8+6*MAXGAU)
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IXGRNO, XGKOLS(MAXXGC), XGNUMV(MAXXGC),
     *   YZPIX(2), NGA, IRET, NDATA, INDX, LOOP
      REAL      VPEAK, RESULT(MAXPRM,2)
C
      INTEGER   RECI(XXGRSZ), KOLS(MAXXGC), PIXKOL, NGAKOL, PEKKOL,
     *   BLKOL, EBLKOL, AMPKOL, CENKOL, WIDKOL, EAMKOL, ECEKOL, EWIKOL
      REAL      RECR(XXGRSZ)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KOLS(1), PIXKOL), (KOLS(2), NGAKOL),
     *   (KOLS(3), PEKKOL), (KOLS(4), BLKOL), (KOLS(5), EBLKOL),
     *   (KOLS(6), AMPKOL), (KOLS(7), CENKOL), (KOLS(8), WIDKOL),
     *   (KOLS(9), EAMKOL),  (KOLS(10), ECEKOL),  (KOLS(11), EWIKOL)
      EQUIVALENCE (RECR, RECI)
C-----------------------------------------------------------------------
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IXGRNO, RECR, BUFFER, IRET)
         IF (IRET.GT.0) GO TO 980
         GO TO 999
         END IF
C                                       Set pointers
      NDATA = MAXXGC
      CALL COPY (NDATA, XGKOLS, KOLS)
C                                       If write fill RECR
      IF (OPCODE.NE.'READ') THEN
         RECI(PIXKOL) = YZPIX(1)
         RECI(PIXKOL+1) = YZPIX(2)
         RECI(NGAKOL) = NGA
         RECR(PEKKOL) = VPEAK
C                                       split out results
         CALL RCOPY (2, RESULT(1,1), RECR(BLKOL))
         CALL RCOPY (2, RESULT(1,2), RECR(EBLKOL))
         INDX = 3
         DO 20 LOOP = 0,MAXGAU-1
            RECR(AMPKOL+LOOP) = RESULT(INDX,1)
            RECR(EAMKOL+LOOP) = RESULT(INDX,2)
            RECR(CENKOL+LOOP) = RESULT(INDX+1,1)
            RECR(ECEKOL+LOOP) = RESULT(INDX+1,2)
            RECR(WIDKOL+LOOP) = RESULT(INDX+2,1)
            RECR(EWIKOL+LOOP) = RESULT(INDX+2,2)
            INDX = INDX + 3
 20         CONTINUE
         END IF
C                                       Process record.
 60   CALL TABIO (OPCODE, 0, IXGRNO, RECR, BUFFER, IRET)
      IXGRNO = IXGRNO + 1
      IF (IRET.GT.0) GO TO 980
C                                       If READ pick data from RECR.
      IF (OPCODE.EQ.'READ') THEN
         CALL COPY (2, RECI(PIXKOL), YZPIX)
         NGA = RECI(NGAKOL)
         VPEAK = RECR(PEKKOL)
         CALL RCOPY (2, RECR(BLKOL), RESULT(1,1))
         CALL RCOPY (2, RECR(EBLKOL), RESULT(1,2))
C                                       solutions
         INDX = 3
         DO 80 LOOP = 0,MAXGAU-1
            RESULT(INDX,1)   = RECR(AMPKOL+LOOP)
            RESULT(INDX,2)   = RECR(EAMKOL+LOOP)
            RESULT(INDX+1,1) = RECR(CENKOL+LOOP)
            RESULT(INDX+1,2) = RECR(ECEKOL+LOOP)
            RESULT(INDX+2,1) = RECR(WIDKOL+LOOP)
            RESULT(INDX+2,2) = RECR(EWIKOL+LOOP)
            INDX = INDX + 3
 80         CONTINUE
         END IF
      GO TO 999
C                                       Error
 980  WRITE (MSGTXT,1980) IRET
C
 985  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('TABXG: TABIO ERROR',I3)
      END
