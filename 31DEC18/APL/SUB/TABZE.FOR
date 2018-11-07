LOCAL INCLUDE 'PZEMAN.INC'
      INTEGER   MAXGAU, MAXPRM, MAXLIS, NPLIM, NMXIMG
      PARAMETER (MAXGAU=8)
      PARAMETER (MAXPRM=1+MAXGAU)
      PARAMETER (MAXLIS = 1000)
      PARAMETER (NPLIM=4096)
      PARAMETER (NMXIMG=2)
LOCAL END
      SUBROUTINE TABZE (OPCODE, BUFFER, IZERNO, ZEKOLS, ZENUMV, YZPIX,
     *   IPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
C-----------------------------------------------------------------------
C! Fits Zeeman models to spectra: IO to ZE table of ZEMAN
C# Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2013-2015, 2017
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
C   Does I/O to ZEMAN Gaussian fit (ZE) extension tables.
C   Usually used after setup by ZEINI.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'READ' = read entry from table.
C                        'WRIT' = write entry in table.
C                        'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open. Should have been returned by
C                        ZEINI or TABINI.
C      IZERNO   I        Next entry number to read or write.
C      ZEKOLS   I(MAXZEC) The column pointer array in order,
C                        pixel(2), #gauss, peak value, baseline(2),
C                        baseline error(2), amp(4), center(4), width(4),
C                        amp error(4), center error(4), width error(4)
C      ZENUMV   I(MAXZEC) Element count in each column.
C   Input/output: (written to or read from baseline file)
C      YZPIX    I(2)     Y , Z pixel number
C      IPEAK    R        Peak flux in the spectrum I polarization
C      RESULT   R(5,2)   Gain, field, and errors
C      NGA      I        actual number Gaussians fit
C      XGAUSV   R(12)    Gauss 1 peak, center, width, Gauss 2, Gauss 3,
C                        Gauss 4
C   Output:
C      IZERNO   I        Next solution number.
C      IRET     I        Error code, 0=>OK else TABIO error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PZEMAN.INC'
      INTEGER   MAXZEC, XZERSZ
      PARAMETER (MAXZEC = 11)
      PARAMETER (XZERSZ = 8+5*MAXGAU)
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IZERNO, ZEKOLS(MAXZEC), ZENUMV(MAXZEC),
     *   YZPIX(2), NGA, IRET
      REAL      IPEAK, RESULT(MAXPRM,2), XGAUSV(3*MAXGAU), XGAUSB(2)
C
      INTEGER   RECI(XZERSZ), KOLS(MAXZEC), PIXKOL, PEKKOL, GAIKOL,
     *   EGAKOL, FLDKOL, EFLKOL, NGAKOL, AMPKOL, CENKOL, WIDKOL,
     *   BLKOL, NDATA, INDX, LOOP
      REAL      RECR(XZERSZ)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KOLS(1), PIXKOL), (KOLS(2), PEKKOL),
     *   (KOLS(3), GAIKOL), (KOLS(4), EGAKOL), (KOLS(5), FLDKOL),
     *   (KOLS(6), EFLKOL), (KOLS(7), NGAKOL), (KOLS(8), AMPKOL),
     *   (KOLS(9), CENKOL), (KOLS(10), WIDKOL), (KOLS(11), BLKOL)
      EQUIVALENCE (RECR, RECI)
C-----------------------------------------------------------------------
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IZERNO, RECR, BUFFER, IRET)
         IF (IRET.GT.0) GO TO 980
         GO TO 999
         END IF
C                                       Set pointers
      NDATA = MAXZEC
      CALL COPY (NDATA, ZEKOLS, KOLS)
C                                       If write fill RECR
      IF (OPCODE.NE.'READ') THEN
         RECI(PIXKOL) = YZPIX(1)
         RECI(PIXKOL+1) = YZPIX(2)
         RECR(PEKKOL) = IPEAK
C                                       split out results
         RECR(GAIKOL) = RESULT(1,1)
         RECR(EGAKOL) = RESULT(1,2)
         INDX = 2
         DO 10 LOOP = 0,MAXGAU-1
            RECR(FLDKOL+LOOP) = RESULT(INDX,1)
            RECR(EFLKOL+LOOP) = RESULT(INDX,2)
            INDX = INDX + 1
 10         CONTINUE
         RECI(NGAKOL) = NGA
         INDX = 1
         DO 20 LOOP = 0,MAXGAU-1
            RECR(AMPKOL+LOOP) = XGAUSV(INDX)
            RECR(CENKOL+LOOP) = XGAUSV(INDX+1)
            RECR(WIDKOL+LOOP) = XGAUSV(INDX+2)
            INDX = INDX + 3
 20         CONTINUE
         IF (ZENUMV(11).GT.0) THEN
            RECR(BLKOL) = XGAUSB(1)
            RECR(BLKOL+1) = XGAUSB(2)
            END IF
         END IF
C                                       Process record.
 60   CALL TABIO (OPCODE, 0, IZERNO, RECR, BUFFER, IRET)
      IZERNO = IZERNO + 1
      IF (IRET.GT.0) GO TO 980
      IF (IRET.LT.0) GO TO 60
C                                       If READ pick data from RECR.
      IF (OPCODE.EQ.'READ') THEN
         CALL COPY (2, RECI(PIXKOL), YZPIX)
         IPEAK = RECR(PEKKOL)
         RESULT(1,1) = RECR(GAIKOL)
         RESULT(1,2) = RECR(EGAKOL)
         INDX = 2
         DO 70 LOOP = 0,MAXGAU-1
            RESULT(INDX,1) = RECR(FLDKOL+LOOP)
            RESULT(INDX,2) = RECR(EFLKOL+LOOP)
            INDX = INDX + 1
 70         CONTINUE

         NGA = RECI(NGAKOL)
         INDX = 1
         DO 80 LOOP = 0,MAXGAU-1
            XGAUSV(INDX)   = RECR(AMPKOL+LOOP)
            XGAUSV(INDX+1) = RECR(CENKOL+LOOP)
            XGAUSV(INDX+2) = RECR(WIDKOL+LOOP)
            INDX = INDX + 3
 80         CONTINUE
         IF (ZENUMV(11).GT.0) THEN
            XGAUSB(1) = RECR(BLKOL)
            XGAUSB(2) = RECR(BLKOL+1)
         ELSE
            XGAUSB(1) = 0.0
            XGAUSB(2) = 0.0
            END IF
         END IF
      GO TO 999
C                                       Error
 980  WRITE (MSGTXT,1980) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('TABZE: TABIO ERROR',I3)
      END
