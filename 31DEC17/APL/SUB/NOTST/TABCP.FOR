      SUBROUTINE TABCP (OPCODE, BUFFER, ICPRNO, CPKOLS, CPNUMV, NUMIF,
     *   NUMFRQ, SOURCE, SUID, VFLUX, IERR)
C-----------------------------------------------------------------------
C! Does I/O to source polarization spectrum (CP) table opened by CPINI
C# EXT-util UV Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2010, 2013, 2015
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
C   Does I/O to source polarization spectrum (CP) extention tables.
C   Usually used after setup by CPINI.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'READ' = read entry from table.
C                        'WRIT' = write entry in table.
C                        'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open. Should have been returned by
C                        CPINI or TABINI.
C      ICPRNO   I        Next entry number to read or write.
C      CPKOLS   I(MAXCPC) The column pointer array in order,
C                        Source name, Source ID, I, Q, U, V
C      CPNUMV   I(MAXCPC) Element count in each column.
C      NUMIF    I        Number of IF's
C      NUMFRQ   I        Number of chns
C   Input/output: (written to or read from baseline file)
C      SOURCE   C*16     Source name
C      SUID     I        Source number
C      VFLUX    R(4,n,m) Spectrum: I/Q/U/V, n channels; m IFS
C   Output:
C      ICPRNO    I      Next solution number.
C      IERR      I      Error code, 0=>OK else TABIO error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXCPC, XCPRSZ
      PARAMETER (MAXCPC = 6)
      PARAMETER (XCPRSZ = 4 * MAXCIF + 5)
C
      CHARACTER OPCODE*4, SOURCE*16
      INTEGER   BUFFER(*), ICPRNO, CPKOLS(MAXCPC), CPNUMV(MAXCPC),
     *   NUMIF, NUMFRQ, SUID, IERR
      REAL      VFLUX(*)
C
      INTEGER   RECI(XCPRSZ), KOLS(MAXCPC), SUKOL, SUIDKL, IKOL, QKOL,
     *   UKOL, VKOL, LOOP, NDATA, INDX, NNDX
      REAL      RECR(XCPRSZ)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KOLS(1), SUKOL), (KOLS(2), SUIDKL), (KOLS(3), IKOL),
     *   (KOLS(4), QKOL), (KOLS(5), UKOL), (KOLS(6), VKOL)
      SAVE RECR
      EQUIVALENCE (RECR, RECI)
C-----------------------------------------------------------------------
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, ICPRNO, RECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 980
         GO TO 999
         END IF
C                                       Check sizes
      NNDX = NUMFRQ * NUMIF
      IF (NNDX.GT.MAXCIF) THEN
         IERR = 1
         MSGTXT = 'TABCP: RECRS TOO BIG FOR BUFFERS'
         GO TO 985
         END IF
      IF (NNDX.LE.0) THEN
         IERR = 1
         MSGTXT = 'TABCP: ZERO SIZE DATA REQUESTED'
         GO TO 985
         END IF
C                                       Set pointers
      NDATA = MAXCPC
      CALL COPY (NDATA, CPKOLS, KOLS)
      NNDX = NUMFRQ * NUMIF
C                                       If write fill RECR
      IF (OPCODE.NE.'READ') THEN
         CALL CHR2H (16, SOURCE, 1, RECR(SUKOL))
         RECI(SUIDKL) = SUID
C                                       First polarization
         INDX = 0
         DO 20 LOOP = 1,NNDX
            INDX = INDX + 1
            RECR(IKOL) = VFLUX(INDX)
            INDX = INDX + 1
            RECR(QKOL) = VFLUX(INDX)
            INDX = INDX + 1
            RECR(UKOL) = VFLUX(INDX)
            INDX = INDX + 1
            RECR(VKOL) = VFLUX(INDX)
            IKOL = IKOL + 1
            QKOL = QKOL + 1
            UKOL = UKOL + 1
            VKOL = VKOL + 1
 20         CONTINUE
         END IF
C                                       Process record.
      CALL TABIO (OPCODE, 0, ICPRNO, RECR, BUFFER, IERR)
      ICPRNO = ICPRNO + 1
      IF (IERR.GT.0) GO TO 980
C                                       If READ pick data from RECR.
      IF (OPCODE.EQ.'READ') THEN
         CALL H2CHR (16, 1, RECR(SUKOL), SOURCE)
         SUID   = RECI(SUIDKL)
C                                       First polarization
         INDX = 0
         DO 80 LOOP = 1,NNDX
            INDX = INDX + 1
            VFLUX(INDX) = RECR(IKOL)
            INDX = INDX + 1
            VFLUX(INDX) = RECR(QKOL)
            INDX = INDX + 1
            VFLUX(INDX) = RECR(UKOL)
            INDX = INDX + 1
            VFLUX(INDX) = RECR(VKOL)
            IKOL = IKOL + 1
            QKOL = QKOL + 1
            UKOL = UKOL + 1
            VKOL = VKOL + 1
 80         CONTINUE
         END IF
      GO TO 999
C                                       Error
 980  WRITE (MSGTXT,1980) IERR
C
 985  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('TABCP: TABIO ERROR',I3)
      END
