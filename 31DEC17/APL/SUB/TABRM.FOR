LOCAL INCLUDE 'PRMFIT.INC'
      INTEGER   MAXCMP, MAXPRM, NCOLRM
      PARAMETER (MAXCMP=4)
      PARAMETER (MAXPRM=4*MAXCMP)
      PARAMETER (NCOLRM=14)
LOCAL END
      SUBROUTINE TABRM (OPCODE, BUFFER, IRMRNO, RMKOLS, RMNUMV, YZPIX,
     *   NCMP, IAVG, PAVG, RESULT, THERMS, IRET)
C-----------------------------------------------------------------------
C! RMFIT answer/work table - rotation measure models
C# Table Extension Polarization
C-----------------------------------------------------------------------
C;  Copyright (C) 2013-2015
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
C   Does I/O to RMFIT rot meas fit (RM) extention tables.
C   Usually used after setup by RMINI.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'READ' = read entry from table.
C                        'WRIT' = write entry in table.
C                        'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open. Should have been returned by
C                        RMINI or TABINI.
C      IRMRNO   I        Next entry number to read or write.
C      RMKOLS   I(NCOLRM) The column pointer array in order,
C                        pixel(2), #comp, I average, P average,
C                        Qzero(4), Uzero(4), rot.measure(4),
C                        Q0 error(4), U0 error(4), RM error(4)
C      RMNUMV   I(NCOLRM) Element count in each column.
C   Input/output: (written to or read from baseline file)
C      YZPIX    I(2)     Y , Z pixel number
C      NCMP     I        actual number Components fit
C      IAVG     R        Average I flux this spectrum
C      PAVG     R        Average P flux this spectrum
C      RESULT   R(*,2)   Comp 1: Q0, U0, RM; Comp 2: Q0 etc.
C      THERMS   R(2,2)   Q, U rms - unweighted, weighted
C   Output:
C      IRMRNO   I        Next solution number.
C      IRET     I        Error code, 0=>OK else TABIO error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PRMFIT.INC'
      INTEGER   XRMRSZ
      PARAMETER (XRMRSZ = 41)
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IRMRNO, RMKOLS(NCOLRM), RMNUMV(NCOLRM),
     *   YZPIX(2), NCMP, IRET
      REAL      IAVG, PAVG, RESULT(MAXPRM,2), THERMS(2,2)
C
      INTEGER   RECI(XRMRSZ), KOLS(NCOLRM), PIXKOL, NCMKOL, IAVKOL,
     *   PAVKOL, P1KOL, T1KOL, RMKOL, SPKOL, EP1KOL, ET1KOL, ERMKOL,
     *   ESPKOL, RMSKOL, WRMKOL, NDATA, INDX, LOOP
      REAL      RECR(XRMRSZ)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KOLS(1), PIXKOL), (KOLS(2), NCMKOL),
     *   (KOLS(3), IAVKOL), (KOLS(4), PAVKOL), (KOLS(5), P1KOL),
     *   (KOLS(6), T1KOL), (KOLS(7), RMKOL), (KOLS(8), SPKOL),
     *   (KOLS(9), EP1KOL), (KOLS(10), ET1KOL),  (KOLS(11), ERMKOL),
     *   (KOLS(12), ESPKOL), (KOLS(13), RMSKOL), (KOLS(14), WRMKOL)
      EQUIVALENCE (RECR, RECI)
C-----------------------------------------------------------------------
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IRMRNO, RECR, BUFFER, IRET)
         IF (IRET.GT.0) GO TO 980
         GO TO 999
         END IF
C                                       Set pointers
      NDATA = NCOLRM
      CALL COPY (NDATA, RMKOLS, KOLS)
C                                       If write fill RECR
      IF (OPCODE.NE.'READ') THEN
         RECI(PIXKOL) = YZPIX(1)
         RECI(PIXKOL+1) = YZPIX(2)
         RECI(NCMKOL) = NCMP
         RECR(IAVKOL) = IAVG
         RECR(PAVKOL) = PAVG
         RECR(RMSKOL) = THERMS(1,1)
         RECR(RMSKOL+1) = THERMS(2,1)
         RECR(WRMKOL) = THERMS(1,2)
         RECR(WRMKOL+1) = THERMS(2,2)
C                                       split out results
         INDX = 1
         DO 20 LOOP = 0,3
            RECR(P1KOL+LOOP)  = RESULT(INDX,1)
            RECR(EP1KOL+LOOP) = RESULT(INDX,2)
            RECR(T1KOL+LOOP)  = RESULT(INDX+1,1)
            RECR(ET1KOL+LOOP) = RESULT(INDX+1,2)
            RECR(RMKOL+LOOP)  = RESULT(INDX+2,1)
            RECR(ERMKOL+LOOP) = RESULT(INDX+2,2)
            RECR(SPKOL+LOOP)  = RESULT(INDX+3,1)
            RECR(ESPKOL+LOOP) = RESULT(INDX+3,2)
            INDX = INDX + 4
 20         CONTINUE
         END IF
C                                       Process record.
 60   CALL TABIO (OPCODE, 0, IRMRNO, RECR, BUFFER, IRET)
      IRMRNO = IRMRNO + 1
      IF (IRET.GT.0) GO TO 980
      IF (IRET.LT.0) GO TO 60
C                                       If READ pick data from RECR.
      IF (OPCODE.EQ.'READ') THEN
         CALL COPY (2, RECI(PIXKOL), YZPIX)
         NCMP = RECI(NCMKOL)
         IAVG = RECR(IAVKOL)
         PAVG = RECR(PAVKOL)
         THERMS(1,1) = RECR(RMSKOL)
         THERMS(2,1) = RECR(RMSKOL+1)
         THERMS(1,2) = RECR(WRMKOL)
         THERMS(2,2) = RECR(WRMKOL+1)
C                                       First polarization
         INDX = 1
         DO 80 LOOP = 0,3
            RESULT(INDX,1)   = RECR(P1KOL+LOOP)
            RESULT(INDX,2)   = RECR(EP1KOL+LOOP)
            RESULT(INDX+1,1) = RECR(T1KOL+LOOP)
            RESULT(INDX+1,2) = RECR(ET1KOL+LOOP)
            RESULT(INDX+2,1) = RECR(RMKOL+LOOP)
            RESULT(INDX+2,2) = RECR(ERMKOL+LOOP)
            RESULT(INDX+3,1) = RECR(SPKOL+LOOP)
            RESULT(INDX+3,2) = RECR(ESPKOL+LOOP)
            INDX = INDX + 4
 80         CONTINUE
         END IF
      GO TO 999
C                                       Error
 980  WRITE (MSGTXT,1980) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('TABRM: TABIO ERROR',I3)
      END
