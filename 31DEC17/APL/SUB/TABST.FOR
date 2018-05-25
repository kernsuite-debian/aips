      SUBROUTINE TABST (OPCODE, BUFFER, ISTRNO, STKOLS, STNUMV, STXY,
     *   STWID, STTYPE, STCOMM, IERR)
C-----------------------------------------------------------------------
C! REads/writes ST (stars) tables
C# EXT-util Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 2011, 2013
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
C   creates and/or opens for writing (and reading) a specified ST table
C   for star positions/widths
C   Inputs:
C      OPCODE   C*4      READ or WRIT (create only if none previously)
C      STKOLS   I(7)     Column pointers
C      STNUMV   I(7)     Coumn counts
C   In/out: (basic)
C      BUFFER   I(512)   TABIO control buffer
C      ISTRNO   I        Record number
C      STXY     D(2)     X, Y values
C      STWID    R(3)     Major, minor axes, position angle
C      STTYPE   I        Star type code
C      STCOMM   C*24     Star label or comment
C   Output:
C      IERR     I        Error codes from TABINI or TABIO
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, STCOMM*24
      INTEGER   BUFFER(512), ISTRNO, STKOLS(*), STNUMV(*), STTYPE, IERR
      REAL      STWID(3)
      DOUBLE PRECISION STXY(2)
C
      INTEGER   NCOLS, NVALS
      PARAMETER (NCOLS = 7)
      PARAMETER (NVALS = 14)
      INTEGER   KOLS(NCOLS), XKOL, YKOL, MAJKOL, MINKOL, PAKOL, TYPKOL,
     *   COMKOL, RECI(NVALS), IROUND
      REAL      RECR(NVALS)
      DOUBLE PRECISION RECD(NVALS/2)
      HOLLERITH RECH(NVALS)
      EQUIVALENCE (RECR, RECI, RECH, RECD)
      EQUIVALENCE (KOLS(1), XKOL), (KOLS(2), YKOL), (KOLS(3), MAJKOL),
     *   (KOLS(4), MINKOL), (KOLS(5), PAKOL), (KOLS(6), TYPKOL),
     *   (KOLS(7), COMKOL)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, ISTRNO, RECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 980
         GO TO 999
         END IF
C                                       local copy column pointers
      CALL COPY (NCOLS, STKOLS, KOLS)
C                                       fill record
      IF (OPCODE.EQ.'WRIT') THEN
         RECD(XKOL)   = STXY(1)
         RECD(YKOL)   = STXY(2)
         IF (STNUMV(3).GT.0) RECR(MAJKOL) = STWID(1)
         IF (STNUMV(4).GT.0) RECR(MINKOL) = STWID(2)
         IF (STNUMV(5).GT.0) RECR(PAKOL ) = STWID(3)
         IF (STNUMV(6).GT.0) RECR(TYPKOL) = STTYPE
         CALL CHR2H (STNUMV(7), STCOMM, 1, RECH(COMKOL))
         END IF
C                                       Process record.
 10   CALL TABIO (OPCODE, 0, ISTRNO, RECR, BUFFER, IERR)
      ISTRNO = ISTRNO + 1
      IF (IERR.GT.0) GO TO 980
C                                       If READ pick data from RECR.
      IF (OPCODE.EQ.'READ') THEN
         STWID(1) = 0.0
         STWID(2) = 0.0
         STWID(3) = 0.0
         STTYPE   = 3
         STCOMM = ' '
         STXY(1)  = RECD(XKOL)
         STXY(2)  = RECD(YKOL)
         IF (STNUMV(3).GT.0) STWID(1) = RECR(MAJKOL)
         IF (STNUMV(4).GT.0) STWID(2) = RECR(MINKOL)
         IF (STNUMV(5).GT.0) STWID(3) = RECR(PAKOL)
         IF (STNUMV(6).GT.0) STTYPE   = IROUND (RECR(TYPKOL))
         IF (STNUMV(7).GT.0) CALL H2CHR (STNUMV(7), 1, RECH(COMKOL),
     *      STCOMM)
         END IF
      GO TO 999
C                                       Error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('TABST: TABIO ERROR',I3)
      END
