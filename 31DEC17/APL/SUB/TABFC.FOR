      SUBROUTINE TABFC (OPCODE, BUF, IFCRNO, FCKOLS, FCNUMV, FLGTIM,
     *   FLGANT, FLGSOR, FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM,
     *   FLGOP, FLGIT, DTYPE, DTIMES, DFLUXS, FLGREA, IERR)
C-----------------------------------------------------------------------
C! REads/writes FC (Flag Command) tables used by OOP EDIT class
C# EXT-util UV Calibration OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 2013
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
C   creates and/or opens for writing (and reading) a specified FC table
C   for Flag Commands from OOP EDIT class
C   Inputs:
C      OPCODE   C*4      READ or WRIT (create only if none previously)
C      FCKOLS   I(11)    Column pointers
C      FCNUMV   I(11)    Coumn counts
C   In/out: (basic)
C      IFCRNO   I        Record number
C      BUF      I(512)   TABIO control buffer
C   In/out: (refer to specific uv data to be flagged)
C      FLGTIM   R(2)     time range (days)
C      FLGANT   I(2)     antenna 1 (0 -> all), 2 (0 -> all w 1)
C      FLGSOR   I        source number (0 -> all)
C      FLGCHN   I(2)     channel number range
C      FLGIF    I(2)     IF number range
C      FLGSTK   C*4      Stokes flag pattern
C      FLGSUB   I        Subarray
C      FLGFQ    I        FQ id number
C      FLGREA   C*24     User reason for flag
C   In/out: (refer to general flag command in EDITA...)
C      FLGNUM   I        FC command number
C      FLGOP    C*8      flagging operation type
C      FLGIT    I(2)     Time range in editor pixels on "time" axis
C      DTYPE    C*8      Type of data referenced in FLGOP
C      DTIMES   R(2)     Time range referenced in FLGOP
C      DFLUXS   R(2)     Flux (or phase, Tsys, ..) range in FLGOP
C   Output:
C      IERR     I        Error codes from TABINI or TABIO
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, FLGOP*8, FLGSTK*4, DTYPE*8, FLGREA*24
      INTEGER   IFCRNO, FCKOLS(*), FCNUMV(*), BUF(512), FLGNUM,
     *   FLGANT(2), FLGSOR, FLGCHN(2), FLGIF(2), FLGSUB, FLGFQ,
     *   FLGIT(2), IERR
      REAL      FLGTIM(2), DTIMES(2), DFLUXS(2)
C
      INTEGER   NCOLS, NVALS
      PARAMETER (NCOLS = 15)
      PARAMETER (NVALS = 29)
      INTEGER   KOLS(NCOLS), NUMKOL, ANTKOL, SORKOL, CHNKOL, IFKOL,
     *   SUBKOL, FQKOL, OPKOL, STKKOL, TIMKOL, DATKOL, CLPKOL, DTSKOL,
     *   ITSKOL, REAKOL, RECI(NVALS)
      REAL      RECR(NVALS)
      HOLLERITH RECH(NVALS)
      EQUIVALENCE (RECR, RECI, RECH)
      EQUIVALENCE (KOLS(1), NUMKOL), (KOLS(2), OPKOL),
     *   (KOLS(3), TIMKOL), (KOLS(4), ANTKOL), (KOLS(5), SORKOL),
     *   (KOLS(6), CHNKOL), (KOLS(7), IFKOL), (KOLS(8), SUBKOL),
     *   (KOLS(9), FQKOL), (KOLS(10), STKKOL), (KOLS(11), DATKOL),
     *   (KOLS(12), CLPKOL), (KOLS(13), DTSKOL), (KOLS(14), ITSKOL),
     *   (KOLS(15), REAKOL)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IFCRNO, RECR, BUF, IERR)
         IF (IERR.GT.0) GO TO 980
         GO TO 999
         END IF
C                                       local copy column pointers
      CALL COPY (NCOLS, FCKOLS, KOLS)
C                                       fill record
      IF (OPCODE.EQ.'WRIT') THEN
         RECI(NUMKOL)   = FLGNUM
         CALL CHR2H (8, FLGOP, 1, RECH(OPKOL))
         RECR(TIMKOL)   = FLGTIM(1)
         RECR(TIMKOL+1) = FLGTIM(2)
         RECI(ANTKOL)   = FLGANT(1)
         RECI(ANTKOL+1) = FLGANT(2)
         RECI(SORKOL)   = FLGSOR
         RECI(CHNKOL)   = FLGCHN(1)
         RECI(CHNKOL+1) = FLGCHN(2)
         RECI(IFKOL)    = FLGIF(1)
         RECI(IFKOL+1)  = FLGIF(2)
         RECI(SUBKOL)   = FLGSUB
         RECI(FQKOL)    = FLGFQ
         CALL CHR2H (4, FLGSTK, 1, RECH(STKKOL))
         CALL CHR2H (8, DTYPE, 1, RECH(DATKOL))
         RECR(CLPKOL)   = DFLUXS(1)
         RECR(CLPKOL+1) = DFLUXS(2)
         RECR(DTSKOL)   = DTIMES(1)
         RECR(DTSKOL+1) = DTIMES(2)
         RECI(ITSKOL)   = FLGIT(1)
         RECI(ITSKOL+1) = FLGIT(2)
         CALL CHR2H (24, FLGREA, 1, RECH(REAKOL))
         END IF
C                                       Process record.
 10   CALL TABIO (OPCODE, 0, IFCRNO, RECR, BUF, IERR)
      IFCRNO = IFCRNO + 1
      IF (IERR.GT.0) GO TO 980
C                                       If READ pick data from RECR.
      IF (OPCODE.EQ.'READ') THEN
         FLGNUM    = RECI(NUMKOL)
         CALL H2CHR (8, 1, RECH(OPKOL), FLGOP)
         FLGTIM(1) = RECR(TIMKOL)
         FLGTIM(2) = RECR(TIMKOL+1)
         FLGANT(1) = RECI(ANTKOL)
         FLGANT(2) = RECI(ANTKOL+1)
         FLGSOR    = RECI(SORKOL)
         FLGCHN(1) = RECI(CHNKOL)
         FLGCHN(2) = RECI(CHNKOL+1)
         FLGIF(1)  = RECI(IFKOL)
         FLGIF(2)  = RECI(IFKOL+1)
         FLGSUB    = RECI(SUBKOL)
         FLGFQ     = RECI(FQKOL)
         CALL H2CHR (4, 1, RECH(STKKOL), FLGSTK)
         CALL H2CHR (8, 1, RECH(DATKOL), DTYPE)
         DFLUXS(1) = RECR(CLPKOL)
         DFLUXS(2) = RECR(CLPKOL+1)
         DTIMES(1) = RECR(DTSKOL)
         DTIMES(2) = RECR(DTSKOL+1)
         FLGIT(1)  = RECI(ITSKOL)
         FLGIT(2)  = RECI(ITSKOL+1)
         CALL H2CHR (24, 1, RECH(REAKOL), FLGREA)
         END IF
      GO TO 999
C                                       Error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('TABFC: TABIO ERROR',I3)
      END
