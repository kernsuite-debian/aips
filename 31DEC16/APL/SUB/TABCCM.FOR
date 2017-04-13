      SUBROUTINE TABCCM (OPCODE, BUFFER, ICCRNO, CCKOLS, CCNUMV,
     *   NUMCOL, X, Y, FLUX, TYPE, PARMS, IERR)
C-----------------------------------------------------------------------
C! Do I/O to CLEAM model (CC) table after setup by CCMINI.
C# EXT-util Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2011, 2015
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
C   Does I/O to CLEAN model (CC) extention tables. Usually used after
C   setup by CCMINI.
C   Inputs:
C      OPCODE  C*4      Operation code:
C                       'READ' = read entry from table.
C                       'WRIT' = write entry in table.
C                       'CLOS' = close file, flush on write
C      BUFFER  I(512)   I/O buffer and related storage, also defines file
C                        if open. Should have been returned by CCMINI or
C                        TABINI.
C      CCKOLS  I(MAXCCC) The column pointer array in order, Flux, X, Y
C                        (and if NUMCOL>3) Major, Minor, PA, type.
C      CCNUMV  I(MAXCCC) Element count in each column.
C      NUMCOL  I         Number of columns, 3 => point components only
C   Input/output: (written to or read from CC file)
C      ICCRNO  I         Next entry number to read or write.
C      X       R         "X" position in degrees
C      Y       R         "Y" position in degrees
C      FLUX    R         Model flux density (Jy)
C      TYPE    I         Model type: (NUMCOL > 3)
C                          0 = point
C                          1 = Gaussian on sky
C                          2 = Convolved Gaussian
C                          3 = Uniform optically thin sphere
C      PARMS   R(*)      Model parameters (NUMCOL > 3)
C                        (min of 3 for Gaussians)
C   Output:
C      IERR    I         Error code, 0=>OK else TABIO error.
C                        Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), CCKOLS(MAXCCC), CCNUMV(MAXCCC), NUMCOL,
     *   ICCRNO, TYPE, IERR
      REAL      X, Y, FLUX, PARMS(*)
C
      INTEGER   KOLS(MAXCCC), FLXKOL, XKOL, YKOL, PRMKOL, TYPKOL
      LOGICAL   DOREAD, DOCLOS
      INTEGER   NDATA, NPARM
      REAL      RECORD(MAXCCC)
      INCLUDE 'INCS:DMSG.INC'
C                                       Note: change this if the
C                                       structure of the CC table
C                                       changes.
      EQUIVALENCE (KOLS(1), FLXKOL), (KOLS(2), XKOL),
     *   (KOLS(3), YKOL), (KOLS(4), PRMKOL), (KOLS(7), TYPKOL)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       Set pointers
      NDATA = MAXCCC
      CALL COPY (NDATA, CCKOLS, KOLS)
C                                       Number of model parameters
      NPARM = NUMCOL - 4
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
         RECORD(FLXKOL) = FLUX
         RECORD(XKOL) = X
         RECORD(YKOL) = Y
         IF (NUMCOL.GT.3) THEN
            IF (CCNUMV(7).GT.0) RECORD(TYPKOL) = TYPE
            IF (CCNUMV(4).GT.0) CALL RCOPY (NPARM, PARMS,
     *         RECORD(PRMKOL))
            END IF
         END IF
C                                       Do I/O
      CALL TABIO (OPCODE, 0, ICCRNO, RECORD, BUFFER, IERR)
      ICCRNO = ICCRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ, pick data from RECORD.
      IF (DOREAD) THEN
         FLUX = RECORD(FLXKOL)
         X = RECORD(XKOL)
         Y = RECORD(YKOL)
         IF (NUMCOL.GT.3) THEN
            IF (CCNUMV(7).GT.0) THEN
               TYPE = RECORD(TYPKOL) + 0.5
            ELSE
               TYPE = 0
               END IF
            IF (CCNUMV(4).GT.0) THEN
               CALL RCOPY (NPARM, RECORD(PRMKOL), PARMS)
            ELSE
               CALL RFILL (NPARM, 0.0, PARMS)
               END IF
            END IF
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, ICCRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABCCM: TABIO ERROR',I3)
      END
