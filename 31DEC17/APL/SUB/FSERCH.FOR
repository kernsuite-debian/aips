      SUBROUTINE FSERCH (FIND, ITYP, INUM, IERR)
C-----------------------------------------------------------------------
C! determines type and number of entries in the common file table (FTAB)
C# IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Finds entry in FTAB and returns file type and number of type
C   Inputs:
C      FIND   I   Index in FTAB where file has been opened
C   Outputs:
C      ITYP  I    Type of file: 1 device, 2 FCB single buffer,
C                 3 FCB double buffer
C      INUM  I    File number within ITYP
C      IERR  I    Error: 0 = OK
C                    1 = FIND doesn't point at legit location
C                    2 = FIND out of range
C-----------------------------------------------------------------------
      INTEGER   FIND, ITYP, INUM, IERR
C
      INTEGER   NMAX, NOFF, NINC, I, IOP
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      INUM = 0
      ITYP = 0
      IERR = 2
      IF (FIND.LE.0) GO TO 999
C                                        non-FTAB files
      NOFF = 1
      NINC = NBTB1/2
      NMAX = NTAB1
      IOP = 1
C                                       full FTAB part table?
      IF (FIND.GE.NOFF+NMAX*NINC) THEN
         NOFF = NOFF + NINC*NMAX
         NINC = NBTB2/2
         NMAX = NTAB2
         IOP = 2
C                                        map type FTAB
         IF (FIND.GE.NOFF+NMAX*NINC) THEN
            NOFF = NOFF + NINC*NMAX
            NINC = NBTB3/2
            NMAX = NTAB3
            IOP = 3
C                                       error
            IF (FIND.GE.NOFF+NMAX*NINC) GO TO 999
            END IF
         END IF
C                                       lands on correct spot?
      I = (FIND - NOFF) / NINC + 1
      IERR = 1
      IF ((I-1)*NINC+NOFF.EQ.FIND) THEN
         IERR = 0
         ITYP = IOP
         INUM = I
         END IF
C
 999  RETURN
      END
