      SUBROUTINE LSERCH (OP, LUN, FIND, MAP, IERR)
C-----------------------------------------------------------------------
C! opens, locates, closes entries in the common file table (FTAB)
C# IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2004
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
C   Subroutine to either find an existing entry in the I/O table FTAB
C   or initialize a new one.
C   Inputs:
C      OP    C*4   Operation 'SRCH' -> Find slot containing LUN,
C                     return FIND
C                  'OPEN' -> Find open slot, create table entry there
C                  'CLOS' -> Clear slot corresponding to LUN
C      LUN   I     Logical unit number to use or seek
C      MAP   L     TRUE if a map file, FALSE otherwise
C      FTAB  I     array containing I/O tables
C   Outputs:
C      FIND  I     Index in FTAB where LUN is stored if operation works
C      IERR  I     Error   0 = OK, Mission completed
C                          1 = slot containing LUN not found
C                          2 = LUN already in use
C                          3 = FTAB full
C                          4 = input error
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   LUN, FIND, IERR
      LOGICAL   MAP
C
      INTEGER   NMAX, NOFF, NINC, I, IOP
      LOGICAL   ISDEV
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       check input
      IERR = 4
      IF ((LUN.LE.0) .OR. (LUN.GT.128)) GO TO 999
      IOP = 0
      IF (OP.EQ.'SRCH') IOP = 1
      IF (OP.EQ.'OPEN') IOP = 2
      IF (OP.EQ.'CLOS') IOP = 3
      IF (IOP.EQ.0) GO TO 999
      ISDEV = MOD (DEVTAB(LUN), 2).EQ.1
      IF ((MAP) .AND. (ISDEV)) GO TO 999
      IERR = 0
C                                        non-FTAB files
 25   NOFF = 1
      NINC = NBTB1/2
      NMAX = NTAB1
C                                       full FTAB part table?
      IF (ISDEV) GO TO 30
         NOFF = 1 + (NBTB1*NTAB1)/2
         NINC = NBTB2/2
         NMAX = NTAB2
C                                        map type FTAB
         IF (.NOT.MAP) GO TO 30
            NOFF = NOFF + (NBTB2*NTAB2)/2
            NINC = NBTB3/2
            NMAX = NTAB3
C                                        LUN already in table?
 30   IF (NMAX.LE.0) GO TO 40
         DO 35 I = 1,NMAX
            FIND = NOFF + (I-1)*NINC
            IF (FTAB(FIND).NE.LUN) GO TO 35
               IF (IOP.EQ.3) FTAB(FIND) = 0
               IF (IOP.EQ.2) IERR = 2
               GO TO 999
 35         CONTINUE
C                                       slot not found
 40   IF (IOP.EQ.2) GO TO 45
         IERR = 1
         GO TO 999
C                                       find empty slot
 45   IF (NMAX.GT.0) THEN
         DO 50 I = 1,NMAX
            FIND = NOFF + (I-1)*NINC
C                                       init UFT entry
            IF (FTAB(FIND).EQ.0) THEN
               FTAB(FIND) = LUN
               IF (.NOT.ISDEV) CALL ZTFILL (FIND, MAP)
               GO TO 999
               END IF
 50         CONTINUE
         END IF
C                                       no empty slots
      IERR = 3
C
 999  RETURN
      END
