      SUBROUTINE MAPCLR (NFILES, FVOL, FCNO, FRW, BUFF)
C-----------------------------------------------------------------------
C! clears status flags in catalog and deletes lists of files
C# Catalog
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
C   MAPCLR clears catalog flags for cataloged images or uv files marked
C   READ or WRITE.  It also destroys any marked as type 2 in FRW.
C   In/out:
C      NFILES  I       Number of files to be cleared (set to 0 at end)
C   Inputs:
C      FVOL    I(*)    Disk volume numbers of the files
C      FCNO    I(*)    Slot numbers of the files
C      FRW     I(*)    A 0 if READ, A 1 if WRITE, a 2 if WRIT,destroy
C                      other values cause no action for that entry
C      BUFF    I(256)  a working buffer.
C   Common:
C      /MAPHDR/        used and changed for cat headers (if any destroy)
C-----------------------------------------------------------------------
      INTEGER   NFILES, FVOL(*), FCNO(*), FRW(*), BUFF(256)
C
      CHARACTER CHTM12*12, CHTM6*6, CHTM2*2, OP*4
      INTEGER   IERR, CATBLK(256), I, J, DUM
      INCLUDE 'INCS:DMSG.INC'
      COMMON /MAPHDR/ CATBLK
C-----------------------------------------------------------------------
C                                       Return if NFILES .LE. 0
      IF (NFILES.LE.0) GO TO 999
C                                       Clear catalog flags.
         DO 20 I = 1,NFILES
            IF ((FVOL(I).LE.0) .OR. (FCNO(I).LE.0)) GO TO 20
            IF ((FRW(I).LT.0) .OR. (FRW(I).GT.2)) GO TO 20
            OP = 'CLRD'
            IF ((FRW(I).EQ.1) .OR. (FRW(I).EQ.2)) OP = 'CLWR'
            CHTM12 = ' '
            CHTM6 = ' '
            CHTM2 = ' '
            DUM = 0
            CALL CATDIR ('CSTA', FVOL(I), FCNO(I), CHTM12, CHTM6, DUM,
     *         CHTM2, DUM, OP, BUFF, IERR)
C                                       Ignore error 10
            IF (IERR.EQ.10) IERR = 0
C                                       If it doesn't exist forget it.
            IF (IERR.EQ.6) THEN
               IERR = 0
               GO TO 20
               END IF
            IF (IERR.EQ.0) GO TO 10
               WRITE (MSGTXT,1000) IERR, FVOL(I), FCNO(I)
               CALL MSGWRT (6)
C                                       Destroy type 2's
 10         IF (FRW(I).EQ.2) CALL MDESTR (FVOL(I), FCNO(I), CATBLK,
     *         BUFF, J, IERR)
 20         CONTINUE
C                                       Reset NFILES to zero.
         NFILES = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAPCLR: ERROR',I3,' CLEARING MAP ON VOL',I3,' SLOT',I4)
      END
