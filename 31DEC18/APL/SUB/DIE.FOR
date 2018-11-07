      SUBROUTINE DIE (ICODE, BUFF)
C-----------------------------------------------------------------------
C! closes down tasks which use DFIL.INC to maintain status of files
C# System IO-util
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
C   DIE does the housekeeping necessary for an orderly death of the
C   task.  Primarily clearing catalog flags and destroying scratch
C   files.  It also calls RELPOP if RQUICK is false.
C   Inputs:
C      ICODE    I        Return code: 0 => good, other => bad end
C      BUFF     I(256)   Work buffer
C   Locations in catalog are communicated by COMMON /CFILES/:
C      NCFILE     I    Number of files marked in catalog.
C      FVOL       I    Volumn numbers of the maps.
C      FCNO       I    Slot numbers of the maps.
C      FRW        I    A 0 if READ , 1 if WRITE clear desired,
C                      a 2 if a new file with Write, destroy on ICODE
C                      bad;  other values => file already closed.
C      NSCR       I    Number of scratch files to be destroyed
C      SCRVOL     I    Scratch file volumn numbers
C      SCRCNO     I    Scratch file catalog numbers.
C   Programmer: W. D. Cotton  June,1980.
C-----------------------------------------------------------------------
      INTEGER   ICODE, BUFF(256)
C
      INTEGER   IND, I, IERR, J, K
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Close all files (except msg)
      BUFF(1) = NTAB1
      BUFF(2) = NTAB2
      BUFF(3) = NTAB3
      BUFF(4) = NBTB1 / 2
      BUFF(5) = NBTB2 / 2
      BUFF(6) = NBTB3 / 2
      IND = 1
      DO 10 I = 1,3
         IF (BUFF(I).LE.0) GO TO 10
            K = BUFF(I)
            DO 5 J = 1,K
               IF ((FTAB(IND).NE.0) .AND. (FTAB(IND).NE.6) .AND.
     *            (FTAB(IND).NE.12)) CALL ZCLOSE (FTAB(IND), IND, IERR)
               IND = IND + BUFF(I+3)
 5             CONTINUE
 10      CONTINUE
C                                       Prevent destroy on normal end
      IF ((ICODE.GT.0) .OR. (NCFILE.LE.0)) GO TO 20
         DO 15 I = 1,NCFILE
            IF (FRW(I).EQ.2) FRW(I) = 1
 15         CONTINUE
C                                       Clear catalog flags.
 20   CALL MAPCLR (NCFILE, FVOL, FCNO, FRW, BUFF)
C                                       Destroy scratch files.
      IF (NSCR.LE.0) GO TO 40
         DO 25 I = 1,NSCR
            FRW(I) = 2
 25         CONTINUE
 30      CALL MAPCLR (NSCR, SCRVOL, SCRCNO, FRW, BUFF)
C                                       If RQUICK = F restart AIPS.
C                                       Send message to user.
 40   CALL DIETSK (ICODE, RQUICK, BUFF)
C
  999 RETURN
      END
