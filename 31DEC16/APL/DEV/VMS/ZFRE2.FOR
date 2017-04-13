      SUBROUTINE ZFRE2 (NVOL, VOLNAM, VLEN, TOTBLK, PCTFUL, FRBLKS)
C-----------------------------------------------------------------------
C! return AIPS data disk free space information
C# Z2 IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004, 2009
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
C   Get AIPS disk free space information.
C   Inputs:
C      NVOL     I       Number of disks
C   Output:
C      VOLNAM   C*(*)   Volume names
C      VLEN     I(*)    Length in characters per volume name
C      TOTBLK   I(*)    Total number of Mbytes on device
C      PCTFUL   R*4(*)  Percentage of device full
C      FRBLKS   I(*)    Number of free Mbytes remaining
C   VMS version
C-----------------------------------------------------------------------
      INTEGER   NVOL, VLEN, TOTBLK(*), FRBLKS(*)
      HOLLERITH VOLNAM(*)
      REAL      PCTFUL(*)
C
      INTEGER   JERR, SYS$GETDVIW, FBLOCK, FBTOT, TBLOCK, TBTOT, TOTFIL,
     *   I, I1, I2, IEND, LISTI(13), JTRIM
      INTEGER*2 LIST2(26)
      CHARACTER CTEMP*1, NAME*64, VOLN*12
      INCLUDE '($DVIDEF)'
      EQUIVALENCE (LISTI, LIST2)
C-----------------------------------------------------------------------
C                                       prepare data structure
      CALL FILL (13, 0, LISTI)
      LIST2(1) = 12
      LIST2(7) = 4
      LIST2(13) = 4
      LIST2(19) = 64
      LIST2(2) = DVI$_VOLNAM
      LIST2(8) = DVI$_FREEBLOCKS
      LIST2(14) = DVI$_MAXBLOCK
      LIST2(20) = DVI$_NEXTDEVNAM
      LISTI(2) = %LOC (VOLN)
      LISTI(5) = %LOC (FBLOCK)
      LISTI(8) = %LOC (TBLOCK)
      LISTI(11) = %LOC (NAME)
C                                       Loop for all AIPS disks.
      DO 100 I = 1,NVOL
         CALL ZEHEX (I, 1, CTEMP)
         NAME = 'DA0' // CTEMP
         IEND = 4
         FBTOT = 0
         TBTOT = 0
         VOLN = 'UNAVAILABLE '
C                                       Call service for all volumes
         DO 40 I2 = 1,99
C                                       Zero fields in case of error.
            FBLOCK = 0
            TBLOCK = 0
C                                       Get device info.
            JERR = SYS$GETDVIW ( , , NAME(:IEND), LISTI, , , , )
C                                       Change any nulls to blanks.
            IEND = JTRIM (VOLN)
C                                       Update totals for this volume.
            FBTOT = FBTOT + FBLOCK
            TBTOT = TBTOT + TBLOCK
C                                       Quit if no more volumes.
            IEND = JTRIM (NAME)
            IF (IEND.EQ.0) GO TO 50
 40         CONTINUE
C                                       Return device info.
 50      IF (I2.GT.1) VOLN = 'MULTI-VOLUME'
         I1 = (I-1) * VLEN + 1
         CALL CHR2H (VLEN, VOLN, I1, VOLNAM)
         FRBLKS(I) = FBTOT / 2 / 1024.0 + 0.5
         TOTBLK(I) = TBTOT / 2 / 1024.0 + 0.5
         IF (TBTOT.GT.0) THEN
            PCTFUL(I) = (100.0 * (TBTOT - FBTOT)) / TBTOT + 0.5
         ELSE
            PCTFUL(I) = 100.0
            END IF
 100     CONTINUE
C
 999  RETURN
      END
