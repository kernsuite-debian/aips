      SUBROUTINE GMCAT (INO, IBLK, IPBLK, IERR)
C-----------------------------------------------------------------------
C! writes a copy-misc-image-catalog-info records into a plot file or TV
C# Plot-util
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
C   This routine will write a 'copy misc info into the image catalog'
C   command record to a graph file.
C   The output record description is:
C            I        opcode, 8 for this record type.
C            I        number of words.
C            I(INO)   miscellaneous information.
C   Inputs:
C      INO     I       number of words of misc info.
C      IBLK    I(INO)  array containing misc info.
C      IPBLK   I       plot file I/O block.
C   Output:
C      IERR    I       error code 0=ok, 3=no. of words over 20,
C                         2 disk error
C   Common:
C      GPHPOS  I       incremented by 2 + INO.
C      GPHRRN  I       incremented by 1 if write to disk necessary.
C-----------------------------------------------------------------------
      INTEGER   INO, IBLK(*), IPBLK(256), IERR
C
      INTEGER   ICODE, NEED, IER
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA ICODE /8/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Too many words.
      IF ((INO.GE.21) .OR. (INO.LT.1)) THEN
         IERR = 3
         WRITE (MSGTXT,1000) INO
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Plot file
      IF (.NOT.GPHDOT) THEN
C                                       Calculate space needed.
         NEED = 254 - INO
         IF (GPHPOS.GT.NEED) THEN
            CALL GPHWRT (IPBLK, IER)
            IF (IER.NE.0) THEN
               IERR = 1
               GO TO 999
               END IF
            END IF
C                                       Put stuff in plot file buff.
         IPBLK(GPHPOS+1) = ICODE
         IPBLK(GPHPOS+2) = INO
         CALL COPY (INO, IBLK, IPBLK(GPHPOS+3))
         GPHPOS = GPHPOS + INO + 2
C                                       TV mode
      ELSE
         CALL COPY (INO, IBLK, GPHCAT(IIOTH))
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GMCAT: MAX WORDS = 20, YOUR WORDS =',I7)
      END
