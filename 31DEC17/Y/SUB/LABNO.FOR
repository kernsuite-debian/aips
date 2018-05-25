      SUBROUTINE LABNO (XNO, IFORM, IPOS, IPBLK, IERR)
C-----------------------------------------------------------------------
C! write a tick mark numeric label in a plot file
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   LABNO will write a number at the appropriate position to label a
C   tick mark.
C   Inputs:
C      XNO    R       the value of the number.
C      IFORM  I       the format to use with the number.
C                     1 = F6.3, 2 = F6.2, 3 = F6.1, 4 = F6.0
C      IPOS   I       position indicator.  1 = below current
C                     position, 2 = left, 3 = above, 4 = right.
C      IPBLK  I(256)  plot file I/O buffer.
C   Output:
C      IERR   I       error code from GCHAR.
C-----------------------------------------------------------------------
      INTEGER   IFORM, IPOS, IPBLK(256), IERR
      REAL      XNO
C
      INTEGER   ILEN
      REAL      DCX, DCY
      CHARACTER CHTEMP*6
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (IFORM.EQ.1) WRITE (CHTEMP,1000) XNO
      IF (IFORM.EQ.2) WRITE (CHTEMP,1010) XNO
      IF (IFORM.EQ.3) WRITE (CHTEMP,1020) XNO
      IF (IFORM.EQ.4) WRITE (CHTEMP,1030) XNO
      CALL CHTRIM (CHTEMP, 6, CHTEMP, ILEN)
C                                      Delete trailing decimal point.
      IF (IFORM.EQ.4) ILEN = ILEN - 1
C                                       Find position.
      DCX = 0.5 - ILEN
      IF (IPOS.EQ.2) DCX = - ILEN - 0.5
      IF (IPOS.EQ.4) DCX = 0.5
      DCY = -0.5
      IF (IPOS.EQ.1) DCY = -1.5
      IF (IPOS.EQ.3) DCY = 0.5
C                                       Write characters to plot file.
      CALL GCHAR (ILEN, 0, DCX, DCY, CHTEMP, IPBLK, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (F6.3)
 1010 FORMAT (F6.2)
 1020 FORMAT (F6.1)
 1030 FORMAT (F6.0)
      END
