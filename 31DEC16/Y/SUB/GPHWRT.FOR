      SUBROUTINE GPHWRT (BUFF, IERR)
C-----------------------------------------------------------------------
C! write plot buffer to file, prepares buffer for more commands
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
C   GPHWRT writes current buffer on graphics file, updates record
C   count, and rezeros the buffer and position pointer
C   In/out:
C      BUFF   I(256)   full graphics buffer in, cleared buffer out
C   Outputs:
C      IERR   I        error code: 0 => ok
C                              other => disk error
C-----------------------------------------------------------------------
      INTEGER   BUFF(256), IERR
C
      INTEGER   NREC
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
      GPHRRN = GPHRRN + 1
      IF (GPHRRN.LE.GPHSIZ) GO TO 10
         NREC = 21
         CALL ZEXPND (GPHLUN, GPHVOL, GPHNAM, NREC, IERR)
         GPHSIZ = GPHSIZ + NREC
         IF (IERR.NE.0) GO TO 999
 10   CALL ZFIO ('WRIT', GPHLUN, GPHIND, GPHRRN, BUFF, IERR)
      GPHPOS = 0
      CALL FILL (256, 0, BUFF)
C
 999  RETURN
      END
