      SUBROUTINE HENCOO (PRGNAM, NAMEOT, CLASOT, SEQOT, DISKOT,
     *   IHLUN, IOBLK, IERR)
C-----------------------------------------------------------------------
C! adds OUTNAME, OUTCLASS, OUTSEQ, OUTDISK to an open history file
C# History
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
C   HENCOO will add OUTNAME, OUTCLASS, OUTSEQ, OUTDISK values to an
C   open history file.
C   Inputs:
C      PRGNAM    C*6     task or verb name
C      NAMEOT    C*12    OUTNAME value
C      CLASOT    C*6     OUTCLASS value
C      SEQOT     I       OUTSEQ value
C      DISKOT    I       OUTDISK value
C      IHLUN     I       logical unit no. of an open history file
C   In/out:
C      IOBLK     I(256)  history work I/O buffer
C   Output:
C      IERR      I       error returned from HIADD. 0=ok
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, NAMEOT*12, CLASOT*6
      INTEGER  SEQOT, DISKOT, IHLUN, IOBLK(256), IERR
C
      CHARACTER HILINE*72
C-----------------------------------------------------------------------
      WRITE (HILINE,1000) PRGNAM, NAMEOT, CLASOT
      CALL HIADD (IHLUN, HILINE, IOBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (HILINE,1010) PRGNAM, SEQOT, DISKOT
      CALL HIADD (IHLUN, HILINE, IOBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'OUTNAME=''',A12,'''',3X,'OUTCLASS=''',A6,'''')
 1010 FORMAT (A6,'OUTSEQ=',I5,3X,'OUTDISK=',I3)
      END
