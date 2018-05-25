      PROGRAM ZSTOPA
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
C  Changes current process name to the name of the users terminal
C  (_TTA3:) by translating the logical name for TT. In the case the
C  login procedure is initiated through DECNET, the assignment of
C  TT is to a FAL.LOG file. In this case no rename is attempted.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER   I4ERR, SYS$TRNLOG, SYS$SETPRN
      INTEGER   I, II
      CHARACTER*16 MYNAME
      CHARACTER*2 TT
      CHARACTER*1 NULL
      DATA TT /'TT'/
C-----------------------------------------------------------------------
C                                       Translate log name TT:
      NULL = CHAR (0)
      I4ERR = SYS$TRNLOG (TT, , MYNAME, , , ,)
      IF (I4ERR.NE.1) GO TO 999
C                                       Skip underscores.
      I = 1
      DO WHILE (MYNAME(I:I).EQ.'_')
         I = I + 1
         END DO
C                                       Change the process name.
      II = INDEX (MYNAME, NULL) - 1
      I4ERR = SYS$SETPRN (MYNAME(I:II) )
 999  CONTINUE
      END
