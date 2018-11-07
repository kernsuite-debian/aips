      SUBROUTINE YINIT (SCRTCH, IERR)
C-----------------------------------------------------------------------
C! initialize everything about the TV
C# TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2008
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
C   YINIT initializes the TV subunits and image catalog
C   Output:
C      SCRTCH  I(1024)    scratch buffer
C      IERR    I          error code of Z...XF - 0 => ok
C                                                2 => input error
C   XAS version first asks the TV server its critical parameters.
C   Inits the TV image header catalog
C-----------------------------------------------------------------------
      INTEGER   SCRTCH(*), IERR
C
      INTEGER   OPCODE, DAT(4), NWSD
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Instruct XAS to init
C                                       was 15
      OPCODE = 29
      CALL FILL (4, 0, DAT)
      CALL ZSSSXF (OPCODE, DAT, 0, SCRTCH, NWSD, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'Init XAS'
         GO TO 990
         END IF
C                                       Now ask it for parameter common
      CALL YDTVC ('GET ', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'Get parameters from XAS'
         GO TO 990
         END IF
C                                       Clear image catalog
      CALL YCINIT (0, SCRTCH)
      GO TO 999
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' on ',A)
      END
