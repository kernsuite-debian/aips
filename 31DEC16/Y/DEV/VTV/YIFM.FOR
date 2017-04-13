      SUBROUTINE YIFM (OP, START, COUNT, PACK, VRTRTC, IFM, IERR)
C-----------------------------------------------------------------------
C! read/write TV Input look-up-table
C# Y2 TV-basic
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
C   YIFM reads/writes a section of TV input function memory.  This
C   look up table takes 13 bits in and gives 8 bits out.
C   Inputs:
C      OP      C*4    'READ' from TV or 'WRIT' to TV
C      START   I      start address of IFM (1 - 8192)
C      COUNT   I      # elements of IFM to transfer (1-8192)
C      PACK    L      T => 2 values/word, F => 1 value/word
C      VRTRTC  L      T => do it only on vertical retrace
C   In/Out:
C      IFM     I(*)   function values (0-255)
C   Output:
C      IERR    I      error code of Z...XF: 0 - ok
C                                           2 - input error
C   Virtual TV version (TV by communication)
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   START, COUNT, IFM(*), IERR
      LOGICAL   PACK, VRTRTC
C
      INTEGER   I
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
C                                       check
      IERR = 2
      IF (START.LT.1) GO TO 900
      IF (START+COUNT-1.GT.8192) GO TO 900
C                                       do it
      BUFFER(1) = START
      BUFFER(2) = COUNT
      BUFFER(3) = 0
      IF (PACK) BUFFER(3) = 1
      BUFFER(4) = 0
      IF (VRTRTC) BUFFER(4) = 1
      I = COUNT - START + 1
      IF (PACK) I = (I + 1) / 2
      IF (OP.EQ.'WRIT') THEN
         CALL COPY (I, IFM, BUFFER(5))
         BUFSR = 0
         BUFSW = 4 + I
      ELSE
         BUFSR = I + 4
         BUFSW = 4
         END IF
      CALL MKYBUF (OP, 'YIFM', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) THEN
         CALL UNYBUF (BUFSR, BUFFER, HEADER)
         CALL COPY (I, BUFFER(5), IFM)
         END IF
      GO TO 999
C                                       error
 900  WRITE (MSGTXT,1900) START, COUNT
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('YIFM: ERROR IN START, COUNT =',2I7)
      END
