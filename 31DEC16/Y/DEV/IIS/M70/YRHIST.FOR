      SUBROUTINE YRHIST (MODE, COLOR, INITI, NINT, HISTOG, IERR)
C-----------------------------------------------------------------------
C! read the histogram of the selected TV output color
C# TV-basic
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
C   YRHIST reads the histogram of the output of a selected OFM of the
C   TV.   Intensity values 0 through OFMOUT may be read.
C   Inputs:
C      MODE    I         selects area to histogram: 0 blotch,
C                        1 not blotch, 2 all, 3 external blotch
C      COLOR   I         bit map of single color (RGB - 4,2,1)
C      INITI   I         first intensity to histo (1 - 1024)
C      NINT    I         # values to get
C   Output:
C      HISTOG  I(NINT)   histogram
C      IERR    I         error code of Z...XF : 0 => ok, 2 => input err
C    IIS Model 70 version
C-----------------------------------------------------------------------
      INTEGER   MODE, COLOR, INITI, NINT, HISTOG(*), IERR
C
      INTEGER   HEADER(8), UNIT, BUFSIZ, NB, COUNT, IX, IY, I, J,
     *   SCRTCH(2048)
      LOGICAL   VRTRTC, COMND1, COMND2
      DATA UNIT, COMND1, COMND2, BUFSIZ /7,.TRUE.,.FALSE.,1/
      DATA VRTRTC /.FALSE./
C-----------------------------------------------------------------------
C                                        check errors
      IERR = 2
      IF ((MODE.LT.0) .OR. (MODE.GT.3)) GO TO 999
      IF ((COLOR.NE.1) .AND. (COLOR.NE.2) .AND. (COLOR.NE.4)) GO TO 999
      IF ((INITI.LT.1) .OR. (NINT.LT.1) .OR. (INITI+NINT-1.GT.1024))
     *   GO TO 999
C                                        write command buffer
      CALL YMKHDR ('WRIT', UNIT, COMND1, BUFSIZ, 0, 0, 0, 0, VRTRTC,
     *   HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF ('WRIT', NB, HEADER, MODE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        read histogram
      COUNT = 2 * NINT
      IY = 2 * (INITI-1)
      IX = MOD (IY, 512)
      IY = IY / 512
      CALL YMKHDR ('READ', UNIT, COMND2, COUNT, IX, IY, COLOR, 0,
     *   VRTRTC, HEADER)
      NB = 2 * COUNT
      CALL ZM70XF ('READ', NB, HEADER, SCRTCH, IERR)
C                                       16 bit words in ZM70XF
C                                       repack into full integers
      CALL ZIPACK ('LTOS', COUNT, SCRTCH, 1, HISTOG)
C
 999  RETURN
      END
