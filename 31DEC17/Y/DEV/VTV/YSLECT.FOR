      SUBROUTINE YSLECT (OP, CHAN, COLOR, LBUFF, IERR)
C-----------------------------------------------------------------------
C! turn gray and graphics planes on and off
C# Y0 TV-util
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
C   YSLECT enables and disables gray and graphics planes
C   Inputs:
C      OP       C*4      'ONNN' or 'OFFF'
C      CHAN     I        channel number ( 1 to NGRAY+NGRAPH)
C      COLOR    I        0 - all,   1,2,3 = R,G,B, resp.
C   Output:
C      LBUFF    I(256)   scratch buffer (for graphics only)
C      IERR     I        error code of Z...XF: 0 - ok, 2 - input error
C   YSLECT sets TVLIMG in the TV device parms common /TVDEV/
C   Virtual TV version (TV by communication).
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, COLOR, LBUFF(*), IERR
C
      INTEGER   ZEOR, ZOR, ZAND, I, MASK
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
C                                        check input
      IERR = 2
      IF ((CHAN.LT.1) .OR. (CHAN.GT.NGRAY+NGRAPH)) GO TO 999
      IF ((OP.NE.'ONNN') .AND. (OP.NE.'OFFF')) GO TO 999
      IF (((COLOR.LT.0) .OR. (COLOR.GT.3)) .AND. (CHAN.LE.NGRAY))
     *   GO TO 999
C                                       send parms
      BUFFER(1) = 0
      IF (OP.EQ.'ONNN') BUFFER(1) = 1
      BUFFER(2) = CHAN
      BUFFER(3) = COLOR
      BUFSW = 3
      BUFSR = 0
      CALL MKYBUF ('WRIT', 'YSLECT', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                        reset common
      IF (IERR.EQ.0) THEN
         MASK = 2 ** (CHAN - 1)
         IF (OP.EQ.'OFFF') MASK = ZEOR (-1, MASK)
         DO 10 I = 1,4
            IF (OP.EQ.'ONNN') THEN
               TVLIMG(I) = ZOR (TVLIMG(I), MASK)
            ELSE
               TVLIMG(I) = ZAND (TVLIMG(I), MASK)
               END IF
 10         CONTINUE
         END IF
C
 999  RETURN
      END
