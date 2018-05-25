      SUBROUTINE YSLECT (OP, CHAN, COLOR, BUFFER, IERR)
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
C      BUFFER   I(256)   scratch buffer (for graphics only)
C      IERR     I        error code of Z...XF: 0 - ok, 2 - input error
C   YSLECT sets TVLIMG in the TV device parms common /TVDEV/
C   Generic version - uses YSPLIT.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, COLOR, BUFFER(*), IERR
C
      INTEGER   ZEOR, ZOR, ZAND, I, IC1, IC2, MASK, ICMS(12), IX, IY
      LOGICAL   F
      INCLUDE 'INCS:DTVC.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                        check input
      IERR = 2
      IF ((CHAN.LT.1) .OR. (CHAN.GT.NGRAY+NGRAPH)) GO TO 999
C                                        gray scales
      IF (CHAN.LE.NGRAY) THEN
         IF ((OP.NE.'ONNN') .AND. (OP.NE.'OFFF')) GO TO 999
         IF ((COLOR.LT.0) .OR. (COLOR.GT.3)) GO TO 999
         IF (COLOR.NE.0) THEN
            IC2 = 4 * COLOR
            IC1 = IC2 - 3
         ELSE
            IC1 = 1
            IC2 = 12
            END IF
         MASK = 2 ** (CHAN-1)
         IF (OP.EQ.'OFFF') MASK = ZEOR (-1, MASK)
         CALL YSPLIT ('READ', IX, IY, ICMS(1), ICMS(5), ICMS(9), F,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         DO 20 I = IC1,IC2
            IF (OP.EQ.'ONNN') ICMS(I) =  ZOR (ICMS(I), MASK)
            IF (OP.EQ.'OFFF') ICMS(I) = ZAND (ICMS(I), MASK)
 20         CONTINUE
C                                       reset TVLIMG
         MASK = 2 ** NGRAY  -  1
         MASK = ZEOR (-1, MASK)
         DO 25 I = 1,4
            TVLIMG(I) = ZAND (TVLIMG(I), MASK)
            TVLIMG(I) = ZOR (TVLIMG(I), ICMS(I))
            TVLIMG(I) = ZOR (TVLIMG(I), ICMS(I+4))
            TVLIMG(I) = ZOR (TVLIMG(I), ICMS(I+8))
 25         CONTINUE
         CALL YSPLIT ('WRIT', IX, IY, ICMS(1), ICMS(5), ICMS(9), F,
     *      IERR)
C                                        graphics planes
      ELSE
         IC1 = CHAN - NGRAY
         CALL YGRAPH (OP, IC1, BUFFER, IERR)
         MASK = 2 ** (CHAN-1)
         IF (OP.EQ.'OFFF') MASK = ZEOR (-1, MASK)
         DO 50 I = 1,4
            IF (OP.EQ.'ONNN') TVLIMG(I) = ZOR (TVLIMG(I), MASK)
            IF (OP.EQ.'OFFF') TVLIMG(I) = ZAND (TVLIMG(I), MASK)
 50         CONTINUE
         END IF
C
 999  RETURN
      END
