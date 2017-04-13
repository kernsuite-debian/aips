      SUBROUTINE YCINIT (IPLANE, BUFF)
C-----------------------------------------------------------------------
C! initialize image catalog for specified TV memory plane
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
C   Initialize image catalog for plane IPLANE - TK now done with TKCATL
C   Input:
C      IPLANE   I        Image plane to initialize
C   Output:
C      BUFF     I(256)   Working buffer
C   GENERIC Y ROUTINE: for local host operated TV device
C-----------------------------------------------------------------------
      INTEGER   IPLANE, BUFF(256)
C
      INTEGER   I1, I2, IP, ILUN, IIND, IER, NREC, J1, J2, LL, IREC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA ILUN /20/
C-----------------------------------------------------------------------
      CALL FILL (256, 0, BUFF)
      CALL ICOPEN (IPLANE, ILUN, IIND, IER)
      IF (IER.NE.0) GO TO 970
      LL = (NIMAGE - 1) / 51
C                                        multi-image gray planes
      IF (IPLANE.LE.NGRAY) THEN
         I1 = IPLANE
         I2 = IPLANE
         IF (I1.GT.0) GO TO 10
            I1 = 1
            I2 = NGRAY
 10      DO 20 IP = I1,I2
            J1 = 1 + (IP - 1) * (NIMAGE + 1 + LL)
            J2 = J1 + LL
            DO 20 NREC = J1,J2
               CALL ZFIO ('WRIT', ILUN, IIND, NREC, BUFF, IER)
               IF (IER.NE.0) GO TO 970
 20         CONTINUE
C                                        single image graphics pl
      ELSE
         I1 = IPLANE
         I2 = IPLANE
         IF (I1.LE.NGRAY+NGRAPH) GO TO 110
            I1 = NGRAY + 1
            I2 = NGRAY + NGRAPH
 110     DO 120 IP = I1,I2
            IREC = IP + NGRAY * (NIMAGE + LL)
            CALL ZFIO ('WRIT', ILUN, IIND, IREC, BUFF, IER)
            IF (IER.NE.0) GO TO 970
 120        CONTINUE
         END IF
      GO TO 980
C                                        error
 970  WRITE (MSGTXT,1970) IER
      CALL MSGWRT (6)
C                                        close up
 980  CALL ZCLOSE (ILUN, IIND, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1970 FORMAT ('YCINIT: IMAGE CATALOG ACCESS ERROR = ',I6)
      END
