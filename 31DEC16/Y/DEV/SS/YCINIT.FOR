      SUBROUTINE YCINIT (IPLANE, BUFF)
C-----------------------------------------------------------------------
C! initialize image catalog for specified TV memory plane
C# Y0 TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1996
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
C   Initialize image catalog for plane IPLANE - TK now done with TKCATL
C   Input:
C      IPLANE   I        Image plane to initialize: 0 all
C   Output:
C      BUFF     I(256)   Working buffer
C   XAS screen server version
C-----------------------------------------------------------------------
      INTEGER   IPLANE, BUFF(256)
C
      INTEGER   I1, I2, IP, ILUN, IIND, IER, NREC, J1, J2, LL, IREC,
     *   OPCODE, NWSD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA ILUN /20/
C-----------------------------------------------------------------------
C                                       using disk image catalog
      IF (TVIMGC.LE.0) THEN
         CALL FILL (256, 0, BUFF)
         CALL ICOPEN (IPLANE, ILUN, IIND, IER)
         IF (IER.NE.0) GO TO 970
         LL = (NIMAGE - 1) / 51
C                                        multi-image gray planes
         IF (IPLANE.LE.NGRAY) THEN
            IF (IPLANE.LE.0) THEN
               I1 = 1
               I2 = NGRAY
            ELSE
               I1 = IPLANE
               I2 = IPLANE
               END IF
            DO 20 IP = I1,I2
               J1 = 1 + (IP - 1) * (NIMAGE + 1 + LL)
               J2 = J1 + LL
               DO 10 NREC = J1,J2
                  CALL ZFIO ('WRIT', ILUN, IIND, NREC, BUFF, IER)
                  IF (IER.NE.0) GO TO 970
 10               CONTINUE
 20            CONTINUE
            END IF
C                                        single image graphics pl
         IF ((IPLANE.GT.NGRAY) .OR. (IPLANE.LE.0)) THEN
            IF (IPLANE.LE.NGRAY) THEN
               I1 = NGRAY + 1
               I2 = NGRAY + NGRAPH
            ELSE
               I1 = IPLANE
               I2 = IPLANE
               END IF
            DO 120 IP = I1,I2
               IREC = IP + NGRAY * (NIMAGE + LL)
               CALL ZFIO ('WRIT', ILUN, IIND, IREC, BUFF, IER)
               IF (IER.NE.0) GO TO 970
 120           CONTINUE
            END IF
C                                       XAS does the image catalog
      ELSE
         BUFF(1) = IPLANE
         CALL FILL (3, 0, BUFF(2))
         OPCODE = 31
         CALL ZSSSXF (OPCODE, BUFF, 0, BUFF(5), NWSD, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1200) IER
            CALL MSGWRT (6)
            END IF
         END IF
      GO TO 980
C                                        error
 970  WRITE (MSGTXT,1970) IER
      CALL MSGWRT (6)
C                                        close up
 980  IF (TVIMGC.LE.0) CALL ZCLOSE (ILUN, IIND, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('YCINIT: XAS IMAGE CATALOG RETURNS ERROR',I5)
 1970 FORMAT ('YCINIT: IMAGE CATALOG ACCESS ERROR = ',I6)
      END
