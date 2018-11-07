      SUBROUTINE TKCATL (OPER, IX, IY, CATBLK, IERR)
C-----------------------------------------------------------------------
C! performs operations on the Graphics image catalog
C# Catalog Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   Read, write, init the Graphics image catalog.  If the TK server is
C   in use (TKTYPE > 0), the TK device must be open.
C   Inputs:
C      OPER    C*4     'INIT' - zero catalog block for current TEK #
C                      'READ' - read catalog block for current TEK #
C                      'WRIT' - write catalog block for current TEK #
C      IX      I       X pixel position (check vs CATBLK on READ)
C      IY      I       Y pixel position (check vs CATBLK on READ)
C   In/out:
C      CATBLK  I(256)  Image header converted for image catalog use
C   Output:
C      IERR    I       Error return:  0 => o.k.
C                         10 => access not allowed for this POPS #
C                         11 => IX, IY not in image
C                         > 0 => error return from ZOPEN, ZFIO
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   IX, IY, CATBLK(256), IERR
C
      CHARACTER INAME*48
      INTEGER   LUN, IND, JERR, WORK(256), I
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTKS.INC'
      DATA T, F /.TRUE., .FALSE./
      DATA LUN /20/
C-----------------------------------------------------------------------
C                                       Is access allowed?
      IERR = 10
      IF (NTKDEV.LE.0) GO TO 999
C                                       real device: disk catalog
      IF (TKTYPE.EQ.0) THEN
C                                       Open image catalog
         CALL ZPHFIL ('IC', 1, 0, 0, INAME, IERR)
         CALL ZOPEN (LUN, IND, 1, INAME, F, T, T, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       INIT, WRIT
         IF ((OPER.EQ.'INIT') .OR. (OPER.EQ.'WRIT')) THEN
            IF (OPER.EQ.'INIT') CALL FILL (256, 0, CATBLK)
            CALL ZFIO ('WRIT', LUN, IND, NTKDEV, CATBLK, IERR)
         ELSE IF (OPER.EQ.'READ') THEN
            CALL ZFIO ('READ', LUN, IND, NTKDEV, CATBLK, IERR)
            IF ((IERR.EQ.0) .AND. ((IX.LT.CATBLK(IICOR)) .OR.
     *         (IX.GT.CATBLK(IICOR+2)) .OR. (IY.LT.CATBLK(IICOR+1)) .OR.
     *         (IY.GT.CATBLK(IICOR+3)))) IERR = 11
         ELSE
            IERR = 2
            END IF
C                                       Always close image catalog
         CALL ZCLOSE (LUN, IND, JERR)
C                                       TK server holds catalog
      ELSE
         CALL TEKFLS (IERR)
C                                       INIT, WRIT catalog
         IF ((OPER.EQ.'INIT') .OR. (OPER.EQ.'WRIT')) THEN
C                                       get its attention
            I = 1
            CALL ZPUTCH (I, WORK(1), 1)
            CALL ZPUTCH (I, WORK(1), 2)
            CALL ZFIO ('WRIT', TKLUN, TKFIND, 2, WORK, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL ZFIO ('READ', TKLUN, TKFIND, 2, WORK, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       send the header
            IF (OPER.EQ.'INIT') THEN
               CALL FILL (256, 0, WORK)
            ELSE
               CALL CATN2L ('L2NI', CATBLK, WORK)
               END IF
            CALL ZFIO ('WRIT', TKLUN, TKFIND, 1024, WORK, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       make sure it got it
            CALL ZFIO ('READ', TKLUN, TKFIND, 1024, WORK, IERR)
C                                       read catalog
         ELSE IF (OPER.EQ.'READ') THEN
C                                       get its attention
            I = 2
            CALL ZPUTCH (I, WORK(1), 1)
            CALL ZPUTCH (I, WORK(1), 2)
            CALL ZFIO ('WRIT', TKLUN, TKFIND, 2, WORK, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       read the header
            CALL ZFIO ('READ', TKLUN, TKFIND, 1024, WORK, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL CATN2L ('N2LI', WORK, CATBLK)
C                                       check the coordinate
            IF ((IX.LT.CATBLK(IICOR)) .OR. (IX.GT.CATBLK(IICOR+2)) .OR.
     *         (IY.LT.CATBLK(IICOR+1)) .OR. (IY.GT.CATBLK(IICOR+3)))
     *         IERR = 11
C                                       bad opcode
         ELSE
            IERR = 2
            END IF
         END IF
C
 999  RETURN
      END

