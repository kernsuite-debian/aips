      SUBROUTINE BLTFIL (NP, NV, XV, YV, GRCHAN, BUF, IERR)
C-----------------------------------------------------------------------
C! fills in closed polygons on a tv "blotch" plane
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   BLTFIL fills in a series of closed polygons on a TV "blotch" plane
C   Inputs:
C      NP      I        # of polygons
C      NV      I(NP)    # of vertices in each polygon
C      XV      I(*)     vertex X-positions list
C      YV      I(*)     vertex Y-positions list
C      GRCHAN  I        graphics channel number
C   Output:
C      BUF     I(*)     scratch buffer (> 1280)
C      IERR    I        ZM70XF error code: 0 ok
C-----------------------------------------------------------------------
      INTEGER   NP, NV(*), XV(*), YV(*), GRCHAN, BUF(*), IERR
C
      INTEGER   ICM, LP, IP, LV, IE, IB, IY, NX, I, IXL(50), IXU(50)
      LOGICAL   WASYNC
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      WASYNC = ISYNCH.EQ.0
C                                       fill buffer
      LP = 2 ** NGRAPH - 1
      CALL FILL (MAXXTV(1), LP, BUF)
      ICM = NGRAY + GRCHAN
      LP = 1
C                                       loop over polygons
      IE = 1
      IB = MAXXTV(2)
      DO 30 IP = 1,NP
         LV = NV(IP) + LP - 1
C                                       find y range of polygon
         DO 20 I = LP,LV
            IB = MIN (IB, YV(I))
            IE = MAX (IE, YV(I))
 20         CONTINUE
         LP = LV + 1
 30      CONTINUE
      IE = MIN (IE+1, MAXXTV(2))
      IB = MAX (IB-1, 1)
C                                       loop over y range
      IF (WASYNC) CALL YHOLD ('ONNN', I)
      DO 60 IY = IB,IE
         CALL BLTLIS (1, 1, NP, NV, XV, YV, IY, NX, IXL, IXU)
         IF (NX.LE.0) GO TO 60
            DO 40 I = 1,NX
               IF ((IXL(I).LT.1) .AND. (IXU(I).LT.1)) GO TO 40
               IF ((IXL(I).GT.MAXXTV(1)) .AND. (IXU(I).GT.MAXXTV(1)))
     *            GO TO 40
                  IXL(I) = MAX (1, IXL(I))
                  IXU(I) = MIN (MAXXTV(1), IXU(I))
                  CALL YCNECT (IXL(I), IY, IXU(I), IY, ICM, BUF, IERR)
                  IF (IERR.NE.0) GO TO 900
 40            CONTINUE
 60         CONTINUE
C
 900  IF (WASYNC) CALL YHOLD ('OFFF', I)
C
 999  RETURN
      END
