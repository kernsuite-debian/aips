      SUBROUTINE BLTLIS (LPOLY, LLV, NPOLY, NVERT, XV, YV, IY, LNX,
     *   IXL, IXU)
C-----------------------------------------------------------------------
C! lists any segments of current row which fall inside blotch regions
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2013
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
C   BLTLIS makes a list of line segments included inside a blotch area
C   Inputs:
C      LPOLY   I        beginning polygon number
C      LLV     I        beginning vertex number
C      NPOLY   I        Number of polygons
C      NVERT   I(*)     Number of vertices in each polygon
C      XV      I(*)     X position of vertices
C      YV      I(*)     Y position of vertices
C      IY      I        Row number
C   Output:
C      LNX     I        Number of segments
C      IXL     I(*)     low x index of segments
C      IXU     I(*)     high x index of segments
C-----------------------------------------------------------------------
      INTEGER   LPOLY, LLV, NPOLY, NVERT(*), XV(*), YV(*), IY, LNX,
     *   IXL(*), IXU(*)
C
      INTEGER   LP, IP, LV, NXE, Y1, Y2, IROUND, I, J, IX
      LOGICAL   SORTED
      REAL      RX, RY, ANG, ANGSUM, XSE(200)
C-----------------------------------------------------------------------
      LNX = 0
      LP = LLV
      RY = IY
      DO 80 IP = LPOLY,NPOLY
         LV = NVERT(IP) + LP - 2
C                                       find intersections
         NXE = 0
         DO 10 I = LP,LV
            Y1 = YV(I)
            Y2 = YV(I+1)
            IF ((IY.GE.MIN(Y1,Y2)) .AND. (IY.LE.MAX(Y1,Y2))) THEN
               NXE = NXE + 1
               IF (Y2.NE.Y1) THEN
                  XSE(NXE) = REAL(XV(I+1)-XV(I)) * REAL(IY-Y1)
     *               / REAL (Y2-Y1) + XV(I)
               ELSE
                  XSE(NXE) = XV(I)
                  END IF
               END IF
 10         CONTINUE
C                                       found some
         IF (NXE.GT.1) THEN
C                                       sort list
 20         SORTED = .TRUE.
            DO 30 I = 2,NXE
               IF (XSE(I-1).GT.XSE(I)) THEN
                  RX = XSE(I-1)
                  XSE(I-1) = XSE(I)
                  XSE(I) = RX
                  SORTED = .FALSE.
                  END IF
 30            CONTINUE
            IF (.NOT.SORTED) GO TO 20
C                                       which are interior
            DO 60 I = 2,NXE
               RX = (XSE(I-1) + XSE(I)) / 2.0
               IX = IROUND (RX)
C                                       test for on an edge
               DO 40 J = LP,LV
                  IF (IY.EQ.YV(J)) THEN
                     IF (IX.EQ.XV(J)) GO TO 55
                     IF ((IX.GE.XV(J)) .AND. (IX.LE.XV(J+1)) .AND.
     *                  (IY.EQ.YV(J+1))) GO TO 55
                     IF ((IX.LE.XV(J)) .AND. (IX.GE.XV(J+1)) .AND.
     *                  (IY.EQ.YV(J+1))) GO TO 55
                     END IF
 35               IF (IX.EQ.XV(J)) THEN
                     IF ((IY.GE.YV(J)) .AND. (IY.LE.YV(J+1)) .AND.
     *                  (IX.EQ.XV(J+1))) GO TO 55
                     IF ((IY.LE.YV(J)) .AND. (IY.GE.YV(J+1)) .AND.
     *                  (IX.EQ.XV(J+1))) GO TO 55
                     END IF
 40               CONTINUE
               ANGSUM = 0.0
               DO 50 J = LP,LV
                  CALL BLTGLE (RX, RY, XV(J), YV(J), XV(J+1),
     *               YV(J+1), ANG)
                  ANGSUM = ANGSUM + ANG
 50               CONTINUE
               IF (ABS(ANGSUM).LT.180.) GO TO 60
 55               LNX = LNX + 1
                  IXL(LNX) = IROUND (XSE(I-1))
                  IXU(LNX) = IROUND (XSE(I))
 60            CONTINUE
            END IF
         LP = LV + 2
 80      CONTINUE
      IF (LNX.LE.0) GO TO 999
C                                       compress list
      I = 1
 85   IF (I.LT.LNX) THEN
         I = I + 1
 90      IF (IXU(I-1).NE.IXL(I)) THEN
            GO TO 85
         ELSE
            J = LNX - I
            IXU(I-1) = IXU(I)
            LNX = LNX - 1
            IF (J.GT.0) THEN
               CALL COPY (J, IXL(I+1), IXL(I))
               CALL COPY (J, IXU(I+1), IXU(I))
               GO TO 90
               END IF
            END IF
         END IF
C
 999  RETURN
      END
