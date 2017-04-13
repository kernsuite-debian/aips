      SUBROUTINE PNTPLT (ITYP, AX, AY, BLC, TRC, BLACKN, COLOR, PLBUF,
     *   IERR)
C-----------------------------------------------------------------------
C! adds to plot mark at pixel coordinate set given
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2002-2005
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
C   PNTPLT plots a symbol in a plot file as given by a set of pixel
C   coordinates defining the center and surround of the symbol.
C   Inputs:
C      ITYP     I         Symbol type
C      AX       R(5)      X pixel of center, 1st end of major axis, 2nd
C                            end o major axis, 1st end minor axis, 2nd
C                            end of minor axis
C      AY       R(5)      Y pixel of center, etc.
C                         AX,AY define the 4 end points of an X
C                         although it does not need to be rectangular
C      BLC      R(2)      Plot lower left corner (pixels)
C      TRC      R(2)      Plot upper right corner (pixels)
C      BLACKN   L         Write dark vectors rather than bright
C      COLOR    L         Write as 3-color vector
C   In/Out:
C      PLBUF    I(256)    Plot IO buffer
C   Output:
C      IERR     I         Error code: 0 => okay
C                                +3 => IO error in plotting
C   PNTPLT plots several kinds of marks: The types are
C   ITYP.......1: Plus sign           12: Five pointed star
C              2: Cross (X)           13: Star of David
C              3: Circle              14: Seven-pointed star
C              4: Box                 15: Eight-pointed star
C              5: Triangle            16: Nine-pointed star
C              6: Diamond             17: Ten-pointed star
C              7: Pentagon            18: 11-pointed star
C              8: Hexagon             19: 12-pointed star
C              9: Septagon            20: 13-pointed star
C              10: Octagon            21: 14-pointed star
C              11: Nine-gon           22: Cross (X) with gap
C                                     23: Vertical line
C-----------------------------------------------------------------------
      REAL      AX(5), AY(5), BLC(2), TRC(2)
      INTEGER   ITYP, PLBUF(256), IERR
      LOGICAL   BLACKN, COLOR
C                                       Max Numb Columns, Label length
      INTEGER   MXLINE
      PARAMETER (MXLINE=72)
      INTEGER   I, IANG, NUMPNT
      REAL      STTYPE, BX(2), BY(2), PX(MXLINE), PY(MXLINE), SINB,
     *   COSB, TX, TY, DANGL
      DOUBLE PRECISION ANGL
      LOGICAL   XING
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IERR = 0
      STTYPE = ITYP
C                                       If type > 24 draw plus
      IF (STTYPE.GT.24) STTYPE = 1
      IF (STTYPE.LT.1) STTYPE = 1
C                                       Assume No marking needed
      NUMPNT = 0
C                                       If star type is a cross
      IF (STTYPE.EQ.2) THEN
         PX(1) = AX(1) - (AX(5) - AX(4) + AX(3) - AX(2)) / 2
         PX(2) = AX(1) + (AX(5) - AX(4) + AX(3) - AX(2)) / 2
         PX(3) = PX(1)
         PX(4) = PX(2)
         PY(1) = AY(1) - (AY(5) - AY(4) + AY(3) - AY(2)) / 2
         PY(2) = AY(1) + (AY(5) - AY(4) + AY(3) - AY(2)) / 2
         PY(3) = PY(2)
         PY(4) = PY(1)
         NUMPNT = 2
         XING = .TRUE.
C                                       If star type is a cross
      ELSE IF (STTYPE.EQ.24) THEN
         PX(1) = AX(1) - (AX(5) - AX(4) + AX(3) - AX(2)) / 2.
         PX(2) = AX(1) - (AX(5) - AX(4) + AX(3) - AX(2)) / 6.
         PX(3) = AX(1) + (AX(5) - AX(4) + AX(3) - AX(2)) / 6.
         PX(4) = AX(1) + (AX(5) - AX(4) + AX(3) - AX(2)) / 2.
         PY(1) = AY(1) - (AY(5) - AY(4) + AY(3) - AY(2)) / 2
         PY(2) = AY(1) - (AY(5) - AY(4) + AY(3) - AY(2)) / 6.
         PY(3) = AY(1) + (AY(5) - AY(4) + AY(3) - AY(2)) / 6.
         PY(4) = AY(1) + (AY(5) - AY(4) + AY(3) - AY(2)) / 2.
         NUMPNT = 2
         XING = .TRUE.
C                                       If star type is a plus
C                                       along major and minor axes
      ELSE IF (STTYPE.EQ.1) THEN
         PX(1) = AX(2)
         PX(2) = AX(3)
         PX(3) = AX(4)
         PX(4) = AX(5)
         PY(1) = AY(2)
         PY(2) = AY(3)
         PY(3) = AY(4)
         PY(4) = AY(5)
         NUMPNT = 2
         XING = .TRUE.
C                                       If star type is a vertical line
C                                       along major axis
      ELSE IF (STTYPE.EQ.23) THEN
         PX(1) = AX(2)
         PX(2) = AX(3)
         PX(3) = AX(2)
         PX(4) = AX(3)
         PY(1) = AY(2)
         PY(2) = AY(3)
         PY(3) = AY(2)
         PY(4) = AY(3)
         NUMPNT = 1
C                                       Star type is a box
      ELSE IF (STTYPE.EQ.4) THEN
         PX(1) =  AX(1) + (AX(2) - AX(3) + AX(4) - AX(5))/2
         PY(1) =  AY(1) + (AY(2) - AY(3) + AY(4) - AY(5))/2
         PX(2) =  AX(1) + (AX(2) - AX(3) - AX(4) + AX(5))/2
         PY(2) =  AY(1) + (AY(2) - AY(3) - AY(4) + AY(5))/2
         PX(3) =  AX(1) + (-AX(2) + AX(3) - AX(4) + AX(5))/2
         PY(3) =  AY(1) + (-AY(2) + AY(3) - AY(4) + AY(5))/2
         PX(4) =  AX(1) + (-AX(2) + AX(3) + AX(4) - AX(5))/2
         PY(4) =  AY(1) + (-AY(2) + AY(3) + AY(4) - AY(5))/2
         NUMPNT = 4
C                                       Star is plus with a gap
      ELSE IF (STTYPE.EQ.22) THEN
         PX(1)  = AX(2)
         PY(1)  = AY(2)
         PX(2)  = ((2.*AX(2)) + AX(3))/3.
         PY(2)  = ((2.*AY(2)) + AY(3))/3.
         PX(3)  = AX(3)
         PY(3)  = AY(3)
         PX(4)  = ((2.*AX(3)) + AX(2))/3.
         PY(4)  = ((2.*AY(3)) + AY(2))/3.
         NUMPNT = 2
         XING = .TRUE.
C                                       Star is a closed fig
      ELSE
         NUMPNT = STTYPE - 2
         IF (STTYPE.EQ.3) NUMPNT = MXLINE/2
C                                       If making a crossing figure
         XING = STTYPE.GT.11
         IF (XING) THEN
C                                       stars have sttype - 5 points
            NUMPNT = NUMPNT - 5
C                                       choose crossing counts
            DANGL = INT ((NUMPNT-2)/3.) + 1
            IF (NUMPNT.GT.8) DANGL = DANGL + 1
         ELSE
            DANGL = 1.
            END IF
C                                       Make and N sided figure
         NUMPNT = MIN (NUMPNT, MXLINE/2)
C                                       Make a mark out of ?? points
         DO 50 IANG = 1,NUMPNT
            ANGL = DANGL * (IANG-1.) * TWOPI / REAL(NUMPNT)
C                                       Compute rotation
            SINB = SIN(ANGL)
            COSB = COS(ANGL)
C                                       add offset to center pos.
            PX(IANG) = AX(1) + ((AX(2)-AX(3)) * COSB +
     *        (AX(4)-AX(5)) * SINB) / 2.
            PY(IANG) = AY(1) + ((AY(2)-AY(3)) * COSB +
     *         (AY(4)-AY(5)) * SINB) / 2.
C                                       Make complimentary figure
            ANGL = ANGL + (TWOPI / REAL(NUMPNT))
            SINB = SIN(ANGL)
            COSB = COS(ANGL)
            PX(IANG+NUMPNT) = AX(1) + ((AX(2)-AX(3)) * COSB +
     *         (AX(4)-AX(5)) * SINB) / 2.
            PY(IANG+NUMPNT) = AY(1) + ((AY(2)-AY(3)) * COSB +
     *         (AY(4)-AY(5)) * SINB) / 2.
 50         CONTINUE
C                                       Turn off crossing for odd figs
         IF (XING .AND. (((NUMPNT)/2)*2.NE.NUMPNT)) XING = .FALSE.
         END IF
C                                       If a closed star mark
      IF ((STTYPE.GE.1) .AND. (NUMPNT.GT.1)) THEN
C                                       Must move first before draw
         TX  = 0
         TY  = 0
C                                       For all line end points
         DO 70 I = 1,NUMPNT
C                                       get start point
            BX(1) = PX(I)
            BY(1) = PY(I)
C                                       If not last line
            IF (I.LT.NUMPNT) THEN
               BX(2) = PX(I+1)
               BY(2) = PY(I+1)
C                                       Else close the figure
            ELSE
               BX(2) = PX(1)
               BY(2) = PY(1)
               END IF
C                                       draw one segment
            CALL PLOTWO (BLC, TRC, BX, BY, TX, TY, BLACKN, COLOR, PLBUF,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
  70        CONTINUE
         END IF
C                                       If a crossing closed star mark
      IF (((STTYPE.LE.2) .OR. (STTYPE.GT.11)) .AND. (NUMPNT.GT.1)
     *   .AND. (XING)) THEN
C                                       For all line end points
         DO 80 I = 1,NUMPNT
C                                       get start point
            BX(1) = PX(I+NUMPNT)
            BY(1) = PY(I+NUMPNT)
C                                       If not last line
            IF (I.LT.NUMPNT) THEN
               BX(2) = PX(I+NUMPNT+1)
               BY(2) = PY(I+NUMPNT+1)
C                                       Else close the figure
            ELSE
               BX(2) = PX(NUMPNT+1)
               BY(2) = PY(NUMPNT+1)
               END IF
C                                       draw one segment
            CALL PLOTWO (BLC, TRC, BX, BY, TX, TY, BLACKN, COLOR, PLBUF,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
  80        CONTINUE
         END IF
C                                       If plus with gap mark
      IF (STTYPE.EQ.22) THEN
C                                       get start point
         BX(1) = AX(4)
         BY(1) = AY(4)
         BX(2) = ((2.*AX(4)) + AX(5))/3.
         BY(2) = ((2.*AY(4)) + AY(5))/3.
C                                       draw one segment
         CALL PLOTWO (BLC, TRC, BX, BY, TX, TY, BLACKN, COLOR, PLBUF,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         BX(1) = AX(5)
         BY(1) = AY(5)
         BX(2) = ((2.*AX(5)) + AX(4))/3.
         BY(2) = ((2.*AY(5)) + AY(4))/3.
C                                       draw opposite segment
         CALL PLOTWO (BLC, TRC, BX, BY, TX, TY, BLACKN, COLOR, PLBUF,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
C                                       If cross with gap mark
      ELSE IF (STTYPE.EQ.24) THEN
C                                       get start point
         BX(1) = PX(1)
         BY(1) = PY(4)
         BX(2) = PX(2)
         BY(2) = PY(3)
C                                       draw one segment
         CALL PLOTWO (BLC, TRC, BX, BY, TX, TY, BLACKN, COLOR, PLBUF,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         BX(1) = PX(3)
         BY(1) = PY(2)
         BX(2) = PX(4)
         BY(2) = PY(1)
C                                       draw opposite segment
         CALL PLOTWO (BLC, TRC, BX, BY, TX, TY, BLACKN, COLOR, PLBUF,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
C                                       vertical line
      ELSE IF (STTYPE.EQ.23) THEN
         CALL PLOTWO (BLC, TRC, PX, PY, TX, TY, BLACKN, COLOR, PLBUF,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      IERR = 0
C
 999  RETURN
      END
