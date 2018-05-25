      SUBROUTINE GRPOLY (MPOLY, IG, NPY, NV, XV, YV, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! uses TV graphics to let user develop polygonal blotch regions
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2009, 2013, 2015
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
C   GRPOLY uses a graphics plane to let the user develop a set of
C   closed polygons as a "blotch" region.
C   Inputs:
C      MPOLY   I          Dimension of NV, XV/YV 10 times this
C      IG      I          graphics plane to use
C   Output:
C      NPY     I          Number of polygons set
C      NV      I(MPOLY)   Number of vertices in each polygon
C      XV      I(*)       X-position of vertices in image
C      YV      I(*)       Y-position of vertices in image
C      SCRTCH  I(*)       Scratch buffer: > 1 line length (> 1280)
C      IERR    I          Error code
C   Common:
C      /MAPHDR/ CATBLK image catalog block of blotched image
C                      (actually used at lower level in YCUCOR)
C-----------------------------------------------------------------------
      INTEGER   MPOLY, IG, NPY, NV(MPOLY), XV(*), YV(*), SCRTCH(*), IERR
C
      INTEGER   ICH, ITW(3), IX(3), IY(3), IPOS, IB, IE, QUAD, IBUT,
     *   I, LB, LE, IBO, J, JERR, ITEMP, KERR, SCROLX, SCROLY
      REAL      PPOS(7), RPOS(2), DLIM
      LOGICAL   T, F, DOIT, ONGR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Check inputs
      IERR = 2
      IF ((IG.LT.1) .OR. (IG.GT.NGRAPH)) GO TO 999
C                                       Init
      CALL YHOLD ('ONNN', IERR)
      ICH = NGRAY + IG
      CALL YZERO (ICH, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZTIME (ITW)
C                                       turn on graphics to be sure
      I = 2 ** (ICH - 1)
      ONGR = MOD (TVLIMG(1)/I, 2) .EQ. 1
      CALL YSLECT ('ONNN', ICH, 0, SCRTCH, IERR)
      IPOS = 1
      IF (IERR.NE.0) GO TO 900
C                                       no scroll on graphics now
      ITEMP = 2 ** NGRAY
      SCROLX = TVSCRX(1)
      SCROLY = TVSCRY(1)
      CALL YSCROL (ITEMP, SCROLX, SCROLY, T, IERR)
      IPOS = 2
      IF (IERR.NE.0) GO TO 900
      NPY = 0
      IB = 0
      IE = 0
C                                       Start new polygon
 40   IF (NPY.GE.MPOLY) GO TO 800
      NPY = NPY + 1
      IB = IE + 1
      IE = IB
      NV(NPY) = 0
      WRITE (MSGTXT,1040) NPY
      CALL MSGWRT (1)
      MSGTXT = 'Press button A to set intermediate vertex'
      CALL MSGWRT (1)
      MSGTXT = 'Press buttons B, C, or D to set final vertex'
      CALL MSGWRT (1)
      MSGTXT = 'C => then reset a vertex,  D => then exit'
      CALL MSGWRT (1)
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
C                                       No scroll correction
C      QUAD = -1
C                                       ON cursor at desired position
      CALL YHOLD ('OFFF', IERR)
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 900
      IF (IERR.EQ.2) CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IPOS = 3
      IF (IERR.NE.0) GO TO 900
C                                       Cursor read loop
 50   CALL YCURSE ('READ', T, T, RPOS, QUAD, IBUT, IERR)
         IPOS = 4
         IF (IERR.NE.0) GO TO 900
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 50
C                                       No button -> no action
         IF (IBUT.EQ.0) GO TO 50
C                                       Time to quit marking
         IF ((IBUT.LT.4) .OR. (IB.LT.IE)) GO TO 60
            NPY = NPY - 1
            GO TO 200
C                                       check new vertex
 60      QUAD = 0
         CALL YCUCOR (RPOS, QUAD, PPOS, KERR)
         IF (KERR.NE.0) THEN
            MSGTXT = 'NOT ON IMAGE - TRY AGAIN'
            CALL MSGWRT (1)
            GO TO 50
            END IF
C                                       Mark new vertex
         NV(NPY) = NV(NPY) + 1
         XV(IE) = RPOS(1) + 0.51
         YV(IE) = RPOS(2) + 0.51
         IF (IB.EQ.IE) XV(IE+1) = XV(IE)
         IF (IB.EQ.IE) YV(IE+1) = YV(IE)
         I = MAX (IB, IE-1)
         IE = IE + 1
C                                       draw line
         CALL IMVECT ('ONNN', ICH, 2, XV(I), YV(I), SCRTCH, IERR)
         IPOS = 6
         IF (IERR.NE.0) GO TO 900
C                                       done with polygon
         IF ((IBUT.LT.2) .AND. (IE.LT.10*MPOLY)) GO TO 50
            NV(NPY) = NV(NPY) + 1
            XV(IE) = XV(IB)
            YV(IE) = YV(IB)
C                                       draw line
            CALL IMVECT ('ONNN', ICH, 2, XV(IE-1), YV(IE-1), SCRTCH,
     *         IERR)
            IPOS = 7
            IF (IERR.NE.0) GO TO 900
C                                       Respond further to buttons
         IF (IE.GE.10*MPOLY) GO TO 810
         IF (IBUT.LT.4) GO TO 40
C                                       Vertex correction area
         IF (IBUT.GE.8) GO TO 200
C                                       some already set box
C                                       wait for indication which
 100  MSGTXT = '*************  Move cursor to vertex to be reset'
      CALL MSGWRT (1)
      MSGTXT = 'Then push button A or B to do resetting -- or'
      CALL MSGWRT (1)
      MSGTXT = 'push C to go to next region  or D to exit'
      CALL MSGWRT (1)
      CALL YCURSE ('READ', T, T, RPOS, QUAD, IBUT, IERR)
      IPOS = 10
      IF (IERR.NE.0) GO TO 900
C                                       check button
      IF (IBUT.GE.8) GO TO 200
C                                       redraw polygons to be safe
      IF (IBUT.LT.4) GO TO 120
         LB = 1
         CALL YHOLD ('ONNN', IERR)
         DO 110 I = 1,NPY
            CALL IMVECT ('ONNN', ICH, NV(I), XV(LB), YV(LB), SCRTCH,
     *         IERR)
            IPOS = 10
            IF (IERR.NE.0) GO TO 900
            LB = LB + NV(I)
 110        CONTINUE
         CALL YHOLD ('OFFF', IERR)
         GO TO 40
C                                       Find nearest corner
 120  DLIM = 0.5
 125  LB = 1
      DO 135 I = 1,NPY
         LE = LB + NV(I) - 1
         DO 130 J = LB,LE
            IF ((ABS(RPOS(1)-XV(J)).LE.DLIM) .AND. (ABS(RPOS(2)
     *         -YV(J)).LE.DLIM)) GO TO 140
 130        CONTINUE
         LB = LE + 1
 135     CONTINUE
      DLIM = DLIM + 1.5
      IF (DLIM.LE.4.0) GO TO 125
      GO TO 100
C                                       Got one
 140  IF (J.EQ.LE) J = LB
      IBO = J
      RPOS(1) = XV(IBO)
      RPOS(2) = YV(IBO)
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      IX(2) = XV(IBO)
      IX(1) = XV(IBO-1)
      IX(3) = XV(IBO+1)
      IF (IBO.EQ.LB) IX(1) = XV(LE-1)
      IY(2) = YV(IBO)
      IY(1) = YV(IBO-1)
      IY(3) = YV(IBO+1)
      IF (IBO.EQ.LB) IY(1) = YV(LE-1)
C                                       Cursor read loop
 150  CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IERR)
         IPOS = 15
         IF (IERR.NE.0) GO TO 900
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 150
C                                       draw new lines
         CALL YHOLD ('ONNN', IERR)
         CALL IMVECT ('OFFF', ICH, 3, IX, IY, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         IX(2) = RPOS(1) + 0.5
         IY(2) = RPOS(2) + 0.5
         CALL IMVECT ('ONNN', ICH, 3, IX, IY, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL YHOLD ('OFFF', IERR)
C                                       mark this one
         IF (IBUT.EQ.0) GO TO 150
            XV(IBO) = IX(2)
            YV(IBO) = IY(2)
            IF (IBUT.LT.4) GO TO 100
            IF (IBUT.LT.8) GO TO 40
C                                       DONE: fill in real boxes
 200  IERR = 2
      IF (NPY.LT.1) GO TO 999
C                                       fill them in for fun
      CALL YHOLD ('ONNN', IERR)
      CALL BLTFIL (NPY, NV, XV, YV, IG, SCRTCH, IERR)
      IPOS = 20
      IF (IERR.NE.0) GO TO 900
      CALL YHOLD ('OFFF', IERR)
C                                       corners with scroll now
      TVSCRX(1) = 0
      TVSCRY(1) = 0
      LB = 1
      DO 230 I = 1,NPY
         LE = LB + NV(I) - 1
         DO 220 J = LB,LE
            RPOS(1) = XV(J)
            RPOS(2) = YV(J)
            QUAD = 0
            CALL YCUCOR (RPOS, QUAD, PPOS, KERR)
            IF (KERR.NE.0) THEN
               WRITE (MSGTXT,1200) KERR
               IF (KERR.EQ.1) WRITE (MSGTXT,1201) XV(J), YV(J)
               CALL MSGWRT (7)
               IF (KERR.NE.1) IERR = KERR
               IF (KERR.NE.1) GO TO 900
               IERR = IERR + KERR
               END IF
            XV(J) = PPOS(1) + 0.5
            YV(J) = PPOS(2) + 0.5
 220        CONTINUE
         LB = LE + 1
 230     CONTINUE
      IF (IERR.GT.0) IERR = 10 + IERR
      TVSCRX(1) = SCROLX
      TVSCRY(1) = SCROLY
      GO TO 900
C                                       Overflow problems
 800  WRITE (MSGTXT,1800) MPOLY
      CALL MSGWRT (7)
      GO TO 900
 810  WRITE (MSGTXT,1810) 10*MPOLY
      CALL MSGWRT (7)
      GO TO 200
C                                       Off cursor, graphics, scroll
C                                       leave graphics on
 900  CALL YCURSE ('OFFF', F, T, RPOS, QUAD, IBUT, JERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1900) IERR, IPOS
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('>>>>>>>>>>>>> Begin setting region number',I4)
 1200 FORMAT ('ERROR',I5,' CONVERTING TV TO PIXEL COORDINATES')
 1201 FORMAT (2I5,' NOT IN ANY VISIBLE IMAGE')
 1800 FORMAT ('REACHED LIMIT OF',I4,' POLYGONS')
 1810 FORMAT ('REACHED LIMIT OF',I5,' VERTICES')
 1900 FORMAT ('GRPOLY: ERROR CODE',I7,' AT',I5)
      END
