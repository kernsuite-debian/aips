      SUBROUTINE YCUCOR (RPOS, QUAD, CORN, IERR)
C-----------------------------------------------------------------------
C! correct cursor position for scroll; return image coordinates, header
C# TV-appl
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
C   YCUCOR takes a cursor position (corrected for zoom, but not scroll)
C   corrects it for scroll, determines the quadrant of the TV, and gets
C   the corresponding image header in common /MAPHDR/ and returns the
C   image coordinates.
C   NOTE WELL: RPOS ON INPUT MUST BE CORRECTED FOR ZOOM AND NOT SCROLL.
C   To get this from a raw TV position call YCURSE with OPCODE 'FXIT'
C   (or, of course, 'READ') and quadrant set to -1.    *************
C   Inputs:
C      RPOS    R(2)    X,Y screen pos before  zoom & scroll
C   Output:
C      QUAD    I       TV quadrant to use for scrolls
C                      Out: if in=-1, no scroll, else find quadrant
C                           (needs real TV pos)
C      CORN    R(7)    Image coordinates (pixels)
C      IERR    I       error code of Z...XF : 0 - ok
C                                             2 - input error
C   Version for IIS Model 70
C-----------------------------------------------------------------------
      REAL       RPOS(2), CORN(7)
      INTEGER    IERR, QUAD
C
      REAL       SRX, SRY, SRS(2)
      INTEGER    MAG, INC, IZ, ISX, ISY, IX, IY, IP, ZAND, MODMAG, J,
     *   NSCR, I, IN, ISCRX, ISCRY
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (SRS(1), SRX),    (SRS(2), SRY)
C-----------------------------------------------------------------------
C                                       uncorrect zoom
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
      SRX = MOD (RPOS(1) - 1.0 + 10.0*MAXXTV(1), REAL(MAXXTV(1))) + 1.0
      SRY = MOD (RPOS(2) - 1.0 + 10.0*MAXXTV(2), REAL(MAXXTV(2))) + 1.0
      IF ((SCXINC.NE.1) .OR. (SCYINC.NE.1)) THEN
         INC = MAX (1, SCXINC/MAG)
         IF (MAG.EQ.1) INC = 1
         IZ = ((TVZOOM(2)-1)/INC) * INC  +  1
         MODMAG = INC * MAG
         ISX = MAG * (SRX - IZ)  +  IZ - MOD(IZ-1, MODMAG)
         INC = MAX (1, SCYINC/MAG)
         IF (MAG.EQ.1) INC = 1
         IZ = MAXXTV(2) - ((MAXXTV(2)-TVZOOM(3))/INC)*INC
         MODMAG = INC * MAG
         IF (MAG.NE.1) SRY = SRY + 1.0/MAG
         IF ((MAXXTV(2).EQ.480) .AND. (MAG.GE.4)) SRY = SRY + 1.0/MAG
         ISY = MAG * (SRY - IZ) + IZ + MOD(MAXXTV(2)-IZ, MODMAG)
      ELSE
         ISX = MAG * (SRX - TVZOOM(2)) + TVZOOM(2) + 0.5
         ISY = MAG * (SRY - TVZOOM(3)) + TVZOOM(3) + 0.5
         END IF
      IX = ISX
      IY = ISY
C                                         What quadr. scroll
      ISCRX = 0
      ISCRY = 0
      QUAD = 4
      IF ((IX.GE.TVSPLT(1)) .AND. (IY.GE.TVSPLT(2))) QUAD = 1
      IF ((IX.LT.TVSPLT(1)) .AND. (IY.GE.TVSPLT(2))) QUAD = 2
      IF ((IX.LT.TVSPLT(1)) .AND. (IY.LT.TVSPLT(2))) QUAD = 3
      NSCR = NGRAY + NGRAPH
      DO 30 I = 1,NSCR
         J = 2 ** (I-1)
         IF (ZAND(J,TVLIMG(QUAD)).NE.0) GO TO 40
 30      CONTINUE
      GO TO 50
 40   IF (I.GT.NGRAY) I = NGRAY + 1
      ISCRX = TVSCRX(I)
      ISCRY = TVSCRY(I)
C                                        correct for zoom
 50   IF ((SCXINC.NE.1) .OR. (SCYINC.NE.1)) THEN
         INC = MAX (1, SCXINC/MAG)
         IF (MAG.EQ.1) INC = 1
         IZ = ((TVZOOM(2)-1)/INC) * INC  +  1
         MODMAG = INC * MAG
         IN = IZ + (IX - IZ + MOD(IZ-1,MODMAG) + MAXXTV(1)) / MAG
     *      - MAXXTV(1) / MAG
         SRX = IN + REAL(MOD(IX-1,MAG))/REAL(MAG)
         INC = MAX (1, SCYINC/MAG)
         IF (MAG.EQ.1) INC = 1
         IZ = MAXXTV(2) - ((MAXXTV(2)-TVZOOM(3))/INC)*INC
         MODMAG = INC * MAG
         IN = IZ + (IY - IZ - MOD(MAXXTV(2)-IZ,MODMAG) - MAXXTV(2))/MAG
     *      + MAXXTV(2)/MAG
         SRY = IN - REAL(MOD(MAXXTV(2)-IY,MAG))/REAL(MAG)
         IF (MAG.NE.1) SRY = SRY - 1.0/MAG
         IF ((MAXXTV(2).EQ.480) .AND. (MAG.GE.4)) SRY = SRY - 1.0/MAG
      ELSE
         SRX = IX - TVZOOM(2)
         SRX = TVZOOM(2) + SRX / REAL (MAG)
         SRY = IY - TVZOOM(3)
         SRY = TVZOOM(3) + SRY / REAL (MAG)
         END IF
C                                       correct for scroll
 70   SRX = MOD (SRX - 1.0 - ISCRX + 10.0*MAXXTV(1), REAL(MAXXTV(1)))+1.
      SRY = MOD (SRY - 1.0 - ISCRY + 10.0*MAXXTV(2), REAL(MAXXTV(2)))+1.
C
      IX = SRX + 0.5
      IY = SRY + 0.5
      DO 80 IP = 1,NSCR
         J = 2 ** (IP-1)
         IF (ZAND(TVLIMG(QUAD), J).EQ.0) GO TO 80
            CALL YCREAD (IP, IX, IY, CATBLK, IERR)
            IF (IERR.EQ.0) GO TO 90
            IF (IERR.GT.1) GO TO 999
 80      CONTINUE
      IERR = 1
      CALL RFILL (7, 0.0, CORN)
      GO TO 999
C                                       found it ok
 90   CALL IMA2MP (SRS, CORN)
      DO 95 J = 3,7
         CORN(J) = CATBLK(IIDEP+J-3)
 95      CONTINUE
C
 999  RETURN
      END
