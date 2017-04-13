      SUBROUTINE CONDRW (IMLUN, IMFIND, IBLUN, IBFIND, XMULT, BLC, TRC,
     *   LEVS, DOCOLR, BBLC, BTRC, RGBLEV, IGBLK, IERR)
C-----------------------------------------------------------------------
C! writes contour plot to a plot file
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2002-2004, 2006, 2015
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
C   CONDRW will write commands to a plot file for the execution of
C   a contour plot.
C   Inputs:
C      IMLUN    I         logical unit number for the contour map file
C      IMFIND   I         FTAB index for open contour map file.
C      IBLUN    I         logical unit number of background gray image
C      IBFIND   I         FTAB index of open background gray image
C      IGBLK    I(256)    I/O block for graph file.
C      XMULT    R         Contour interval (image units)
C      BLC      R(7)      Bottom left corner
C      TRC      R(7)      Top right corner
C      LEVS     R(30)     Selected contour intervals in increasing order
C                         (any decrease terminates the list)
C      DOCOLR   I         0 no color, 1, use existing color, 2 use
C                         RGBLEV
C      BBLC     I(2)      BLC of blanked area
C      BTRC     I(2)      TRC of blanked area
C      RGBLEV   R(3,30)   RGB colors when level i not blacked
C   Common:
C      CATBLK   I(256)  map header.
C      CNTRBU   R(8192) buffers
C   Output:
C      IERR     I   error code. 0 = ok.
C-----------------------------------------------------------------------
      INTEGER   IMLUN, IMFIND, IBLUN, IBFIND, IGBLK(256), DOCOLR,
     *   BBLC(2), BTRC(2), IERR
      REAL      XMULT, BLC(7), TRC(7), LEVS(*), RGBLEV(3,*)
C
      REAL      VAL(3), XPOS(3), YPOS(3), TEMP, VC, VL, VM, VS, XA, XB,
     *   XL, XLAST, XM, XS, YA, YB, YL, YLAST, YM, YS, DELTAX, DELTAY,
     *   TLEV, XLEV, ALEVS(30)
      INTEGER   IPERM(3,6), IBLCX, IBLCY, IBLCY1, ICOL, II, INDEX,
     *   INLEVS, INPIXS, IPLUS, IPOS, IROW, ISKIP, ITRCX, ITRCXM,
     *   ITRCY, ITRI, I, MININT, LOCINT, IROUND, BPOS, ISLEV, JJ, BCUT,
     *   BVAL, BXOFF, BXSIZ, LASTC
      LOGICAL   DOBACK
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DCNT.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA IPERM /1,3,2, 3,2,1, 3,1,2, 2,1,3, 1,2,3, 2,3,1/
C-----------------------------------------------------------------------
      DOBACK = (IBLUN.GT.0) .AND. (IBFIND.GT.0) .AND. (DOCOLR.LE.0)
      IF (DOBACK) BCUT = 3.0 * (GPHCUT * GPHTHI + (1.0-GPHCUT) * GPHTLO)
      ISKIP = 2
      XLAST = -1000.
      YLAST = -1000.
      TEMP = MAX (TRC(1)-BLC(1), TRC(2)-BLC(2))
      TEMP = 10 - 3 * LOG10 (TEMP)
      LOCINT = IROUND (TEMP)
      IF (LOCINT.LT.2) LOCINT = 2
      IBLCY = BLC(2) + .5
      ITRCY = TRC(2) + .5
      ITRCX = TRC(1) + .5
      IBLCX = BLC(1) + .5
      INPIXS = ITRCX - IBLCX + 1
      BXSIZ = BTRC(1) - BBLC(1) + 1
      BXOFF = BBLC(1) - IBLCX
      LASTC = 0
C                                       magic parms for dashed lines
      XLEV = 256.0 / INPIXS
      TLEV = 256.0 / (ITRCY - IBLCY + 1.0)
      ISLEV = SQRT (1.0 / (XLEV * TLEV)) + 0.1
      IF (ISLEV.LT.1) ISLEV = 1
      IF (XLEV.LT.1.0) XLEV = (SQRT (XLEV) + 3.0*XLEV) / 4.0
C                                       Determine number of levels.
C                                       and convert to absolute levels.
      TEMP = LEVS(1)
      ALEVS(1) = XMULT * LEVS(1)
      DO 45 INLEVS = 2,30
         IF (TEMP.GE.LEVS(INLEVS)) GO TO 50
         TEMP = LEVS(INLEVS)
         ALEVS(INLEVS) = XMULT * LEVS(INLEVS)
 45      CONTINUE
 50   INLEVS = INLEVS - 1
C                                       Read and save first row.
      CALL MDISK ('READ', IMLUN, IMFIND, BUFF, IPOS, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'Contour', IERR
         GO TO 990
         END IF
      IF ((IBLCY.GE.BBLC(2)) .AND. (IBLCY.LE.BTRC(2)))
     *   CALL RFILL (BXSIZ, FBLANK, BUFF(IPOS+BXOFF))
      CALL RCOPY (INPIXS, BUFF(IPOS), RLROW)
      IF (DOBACK) THEN
         CALL MDISK ('READ', IBLUN, IBFIND, BBUFF, BPOS, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) 'Background', IERR
            GO TO 990
            END IF
         CALL GSCALE (GPHFUN, GPHRNG, INPIXS, 1, BBUFF(BPOS),
     *      IBBUFF(BPOS))
         CALL COPY (INPIXS, IBBUFF(BPOS), IBLROW)
         END IF
C                                       Loop over all rows.
      IBLCY1 = IBLCY + 1
      DO 300 IROW = IBLCY1,ITRCY
C                                       Read proper row.
         CALL MDISK ('READ', IMLUN, IMFIND, BUFF, IPOS, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) 'Contour', IERR
            GO TO 990
            END IF
         IF ((IROW.GE.BBLC(2)) .AND. (IROW.LE.BTRC(2))) THEN
            CALL RFILL (BXSIZ, FBLANK, BUFF(IPOS+BXOFF))
            END IF
         IF (DOBACK) THEN
            CALL MDISK ('READ', IBLUN, IBFIND, BBUFF, BPOS, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) 'Background', IERR
               GO TO 990
               END IF
            CALL GSCALE (GPHFUN, GPHRNG, INPIXS, 1, BBUFF(BPOS),
     *         IBBUFF(BPOS))
            END IF
C                                       Loop over all pixels in row.
         IPLUS = 0
         ITRCXM = ITRCX - 1
         DO 250 ICOL = IBLCX,ITRCXM
            IPLUS = IPLUS + 1
C                                       Init values
            VAL(1) = BUFF(IPOS+IPLUS-1)
            VAL(2) = BUFF(IPOS+IPLUS)
            VAL(3) = RLROW(IPLUS)
            IF (DOBACK) BVAL = IBBUFF(BPOS+IPLUS-1) + IBBUFF(BPOS+IPLUS)
     *         + IBLROW(IPLUS)
C                                       Init positions.
            XPOS(1) = ICOL
            XPOS(2) = ICOL + 1
            XPOS(3) = ICOL
            YPOS(1) = IROW
            YPOS(2) = IROW
            YPOS(3) = IROW - 1
C                                       Loop for both triangles.
            DO 200 ITRI = 1,2
C                                       Changes for 2nd triangle.
               IF (ITRI.EQ.2) THEN
                  VAL(1) = RLROW(IPLUS+1)
                  IF (DOBACK) BVAL = BVAL - IBBUFF(BPOS+IPLUS-1) +
     *               IBLROW(IPLUS+1)
                  XPOS(1) = ICOL + 1
                  YPOS(1) = IROW - 1
                  END IF
C                                       Order points in triangle.
               DO 130 II = 1,3
                  IF (VAL(II).EQ.FBLANK) GO TO 200
 130              CONTINUE
               INDEX = 0
               IF (VAL(1).GT.VAL(2)) INDEX = 1
               IF (VAL(3).GE.VAL(1)) INDEX = INDEX + 2
               IF (VAL(2).GE.VAL(3)) INDEX = INDEX + 4
C                                       find large, med, small
C                                       values and X,Y positions.
               II = IPERM(1,INDEX)
               VL = VAL(II)
               XL = XPOS(II)
               YL = YPOS(II)
C
               II = IPERM(2,INDEX)
               VM = VAL(II)
               XM = XPOS(II)
               YM = YPOS(II)
C
               II = IPERM(3,INDEX)
               VS = VAL(II)
               XS = XPOS(II)
               YS = YPOS(II)
C                                       Loop for all levels.
               DO 190 II = 1,INLEVS
                  VC = ALEVS(II)
C                                       Cut down negatives
                  IF (VC.GE.0.0) GO TO 140
                     IF ((XLEV.LT.2.85) .AND. (ITRI.EQ.2)) GO TO 190
                     IF (XLEV.GE.1.0) GO TO 140
                        JJ = IROW + ICOL + II
                        IF (MOD(JJ, ISLEV).NE.0) GO TO 190
 140              IF ((VC.GT.VL) .OR. ((VL-VS).LE.0.0)) GO TO 200
C                                       If level not right, next lev.
                  IF (VC.LE.VS) GO TO 190
C                                       Interpolate btwn max two corns.
                  TEMP = (VC-VS) / (VL-VS)
                  XA = TEMP * (XL-XS) + XS
                  YA = TEMP * (YL-YS) + YS
C                                       See which corners 2nd pt. btwn.
                  IF (VC.GT.VM) GO TO 150
                  IF (VM.EQ.VS) GO TO 150
C                                       Level btwn med & small corners.
                     TEMP = (VC-VS) / (VM-VS)
                     XB = TEMP * (XM-XS) + XS
                     YB = TEMP * (YM-YS) + YS
                     GO TO 160
C                                       Level btwn large & med corners.
 150                 TEMP = (VC-VM) / (VL-VM)
                     XB = TEMP * (XL-XM) + XM
                     YB = TEMP * (YL-YM) + YM
C                                       Issue position & write commands
C                                       We can avoid position command
C                                       if we switch A and B.
 160              IF ((XLAST.EQ.XB) .AND. (YLAST.EQ.YB)) THEN
                     TEMP = XA
                     XA = XB
                     XB = TEMP
                     TEMP = YA
                     YA = YB
                     YB = TEMP
C                                       See if we need to position.
                  ELSE IF ((XLAST.NE.XA) .OR. (YLAST.NE.YA)) THEN
                     CALL GPOS (XA, YA, IGBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     END IF
C                                       Draw vector.
                  IF (VC.GE.0.0) THEN
                     IF ((DOBACK) .AND. (BVAL.GT.BCUT)) THEN
                        CALL GDVEC (XB, YB, IGBLK, IERR)
                     ELSE IF (DOCOLR.GT.0) THEN
                        IF ((DOCOLR.EQ.2) .AND. (LASTC.NE.II)) THEN
                           CALL G3VCOL (RGBLEV(1,II), RGBLEV(2,II),
     *                        RGBLEV(3,II), IGBLK, IERR)
                           IF (IERR.NE.0) GO TO 999
                           LASTC = II
                           END IF
                        CALL G3VEC (XB, YB, IGBLK, IERR)
                     ELSE
                        CALL GVEC (XB, YB, IGBLK, IERR)
                        END IF
                     IF (IERR.NE.0) GO TO 999
                     XLAST = XB
                     YLAST = YB
                     GO TO 190
C                                       Negative contours broken
                  ELSE
                     TEMP = LOCINT * SQRT (((XB-XA)**2 + (YB-YA)**2)
     *                  / 2.0)
                     MININT = IROUND (TEMP)
                     IF (MININT.LT.2) MININT = 2
                     DELTAX = (XB - XA) / MININT
                     DELTAY = (YB - YA) / MININT
                     DO 185 I = 1,MININT,2
                        XB = XA + DELTAX
                        YB = YA + DELTAY
                        IF ((DOBACK) .AND. (BVAL.GT.BCUT)) THEN
                           CALL GDVEC (XB, YB, IGBLK, IERR)
                        ELSE IF (DOCOLR.GT.0) THEN
                           IF ((DOCOLR.EQ.2) .AND. (LASTC.NE.II)) THEN
                              CALL G3VCOL (RGBLEV(1,II), RGBLEV(2,II),
     *                           RGBLEV(3,II), IGBLK, IERR)
                              IF (IERR.NE.0) GO TO 999
                              LASTC = II
                              END IF
                           CALL G3VEC (XB, YB, IGBLK, IERR)
                        ELSE
                           CALL GVEC (XB, YB, IGBLK, IERR)
                           END IF
                        IF (IERR.NE.0) GO TO 999
                        IF (I.LT.MININT-1) THEN
                           XA = XB + DELTAX
                           YA = YB + DELTAY
                           CALL GPOS (XA, YA, IGBLK, IERR)
                           IF (IERR.NE.0) GO TO 999
                           END IF
 185                    CONTINUE
                     XLAST = XB
                     YLAST = YB
                     END IF
 190              CONTINUE
 200           CONTINUE
 250        CONTINUE
         CALL RCOPY (INPIXS, BUFF(IPOS), RLROW)
         IF (DOBACK) CALL COPY (INPIXS, IBBUFF(BPOS), IBLROW)
 300     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ON ',A,' IMAGE, MDISK ERROR',I6)
      END
