      SUBROUTINE LAYOUT (CHAN, INFILE, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! Generates panel layout plot on the TV
C# Graphics Map-util TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2015
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
C   Plot the boxes
C   Input:
C      CHAN     I        Channel to use (1-NGRAY+NGRAPH)
C      INFILE   C*48     In file name
C   Output:
C      SCRTCH   I(*)     Scratch
C      IERR     I        0 => OK
C   Common
C      DCAT.INC          Image header including TV corners
C-----------------------------------------------------------------------
      INTEGER   CHAN, SCRTCH(*), IERR
      CHARACTER INFILE*(*)
C
      INTEGER   JTRIM, MSGSAV, INC, LUN, FIND, KBPLIM, KBP, NC, I, NP,
     *   J, IP, VLNPL(6,2), VLNUM, K, IPX(2), IPY(2), IROUND
      CHARACTER LFILE*64, LINE*80
      DOUBLE PRECISION D, THETA, DTHETA
      REAL      R1, R2, X(5), Y(5), DX, DY, PX0, PY0, PX(2), PY(2),
     *   VLRPL(2,6,2), BLC(2), TRC(2), PP(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN /3/
      DATA VLNPL /12,16,24,40,40,40, 20,20,40,40,40,40/
      DATA VLRPL /1.983,3.683, 3.683,5.563, 5.563,7.391, 7.391,9.144,
     *   9.144,10.87, 10.87,12.5,
     *   1.676,3.518, 3.518,5.423, 5.423,7.277, 7.277,9.081,
     *   9.081,10.808, 10.808,12.500/
C-----------------------------------------------------------------------
      IERR = 0
      IF ((CATR(KRCIC).EQ.0.0) .OR. (CATR(KRCIC+1).EQ.0.0)) GO TO 999
      PX0 = CATR(KRCRP)
      PY0 = CATR(KRCRP+1)
      DX = 1.0 / CATR(KRCIC)
      DY = 1.0 / CATR(KRCIC+1)
      MSGSAV = MSGSUP
      VLNUM = 0
      IF (INFILE.EQ.' ') VLNUM = 1
      IF (INFILE.EQ.'VLA') VLNUM = 1
      IF (INFILE.EQ.'VLBA') VLNUM = 2
      BLC(1) = CATBLK(IIWIN)
      BLC(2) = CATBLK(IIWIN+1)
      TRC(1) = CATBLK(IIWIN+2)
      TRC(2) = CATBLK(IIWIN+3)
C                                       open layout file
      IF (VLNUM.LE.0) THEN
         INC = JTRIM (INFILE)
         LFILE = INFILE(:INC) // '.layout'
         CALL ZTXOPN ('READ', LUN, FIND, LFILE, .FALSE., IERR)
C                                       panel layout
         IF (IERR.EQ.0) THEN
C                                       read number of cards
            CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ LAYOUT LINE 1'
               GO TO 990
               END IF
            KBPLIM = JTRIM (LINE)
            KBP = 1
            CALL GETNUM (LINE, KBPLIM, KBP, D)
            IF (D.EQ.DBLANK) GO TO 980
            NC = D
            DO 40 J = 1,NC
               CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ LAYOUT DATA LINE'
                  GO TO 990
                  END IF
               KBPLIM = JTRIM (LINE)
               KBP = 1
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               NP = D
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               R1 = D
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               R2 = D
               DTHETA = 360.0D0 / NP * DG2RAD
               THETA = 0.0
               DO 30 IP = 1,NP
                  X(1) = R1 * SIN (THETA)
                  X(2) = R2 * SIN (THETA)
                  X(3) = R2 * SIN (THETA+DTHETA)
                  X(4) = R1 * SIN (THETA+DTHETA)
                  X(5) = X(1)
                  Y(1) = R1 * COS (THETA)
                  Y(2) = R2 * COS (THETA)
                  Y(3) = R2 * COS (THETA+DTHETA)
                  Y(4) = R1 * COS (THETA+DTHETA)
                  Y(5) = Y(1)
                  DO 20 I = 1,4
                     PX(1) = PX0 + DX * X(I)
                     PX(2) = PX0 + DX * X(I+1)
                     PY(1) = PY0 - DY * Y(I)
                     PY(2) = PY0 - DY * Y(I+1)
                     CALL LINLIM (BLC, TRC, PX, PY, IERR)
                     IF (IERR.EQ.0) THEN
                        DO 10 K = 1,2
                           PP(1) = PX(K)
                           PP(2) = PY(K)
                           CALL MP2IMA (PP, PP)
                           IPX(K) = IROUND (PP(1))
                           IPY(K) = IROUND (PP(2))
 10                        CONTINUE
                        CALL IMVECT ('ONNN', CHAN, 2, IPX, IPY, SCRTCH,
     *                     IERR)
                        END IF
 20                  CONTINUE
                  THETA = THETA + DTHETA
 30               CONTINUE
 40            CONTINUE
            CALL ZTXCLS (LUN, FIND, IERR)
            END IF
C                                       known arrays
      ELSE
         NC = 6
         DO 240 J = 1,NC
            NP = VLNPL(J,VLNUM)
            THETA = 0.0
            DTHETA = 360.0D0 / NP * DG2RAD
            R1 = VLRPL(1,J,VLNUM)
            R2 = VLRPL(2,J,VLNUM)
            DO 230 IP = 1,NP
               X(1) = R1 * SIN (THETA)
               X(2) = R2 * SIN (THETA)
               X(3) = R2 * SIN (THETA+DTHETA)
               X(4) = R1 * SIN (THETA+DTHETA)
               X(5) = X(1)
               Y(1) = R1 * COS (THETA)
               Y(2) = R2 * COS (THETA)
               Y(3) = R2 * COS (THETA+DTHETA)
               Y(4) = R1 * COS (THETA+DTHETA)
               Y(5) = Y(1)
               DO 220 I = 1,4
                  PX(1) = PX0 + DX * X(I)
                  PX(2) = PX0 + DX * X(I+1)
                  PY(1) = PY0 - DY * Y(I)
                  PY(2) = PY0 - DY * Y(I+1)
                  CALL LINLIM (BLC, TRC, PX, PY, IERR)
                  IF (IERR.EQ.0) THEN
                     DO 210 K = 1,2
                        PP(1) = PX(K)
                        PP(2) = PY(K)
                        CALL MP2IMA (PP, PP)
                        IPX(K) = IROUND (PP(1))
                        IPY(K) = IROUND (PP(2))
 210                    CONTINUE
                     CALL IMVECT ('ONNN', CHAN, 2, IPX, IPY, SCRTCH,
     *                  IERR)
                     END IF
 220              CONTINUE
               THETA = THETA + DTHETA
 230           CONTINUE
 240        CONTINUE
         END IF
      GO TO 999
C                                       bad value
 980  IERR = 2
      MSGTXT = 'ILLEGAL VALUE READ'
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LAYOUT ERROR',I3,' ON ',A)
      END
