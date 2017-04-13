      SUBROUTINE COLORC (ITYPE, NLEVS, NSTART, NCONT, GAMMA, BUFFER,
     *   IERR)
C-----------------------------------------------------------------------
C! write color contour OFM to TV from standard sets
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2004, 2008
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
C   COLORC writes a color contour OFM using a standard table &
C   sequence of colors.
C   Inputs:
C      ITYPE   I          Which table: 1 AIPS/dutch 9c 2 Lincoln 8c
C                          3 Dutch 10c, 4 IMPS 8c, 4 IMPS 64c
C      NLEVS   I          number of levels (256 or 1024 usually)
C      NSTART  I          intensity level of first contour
C      NCONT   I          intensity range to contour
C      GAMMA   R          gamma power for color correction
C   Output:
C      BUFFER  R(TVMOFM)  scratch buffer
C      IERR    I          error code of ZM70XF: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   ITYPE, NLEVS, NSTART, NCONT, IERR
      REAL      GAMMA, BUFFER(*)
C
      INTEGER   NMAX, NC, ISTP, IS, IC, NS(5), ICOL, NCOL, HOLCOL(10,3),
     *   IMP8(8,3), IMP64(64,3), RED8(8), GREEN8(8), BLUE8(8), RED6(64),
     *   GREEN6(64), BLUE6(64), HOLSPE(9,3), I, INCR, IT, II, JJ,
     *   REDL(8), GREENL(8), BLUEL(8), LINC(8,3)
      REAL      GAMINV, C
      LOGICAL   VRTRTC, WASYNC
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (IMP8(1,1), RED8(1)),    (IMP64(1,1), RED6(1)),
     *            (IMP8(1,2), GREEN8(1)),  (IMP64(1,2), GREEN6(1)),
     *            (IMP8(1,3), BLUE8(1)),   (IMP64(1,3), BLUE6(1)),
     *            (LINC(1,1), REDL(1)),    (LINC(1,2), GREENL(1)),
     *            (LINC(1,3), BLUEL(1))
      DATA VRTRTC /.TRUE./
      DATA NS /9,8,10,8,64/
C
      DATA HOLCOL /5, 10,   0,  0, 100, 100, 100,  0, 40, 100,
     *             5, 50,   0, 35,   0,  35, 100, 70,  0, 100,
     *             5, 80, 100,  0,   0,   0,   0,  0, 40, 100/
      DATA HOLSPE /6, 14,   0,  6,  0,  0, 100, 100, 100,
     *             6,  0,   0, 30, 23, 90, 100,  35,   0,
     *             6, 28,  50, 80,  0,  0,   0,   0,   0/
C                                       Lincoln Greenhill idea
      DATA LINC / 148,   0,   0,   0, 238, 238, 238, 119,
     *              0,   0, 225, 238, 238, 154,   0,  59,
     *            211, 238, 238,   0,   0,   0,   0,  16/
C                                       M.Lesser's "IMPS" colors
C     DATA RED8   / 0, 20, 10,  0,  0, 10, 30, 80/
C     DATA GREEN8 / 0, 31, 30, 20, 40, 60, 40,  0/
C     DATA BLUE8  / 0, 29, 40, 60, 40, 10, 10,  0/
C                                       IMPS actual colors
      DATA RED8   / 7,  0,  0,  0, 80, 80, 80, 80/
      DATA GREEN8 / 7,  0, 80, 80,  0,  0, 80, 80/
      DATA BLUE8  / 7, 80,  0, 80,  0, 80,  0, 80/
C                                       Corrected IMPS RGB colors
      DATA GREEN6 / 10 * 0, 1, 1, 2, 3, 3, 4, 5, 5, 6, 7, 7, 8, 8, 9,
     *   10, 10, 11, 12, 12, 13, 14, 14, 7 * 15, 14, 14, 13, 12, 12,
     *   11, 11, 10, 9, 9, 8, 7, 7, 6, 6, 5, 4, 4, 3, 3, 2, 1, 1, 0, 0/
      DATA RED6   / 29 * 0, 2, 4, 6, 8, 10, 12, 14, 27 * 15, 15/
      DATA BLUE6  / 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
     *   15, 15, 14, 14, 14, 13, 12, 11, 9, 8, 7, 6, 5, 4, 3, 2, 1,
     *   32 * 0/
C-----------------------------------------------------------------------
      GAMINV = GAMMA
      IF (GAMINV.LE.0.0) GAMINV = 1.0
      GAMINV = 1.0 / GAMINV
      IT = ITYPE
      IF ((IT.LT.1) .OR. (IT.GT.5)) IT = 1
C     IF (IT.EQ.4) GAMINV = 1.0
      NMAX = MIN (OFMINP+1, NLEVS)
      IF (NMAX.LE.10) NMAX = LUTOUT + 1
      IF (NMAX.GT.OFMINP) NMAX = OFMINP + 1
      INCR = 1
      IF ((IT.EQ.4) .AND. (NCONT.LT.NS(IT)/2)) INCR = 2
      NC = NSTART + 1
      ISTP = MAX (1, NCONT/(NS(IT)/INCR))
      NC = MAX (1, MIN (NC, NMAX - (NS(IT)/INCR)*ISTP + 1))
      WASYNC = ISYNCH.EQ.0
      IF (WASYNC) CALL YHOLD ('ONNN', I)
C                                        loop thru colors
      DO 30 NCOL = 1,3
         IC = OFMINP + 1
         CALL RFILL (IC, 0.0, BUFFER)
         IC = ISTP + 1
         IS = 1
         DO 20 I = 1,NMAX
            IF (IC.LT.ISTP) GO TO 10
               IF (IT.EQ.1) C = HOLSPE(IS, NCOL) / 100.0
               IF (IT.EQ.2) C = (LINC(IS, NCOL) / 255.0) ** TVGAMA
               IF (IT.EQ.3) C = HOLCOL(IS, NCOL) / 100.0
               IF (IT.EQ.4) C = IMP8 (IS, NCOL) / 80.0
               IF (IT.EQ.5) C = IMP64 (IS, NCOL) / 15.0
               C = C**GAMINV
               IS = MIN (IS+INCR, NS(IT))
               IC = 0
 10            CONTINUE
            IF (I.GE.NC) IC = IC + 1
            BUFFER(I) = C
 20         CONTINUE
         BUFFER(1) = 0
C                                        extend to fill table
         I = OFMINP + 1
         JJ = NMAX
         I = I / JJ
         DO 21 II = 2,I
            CALL RCOPY (NMAX, BUFFER, BUFFER(JJ+1))
            JJ = JJ + NMAX
 21         CONTINUE
C                                        send to TV
         ICOL = 2 ** (3-NCOL)
         CALL YOFM ('WRIT', ICOL, VRTRTC, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 990
 30      CONTINUE
C
 990  IF (WASYNC) CALL YHOLD ('OFFF', I)
C
 999  RETURN
      END
