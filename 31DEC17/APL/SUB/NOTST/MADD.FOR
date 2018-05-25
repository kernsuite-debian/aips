      SUBROUTINE MADD (INLUN1, INLUN2, OUTLUN, FACT1, FACT2,
     *   DATA1, DATA2, DATA3, XMIN, XMAX, BLANK, IERR)
C-----------------------------------------------------------------------
C! Routine to add windows of open images.
C# IO-wawa Map
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
C   MADD (for use with WAWA IO only) will add one windowed section of
C   an open map file to another windowed section of an open map file and
C   put the results into another open windowed file.  The second input
C   map may be the same as the output map (opened under a different LUN)
C   if the windows do not overlap or overlap exactly.
C   THIS PROGRAM USES COMMON MAPHDR AS A WORK BUFFER.
C    Inputs:
C          INLUN1  I   input map 1 LUN.
C          INLUN2  I   input map 2 LUN.
C          OUTLUN  I   output map LUN.
C          FACT1   R   multiplication factor for map 1.
C          FACT2   R   multiplication factor for map 2.
C          DATA1   R(*)   IO buffer big enough for 1 row.
C          DATA2   R(*)   IO buffer big enough for 1 row.
C          DATA3   R(*)   IO buffer big enough for 1 row.  If input
C                  map two is the same as the output map, and the
C                  windows overlap exactly, this buffer can be the
C                  same as DATA2.
C     Outputs:
C          XMIN    R   map min of windowed output map.
C          XMAX    R   map max of windowed output map.
C          BLANK   L   True if magic value blank pixel found else false
C          IERR    I   standard WAWA IO error code.
C-----------------------------------------------------------------------
      CHARACTER C0*12
      REAL      DATA1(*), DATA2(*), DATA3(*)
      REAL      FACT1, FACT2, XMAX, XMIN, D0
      INTEGER   IBLC1(7), ITRC1(7), IBLC2(7), ITRC2(7), IBLC3(7),
     *   ITRC3(7), IERR, INODI1, INODI2, INODI3, INVOL2, OUTLUN,
     *   INLUN1, IPIXS1, IPIXS2, IPIXS3, IPIXS4, IPIXS5, IPIXS6, IPIXS7,
     *   IDMAX, IDIM, INSLT2, OUTSLT, OUTVOL, IPTR, INLUN2,
     *   I7, I6, I5, I4, I3, I2, I1
      LOGICAL   DOCLR, T, F, BLANK
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T /.TRUE./, F /.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize XMIN and XMAX
      XMIN = 1.0E30
      XMAX = - XMIN
      BLANK = F
C                                       Get window information
      CALL GETWIN (INLUN1, INODI1, IBLC1, ITRC1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETWIN (INLUN2, INODI2, IBLC2, ITRC2, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETWIN (OUTLUN, INODI3, IBLC3, ITRC3, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Verify windows match.
      IDMAX = MAX (INODI1, INODI2)
      IDMAX = MAX (IDMAX, INODI3)
      DO 20 IDIM = 1,IDMAX
         IF (((ITRC1(IDIM) - IBLC1(IDIM)).NE.(ITRC2(IDIM) - IBLC2(IDIM))
     *      .AND. ((ITRC1(IDIM) - IBLC1(IDIM))).NE.(ITRC3(IDIM) -
     *      IBLC3(IDIM))))
     *      GO TO 980
 20      CONTINUE
C                                       Figure out if IN/OUT file.
      DO 70 IPTR = 1,EFIL
         IF (FILTAB(POLUN,IPTR).NE.INLUN2) GO TO 30
            INSLT2 = FILTAB(POCAT,IPTR)
            INVOL2 = FILTAB(POVOL,IPTR)
C
 30      CONTINUE
         IF (FILTAB(POLUN,IPTR).NE.OUTLUN) GO TO 70
            OUTSLT = FILTAB(POCAT,IPTR)
            OUTVOL = FILTAB(POVOL,IPTR)
 70      CONTINUE
C                                       If in equals out we must clear
C                                       READ later on.
      DOCLR = F
      IF ((OUTVOL.EQ.INVOL2) .AND. (OUTSLT.EQ.INSLT2)) DOCLR = T
C                                       Do add.
      IPIXS1 = ITRC1(1) - IBLC1(1) + 1
      IPIXS2 = ITRC1(2) - IBLC1(2) + 1
      IPIXS3 = ITRC1(3) - IBLC1(3) + 1
      IPIXS4 = ITRC1(4) - IBLC1(4) + 1
      IPIXS5 = ITRC1(5) - IBLC1(5) + 1
      IPIXS6 = ITRC1(6) - IBLC1(6) + 1
      IPIXS7 = ITRC1(7) - IBLC1(7) + 1
      DO 700 I7 = 1,IPIXS7
      DO 600 I6 = 1,IPIXS6
      DO 500 I5 = 1,IPIXS5
      DO 400 I4 = 1,IPIXS4
      DO 300 I3 = 1,IPIXS3
      DO 200 I2 = 1,IPIXS2
         CALL MAPIO ('READ', INLUN1, DATA1, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL MAPIO ('READ', INLUN2, DATA2, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       If IN2 = OUT we must clear this
C                                       READ so we can set WRIT status.
         IF (.NOT.DOCLR) GO TO 80
            CALL CATDIR ('CSTA', INVOL2, INSLT2, C0, C0, D0, C0, D0,
     *         'CLRD', CATBLK, IERR)
            DOCLR = F
C
 80      CONTINUE
         DO 100 I1 = 1,IPIXS1
            IF ((DATA1(I1).NE.FBLANK) .AND. (DATA2(I1).NE.FBLANK))
     *         THEN
C                                       Not blanked pixel.
               DATA3(I1) = FACT1 * DATA1(I1) + FACT2 * DATA2(I1)
               XMIN = MIN (XMIN, DATA3(I1))
               XMAX = MAX (XMAX, DATA3(I1))
C                                       Blanked pixel.
            ELSE
               DATA3(I1) = FBLANK
               BLANK = T
               END IF
C
 100        CONTINUE
         CALL MAPIO ('WRIT', OUTLUN, DATA3, IERR)
         IF (IERR.NE.0) GO TO 999
 200     CONTINUE
 300     CONTINUE
 400     CONTINUE
 500     CONTINUE
 600     CONTINUE
 700     CONTINUE
      GO TO 999
C                                       Window mis-match error.
 980  CONTINUE
      IERR = 16
C
 999  RETURN
      END
