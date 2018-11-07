      SUBROUTINE YCWRIT (IPLANE, IMAWIN, CATBLK, BUFF, IERR)
C-----------------------------------------------------------------------
C! write image header to image catalog, update image catalog directory
C# Y0 TV-util
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
C   Write image catalog block in CATBLK into image catalog
C   Inputs:
C      IPLANE   I        image plane involved
C      IMAWIN   I(4)     Corners of image on screen
C      CATBLK   I(256)   Image catalog block
C  Outputs:
C      BUFF     I(256)   working buffer
C      IERR     I        error code: 0 => ok
C                           1 => no room in catalog
C                           2 => IO problems
C   Generic Y routine: for local host operated TV device
C-----------------------------------------------------------------------
      INTEGER   IPLANE, IMAWIN(4), CATBLK(256), BUFF(256), IERR
C
      INTEGER   ILUN, IIND, IER, NLAST, I, J, LL, NREC, NREC0, JREC
      LOGICAL   OVER, DONE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA ILUN /20/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open image catalog
      CALL ICOPEN (IPLANE, ILUN, IIND, IER)
      IF (IER.NE.0) GO TO 970
      LL = (NIMAGE - 1) / 51
C                                       multiple image planes
      IF (IPLANE.GT.NGRAY) GO TO 200
         NREC = 1 + (IPLANE - 1) * (NIMAGE + 1 + LL)
         NREC0 = NREC
C                                       Get directory block
         CALL ZFIO ('READ', ILUN, IIND, NREC, BUFF, IER)
         IF (IER.NE.0) GO TO 970
         NLAST = BUFF(1)
         DONE = .FALSE.
C                                       Search catalog
         DO 150 I = 1,NIMAGE
            IF ((I.EQ.1) .OR. (MOD(I,51).NE.1)) GO TO 130
               CALL ZFIO ('WRIT', ILUN, IIND, NREC, BUFF, IER)
               IF (IER.NE.0) GO TO 970
               NREC = NREC + 1
               CALL ZFIO ('READ', ILUN, IIND, NREC, BUFF, IER)
               IF (IER.NE.0) GO TO 970
 130        J = 5 * MOD (I-1, 51) + 2
C                                       Is slot empty
            IF (BUFF(J).EQ.0) GO TO 140
C                                       Does image overlap old image
               OVER = (IMAWIN(1).LE.BUFF(J+1)) .AND.
     *            (IMAWIN(2).LE.BUFF(J+2))
               OVER = OVER .AND. (IMAWIN(3).GE.BUFF(J+3)) .AND.
     *            (IMAWIN(4).GE.BUFF(J+4))
               IF (.NOT.OVER) GO TO 150
C                                       Clear slot
               IF (.NOT.DONE) GO TO 140
                  BUFF(J) = 0
                  GO TO 150
C                                       on EMPTY or OVER and NOT.DONE
C                                       update directory and write new
C                                       block
 140        IF (DONE) GO TO 150
               NLAST = NLAST + 1
               BUFF(1) = NLAST
               BUFF(J) = NLAST
               CALL COPY (4, IMAWIN, BUFF(J+1))
C                                       Write requested image block
               JREC = NREC0 + I + LL
               CALL ZFIO ('WRIT', ILUN, IIND, JREC, CATBLK, IER)
               IF (IER.NE.0) GO TO 970
               DONE = .TRUE.
 150        CONTINUE
C                                       No room in catalog
         IF (DONE) GO TO 160
            WRITE (MSGTXT,1150) IPLANE
            IERR = 1
            GO TO 980
C                                       Update directory block
 160     CONTINUE
            CALL ZFIO ('WRIT', ILUN, IIND, NREC, BUFF, IER)
            IF (IER.NE.0) GO TO 970
            IF (NREC.EQ.NREC0) GO TO 990
            CALL ZFIO ('READ', ILUN, IIND, NREC0, BUFF, IER)
            IF (IER.NE.0) GO TO 970
            BUFF(1) = NLAST
            CALL ZFIO ('WRIT', ILUN, IIND, NREC0, BUFF, IER)
            IF (IER.NE.0) GO TO 970
            GO TO 990
C                                       graphics: write block
 200  IF (IPLANE.LE.NGRAY+NGRAPH) THEN
         NREC = IPLANE + NGRAY * (NIMAGE + LL)
         CALL ZFIO ('WRIT', ILUN, IIND, NREC, CATBLK, IER)
         IF (IER.NE.0) GO TO 970
      ELSE
         IERR = 3
         END IF
      GO TO 990
C                                       Errors
 970  IF (IER.EQ.11) THEN
         IERR = 3
         WRITE (MSGTXT,1970)
      ELSE
         IERR = 2
         WRITE (MSGTXT,1971) IER
         END IF
 980  CALL MSGWRT (6)
C
 990  CALL ZCLOSE (ILUN, IIND, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT ('YCWRIT: CATALOG FULL FOR PLANE',I6)
 1970 FORMAT ('YCWRIT: NO LONGER HANDLES TK DEVICES')
 1971 FORMAT ('YCWRIT: NO ACCESS TO IMAGE CATALOG, ERROR =',I6)
      END
