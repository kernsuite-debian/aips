      SUBROUTINE YCREAD (IPLANE, IX, IY, CATBLK ,IERR)
C-----------------------------------------------------------------------
C! read the image catalog, return image header for TV only
C# Y0 TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 1998, 2000
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
C   Read image catalog block into CATBLK - TV only (TK in TKCATL)
C   Inputs:
C      IPLANE  I       plane containing image whose block is wanted
C      IX      I       X pixel coordinate of a point within image
C      IY      I       Y pixel coordinate of point within image
C   Outputs:
C      CATBLK  I(256)  Image catalog block
C      IERR    I       error codes: 0 => ok
C                         1 => IX, IY lies outside image
C                         2 => Catalog i/o errors
C                         3 => refers to TK device
C   XAS (SSS) Y routine: for image catalog inside XAS
C-----------------------------------------------------------------------
      INTEGER   IPLANE, IX, IY, CATBLK(256), IERR
C
      INTEGER   ILUN, IIND, IER, I, J, NLAST, NNOW, NSEQ, LL, NREC,
     *   DAT(4), NWSD, OPCODE
      LOGICAL   IN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA ILUN /20/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Image catalog on disk
      IF (TVIMGC.LE.0) THEN
C                                       Open image catalog
         CALL ICOPEN (IPLANE, ILUN, IIND, IER)
         IF (IER.NE.0) GO TO 970
         LL = (NIMAGE - 1) / 51
C                                       Read image catalog directory
C                                       Single-image planes
         IF (IPLANE.GT.NGRAY) THEN
            NREC = IPLANE + NGRAY * (NIMAGE + LL)
            IF (IPLANE.LE.NGRAY+NGRAPH) THEN
               CALL ZFIO ('READ', ILUN, IIND, NREC, CATBLK, IER)
               IF (IER.NE.0) GO TO 970
            ELSE
               IERR = 3
               END IF
C                                       Multi-image planes
         ELSE
            NREC = (IPLANE - 1) * (1 + NIMAGE + LL)
C                                       Search directory
            NLAST = 0
            NSEQ = 0
            DO 50 I = 1,NIMAGE
               IF (MOD(I,51).EQ.1) THEN
                  NREC = NREC + 1
                  CALL ZFIO ('READ', ILUN, IIND, NREC, CATBLK, IER)
                  IF (IER.NE.0) GO TO 970
                  END IF
               J = 5 * MOD (I-1, 51) + 2
               NNOW = CATBLK(J)
C                                       entry empty or older than
C                                       present last entry?
               IF (NNOW.GT.NLAST) THEN
C                                       Does image contain IX, IY?
                  IN = (IX.GE.CATBLK(J+1)) .AND. (IX.LE.CATBLK(J+3))
                  IN = (IY.GE.CATBLK(J+2)) .AND. (IY.LE.CATBLK(J+4))
     *               .AND. (IN)
                  IF (IN) THEN
                     NLAST = NNOW
                     NSEQ = I
                     END IF
                  END IF
 50            CONTINUE
C                                       Did we find anything?
            IF (NLAST.LE.0) THEN
               IERR = 1
C                                       Read requested image block
            ELSE
               NREC = NSEQ + IPLANE * (NIMAGE + 1 + LL) - NIMAGE
               CALL ZFIO ('READ', ILUN, IIND, NREC, CATBLK, IER)
               IF (IER.NE.0) GO TO 970
               END IF
            END IF
         GO TO 980
C                                       XAS has image catalog
      ELSE
         DAT(1) = IPLANE
         DAT(2) = IX
         DAT(3) = IY
         DAT(4) = 0
         OPCODE = 32
         CALL ZSSSXF (OPCODE, DAT, 0, CATBLK, NWSD, IER)
         IF (IER.EQ.0) THEN
            CALL CATN2L ('N2LI', CATBLK, CATBLK)
         ELSE IF (IER.EQ.-2) THEN
            IERR = 1
         ELSE
            IERR = 2
            WRITE (MSGTXT,1200) IER
            CALL MSGWRT (6)
            END IF
         GO TO 990
         END IF
C                                       IO error
 970  IF (IER.EQ.11) THEN
         WRITE (MSGTXT,1970)
         IERR = 3
      ELSE
         WRITE (MSGTXT,1971) IER
         IERR = 2
         END IF
      CALL MSGWRT (6)
C
 980  CALL ZCLOSE (ILUN, IIND, IER)
C                                       Check if pixel inside graphics
 990  IF ((IPLANE.GT.NGRAY) .AND. (IERR.EQ.0)) THEN
         IF ((IX.LT.CATBLK(IICOR)) .OR. (IX.GT.CATBLK(IICOR+2))
     *      .OR. (IY.LT.CATBLK(IICOR+1)) .OR. (IY.GT.CATBLK(IICOR+3)))
     *      IERR = 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('YCREAD: XAS IMAGE CATALOG RETURNS ERROR',I5)
 1970 FORMAT ('YCREAD: NO LONGER HANDLES TK DEVICES')
 1971 FORMAT ('YCREAD: CAN''T ACCESS IMAGE CATALOG, ERROR =',I6)
      END
