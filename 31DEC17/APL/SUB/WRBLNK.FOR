      SUBROUTINE WRBLNK (IPLANE, NODIM, IDLUN, IDIND, IERR)
C-----------------------------------------------------------------------
C! write blanked pixels at all pixels corresponding to specified pixel
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   WRBLNK will write blank pixels at a specified pixel position
C   throughout an image file.  The file must be open.
C   Inputs:
C      IPLANE   I   The pixel value on the NODIM st axis
C      NODIM    I   the axis number on which the pixel occurs
C                          ( > 1 )
C      IDLUN    I   the logical unit number for the R
C                      destination file.
C      IDIND    I   the FTAB pointer for the destination file.
C   Outputs:
C      IERR     I   error indicator: from MINI3, MDISK
C   Common:
C      /MAPHDR/ input   Catblk of the output map
C      /BUFRS/  output  IO buffer scratch area > MAXIMG f.p.
C-----------------------------------------------------------------------
      INTEGER   IPLANE, NODIM, IDLUN, IDIND, IERR
C
      INTEGER   BLKOF, IWIN(4), IDEPTH(5), LNY, IDBSIZ, INX, INY, IROW,
     *   CATBLK(256), I3, I3B, I4, I4B, I5, I5B, I6, I6B, I7, I7B, IDPOS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PMAD.INC'
      REAL      RBLK(MABFSS)
      COMMON /BUFRS/ RBLK
      COMMON /MAPHDR/ CATBLK
C-----------------------------------------------------------------------
C                                       init output
      IERR = 2
      IF (NODIM.LE.1) GO TO 999
      IDBSIZ = MABFSS * 2
      INX = CATBLK(KINAX)
      INY = CATBLK(KINAX+1)
      LNY = INY
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = INX
      IWIN(4) = INY
      IF (NODIM.EQ.2) IWIN(2) = IPLANE
      IF (NODIM.EQ.2) IWIN(4) = IPLANE
      IF (NODIM.EQ.2) LNY = 1
      CALL RFILL (MABFSS, FBLANK, RBLK)
C                                       prepare loop
      I7B = 1
      I6B = 1
      I5B = 1
      I4B = 1
      I3B = 1
      IF (CATBLK(KIDIM).LT.3) GO TO 10
         I3B = MAX (1, CATBLK(KINAX+2))
         IF (NODIM.EQ.3) I3B = 1
      IF (CATBLK(KIDIM).LT.4) GO TO 10
         I4B = MAX (1, CATBLK(KINAX+3))
         IF (NODIM.EQ.4) I4B = 1
      IF (CATBLK(KIDIM).LT.5) GO TO 10
         I5B = MAX (1, CATBLK(KINAX+4))
         IF (NODIM.EQ.5) I5B = 1
      IF (CATBLK(KIDIM).LT.6) GO TO 10
         I6B = MAX (1, CATBLK(KINAX+5))
         IF (NODIM.EQ.6) I6B = 1
      IF (CATBLK(KIDIM).LT.7) GO TO 10
         I7B = MAX (1, CATBLK(KINAX+6))
         IF (NODIM.EQ.7) I7B = 1
 10   DO 100 I7 = 1,I7B
      DO 100 I6 = 1,I6B
      DO 100 I5 = 1,I5B
      DO 100 I4 = 1,I4B
      DO 100 I3 = 1,I3B
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
         IF (NODIM.GT.2) IDEPTH(NODIM-2) = IPLANE
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, BLKOF, IERR)
         IF (IERR.NE.0) GO TO 999
         BLKOF = BLKOF + 1
         CALL MINIT ('WRIT', IDLUN, IDIND, INX, INY, IWIN, RBLK, IDBSIZ,
     *      BLKOF, IERR)
         IF (IERR.EQ.0) GO TO 20
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (7)
            GO TO 999
C                                       write for all rows.
 20      DO 60 IROW = 1,LNY
            CALL MDISK ('WRIT', IDLUN, IDIND, RBLK, IDPOS, IERR)
            IF (IERR.EQ.0) GO TO 60
               WRITE (MSGTXT,1020) IERR
               CALL MSGWRT (7)
               GO TO 999
 60         CONTINUE
C                                       Write last buffer.
         CALL MDISK ('FINI', IDLUN, IDIND, RBLK, IDPOS, IERR)
         IF (IERR.EQ.0) GO TO 100
            WRITE (MSGTXT,1060) IERR
            CALL MSGWRT (7)
            GO TO 999
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WRBLNK: DOUBLE BUFFER INIT WRITE ERROR. MINIT ERR =',I5)
 1020 FORMAT ('WRBLNK: DOUBLE BUFFERED WRITE ERROR. MDISK ERR =',I5)
 1060 FORMAT ('WRBLNK: ERROR WRITING LAST BUFFER. MDISK ERR =',I5)
      END
