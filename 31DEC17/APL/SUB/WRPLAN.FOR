      SUBROUTINE WRPLAN (IPLANE, NODIM, INP, BLC, TRC, ISLUN, ISIND,
     *   IDLUN, IDIND, SCALE, XMIN, XMAX, IERR)
C-----------------------------------------------------------------------
C! copies an N dimensional plane to a N or N+1 dimensional image
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2010
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
C   WRPLAN will read from an nD image file, and write to a specific
C   plane in a corresponding n+1D file.  The two files must be open
C   before this routine is called.
C   Inputs:
C      IPLANE   I   The plane to write to in the n+1D map.
C      NODIM    I   the axis number to which IPLANE applies > 1
C      INP      I(7)   the number of pixels on each axis (input)
C      BLC      R(7)   bottom left hand corner of nD map.
C                            (No defaults).
C      TRC      R(7)   top right hand corner of nD map.
C      ISLUN    I   the logical unit number of the source file.
C      ISIND    I   the FTAB pointer for the source file.
C      IDLUN    I   the logical unit number for the R
C                          destination file.
C      IDIND    I   the FTAB pointer for the destination file.
C      SCALE    R   multiple the input image by SCALE
C   In/out:
C      XMIN     R   map minimum scaled as output map
C      XMAX     R   map maximum scaled as output map
C   Outputs:
C      IERR     I   error indicator: >= 0 from IO routines
C                          < 0 => ok, but some output pixels blanked
C   Common:
C      /MAPHDR/ input    CATBLK of output map
C      /BUFRS/  output   B1(MABFSS), B2(MABFSS) scratch IO buffers
C-----------------------------------------------------------------------
      INTEGER   IPLANE, NODIM, INP(*), ISLUN, ISIND, IDLUN, IDIND, IERR
      REAL      BLC(7), TRC(7), SCALE, XMIN, XMAX
C
      INTEGER   IBLKOF, IWIN(4), DEPTH(5), JWIN(4), OBLKOF, IBKSIZ,
     *   I3, I3A, I3B, I4, I4A, I4B, I5, I5A, I5B, I6, I6A, I6B, I7,
     *   I7A, I7B, IDNX, IDNY, IDPOS, IPIX, IROW, ISPOS, CATBLK(256),
     *   JDNY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PMAD.INC'
      REAL      BUF1(MABFSS), BUF2(MABFSS), TEMP
      COMMON /BUFRS/ BUF1, BUF2
      COMMON /MAPHDR/ CATBLK
C-----------------------------------------------------------------------
      IERR = 2
      IF ((NODIM.LT.2) .OR. (NODIM.GT.KICTPN)) GO TO 999
C                                       Set window, input file.
      IWIN(1) = BLC(1) + .01
      IWIN(2) = BLC(2) + .01
      IWIN(3) = TRC(1) + .01
      IWIN(4) = TRC(2) + .01
      IBKSIZ = 2 * MABFSS
C                                       init output
      IDNX = TRC(1) - BLC(1) + 1.01
      IDNY = TRC(2) - BLC(2) + 1.01
      JDNY = IDNY
      JWIN(1) = 1
      JWIN(2) = 1
      JWIN(3) = IDNX
      JWIN(4) = IDNY
      IF (NODIM.EQ.2) JWIN(2) = IPLANE
      IF (NODIM.EQ.2) JWIN(4) = IPLANE + IDNY - 1
      IF (NODIM.EQ.2) IDNY = CATBLK(KINAX+1)
      I3A = BLC(3) + 0.01
      I4A = BLC(4) + 0.01
      I5A = BLC(5) + 0.01
      I6A = BLC(6) + 0.01
      I7A = BLC(7) + 0.01
      I3B = TRC(3) + 0.01
      I4B = TRC(4) + 0.01
      I5B = TRC(5) + 0.01
      I6B = TRC(6) + 0.01
      I7B = TRC(7) + 0.01
      DO 70 I7 = I7A,I7B
      DO 70 I6 = I6A,I6B
      DO 70 I5 = I5A,I5B
      DO 70 I4 = I4A,I4B
      DO 70 I3 = I3A,I3B
         DEPTH(1) = I3
         DEPTH(2) = I4
         DEPTH(3) = I5
         DEPTH(4) = I6
         DEPTH(5) = I7
         CALL COMOFF (7, INP, DEPTH, IBLKOF, IERR)
         IF (IERR.NE.0) GO TO 999
         IBLKOF = IBLKOF + 1
         DEPTH(1) = I3 - I3A + 1
         DEPTH(2) = I4 - I4A + 1
         DEPTH(3) = I5 - I5A + 1
         DEPTH(4) = I6 - I6A + 1
         DEPTH(5) = I7 - I7A + 1
         IF (NODIM.GT.2) DEPTH(NODIM-2) = IPLANE
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), DEPTH, OBLKOF, IERR)
         IF (IERR.NE.0) GO TO 999
         OBLKOF = OBLKOF + 1
C                                       Initialize for double buffering
         CALL MINIT ('READ', ISLUN, ISIND, INP(1), INP(2), IWIN, BUF1,
     *      IBKSIZ, IBLKOF, IERR)
         IF (IERR.EQ.0) GO TO 10
            WRITE (MSGTXT,1000) IERR
            GO TO 995
 10      CALL MINIT ('WRIT', IDLUN, IDIND, IDNX, IDNY, JWIN, BUF2,
     *      IBKSIZ, OBLKOF, IERR)
         IF (IERR.EQ.0) GO TO 20
            WRITE (MSGTXT,1010) IERR
            GO TO 995
C                                       Read - write for all rows.
 20      DO 60 IROW = 1,JDNY
            CALL MDISK ('READ', ISLUN, ISIND, BUF1, ISPOS, IERR)
            IF (IERR.EQ.0) GO TO 30
               WRITE (MSGTXT,1020) IERR
               GO TO 995
 30         CALL MDISK ('WRIT', IDLUN, IDIND, BUF2, IDPOS, IERR)
            IF (IERR.EQ.0) GO TO 40
               WRITE (MSGTXT,1030) IERR
               GO TO 995
C                                       transfer to write buf
 40         DO 50 IPIX = 1,IDNX
               TEMP = BUF1(ISPOS+IPIX-1)
               IF (TEMP.NE.FBLANK) THEN
                  TEMP = TEMP * SCALE
                  XMIN = MIN (TEMP, XMIN)
                  XMAX = MAX (TEMP, XMAX)
                  END IF
               BUF2(IDPOS+IPIX-1) = TEMP
 50            CONTINUE
 60         CONTINUE
C                                       Write last buffer.
         CALL MDISK ('FINI', IDLUN, IDIND, BUF2, IDPOS, IERR)
         IF (IERR.EQ.0) GO TO 70
            WRITE (MSGTXT,1060) IERR
            GO TO 995
 70      CONTINUE
      GO TO 999
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WRPLAN: DOUBLE BUFFER INIT READ ERROR. MINIT ERR=',I5)
 1010 FORMAT ('WRPLAN: DOUBLE BUFFER INIT WRITE ERROR. MINIT ERR =',I5)
 1020 FORMAT ('WRPLAN: DOUBLE BUFFERED READ ERROR. MDISK ERR =',I5)
 1030 FORMAT ('WRPLAN: DOUBLE BUFFERED WRITE ERROR. MDISK ERR =',I5)
 1060 FORMAT ('WRPLAN: ERROR WRITING LAST BUFFER. MDISK ERR =',I5)
      END
