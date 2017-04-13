      SUBROUTINE SUBGET (IDISK, ICNO, CORN, NOSCR, NX, NY, BUFF1, BUFF2,
     *   BUFSZ1, BUFSZ2, LUN1, LUN2, IRET)
C-----------------------------------------------------------------------
C! reads subimage of a plane and writes it to scratch file with shifts
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2008
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
C   SUBGET is a version of PLNGET intended to read an image which is
C   already a subimage and to put it into a larger zero-filled image
C   plane suitable for a FFT.  Note that PLNGET assumes that the
C   subimage it reads is a power of 2 or otherwise suitable for rotation
C   about the center point (i.e. the image repeats infinitely in all 4
C   directions).  SUBGET reads the subimage and pads it up to a power of
C   2 in each axis forcing the reference pixel to end up at the center
C   of the image that will be FFTed.  Blanked pixels are zeroed.
C   Inputs:
C      IDISK    I      Input image disk number.
C      ICNO     I      Input image catalog slot number.
C      CORN     I(7)   BLC in input image (1 & 2 ignored)
C      NOSCR    I      Scratch file number in common /CFILES/ for outpu.
C      NX       I      Dimension of output file in X
C      NY       I      Dimension of output file in Y
C      BUFF1    R(*)   Work buffer
C      BUFF2    R(*)   Work buffer.
C      BUFSZ1   I      Size in AIPS bytes of BUFF1
C      BUFSZ2   I      Size in AIPS bytes of BUFF2
C      LUN1     I      Logical unit number for input file
C      LUN2     I      Logical unit number to use for output
C   Output:
C      IRET     I      Return error code, 0 => OK,
C                       1 = couldn't copy input CATBLK
C                       2 = wrong number of bits/pixel in input map.
C                       3 = input map has inhibit bits.
C                       4 = couldn't open output map file.
C                       5 = couldn't init input map.
C                       6 = couldn't init output map.
C                       7 = read error input map.
C                       8 = write error output map.
C                       9 = error computing block offset
C                       10 = output file too small.
C   Common:
C      /MAPHDR/ CATBLK  is set to the input file CATBLK.
C-----------------------------------------------------------------------
      INTEGER   IDISK, ICNO, CORN(7), NOSCR, NX, NY, BUFSZ1, BUFSZ2,
     *   LUN1, LUN2, IRET
      REAL      BUFF1(*), BUFF2(*)
C
      CHARACTER PHNAME*48, IFILE*48
      INTEGER   IERR, WIN(4), FIND1, FIND2, BIND1, BIND2, BO, RBO, I4,
     *   IFIRST, ILAST, KORN(7), MX, II, IO, MY, IWIN(4), MMX, LX, LY,
     *   MXOFF, MYOFF, IROUND, IX, IY, I
      LOGICAL   T, F
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA RBO /1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
      FIND1 = 0
      FIND2 = 0
C                                       Read input CATBLK
      CALL CATIO ('READ', IDISK, ICNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR, 'READ', 'INPUT CATBLK'
         GO TO 990
         END IF
C                                       Determine mapsize
      LX = CATBLK(KINAX)
      LY = CATBLK(KINAX+1)
      IX = IROUND (CATR(KRCRP))
      IY = IROUND (CATR(KRCRP+1))
      CALL IMGSIZ (CATBLK, CATR, MX, MY, MXOFF, MYOFF, IRET)
      IF ((MX.GT.NX) .OR. (MY.GT.NY)) THEN
         WRITE (MSGTXT,1020) NX, NY, MX, MY
         IRET = 10
         GO TO 990
         END IF
C                                       Check defaults on IWIN
      CALL FILL (4, 0, IWIN)
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = LX
      IWIN(4) = LY
C                                        Determine input window size.
      MMX = IWIN(3) - IWIN(1) + 1
      IF (LX.EQ.MX) MMX = MMX - 1
C                                        Determine first and last
C                                        output rows for read.
      IFIRST = NY / 2 + 2 - IY
      ILAST = IFIRST + (IWIN(4) - IWIN(2))
C                                        Check defaults on CORN
      IERR = 0
      DO 30 I4 = 1,KICTPN
         KORN(I4) = 1
         IF (I4.LE.CATBLK(KIDIM)) THEN
            KORN(I4) = MAX (CORN(I4), 1)
            IF (CATBLK(KINAX+I4-1).LE.1) KORN(I4) = 1
            IF (CATBLK(KINAX+I4-1).LT.KORN(I4)) IERR = 2
            END IF
 30      CONTINUE
C                                       Set input BLOCK offset.
      IF (IERR.EQ.0) THEN
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), KORN(3), BO, IERR)
         BO = BO + 1
         END IF
      IF (IERR.NE.0) THEN
         IRET = 9
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
C                                       Set window for output.
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
C                                       Open output map file.
      CALL ZPHFIL ('SC', SCRVOL(NOSCR), SCRCNO(NOSCR), 1, PHNAME, IERR)
      CALL ZOPEN (LUN2, FIND2, SCRVOL(NOSCR), PHNAME, T, T, F, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1000) IERR, 'OPEN', 'OUTPUT FILE'
         GO TO 990
         END IF
C                                       Open input file.
      CALL ZPHFIL ('MA', IDISK, ICNO, 1, IFILE, IERR)
      CALL ZOPEN (LUN1, FIND1, IDISK, IFILE, T, T, F, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1000) IERR, 'OPEN', 'INPUT FILE'
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       Init files.
      CALL MINIT ('READ', LUN1, FIND1, LX, LY, IWIN, BUFF1, BUFSZ1, BO,
     *   IERR)
      IF (IERR.NE.0) THEN
         IRET = 5
         WRITE (MSGTXT,1000) IERR, 'INIT', 'INPUT FILE'
         GO TO 970
         END IF
      CALL MINIT ('WRIT', LUN2, FIND2, NX, NY, WIN, BUFF2, BUFSZ2, RBO,
     *   IERR)
      IF (IERR.NE.0) THEN
         IRET = 6
         WRITE (MSGTXT,1000) IERR, 'INIT', 'OUTPUT FILE'
         GO TO 970
         END IF
C                                       Finally do what you're here for
      DO 100 I4 = 1,NY
C                                       Write real map.
         CALL MDISK ('WRIT', LUN2, FIND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            IRET = 8
            WRITE (MSGTXT,1090) IERR, I4
            GO TO 970
            END IF
C                                       Zero fill output row.
         CALL RFILL (NX, 0.0, BUFF2(BIND2))
C                                       Check if data for this row.
         IF ((I4.GE.IFIRST) .AND. (I4.LE.ILAST)) THEN
C                                       Read map row.
            CALL MDISK ('READ', LUN1, FIND1, BUFF1, BIND1, IERR)
            IF (IERR.NE.0) THEN
               IRET = 7
               WRITE (MSGTXT,1105) IERR, I4
               GO TO 970
               END IF
C                                       Move to output buffer.
            II = BIND1
            IO = BIND2 + NX/2 + 1 - IX
            DO 50 I = 1,MMX
               IF (BUFF1(II).EQ.FBLANK) THEN
                  BUFF2(IO) = 0.0
               ELSE
                  BUFF2(IO) = BUFF1(II)
                  END IF
               II = II + 1
               IO = IO + 1
 50            CONTINUE
            IF (LX.EQ.MX) THEN
               IO = BIND2 + NX/2 - IX
               IF (BUFF1(II).EQ.FBLANK) THEN
                  BUFF2(IO) = 0.0
               ELSE
                  BUFF2(IO) = BUFF1(II)
                  END IF
               END IF
            END IF
 100     CONTINUE
C                                       Finish write.
      CALL MDISK ('FINI', LUN2, FIND2, BUFF2, BIND2, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         WRITE (MSGTXT,1090) IERR, NY
         GO TO 970
      ELSE
         GO TO 975
         END IF
C                                       Close real map file.
 970  CALL MSGWRT (8)
 975  IF (FIND1.GT.0) CALL ZCLOSE (LUN1, FIND1, IERR)
C                                       Close integer map file.
 980  IF (FIND2.GT.0) CALL ZCLOSE (LUN2, FIND2, IERR)
      GO TO 999
C                                       message only
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUBGET: ERROR',I5,1X,A,'ING ',A)
 1020 FORMAT ('SUBGET: OUTPUT MAP TOO SMALL',2I6,' .LT. ',2I6)
 1030 FORMAT ('SUBGET: ERROR',I3,' COMPUTING BLOCK OFFSET')
 1090 FORMAT ('SUBGET: WRITE ERROR',I3,' ROW ',I5)
 1105 FORMAT ('SUBGET: READ ERROR',I3,' ROW ',I5)
      END
