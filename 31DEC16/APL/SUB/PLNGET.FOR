      SUBROUTINE PLNGET (IDISK, ICNO, CORN, JWIN, XOFF, YOFF, NOSCR,
     *   NX, NY, BUFF1, BUFF2, BUFSZ1, BUFSZ2, LUN1, LUN2, IRET)
C-----------------------------------------------------------------------
C! reads subimage of a plane and writes it to scratch file with shifts
C# Map-util
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
C   PLNGET reads a selected portion of a selected plane parallel to the
C   front and writes it into a specified scratch file.  The output file
C   will be zero padded and a shift of the center may be specified.  If
C   the input window is unspecified (0's) and the output file is smaller
C   than the input file, the NX x NY region about position (MX/2+1-OFFX,
C   MY/2+1-OFFY) in the input map will be used where MX,MY is the size
C   of the input map.  NOTE: If both XOFF and/or YOFF and a window
C   (JWIN) which does not contain the whole map, XOFF and YOFF will
C   still be used to end-around rotate the region inside the window.
C   The image header is taken from the disk catalog AND explicitly will
C   not handle blanked images.   ******
C   Inputs:
C      IDISK    I      Input image disk number.
C      ICNO     I      Input image catalog slot number.
C      CORN     I(7)   BLC in input image (1 & 2 ignored)
C      JWIN     I(4)   Window in plane.
C      XOFF     I      offset in cells in first dimension of the center
C                      from MX/2+1 (MX 1st dim. of input win.)
C      YOFF     I      offset in cells in second dimension of the center
C                      from MY/2+1 (MY 2nd dim. of input win.)
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
C   Programmer: W. D. Cotton, May 1982.
C-----------------------------------------------------------------------
      INTEGER   IDISK, ICNO, CORN(7), JWIN(4), XOFF, YOFF, NOSCR, NX,
     *   NY, BUFSZ1, BUFSZ2, LUN1, LUN2, IRET
      REAL      BUFF1(*), BUFF2(*)
C
      CHARACTER PHNAME*48, IFILE*48
      INTEGER   IERR, WIN(4), FIND1, FIND2, BIND1, BIND2, BO, RBO, I4,
     *   IFIRST, ILAST, IOUT, KORN(7), IADD, INDEX, LIM, IOFF, LIM1, MX,
     *   MY, JOFF1, JOFF2, LIMIT, NUM, OFFX, OFFY, IWIN(4), MMX, MMY
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
      OFFX = XOFF
      OFFY = YOFF
      FIND1 = 0
      FIND2 = 0
C                                       Read input CATBLK
      CALL CATIO ('READ', IDISK, ICNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Make sure there are NO blanks
 20   IF (CATR(KRBLK).EQ.0.0) GO TO 30
         IRET = 3
         WRITE (MSGTXT,1020)
         GO TO 990
C                                       Determine mapsize
 30   MX = CATBLK(KINAX)
      MY = CATBLK(KINAX+1)
C                                       Check defaults on IWIN
      CALL COPY (4, JWIN, IWIN)
      IF ((MX.LE.NX) .OR. ((IWIN(1).NE.0) .AND. (IWIN(3).NE.0)))
     *   GO TO 35
         IWIN(1) = (MX/2+1) - (NX/2) - OFFX
         IWIN(3) = (MX/2+1) + (NX/2-1) - OFFX
         OFFX = 0
 35   IF ((MY.LE.NY) .OR. ((IWIN(2).NE.0).AND.(IWIN(3).NE.0)))
     *   GO TO 40
         IWIN(2) = (MY/2+1) - (NY/2) - OFFY
         IWIN(4) = (MY/2+1) + (NY/2-1) - OFFY
         OFFY = 0
 40   IF (IWIN(1).LE.0) IWIN(1) = 1
      IF (IWIN(2).LE.0) IWIN(2) = 1
      IF ((IWIN(3).LE.0) .OR. (IWIN(3).GT.MX)) IWIN(3) = MX
      IF ((IWIN(4).LE.0) .OR. (IWIN(4).GT.MY)) IWIN(4) = MY
C                                        Determine input window size.
      MMX = IWIN(3) - IWIN(1) + 1
      MMY = IWIN(4) - IWIN(2) + 1
C                                        Determine first and last
C                                        output rows for read.
      IFIRST = ((NY - MMY) / 2.0) + 1.6
      ILAST = IFIRST + (IWIN(4) - IWIN(2))
C                                        Check defaults on CORN
      IERR = 0
      DO 45 I4 = 1,KICTPN
         KORN(I4) = 1
         IF (I4.LE.CATBLK(KIDIM)) THEN
            KORN(I4) = MAX (CORN(I4), 1)
            IF (CATBLK(KINAX+I4-1).LE.1) KORN(I4) = 1
            IF (CATBLK(KINAX+I4-1).LT.KORN(I4)) IERR = 2
            END IF
 45      CONTINUE
C                                       Set input BLOCK offset.
      IF (IERR.EQ.0) THEN
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), KORN(3), BO, IERR)
         BO = BO + 1
         END IF
      IF (IERR.EQ.0) GO TO 50
         IRET = 9
         WRITE (MSGTXT,1045) IERR
         GO TO 990
C                                       Set window for output.
 50   WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
C                                        Make sure input window .le.
C                                        output file size.
      IF ((NX.GE.MMX) .AND. (NY.GE.MMY)) GO TO 55
         IRET = 10
         WRITE (MSGTXT,1050) NX, NY, MMX, MMY
         GO TO 990
 55   INDEX = - OFFY
      IF (INDEX.LT.0) INDEX = MMY - OFFY
      WIN(2) = INDEX + 1
C                                       If OFFX positive
      LIM = OFFX
      JOFF1 = MMX - OFFX - 1
      JOFF2 = - OFFX - 1
      IADD = (NX - MMX) / 2.0 + 0.6
C                                       If OFFX .LE. 0
      IF ((OFFX.GT.0)) GO TO 60
         LIM = MMX + OFFX
         JOFF1 = - OFFX - 1
         JOFF2 = - MMX - OFFX - 1
 60   JOFF1 = JOFF1 + IADD
      JOFF2 = JOFF2 + IADD
      LIMIT = MMX
      LIM = MIN (LIM, LIMIT)
      LIM1 = LIM + 1
C                                       Open output map file.
      CALL ZPHFIL ('SC', SCRVOL(NOSCR), SCRCNO(NOSCR), 1, PHNAME, IERR)
      CALL ZOPEN (LUN2, FIND2, SCRVOL(NOSCR), PHNAME, T, T, F, IERR)
      IF (IERR.EQ.0) GO TO 65
         IRET = 4
         WRITE (MSGTXT,1060) IERR
         GO TO 990
C                                       Open input file.
 65   CALL ZPHFIL ('MA', IDISK, ICNO, 1, IFILE, IERR)
      CALL ZOPEN (LUN1, FIND1, IDISK, IFILE, T, T, F, IERR)
      IF (IERR.EQ.0) GO TO 70
         IRET = 4
         WRITE (MSGTXT,1065) IERR
         CALL MSGWRT (8)
         GO TO 980
C                                       Init files.
 70   CALL MINIT ('READ', LUN1, FIND1, MX, MY, IWIN, BUFF1, BUFSZ1, BO,
     *   IERR)
      IF (IERR.EQ.0) GO TO 80
         IRET = 5
         WRITE (MSGTXT,1070) IERR
         GO TO 970
 80   CALL MINIT ('WRIT', LUN2, FIND2, NX, NY, WIN, BUFF2, BUFSZ2, RBO,
     *   IERR)
      IOUT = WIN(2) - 1
      IF (IERR.EQ.0) GO TO 90
         IRET = 6
         WRITE (MSGTXT,1080) IERR
         GO TO 970
C                                       Finally do what you're here for
 90   DO 200 I4 = 1,NY
         IOUT = IOUT + 1
C                                       Restart at first of output file
C                                       Finish write.
         IF (IOUT.LE.NY) GO TO 100
            CALL MDISK ('FINI', LUN2, FIND2, BUFF2, BIND2, IERR)
            IF (IERR.EQ.0) GO TO 95
               IRET = 8
               WRITE (MSGTXT,1090) IERR, NY
               GO TO 970
 95         INDEX = 1
            WIN(4) = WIN(2) - 1
            WIN(2) = 1
            CALL MINIT ('WRIT', LUN2, FIND2, NX, NY, WIN, BUFF2, BUFSZ2,
     *         RBO, IERR)
            IOUT = WIN(2)
            IF (IERR.EQ.0) GO TO 100
               IRET = 6
               WRITE (MSGTXT,1080) IERR
               GO TO 970
C                                       Write real map.
 100     CALL MDISK ('WRIT', LUN2, FIND2, BUFF2, BIND2, IERR)
         IF (IERR.EQ.0) GO TO 105
            IRET = 8
            WRITE (MSGTXT,1090) IERR, I4
            GO TO 970
C                                       Zero fill output row.
 105     CALL RFILL (NX, 0.0, BUFF2(BIND2))
C                                       Check if data for this row.
         IF ((IOUT.GE.IFIRST) .AND. (IOUT.LE.ILAST)) THEN
C                                       Read map row.
            CALL MDISK ('READ', LUN1, FIND1, BUFF1, BIND1, IERR)
            IF (IERR.EQ.0) GO TO 110
               IRET = 7
               WRITE (MSGTXT,1105) IERR, I4
               GO TO 970
C                                       Move to output buffer.
 110        IOFF = BIND2 + JOFF1
            CALL RCOPY (LIM, BUFF1(BIND1), BUFF2(IOFF+1))
 130        IF (LIM.LT.LIMIT) THEN
               IOFF = BIND2 + JOFF2
               NUM = LIMIT - LIM1 + 1
               CALL RCOPY (NUM, BUFF1(BIND1+LIM1-1), BUFF2(IOFF+LIM1))
               END IF
            END IF
 200     CONTINUE
C                                       Finish write.
      CALL MDISK ('FINI', LUN2, FIND2, BUFF2, BIND2, IERR)
      IF (IERR.EQ.0) GO TO 975
         IRET = 8
         WRITE (MSGTXT,1090) IERR, NY
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
 1000 FORMAT ('PLNGET: ERROR',I3,' READING INPUT CATBLK')
 1010 FORMAT ('PLNGET: WRONG NUMBER OF BITS/PIXEL =',I5,' USE CNVRT')
 1020 FORMAT ('PLNGET: MAP IS BLANKED AND I AM NOT ALLOWED TO HANDLE',
     *   ' IT')
 1045 FORMAT ('PLNGET: ERROR',I3,' COMPUTING BLOCK OFFSET')
 1050 FORMAT ('PLNGET: OUTPUT MAP TOO SMALL',2I6,' .LT. ',2I6)
 1060 FORMAT ('PLNGET: ERROR',I3,' OPENING OUTPUT FILE')
 1065 FORMAT ('PLNGET: ERROR',I3,' OPENING INPUT FILE')
 1070 FORMAT ('PLNGET: CANNOT INIT INPUT MAP, ERROR',I3)
 1080 FORMAT ('PLNGET: CANNOT INIT OUTPUT MAP, ERROR',I3)
 1090 FORMAT ('PLNGET: WRITE ERROR',I3,' ROW ',I5)
 1105 FORMAT ('PLNGET: READ ERROR',I3,' ROW ',I5)
      END
