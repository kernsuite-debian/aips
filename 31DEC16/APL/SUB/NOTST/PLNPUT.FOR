      SUBROUTINE PLNPUT (IDISK, ICNO, CORN, JWIN, NOSCR, NX, NY, BUFF1,
     *   BUFF2, BUFSZ1, BUFSZ2, LUN1, LUN2, IRET)
C-----------------------------------------------------------------------
C! Copies a subregion of a scratch file image to a cataloged image.
C# Map IO-appl
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
C   PLNPUT writes a subregion of a  scratch file image into a cataloged
C   image.
C   Input:
C      IDISK    I      Output image disk number.
C      ICNO     I      Output image catalog slot number.
C      CORN     I(7)   BLC in Output image (1 & 2 ignored)
C      JWIN     I(4)   Window in plane in input image.
C      NOSCR    I      Scratch file number in common /CFILES/ for
C                      input scratch file.
C      NX       I      X-dimension of input file.
C      NY       I      Y-dimension of input file.
C      BUFF1    R(*)   Work buffer
C      BUFF2    R(*)   Work buffer.
C      BUFSZ1   I      Size in bytes of BUFF1.
C      BUFSZ2   I      Size in bytes of BUFF2
C      LUN1     I      Logical unit number to use.
C      LUN2     I      Second loical unit number to use.
C   Output:
C      IRET     I      Return error code: 0 => OK
C                         1 = couldn't read output CATBLK.
C                         2 = Output bits/pixel not allowed.
C                         3 = Output and input windows not same.
C                         4 = couldn't open input map file.
C                         5 = couldn't init output map.
C                         6 = couldn't init input map.
C                         7 = read error input map.
C                         8 = write error output map.
C                         9 = error writing header to catalog
C                        10 = error computing block offset.
C   Commons:
C      CATBLK in /MAPHDR/ is used as the map header.
C             Of particular importance is the data max/min values
C             which must apply to the map.  As this is read from the
C             catalog it must be updated by a call to CATIO etc.
C             before calling this routine.
C-----------------------------------------------------------------------
      INTEGER   IDISK, ICNO, CORN(7), JWIN(4), NOSCR, NX, NY, BUFSZ1,
     *   BUFSZ2, LUN1, LUN2, IRET
      REAL      BUFF1(*), BUFF2(*)
C
      CHARACTER OFILE(6)*48
      INTEGER   IERR, WIN(4), OBO, I4, IWIN(4), KORN(7), FIND1,
     *   FIND2, BIND1, BIND2, MX, MY, NNX, NNY, RBO, I, BOTEMP
      LOGICAL   T, F
      REAL      XMAX, XMIN
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA OBO, RBO /1,1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Read output CATBLK.
      CALL CATIO ('READ', IDISK, ICNO, CATBLK, 'REST', BUFF2, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.3)) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Determine max, min and mapsize.
      XMAX = CATR(KRDMX)
      XMIN = CATR(KRDMN)
      MX = CATBLK(KINAX)
      MY = CATBLK(KINAX+1)
C                                       Set window for maps.
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = MX
      WIN(4) = MY
C                                        Check defaults on IWIN
      CALL COPY (4, JWIN, IWIN)
      IF (IWIN(1).LE.0) IWIN(1) = 1
      IF (IWIN(2).LE.0) IWIN(2) = 1
      IF ((IWIN(3).LE.0) .OR. (IWIN(3).GT.(MX+IWIN(1)-1)))
     *   IWIN(3) = MX + IWIN(1) - 1
      IF ((IWIN(4).LE.0) .OR. (IWIN(4).GT.(MY+IWIN(2)-1)))
     *   IWIN(4) = MY + IWIN(2) - 1
C                                        Check defaults on CORN
      DO 30 I4 = 1,KICTPN
         KORN(I4) = MAX (CORN(I4), 1)
 30      CONTINUE
C                                       Set output BLOCK offset.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), KORN(3), BOTEMP, IERR)
      OBO = BOTEMP + 1
      IF (IERR.NE.0) THEN
         IRET = 10
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                        Check that input and output
C                                        windows are the same size.
 35   NNX = IWIN(3) - IWIN(1) + 1
      NNY = IWIN(4) - IWIN(2) + 1
      IF ((NNX.EQ.MX) .AND. (NNY.EQ.MY)) GO TO 40
         IRET = 3
         WRITE (MSGTXT,1040) NNX, NNY, MX, MY
         CALL MSGWRT (8)
         GO TO 980
C                                       Open output file.
 40   CALL ZPHFIL ('MA', IDISK, ICNO, 1, OFILE, IERR)
      CALL ZOPEN (LUN2, FIND2, IDISK, OFILE, T, T, F, IERR)
      IF (IERR.EQ.0) GO TO 60
         IRET = 4
         WRITE (MSGTXT,1065) IERR
         CALL MSGWRT (8)
         GO TO 980
C                                       Open input map file.
 60   CALL ZPHFIL ('SC', SCRVOL(NOSCR), SCRCNO(NOSCR), 1, OFILE, IERR)
      CALL ZOPEN (LUN1, FIND1, SCRVOL(NOSCR), OFILE, T, F, F, IERR)
      IF (IERR.EQ.0) GO TO 70
         IRET = 4
         WRITE (MSGTXT,1060) IERR
         CALL MSGWRT (8)
         GO TO 980
C                                       Init files.
 70   CALL MINIT ('READ', LUN1, FIND1, NX, NY, IWIN, BUFF1, BUFSZ1, RBO,
     *   IERR)
      IF (IERR.EQ.0) GO TO 80
         IRET = 5
         WRITE (MSGTXT,1070) IERR
         CALL MSGWRT (8)
         GO TO 970
 80   CALL MINIT ('WRIT', LUN2, FIND2, MX, MY, WIN, BUFF2, BUFSZ2, OBO,
     *   IERR)
      IF (IERR.EQ.0) GO TO 100
         IRET = 6
         WRITE (MSGTXT,1080) IERR
         CALL MSGWRT (8)
         GO TO 970
C                                       Finally do what you're here for
 100  DO 200 I = 1,MY
C                                       Read map row.
         CALL MDISK ('READ', LUN1, FIND1, BUFF1, BIND1, IERR)
         IF (IERR.NE.0) THEN
            IRET = 7
            WRITE (MSGTXT,1100) IERR,I
            CALL MSGWRT (8)
            GO TO 970
            END IF
C                                       Write output map.
         CALL MDISK ('WRIT', LUN2, FIND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            IRET = 8
            WRITE (MSGTXT,1110) IERR,I
            CALL MSGWRT (8)
            GO TO 970
            END IF
C                                       Move to output buffer.
         CALL RCOPY (MX, BUFF1(BIND1), BUFF2(BIND2))
 200     CONTINUE
C                                       Finish write.
      CALL MDISK ('FINI', LUN2, FIND2, BUFF2, BIND2, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         WRITE (MSGTXT,1110) IERR,MY
         CALL MSGWRT (6)
         END IF
C                                       Close real map file.
 970  CALL ZCLOSE (LUN1, FIND1, IERR)
C                                       Update catlg of output map
 980  CALL CATIO ('UPDT', IDISK, ICNO, CATBLK, 'REST', BUFF2, IERR)
      IF (IERR.NE.0) THEN
         IRET = 9
         WRITE (MSGTXT,1980) IERR
         CALL MSGWRT (8)
         END IF
C                                       Close it
 990  CALL ZCLOSE (LUN2, FIND2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PLNPUT: ERROR',I3,' READING OUTPUT CATBLK')
 1030 FORMAT ('PLNPUT: ERROR',I3,' COMPUTING BLOCK OFFSET')
 1040 FORMAT ('PLNPUT: INPUT WINDOW SIZE=',2I5,' OUTPUT=',2I5)
 1060 FORMAT ('PLNPUT: CANNOT OPEN INPUT MAP FILE, ERROR',I3)
 1065 FORMAT ('PLNPUT: ERROR',I3,' OPENING INPUT FILE')
 1070 FORMAT ('PLNPUT: CANNOT INIT INPUT MAP, ERROR',I3)
 1080 FORMAT ('PLNPUT: CANNOT INIT OUTPUT MAP, ERROR',I3)
 1100 FORMAT ('PLNPUT: READ ERROR',I3,' ROW ',I5)
 1110 FORMAT ('PLNPUT: WRITE ERROR',I3,' ROW ',I5)
 1980 FORMAT ('PLNPUT: ERROR',I3,' UPDATING CATALOGED MAP HEADER')
      END
