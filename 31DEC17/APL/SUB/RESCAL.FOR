      SUBROUTINE RESCAL (SCALE, OFFSET, DISK, CNO, LUN1, LUN2,
     *   BUFF1, BUFSZ1, BUFF2, BUFSZ2, IRET)
C-----------------------------------------------------------------------
C! Scales and offsets a cataloged image, updates CATBLK
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   RESCAL Scales an image, updates the CATBLK on disk and leaves the
C   CATBLK in common /MAPHDR/.
C   Input:
C      SCALE       D     Scaling factor
C      OFFSET      D     Offset (new = old * SCALE + OFFSET
C      DISK        I     Output image disk number.
C      CNO         I     Output image catalog slot number.
C      LUN1, LUN2  I     Log. unit numbers to use.
C      BUFSZ1      I     Size in bytes of BUFF1
C      BUFSZ2      I     Size in bytes of BUFF2
C   Output:
C      BUFF1       R(*)  Work buffer
C      BUFF2       R(*)  Work buffer.
C      IRET        I     Return error code: 0 => okay
C                           1 = couldn't read output CATBLK.
C                           3 = couldn't create temporary file
C                           4 = couldn't open input map file.
C                           5 = couldn't init/read input map.
C                           6 = couldn't init/write output map.
C                           7 = couldn't destroy old file
C                           8 = error writing header to catalog
C                           9 = rename fails: image is LOST!!!!
C   Commons:
C      /MAPHDR/ CATBLK is read from disk, modified in Max/Min, scaling,
C                      and blanked indicators and put back on disk
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, LUN1, LUN2, BUFSZ1, BUFSZ2, IRET
      DOUBLE PRECISION SCALE, OFFSET
      REAL      BUFF1(*), BUFF2(*)
C
      INTEGER   IERR, WIN(4), BO, NX, NY, FIND1, FIND2, BIND1, BIND2,
     *   I1, I2, I3, I4, I5, I6, I7, LIM3, LIM4, LIM5, LIM6, LIM7,
     *   KORN(7), ISIZE, LSIZE, LOCS(2), KEYTYP(2)
      LOGICAL   T, F, WASBLK
      CHARACTER V1FILE*48, V2FILE*48, HDRKEY(2)*8
      REAL      XMAX, XMIN, TEMP, HDRVAL(4)
      DOUBLE PRECISION HSCALE, HOFFST
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA HDRKEY /'ISCALE  ','IZERO   '/
C-----------------------------------------------------------------------
      IRET = 0
      FIND1 = 0
      FIND2 = 0
C                                       Read CATBLK.
      CALL CATIO ('READ', DISK, CNO, CATBLK, 'REST', BUFF2, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.3)) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Image size
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
C                                       Set window for maps.
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
C                                       create output file
      CALL ZPHFIL ('MA', DISK, CNO, 2, V2FILE, IERR)
      CALL ZDESTR (DISK, V2FILE, IERR)
      IF (IERR.GT.1) THEN
         IRET = 3
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      CALL MAPSIZ (CATBLK(KIDIM), CATBLK(KINAX), ISIZE)
      CALL ZCREAT (DISK, V2FILE, ISIZE, .TRUE., LSIZE, IERR)
      IF (IERR.NE.0) THEN
         IRET = 3
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Open output file.
      CALL ZOPEN (LUN2, FIND2, DISK, V2FILE, T, F, F, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1030) IERR
         FIND2 = 0
         GO TO 990
         END IF
C                                       Open input map file.
      CALL ZPHFIL ('MA', DISK, CNO, 1, V1FILE, IERR)
      CALL ZOPEN (LUN1, FIND1, DISK, V1FILE, T, F, F, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1035) IERR
         FIND1 = 0
         GO TO 990
         END IF
C                                       Loop over planes
      I6 = CATBLK(KIDIM)
      I7 = 7 - I6
      IF (I7.GT.0) CALL FILL (I7, 1, CATBLK(KINAX+I6))
      LIM3 = MAX (1, CATBLK(KINAX+2))
      LIM4 = MAX (1, CATBLK(KINAX+3))
      LIM5 = MAX (1, CATBLK(KINAX+4))
      LIM6 = MAX (1, CATBLK(KINAX+5))
      LIM7 = MAX (1, CATBLK(KINAX+6))
      KORN(1) = 1
      KORN(2) = 1
      WASBLK = .FALSE.
      XMAX = -1.E20
      XMIN = -XMAX
      DO 500 I7 = 1,LIM7
         KORN(7) = I7
         DO 500 I6 = 1,LIM6
            KORN(6) = I6
            DO 500 I5 = 1,LIM5
               KORN(5) = I5
               DO 500 I4 = 1,LIM4
                  KORN(4) = I4
                  DO 500 I3 = 1,LIM3
      KORN(3) = I3
C                                       Set BLOCK offset.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), KORN(3), BO, IERR)
      BO = BO + 1
      IF (IERR.NE.0) THEN
         IRET = 5
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
C                                       Init files.
      CALL MINIT ('READ', LUN1, FIND1, NX, NY, WIN, BUFF1, BUFSZ1, BO,
     *   IERR)
      IF (IERR.NE.0) THEN
         IRET = 5
         WRITE (MSGTXT,1070) IERR
         GO TO 990
         END IF
      CALL MINIT ('WRIT', LUN2, FIND2, NX, NY, WIN, BUFF2, BUFSZ2, BO,
     *   IERR)
      IF (IERR.NE.0) THEN
         IRET = 6
         WRITE (MSGTXT,1080) IERR
         GO TO 990
         END IF
C                                       Finally do what you're here for
      DO 200 I2 = 1,NY
C                                       Read map row.
         CALL MDISK ('READ', LUN1, FIND1, BUFF1, BIND1, IERR)
         IF (IERR.NE.0) THEN
            IRET = 5
            WRITE (MSGTXT,1100) IERR, I2
            GO TO 990
            END IF
C                                       Write output map.
         CALL MDISK ('WRIT', LUN2, FIND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            IRET = 6
            WRITE (MSGTXT,1110) IERR, I2
            GO TO 990
            END IF
C                                       Scale
         DO 150 I1 = 1,NX
C                                       Blanked?
            IF (BUFF1(BIND1+I1-1).EQ.FBLANK) THEN
               BUFF2(BIND2+I1-1) = FBLANK
               WASBLK = .TRUE.
            ELSE
               TEMP = BUFF1(BIND1+I1-1) * SCALE + OFFSET
               BUFF2(BIND2+I1-1) = TEMP
               XMAX = MAX (XMAX, TEMP)
               XMIN = MIN (XMIN, TEMP)
               END IF
 150        CONTINUE
 200     CONTINUE
C                                       Finish write.
      CALL MDISK ('FINI', LUN2, FIND2, BUFF2, BIND2, IERR)
      IF (IERR.NE.0) THEN
         IRET = 6
         WRITE (MSGTXT,1110) IERR, NY
         CALL MSGWRT (6)
         END IF
C                                       End loop
 500                 CONTINUE
C                                       Close files
      CALL ZCLOSE (LUN1, FIND1, IERR)
      CALL ZCLOSE (LUN2, FIND2, IERR)
      FIND1 = 0
      FIND2 = 0
C                                       Now the scary part
C                                       destroy input
      CALL ZDESTR (DISK, V1FILE, IERR)
      IF (IERR.NE.0) THEN
         IRET = 7
         WRITE (MSGTXT,1500) IERR
         GO TO 990
         END IF
C                                       rename output to input
      CALL ZRENAM (DISK, V2FILE, V1FILE, IERR)
      IF (IERR.NE.0) THEN
         IRET = 9
         WRITE (MSGTXT,1510) IERR
         GO TO 990
         END IF
C                                       Update catlg of output map
      CATR(KRDMX) = XMAX
      CATR(KRDMN) = XMIN
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
      CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFF2, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         WRITE (MSGTXT,1980) IERR
         CALL MSGWRT (8)
         END IF
C                                       header extension portion
      HSCALE = 1.0D0
      HOFFST = 0.0D0
      CALL CATKEY ('REED', DISK, CNO, HDRKEY, 2, LOCS, HDRVAL, KEYTYP,
     *   BUFF1, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LE.20)) THEN
         IRET = 8
         WRITE (MSGTXT,1981) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IF (LOCS(1).GT.0) CALL RCOPY (NWDPDP, HDRVAL(LOCS(1)), HSCALE)
      IF (LOCS(2).GT.0) CALL RCOPY (NWDPDP, HDRVAL(LOCS(2)), HOFFST)
      LOCS(1) = 1
      LOCS(2) = 1 + NWDPDP
      KEYTYP(1) = 1
      KEYTYP(2) = 1
      HSCALE = SCALE * HSCALE
      HOFFST = OFFSET + HOFFST * SCALE
      CALL RCOPY (NWDPDP, HSCALE, HDRVAL(LOCS(1)))
      CALL RCOPY (NWDPDP, HOFFST, HDRVAL(LOCS(2)))
      CALL CATKEY ('WRIT', DISK, CNO, HDRKEY, 2, LOCS, HDRVAL, KEYTYP,
     *   BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         WRITE (MSGTXT,1982) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IF (FIND1.GT.0) CALL ZCLOSE (LUN1, FIND1, IERR)
      IF (FIND2.GT.0) CALL ZCLOSE (LUN2, FIND2, IERR)
      CALL ZDESTR (DISK, V2FILE, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RESCAL: ERROR',I3,' READING CATBLK')
 1010 FORMAT ('RESCAL: CAN''T DESTROY OLD TEMPORARY FILE, ERROR',I5)
 1020 FORMAT ('RESCAL: CAN''T CREATE TEMPORARY FILE, ERROR',I5)
 1030 FORMAT ('RESCAL: ERROR',I3,' OPENING TEMPORARY FILE')
 1035 FORMAT ('RESCAL: CANNOT OPEN INPUT FILE, ERROR',I3)
 1050 FORMAT ('RESCAL: ERROR',I3,' COMPUTING BLOCK OFFSET')
 1070 FORMAT ('RESCAL: CAN''T INIT INPUT FILE, ERROR',I3)
 1080 FORMAT ('RESCAL: CAN''T INIT TEMPORARY FILE, ERROR',I3)
 1100 FORMAT ('RESCAL: READ ERROR',I3,' ROW ',I5)
 1110 FORMAT ('RESCAL: WRITE ERROR',I3,' ROW ',I5)
 1500 FORMAT ('RESCAL: CAN''T DESTROY INPUT FILE, ERROR',I5)
 1510 FORMAT ('RESCAL: CAN''T RENAME RESCALED FILE: IMAGE IS LOST!,',
     *   ' ERROR',I5)
 1980 FORMAT ('RESCAL: ERROR',I3,' UPDATING CATALOGED MAP HEADER')
 1981 FORMAT ('RESCAL: ERROR',I3,' READING MAP HEADER KEYWORDS')
 1982 FORMAT ('RESCAL: ERROR',I3,' WRITING MAP HEADER KEYWORDS')
      END
