LOCAL INCLUDE 'SHADO.INC'
C                                       Local include for SHADO
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   NMAX
      PARAMETER (NMAX = 2000)
      INTEGER   NUMHA, NUMDEL
C
      INTEGER  NANT
      LOGICAL  DOLATX
      HOLLERITH XINFIL(12), XOFILE(12)
      CHARACTER INFILE*48, OFILE*48
      REAL  APARM(10), BUFFER(UVBFSS), HASTEP, DECMIN, DESTEP
      DOUBLE PRECISION SITELA, SITELO, DELTA, HMIN, ELMIN,
     *   BLKMAX, BLKREJ, FREQNM, LX(NMAX), LY(NMAX), LZ(NMAX),
     *   DIAM(NMAX), XCENT, YCENT, ZCENT
      COMMON /BUFRS/ BUFFER
      COMMON /INPARM/ XINFIL, XOFILE, APARM
      COMMON /CHRCOM/ INFILE, OFILE
      COMMON /FRED/ XCENT, YCENT, ZCENT, SITELA, SITELO, DELTA, HMIN,
     *   ELMIN, BLKMAX, LX, LY, LZ, DIAM, BLKREJ, FREQNM, NANT,
     *   DOLATX, NUMHA, NUMDEL, DECMIN, DESTEP, HASTEP
LOCAL END
      PROGRAM SHADO
C-----------------------------------------------------------------------
C! Caculates percentage of shadowed antennas at the array
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2003-2004, 2012
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
C   SHADO creates output file as a table of the sensitivity loosing
C   because of shadowing. The table can be created at LATEX format, at
C   text format or as a print out at the display
C   (for the table columns > 11)
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         INFILE        Input file name. (antenna info.)
C      OUTFILE        OFILE         Output file name
C      APARM(10)      APARM         User specified array.
C                                   1: Min source declin., deg
C                                   2: Number of declinations
C                                   3: Step at declination, deg
C                                   4: Min hour angle, hours
C                                   5: Number of hour angles
C                                   6: Step at hour angle, hours
C                                   7: Min antenna elevation, deg
C                                   8: Max blockage allowed
C                                   9: LATEX/TEXT format of OUTFILE
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET, IDEL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SHADO.INC'
      DATA PRGM /'SHADO '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVFILN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      DO 10 IDEL = 1, NUMDEL
         DELTA = DECMIN + (NUMDEL-IDEL)*DESTEP
         CALL GETUV (IDEL, IRET)
         IF (IRET.NE.0) GO TO 990
   10    CONTINUE
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE UVFILN (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVFILN gets input parameters for SHADO
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                4 => error creating output file.
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER  JERR, IERR, NPARM
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SHADO.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JERR = 0
C                                       Get input parameters.
      NPARM = 34
      CALL GTPARM (PRGN, NPARM, RQUICK, XINFIL, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Crunch input parameters.
C                                       Convert characters
 10   CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (48, 1, XOFILE, OFILE)
C                                       Get other inputs
      CALL GETINP (IERR)
      IF (IERR.NE.0) JERR = IERR
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVFILN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
      END
      SUBROUTINE GETUV (IDEL, IRET)
C-----------------------------------------------------------------------
C   GETUV calculates the percentages of the shadowed antennas and
C   records its to the table at the LATEX or TEXT format for the given
C   set of declinations and hour angles.
C
C   INPUT:  IDEL   I    Declination
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IDEL
      INTEGER   IRET, IH, LUNPR, PFIND, NCH, JTRIM
      LOGICAL   FA
      INCLUDE 'SHADO.INC'
C                                       Local variables
      DOUBLE PRECISION H, AZ, EL(NMAX), PARANG, FI, FJ,
     *   UANT(NMAX), VANT(NMAX), WANT(NMAX), HANT,
     *   ANTLON, ANTLAT, XANT, HGRWCH,
     *   YANT, ZANT, XYANT, ANBMAX(NMAX)
      INTEGER   I, J
C
      REAL      ETA, ETAEFF(100), HOURAN(100)
      CHARACTER LINE*80
      CHARACTER CH(100)
      INTEGER   NANUSE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      DO 100 IH = 1, NUMHA
         CH(IH) = 'c'
         H = HMIN + (IH - 1) * HASTEP
         HOURAN(IH) = H * 24/TWOPI
C                                       H is hour angle at the array
C                                       center
         HGRWCH = H + SITELO
C                                       HGRWCH is hour angle at
C                                       GREENWICH
         DO 10 I = 1, NANT
C                                       LX, LY, LZ, XCENT, YCENT, ZCENT
C                                       are in RH coordinate system
            XANT = XCENT + LX(I)
            YANT = YCENT + LY(I)
            ZANT = ZCENT + LZ(I)
            XYANT = SQRT(XANT*XANT + YANT*YANT)
C                                       Longitute of the antenna
C                                       (positive to west)
            ANTLON = ATAN2(-YANT, XANT)
            ANTLAT = ATAN2(ZANT, XYANT)
C                                       HANT is hour angle at the
C                                       given antenna
            HANT = H + ANTLON - SITELO
            CALL GETANG (HANT, DELTA, ANTLAT, AZ, EL(I), PARANG)
   10       CONTINUE
C
         CALL UVANT (HGRWCH, DELTA, NANT, LX, LY, LZ, UANT, VANT, WANT)
C                                       find the shadowed antennas
         DO 30 I = 1,NANT
            ANBMAX(I) = 0
            DO 20 J = 1,NANT
C                                       find the maximum shadoing
C                                       of antenna I by any others
               IF (I .EQ. J) GO TO 20
               CALL BLOCK (I, J, UANT, VANT, WANT, DIAM, FI, FJ)
C                                       FI, FJ are the fractional
C                                       blockage of antenna I,J
               ANBMAX(I) = MAX(ANBMAX(I), FI)
   20          CONTINUE
   30       CONTINUE
C
         ETA = 0
         NANUSE = 0
         DO 40 I = 1, NANT
            IF (EL(I) .GE. ELMIN) THEN
               NANUSE = NANUSE + 1
C                                       calculate rejected antennas
C                                       on the basis of blockage
               IF(ANBMAX(I) .GT. BLKREJ) THEN
                  ETA = ETA + 1
                  END IF
               END IF
   40       CONTINUE
          IF (NANUSE.EQ.0) THEN
             ETAEFF(IH) = FBLANK
          ELSE
             ETAEFF(IH) = 1-ETA / NANUSE
             END IF
 100     CONTINUE
C                                       print the line of the
C                                       sensitifity lost due to
C                                       shadowing at the LATEX format
      IF (OFILE(1:1) .NE. ' ') THEN
         LUNPR = 10
         FA = .FALSE.
         IF (IDEL .EQ. 1) THEN
C                                       open the output file only if
C                                       NUMHA .LE. 11
            IF (NUMHA .LE. 11) THEN
               CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, FA, IRET)
            ELSE
               WRITE (MSGTXT, 1000) NUMHA
               CALL MSGWRT (8)
               GO TO 990
               END IF
C
            IF (DOLATX) THEN
C
               LINE = '\\documentclass{article}'
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
               LINE = '\\begin{document}'
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
               LINE = '\\begin{table}[h]'
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
               LINE = '\\begin{center}'
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
               LINE = '\\caption{Sensitivity Considering Shadowing.}'
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
               LINE = '\\vspace{5mm}'
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
               WRITE (LINE,1100) (CH(IH), IH = 1,NUMHA)
               NCH = JTRIM(LINE)
               WRITE (LINE(NCH+1:80),1200)
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
C               LINE = '& \\multicolumn{9}{|c|}{Hour Angle, hours} \\\\'
               WRITE (LINE,1400) NUMHA
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
               LINE = 'Decl., deg'
               DO 120 IH = 1,NUMHA
                  WRITE (LINE(5*(IH-1)+11:80),1600) HOURAN(IH)
  120             CONTINUE
C
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
               LINE = '\\\\ \\hline'
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            ELSE
               WRITE (LINE,1800) (HOURAN(IH), IH = 1,NUMHA)
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               END IF
C
            END IF
C
         IF (NUMHA .GT. 11) GO TO 990
C
         IF (DOLATX) THEN
            WRITE (LINE, 2000) DELTA/DG2RAD
            DO 160 IH = 1,NUMHA
               WRITE (LINE(6*(IH-1)+5:80),2200) ETAEFF(IH)
  160          CONTINUE
            LINE(6*NUMHA+5:80) = ' \\\\'
            NCH = JTRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
         ELSE
            WRITE (LINE, 2400) DELTA/DG2RAD, (ETAEFF(IH), IH = 1
     *         ,NUMHA)
            NCH = JTRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            END IF
C                                       close the file
         IF (IDEL.EQ.NUMDEL) THEN
            IF (DOLATX) THEN
C
               LINE = ' \\hline'
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
               LINE = '\\end{tabular}'
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
               LINE = '\\end{center}'
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
               LINE = '\\end{table}'
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C
               LINE = '\\end{document}'
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               END IF
C                                       close the output file
            CALL ZTXCLS (LUNPR, PFIND, IRET)
C
            END IF
         END IF
C
  990 CONTINUE
      IF (IDEL .EQ. 1) THEN
         PRINT 2600, (HOURAN(IH), IH = 1,NUMHA)
         END IF
      PRINT 2800, DELTA/DG2RAD, (ETAEFF(IH), IH = 1,NUMHA)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('An output file is not created because NUMHA=',I3, '>11')
 1100 FORMAT ('\\begin{tabular}{|c|', 11(1X,A1))
 1200 FORMAT ('|} \\hline')
 1400 FORMAT ('& \\multicolumn{',I2,'}{|c|}{Hour Angle, hours} \\\\')
 1600 FORMAT ('&',F4.0)
 1800 FORMAT ('Decl., deg', 11(1X, F4.0, 1X))
 2000 FORMAT (F4.0, 1X)
 2200 FORMAT ('&', F5.2)
 2400 FORMAT (3X, F4.0, 3X, 11( F6.2))
 2600 FORMAT ('Decl., deg', 21(1X, F4.0, 1X))
 2800 FORMAT (3X, F4.0, 3X, 21( F6.2))
      END
      SUBROUTINE GETINP (IERR)
C-----------------------------------------------------------------------
C   Routine to look for information from the input adverbs and input file
C   Input from common:  INFILE  C*48 File name.
C   Output:             IERR                I    Return code 0=>OK
C-----------------------------------------------------------------------
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'SHADO.INC'
C-----------------------------------------------------------------------
      NANT = 0
C                                       Read antenna info from file
      CALL GETANT (INFILE, IERR)
      IF (IERR.NE.0) GO TO 999
      DECMIN = APARM(1)
      IF (DECMIN.EQ.0) DECMIN = -30
      DECMIN = DECMIN * DG2RAD
C                                       Min declination in radians
      NUMDEL = APARM(2)
      IF (NUMDEL.EQ.0) NUMDEL = 12
C
      DESTEP = APARM(3)
      IF (DESTEP.EQ.0) DESTEP = 10
      DESTEP = DESTEP * DG2RAD
C                                       Step in declination in radians
      HMIN =  APARM(4)
      IF (HMIN.EQ.0) HMIN=-4
      HMIN =  HMIN * TWOPI / 24.0D0
C                                       Min hour angle in radians
      DOLATX = APARM(9) .EQ. 0
C                                       output file format is latex?
      NUMHA = APARM(5)
      IF (NUMHA.EQ.0) NUMHA = 9
C                                       Number of hour angles
      HASTEP =  APARM(6)
      IF (HASTEP.EQ.0) HASTEP = 1
      HASTEP = HASTEP * TWOPI / 24.0D0
C                                       Step in hour angle in radians
      ELMIN = APARM(7) * DG2RAD
C                                       Min elevation in radians
      BLKREJ = APARM(8)
C                                       Allowed plockage in times
C
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE GETANT (FILE, IERR)
C-----------------------------------------------------------------------
C  This subroutine reads, from an input file specified by name "file",
C  the array information.
C  The first (ascii) data record should specify NANT, the number of
C  array antennas.
C  The second data record should specify the site latitude and
C  longitude(west), in degrees.
C  The third record must specify two conversion factors: first,
C  a multiplicative factor for converting antenna coordinates to meters,
C  and, second, a factor for converting antenna diameters to meters.
C  (E.g., if the station coordinates are in nanoseconds and the antenna
C  diameters are in feet, then the two factors that need to be supplied
C  are 0.2997924562 (approximately) and 0.3048 (exactly).)
C  Then, NANT records must follow, each with five numbers:  The coordinate
C  system identificator; the three coordinates of the antenna location;
C  antenna diameter.
C   Inputs:
C    FILE     C*48  File name
C   Outputs in common:
C    NANT        I     Number of antennas
C    LX(*)    D     "X" coordinate of antennas (m)
C    LY(*)    D     "Y" coordinate of antennas (m)
C    LZ(*)    D     "Z" coordinate of antennas (m)
C    DIAM(*)  D     Antenna diameters (m)
C    SITELA   D     Site latidude (rad)
C    SITELO   D     Site longitude (rad)
C   Outputs:
C    IERR     I     Return code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      INTEGER   IERR
      INTEGER   LUN, FIND, NBYTES, KBP, CENTER
      LOGICAL   F
C
      DOUBLE PRECISION XTEMP, YTEMP, ZTEMP, LAT, LONG, ALT, EARTHR
      INTEGER   I, COOR, JT, JTRIM
      CHARACTER LINE*80
      DOUBLE PRECISION CONST1, CONST2, X, SINLA, SINLO, COSLA, COSLO
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'SHADO.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      EARTHR = 6378.0D3
C                                       Open text file
      LUN = 10
      CALL ZTXOPN ('READ', LUN, FIND, FILE, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
C                                       Get number of antennas
10    CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       skip comments
      IF (LINE(1:1) .EQ. ';') GO TO 10
C                                       Get value
      KBP = 1
      NBYTES = 80
      CALL GETNUM (LINE, NBYTES, KBP, X)
      NANT = X + 0.1
C                                       Tell user
      WRITE (MSGTXT,1100) NANT
      CALL MSGWRT (6)
C                                       get site latitude, longtitude
20    CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       skip comments
      IF (LINE(1:1) .EQ. ';') GO TO 20
C                                       Get value
      KBP = 1
      CALL GETNUM (LINE, NBYTES, KBP, X)
      SITELA = X
      CALL GETNUM (LINE, NBYTES, KBP, X)
      SITELO = X
C                                       Tell user
      WRITE (MSGTXT,1200) SITELA
      CALL MSGWRT (6)
      WRITE (MSGTXT,1300) SITELO
      CALL MSGWRT (6)
      SITELA = SITELA * DG2RAD
      SITELO = SITELO * DG2RAD
C                                       Latitude, Lontitude of the side
C                                       in radians
C
C                                       Get conversion constants
30    CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       skip comments
      IF (LINE(1:1) .EQ. ';') GO TO 30
C                                       Get values
      KBP = 1
      CALL GETNUM (LINE, NBYTES, KBP, X)
      CONST1 = X
      CALL GETNUM (LINE, NBYTES, KBP, X)
      CONST2 = X
C                                       Tell user
      WRITE (MSGTXT,1400) CONST1
      CALL MSGWRT (6)
      WRITE (MSGTXT,1500) CONST2
      CALL MSGWRT (6)
      SINLA = SIN(SITELA)
      SINLO = SIN(SITELO)
      COSLA = COS(SITELA)
      COSLO = COS(SITELO)
      XCENT = EARTHR * COSLA * COSLO
      YCENT = EARTHR * COSLA * (-SINLO)
      ZCENT = EARTHR * SINLA
C                                       Read antenna info
      DO 100 I = 1,NANT
40       CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         JT = JTRIM (LINE)
C                                       skip comments
         IF (LINE(1:1) .EQ. ';') GO TO 40

         IF (IERR.NE.0) GO TO 980
C                                       Get values
         KBP = 1
C                                       Coordinate system
         CALL GETNUM (LINE, NBYTES, KBP, X)
         COOR = X
C
C                                       Do not use the conversion
C                                       factor if position is given in
C                                       LAT, LONG, ALT
         IF (COOR .EQ. 2) CONST1 = 1
C
         CALL GETNUM (LINE, NBYTES, KBP, X)
         LX(I) = X * CONST1
         CALL GETNUM (LINE, NBYTES, KBP, X)
         LY(I) = X * CONST1
         CALL GETNUM (LINE, NBYTES, KBP, X)
         LZ(I) = X * CONST1
         CALL GETNUM (LINE, NBYTES, KBP, X)
         DIAM(I) = X * CONST2
C                                       Coordinates are relatively the
C                                       array center or relatively the
C                                       Earth center
         CENTER = 0
C
         IF (COOR .EQ. 2) THEN
            LAT = LX(I) * DG2RAD
            LONG = LY(I) * DG2RAD
            ALT = LZ(I)
            WRITE (MSGTXT,1600) I, LX(I), LY(I), LZ(I)
         ELSE
            WRITE (MSGTXT,1700) I, LX(I), LY(I), LZ(I)
            END IF
         CALL MSGWRT (6)
C
         IF (COOR .EQ. 1) THEN
C                                       Recalculate antenna coordinates
C                                       from the RH coordinate system
C                                       (Z in local zenith) to the RH
C                                       equatorial coordinate system
C
            XTEMP = -LX(I) * (-SINLO) - LY(I) * SINLA * COSLO
     *         + LZ(I) * COSLA * COSLO
            YTEMP = LX(I) * COSLO - LY(I) * SINLA * (-SINLO)
     *         + LZ(I) * COSLA * (-SINLO)
            ZTEMP = LY(I) * COSLA + LZ(I) * SINLA
            LX(I) = XTEMP
            LY(I) = YTEMP
            LZ(I) = ZTEMP
            END IF
C
         IF (COOR .EQ. 2) THEN
C                                       Recalculate antenna coordinates
C                                       from LAT, LON(+ to west) to the
C                                       RH equatorial coordinate system
            LX(I) = (EARTHR + ALT) * COS(LAT) * COS(LONG)
            LY(I) = (EARTHR + ALT) * COS(LAT) * SIN(-LONG)
            LZ(I) = (EARTHR + ALT) * SIN(LAT)
            CENTER = 1
            END IF
         IF (COOR .EQ. 3) THEN
C                                       Recalculate antenna coordinates
C                                       from the RH coordinate system.
C                                       X is to the array center
C                                       meridian VLA for example
C                                       (Z in local zenith) to the RH
C                                       equatorial coordinate system
C
            XTEMP = LX(I) * COSLO + LY(I) * SINLO
            YTEMP = -LX(I) * SINLO + LY(I) * COSLO
            ZTEMP = LZ(I)
            LX(I) = XTEMP
            LY(I) = YTEMP
            LZ(I) = ZTEMP
            END IF
C                                       recalculate coordinates to
C                                       the origin at the array center
         LX(I) = LX(I) - CENTER * XCENT
         LY(I) = LY(I) - CENTER * YCENT
         LZ(I) = LZ(I) - CENTER * ZCENT
         WRITE (MSGTXT,1700) I, LX(I), LY(I), LZ(I)
         CALL MSGWRT (6)
 100     CONTINUE
C                                       close the file
      CALL ZTXCLS (LUN, FIND, IERR)

      GO TO 999
C                                       Read error
 980  WRITE (MSGTXT,1800) IERR
      GO TO 990
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I3,' OPENING ANTENNA INFO TEXT FILE')
 1100 FORMAT ('Number of antennas = ',I4)
 1200 FORMAT ('Site latitude = ',F8.3,' degrees')
 1300 FORMAT ('Site longtitude = ',F8.3,' degrees')
 1400 FORMAT ('Coordinate conversion factor to meters =',1PE12.5)
 1500 FORMAT ('Diameter conversion factor to meters =',1PE12.5)
 1600 FORMAT ('A(',I4,'): ',F4.1,'deg; ',
     *   F6.1,'deg; ', F6.1,'; d=',F4.0,
     *   ' ef=', F5.2, ' T=', F6.1, 1X, I2)
 1700 FORMAT ('A(',I4,'):',3F9.0,'; d=',F4.0,
     *   ' ef=', F5.2, ' T=', F6.1, 1X, I2)
 1800 FORMAT ('ERROR ',I3,' READING ANTENNA INFO TEXT FILE')
      END
      SUBROUTINE GETANG (H, DELTA, SLAT, AZ, EL, PARANG)
C-----------------------------------------------------------------------
C   Given the hour-angle h, the source declination delta, and the
C   geographic latitude, slat, of the site, this subroutine computes
C   the azimuth, the elevation, and the parallactic angle.
C   Inputs:
C    H        D    Hour angle (rad)
C    DELTA    D    Source declination (rad)
C    SLAT     D    Site latitude (rad)
C   Outputs:
C    AZ       D    Azimuth (rad)
C    EL       D    Elevation (rad)
C    PARANG   D    Parallactic angle (rad)
C-----------------------------------------------------------------------
      DOUBLE PRECISION H, DELTA, SLAT, AZ, EL, PARANG
      DOUBLE PRECISION Z, SINQ, COSQ, SINA, COSA
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      Z = ACOS (SIN (DELTA) * SIN (SLAT) + COS (DELTA) * COS (SLAT) *
     *   COS (H))
      EL = PI / 2.0D0 - Z
      IF (Z.EQ.0D0) THEN
         PARANG = 0.0D0
         AZ = 0.0D0
      ELSE
         SINQ = SIN (H) * COS (SLAT) / SIN (Z)
         COSQ = SIN (SLAT) / SIN (Z) / COS (DELTA) - TAN (DELTA) /
     *      TAN (Z)
         PARANG = ATAN2 (SINQ, COSQ)
         SINA = -COS (DELTA) * SIN (H) / SIN (Z)
         COSA = (SIN (DELTA) * COS (SLAT) - COS (DELTA) * COS (H) *
     *      SIN (SLAT)) / SIN (Z)
         AZ = ATAN2 (SINA, COSA)
         IF (AZ.LT.0.0D0) AZ = AZ + 2.0D0 * PI
         END IF
C
 999  RETURN
      END
      SUBROUTINE BLOCK (I, J, UANT, VANT, WANT, DIAM, FI, FJ)
C-----------------------------------------------------------------------
C  Subroutine to compute geometric shadowing.
C  Given i and j, antenna-based (u,v,w), and the antenna diameters,
C  this subroutine computes the fractional blockage fi of antenna i by
C  antenna j, and the fractional blockage fj of antenna j by antenna i.
C  Whenever blockage occurs, if w<0 then it is the case that antenna j
C  is blocked by antenna i, if w>0 then antenna i has been blocked by
C  antenna j, and if w = 0 then the antennas have run into each other.
C  So generally, on return, one - but not both - of fi and fj may be
C  nonzero.
C  The antenna profiles are assumed to be circular, and the antennas
C  are assumed to be pointed the same way (a reasonably valid
C  assumption for the case of a compact array configuration, which
C  is the only case where one generally worries about shadowing).
C   Inputs:
C    I       I    First antenna number
C    J       I    Second antenna number
C    UANT(*) D    Array of U values (m)
C    VANT(*) D    Array of V values (m)
C    WANT(*) D    Array of W values (m)
C    DIAM(*) D    Array of diameters (m)
C   Outputs:
C    FI      D    Fractional blockage of antenna I
C    FJ      D    Fractional blockage of antenna J
C-----------------------------------------------------------------------
      INTEGER   I, J
      DOUBLE PRECISION    UANT(*), VANT(*), WANT(*), DIAM(*), FI, FJ
      DOUBLE PRECISION    U, V, W, SEP, RMIN, RMAX, SINA, SINB,
     *   AREA, S, C, A, B
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      U = UANT(J)-UANT(I)
      V = VANT(J)-VANT(I)
      W = WANT(J)-WANT(I)
      SEP = SQRT (U**2 + V**2)
      RMIN = 0.5D0 * MIN (DIAM(I), DIAM(J))
      RMAX = 0.5D0 * MAX (DIAM(I), DIAM(J))
      IF (SEP.GE.RMIN+RMAX) THEN
         FI = 0.0D0
         FJ = 0.0D0
      ELSE IF (SEP+RMIN.LE.RMAX) THEN
         FI = MIN (1.0D0, (DIAM(J) / DIAM(I))**2)
         FJ = MIN (1.0D0, (DIAM(I) / DIAM(J))**2)
      ELSE
         C = SEP / (0.5D0*DIAM(I))
         S = DIAM(J) / DIAM(I)
         SINB = SQRT ((S+C+1.0D0) * (S+C-1.0D0) * (S-C+1.0D0) *
     *      (C-S+1.0D0)) / (2.0D0*C)
C  Or,
C        SINB = SQRT(2D0*((C*S)**2+C**2+S**2)-C**4-S**4-1D0)/(2D0*C)
C
         SINA = SINB / S
C                                       Due to roundoff, sina or sinb
C                                       might be ever so slightly larger
C                                       than 1. In the case of unequal
C                                       radii, with the center of one
C                                       antenna pattern inside the
C                                       other:
         SINB = MIN (1.0D0, SINB)
         SINA = MIN (1.0D0, SINA)
C
         B = ASIN (SINB)
         A = ASIN (SINA)
         AREA = (S**2*A+B) - (S**2*SINA*COS (A)+SINB*COS (B))
         FI = AREA / PI
         FJ = FI / S**2
      END IF
      IF (W.LT.0.0D0) FI = 0.0D0
      IF (W.GT.0.0D0) FJ = 0.0D0
 999  RETURN
      END
      SUBROUTINE UVANT (H, DELTA, NANT, LX, LY, LZ, UANT, VANT, WANT)
C-----------------------------------------------------------------------
C  Given the hour-angle h, the source declination delta, the number
C  of elements nant, and the element locations (lx(i),ly(i),lz(i)),
C  i = 1,...,nant, this subroutine computes the antenna-based spatial
C  frequency coordinates (uant(i),vant(i),want(i)), i = 1,...,nant.
C   Inputs:
C    H        D    Hour angle (rad)
C    DELTA    D    Source declination (rad)
C    NANT     I    Number of antennas
C    LX(*)    D    "X" component of antenna position (m)
C    LY(*)    D    "Y" component of antenna position (m)
C    LZ(*)    D    "Z" component of antenna position (m)
C   Outputs:
C    UANT(*)  D    "U" component of project coordinate (m)
C    VANT(*)  D    "V" component of project coordinate (m)
C    WANT(*)  D    "W" component of project coordinate (m)
C-----------------------------------------------------------------------
      INTEGER   NANT
      DOUBLE PRECISION H, DELTA, LX(*), LY(*), LZ(*), UANT(*),
     *   VANT(*), WANT(*)
      INTEGER   I
      DOUBLE PRECISION SH, CH, SD, CD
C-----------------------------------------------------------------------
      SH = SIN (H)
      CH = COS (H)
      SD = SIN (DELTA)
      CD = COS (DELTA)
      DO 100 I = 1,NANT
         UANT(I) = SH * LX(I) + CH * LY(I)
         VANT(I) = -SD * (CH * LX(I) - SH * LY(I)) + CD * LZ(I)
         WANT(I) = CD * (CH * LX(I) - SH * LY(I)) + SD * LZ(I)
 100     CONTINUE
C
 999  RETURN
      END
