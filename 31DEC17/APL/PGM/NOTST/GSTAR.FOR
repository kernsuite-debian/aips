      PROGRAM GSTAR
C-----------------------------------------------------------------------
C! Task converts Guide Star table (UK) into a ST table.
C# EXT-util Map-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2006, 2009
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
C   GSTAR will convert a Guide Star Table positions and brightnesses
C   of stars in the Hubble Guide Star catalog into a ST table attached
C   to an AIPS image.  The ST file will be in standard tables format.
C    INPUTS:   (from AIPS)
C              USERID   R   user number, 0 means use logon user
C                       number, 32000 means any user can be accessed.
C              INNAME   R(3)   name of primary file.
C              INCLASS  R(2)   class of primary file.
C              INSEQ    R   sequence number of primary file.
C              INDISK   R    disk volume number. 0 means try all.
C              OUTVERS  R    ST file version number.
C              FACTOR   R    ST type to create
C              EPOCH    R    Epoch of coordinates for stars
C-----------------------------------------------------------------------
C                                       Max Number Columns, Label Length
      INTEGER MXSTCL, MXSTLB, MXSTAR
      PARAMETER (MXSTCL=7, MXSTLB=24, MXSTAR=4000)
C                                       Number of coordinate systems
      INTEGER   NCRD
      PARAMETER (NCRD = 7)
      CHARACTER CRD1*40, CRD2*40, CRDNAM(NCRD)*40, TABEPO(NCRD)*8
      CHARACTER PRGNAM*6, HILINE*72, NAMIN*12, CLSIN*6, TYPIN*2,
     *   ATIME*8, ADATE*12, STFILE*48, SBUF*80, TTITLE*28, ATYPE(18)*4,
     *   NTYPE(7)*8, CHTM12*12, TITLE*48, UNITS(MXSTCL)*8, STRCHR*24
      HOLLERITH HOLTMP(6), XNAMIN(3), XCLSIN(2), STLABL(6)
      DOUBLE PRECISION XPOS, YPOS, XIN, YIN, ROTN, DMAG, CRDPRM(11)
      REAL      DSKIN, SEQIN, XOUVER, DXPOS, DYPOS, POSANG, STTYPE,
     *   RADEGS(MXSTAR), DECDGS(MXSTAR), MAGS(MXSTAR), FACTOR, XINVER,
     *   EPOCH
      INTEGER  I, IWBUFF(256), IMFIND, GSCIDS(MXSTAR), NSTAR, INVER,
     *   IMLUN, IERR, IRETCD, ISEQ, INPRMS, ISLOT, IUSER, IVOL, ISTAR,
     *   IROUND, SLUN, HLUN, IT(3), ID(3), J, K, BUF(512),
     *   XBUF(256), NKEY, NREC, NCOL, OUVER, IEPOCH, ICTYPE(MXSTCL),
     *   ICOL, IER, IRNO
      LOGICAL   T, F, SAVE, EQUAL, QUICK, NEWVER
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, XINVER, XOUVER,
     *   FACTOR, EPOCH
      COMMON /GSTARD/ XPOS, YPOS, DXPOS, DYPOS, POSANG, STTYPE, STLABL
      DATA IMLUN, SLUN, HLUN /16,28,29/
      DATA PRGNAM /'GSTAR '/
      DATA TYPIN /'  '/
      DATA T, F /.TRUE.,.FALSE./
      DATA TTITLE /'AIPS ST star positions table'/
      DATA ATYPE /'TIME','FREQ','LAMB','VELO','FELO','    ','DIST',
     *   'ANGL','ELON','ELAT', 'GLON','GLAT','RA  ','RA--','LL  ',
     *   'DEC ','DEC-','MM  '/
      DATA NTYPE /'SECONDS ', 'HERTZ   ', 'METERS  ',
     *   'METR/SEC', 'METR/SEC', 'PIXELS  ', 'DEGREES '/
      DATA CRDNAM/'EQUATORIAL b1950.0',
     *   'EQUATORIAL b1900.0',
     *   'EQUATORIAL J2000.0',
     *   'GALACTIC',
     *   'OLD GALACTIC (OHLSSON)',
     *   'OLD GALACTIC (VAN TULDER)',
     *   'SUPERGALACTIC (REVISED)'/
C                                       8 character version of CRDNAM
      DATA TABEPO/'b1950.0 ','b1900.0 ','J2000.0 ','Galactic',
     *            'Gal OHLS','Gal VanT','SuperGal'/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      SAVE = F
C                                       Get input values from AIPS.
      INPRMS = 11
      IRETCD = 0
      CALL GTPARM (PRGNAM, INPRMS, QUICK, XNAMIN, IWBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRETCD = 8
         END IF
      IF (QUICK) CALL RELPOP (IRETCD, IWBUFF, IERR)
      IF (IRETCD.NE.0) GO TO 990
      IRETCD = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
C
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      IUSER = NLUSER
      OUVER = IROUND (XOUVER)
      INVER = MAX( 0, IROUND (XINVER))
C                                       Set Star Output Type
      FACTOR = MAX (1, IROUND (FACTOR))
      IF (FACTOR.GT.24.0) FACTOR = 24.0
C                                       Determine coordinate transform
C                                       for CRDTRN
      IEPOCH = IROUND (EPOCH)
C                                       Default is J 2000
      IF (EPOCH.LE.0.5 .OR. EPOCH.GE.7.4) IEPOCH = 3
C                                       If Epoch near a date
      IF (ABS(EPOCH-1900.).LT.1) IEPOCH = 2
      IF (ABS(EPOCH-1950.).LT.1) IEPOCH = 1
      IF (ABS(EPOCH-2000.).LT.1) IEPOCH = 3
C                                       First is 1900
      IF (EPOCH.EQ.1) IEPOCH = 2
C                                       Second is 1950
      IF (EPOCH.EQ.2) IEPOCH = 1
C                                       Set up for coordinate transform
C                                       Stars are in J2000
      CRD1 = CRDNAM(3)
C                                       Set output coordinate system
      CRD2 = CRDNAM(IEPOCH)
C                                       Find begin and end of label
      CALL TXTLEN (CRDNAM(IEPOCH), I, J)
C                                       If no blanks
C                                       Use beginning to end
      IF (J.LE.0) THEN
         J = I - 1
         I = 1
         END IF
C                                       Tell user
      MSGTXT = 'Output Star positions in '//CRDNAM(IEPOCH)(I:J)//
     *            ' Coordinates'
      CALL MSGWRT(3)
C                                       Set up for transform
      CALL CRDSET ( CRD1, CRD2, CRDPRM, IERR)
C                                       Open map file & get header.
 20   CALL MAPOPN ('HDWR', IVOL, NAMIN, CLSIN, ISEQ, TYPIN, IUSER,
     *   IMLUN, IMFIND, ISLOT, CATBLK, IWBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Fill in defaults in PARMS
      DSKIN = IVOL
C                                       Open and read Guide Star table.
      CALL GSCRED (MXSTAR, IVOL, ISLOT, INVER, NSTAR, GSCIDS, RADEGS,
     *   DECDGS, MAGS, IERR)
      IF (IERR.NE.0 .OR. NSTAR.LT.1) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       Create/open ST file
      NCOL = MXSTCL
      NKEY = 1
      NREC = 50
C                                       data types: 2 D, 4 R, Chars
      CALL FILL (256, 0, XBUF)
C                                       First two entries are Ra+Dec
      XBUF(129) = 11
      XBUF(130) = 11
C                                       Second two are Star size
      XBUF(131) = 12
      XBUF(132) = 12
C                                       Fifth is orientation
      XBUF(133) = 12
C                                       Sixth is type (cross, ellipse)
      XBUF(134) = 12
C                                       Seventh is Character Star label
      XBUF(135) = (MXSTLB*10) + 3
C                                       create/open
      CALL TABINI ('WRIT', 'ST', IVOL, ISLOT, OUVER, CATBLK, SLUN, NKEY,
     *    NREC, NCOL, XBUF, BUF, IERR)
      NEWVER = IERR.EQ.-1
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1030) IERR, OUVER
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       table title
      CALL CHR2H (28, TTITLE, 1, BUF(101))
C                                       write column titles
      TITLE = ' '
      DO 45 ICOL = 1,NCOL
         IRNO = ICOL
         J = MOD (ICOL-1, 2) * 2
C                                       Fixed ST column labels
C                                       If an RA, Dec Coord
         IF (IEPOCH.LE.3) THEN
            IF (ICOL.EQ.1) TITLE = 'RA---ARC'
            IF (ICOL.EQ.2) TITLE = 'DEC--ARC'
            IF (ICOL.EQ.3) TITLE = 'DELTRA--'
            IF (ICOL.EQ.4) TITLE = 'DELTDEC-'
         ELSE
C                                       Else a LONG-LAT coordiante
            IF (ICOL.EQ.1) TITLE = 'GLON-ARC'
            IF (ICOL.EQ.2) TITLE = 'GLAT-ARC'
            IF (ICOL.EQ.3) TITLE = 'DELTGLON'
            IF (ICOL.EQ.4) TITLE = 'DELTGLAT'
C                                       End if RA-DEC Coordiante
            END IF
         IF (ICOL.EQ.5) TITLE = 'POSANG'
         IF (ICOL.EQ.6) TITLE = 'STARTYPE'
         IF (ICOL.EQ.7) TITLE = 'LABEL'
         CALL CHR2H (MXSTLB, TITLE, 1, HOLTMP)
         CALL TABIO ('WRIT', 3, IRNO, HOLTMP, BUF, IERR)
         IF (IERR.NE.0) GO TO 960
 45      CONTINUE
C                                       Put coordiate keyword in header
      CALL CHR2H (8, TABEPO(IEPOCH), 1, HOLTMP)
      NKEY = 1
      CALL TABKEY ('WRIT', 'TABEPOCH', NKEY, BUF, 1, HOLTMP, 3, IERR)
C                                       Figure out units
      DO 55 I = 1,2
         ICTYPE(I) = 1
         J = (I-1) * 2 + KHCTP
         UNITS(I) = ' '
         DO 50 K = 1,18
            CALL H2CHR (4, 1, CATH(J), CHTM12)
            EQUAL = CHTM12(1:4) .EQ. ATYPE(K)(1:4)
            IF (.NOT.EQUAL) GO TO 50
               IF (K.LT.7) UNITS(I) = NTYPE(K)
               IF (K.GE.7) UNITS(I) = NTYPE(7)
               IF (K.GT.12) ICTYPE(I) = 2
               IF (K.GT.15) ICTYPE(I) = 3
               J = I + 2
               UNITS(J) = UNITS(I)
               GO TO 55
 50         CONTINUE
 55      CONTINUE
C                                       Angles in degrees
      UNITS(1) = NTYPE(7)
      UNITS(2) = NTYPE(7)
      UNITS(3) = NTYPE(7)
      UNITS(4) = NTYPE(7)
      UNITS(5) = NTYPE(7)
      ICTYPE(5) = 10
C                                       Star type is an index
C                                       0=cross, 1=ellipse, etc
      UNITS(6) = 'INDEX   '
      ICTYPE(6) = 10
C                                       Lable  is an arbitary string
      UNITS(7) = 'STRING  '
      ICTYPE(6) = 10
C                                       write units
      DO 60 ICOL = 1,NCOL
         IRNO = ICOL
         CALL CHR2H (8, UNITS(ICOL), 1, HOLTMP)
         CALL TABIO ('WRIT', 4, IRNO, HOLTMP, BUF, IERR)
         IF (IERR.NE.0) GO TO 960
 60      CONTINUE
C                                       Process record loop
      DO 100 ISTAR = 1, NSTAR
C
         XIN = RADEGS(ISTAR)
         YIN = DECDGS(ISTAR)
C                                       Transform coordinates
         CALL CRDTRN ( XIN, YIN, CRDPRM, XPOS, YPOS, ROTN)
C                                       Tell user about transform
         IF (ISTAR.EQ.1) THEN
            MSGTXT = 'First Star J2000 RA and DEC: '
            CALL MSGWRT(3)
            WRITE (MSGTXT, 1050) XIN, YIN
            CALL MSGWRT(3)
            MSGTXT = 'Output Coordinates are: '
            CALL MSGWRT(3)
            WRITE (MSGTXT, 1050) XPOS, YPOS
            CALL MSGWRT(3)
            WRITE (MSGTXT, 1060) ROTN
            CALL MSGWRT(3)
            END IF
C                                       Convert Magnitudes to Arcs
         DMAG = ( MAGS(ISTAR) - 24.71) / (-4.78)
         DXPOS = EXP( DMAG) / (2.*3600.)
         DYPOS = DXPOS
C                                       Keep the Magnitude readable
         POSANG = MAGS(ISTAR) / 1000
         STTYPE = FACTOR + 0.001
         STRCHR = ' '
C                                       Create Label out of ID
         WRITE(STRCHR,1100,ERR=80) IROUND (GSCIDS(ISTAR))
C                                       Convert label to hollerith
 80      CALL CHR2H (MXSTLB, STRCHR, 1, STLABL(1))
C                                       Write one star
         CALL TABIO ('WRIT', 0, ISTAR, XPOS, BUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1150) IERR
            CALL MSGWRT (8)
            GO TO 960
            END IF
 100     CONTINUE
C                                       Normal EOF
 200  CALL TABIO ('CLOS', 0, ISTAR, XPOS, BUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1150) IERR
         CALL MSGWRT (8)
         GO TO 960
         END IF
      WRITE (MSGTXT,1210) NSTAR, OUVER
      CALL MSGWRT (4)
      IRETCD = 0
C                                       Add to history file
      CALL HIINIT (3)
      CALL HIOPEN (HLUN, IVOL, ISLOT, IWBUFF, IER)
      IF (IER.NE.0) GO TO 980
C                                       Prepare text and add to file
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (SBUF,1211,ERR=215) TSKNAM, OUVER, NSTAR, ADATE, ATIME
 215  HILINE = SBUF
      CALL HIADD (HLUN, HILINE, IWBUFF, IER)
      CALL HICLOS (HLUN, T, IWBUFF, IER)
      GO TO 980
C                                       ERRORS:
C                                       Kill the ST file
 960  CALL ZCLOSE (SLUN, BUF(82), IERR)
      CALL H2CHR (24, 1, BUF(17), STFILE)
      CALL ZDESTR (IVOL, STFILE, IERR)
C                                       TABINI called FXHDEX
      DO 965 I = 1,KIEXTN
         J = I - 1
         CALL H2CHR (2, 1, CATH(KHEXT+J), CHTM12)
         EQUAL = CHTM12(1:2) .EQ. 'ST'
         IF (.NOT.EQUAL) GO TO 965
            IF (OUVER.EQ.CATBLK(KIVER+J)) CATBLK(KIVER+J) =
     *         CATBLK(KIVER+J) - 1
            SAVE = T
            GO TO 980
 965     CONTINUE
C                                       Close map file.
 980  CALL MAPCLS ('WRIT', IVOL, ISLOT, IMLUN, IMFIND, CATBLK, SAVE,
     *   IWBUFF, IERR)
C
 990  CALL DIETSK (IRETCD, QUICK, IWBUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. GTPARM ERR =',I5)
 1020 FORMAT ('ERROR',I4,' OPENING Guide Star Table (UK)')
 1030 FORMAT ('ERROR',I4,' CREATING/OPENING ST FILE VERSION',I4)
 1050 FORMAT ( D14.5,',',D14.5,' Degrees')
 1060 FORMAT ('Position Angle of J2000 north is ',D14.5,
     *   ' Degrees')
 1100 FORMAT ('S',I5,'                  ')
 1115 FORMAT ('ERROR PARSING LINE',I6,': LINE IGNORED')
 1150 FORMAT ('ERROR',I4,' WRITING ST FILE')
 1210 FORMAT ('Wrote',I6,' lines in ST file version',I4)
 1211 FORMAT (A6,' Version=',I3,' Lines=',I5,
     *   '  / Star file created ',A,A)
      END
