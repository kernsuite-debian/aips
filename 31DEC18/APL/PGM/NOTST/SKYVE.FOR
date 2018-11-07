LOCAL INCLUDE 'SKYVE.INC'
C                                       Local common for SKYVE.
      LOGICAL   DOZERO
      INTEGER   IMSIZE(2), INSEQ, INDISK, IUSER, OUTDSK, OUTSEQ, SN
      REAL      BPARM(10), CPARM(10), SX, SXX, SXY, SY, SYY
      DOUBLE PRECISION CNPIX1, CNPIX2, DEC0, DSSX(13), DSSY(13), EQUINX,
     *          RA0, SCALE, XOFF0, XPIXSZ, YOFF0, YPIXSZ
      CHARACTER INNAME*12, INCLAS*6, OUTNAM*12, OUTCLS*6
      COMMON /INPARM/ IUSER, INSEQ, INDISK, OUTSEQ, OUTDSK, IMSIZE,
     *                BPARM, CPARM, DOZERO
      COMMON /INCHAR/ INNAME, INCLAS, OUTNAM, OUTCLS
      COMMON /DSSHDR/ CNPIX1, CNPIX2, SCALE, XPIXSZ, YPIXSZ, EQUINX,
     *                RA0, DEC0, XOFF0, YOFF0, DSSX, DSSY
      COMMON /SKATS/  SN, SX, SXX, SXY, SY, SYY
LOCAL END
      PROGRAM SKYVE
C-----------------------------------------------------------------------
C! Regrid an image from one coordinate frame and geometry to another.
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2008-2010. 2013, 2015
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
C     SKYVE regrids an image from one coordinate frame and geometry to
C     a second frame and geometry specified by adverb values.
C
C     Adverbs:
C         INNAME     Input map name.
C         INCLASS    Input map class.
C         INSEQ      Input map sequence number.
C         INDISK     Input disk number.
C         OUTNAME    Output map name.
C         OUTCLASS   Output map class.
C         OUTSEQ     Output map sequence number.
C         OUTDISK    Output disk number.
C         IMSIZE     Image size in pixels.
C         BPARM      Output map parameters
C                       1) coordinate system
C                          1: equatorial (mean of epoch)
C                          2: galactic
C                          3: ecliptic (mean of epoch)
C                       2) epoch of mean coordinates
C                       3) epoch prefix
C                          1: Julian    (eg J2000.0)
C                          2: Besselian (eg B1950.0)
C                       4) projection,
C                           1: SIN, sine (orthographic)
C                           2: TAN, tangent (gnomonic)
C                           3: ARC, arc projection (zenithal
C                                   equidistant)
C                           4: NCP, north celestial pole tangent
C                                   projection
C                           8: STG, stereographic
C                           5: GLS, global sinusoid (Sanson-Flamsteed)
C                           6: MER, Mercator
C                           7: AIT, Aitov
C                           9  CAR: Cartesion
C                          10: MOL: Molweide's
C                          11: PAR: Parabolic (Craster)
C                       5) Code for blanking on output,
C                           0: indefinite
C                           1: zero
C         CPARM      Output axis specification.
C                      1-5: First axis,
C                       1: reference value, hour (or degree)
C                       2: reference value, min  (or arcmin)
C                       3: reference value, sec  (or arcsec)
C                       4: reference pixel
C                       5: coordinate increment (arcsec)
C                      6-10: Second axis, similarly.
C
C     Called:
C          SKYVE:  {SKYHI, SKYDO, SKYINI, SKYSET}
C          APLSUB: {DIE, MSGWRT}
C
C     Algorithm:
C          Subroutine call tree:
C
C          SKYVE  - SKYINI - DSSIN
C                 - SKYSET - DSSEQ
C                          - CRDSET
C                          - CRDTRN
C                 - SKYDO  - SKTRAN - SETLOC
C                                   - XYVAL
C                                   - CRDTRN
C                                   - DSSPIX - EQSTD
C                                            - DSSCRD
C                          - RGINTP - INT3X3
C                 - SKYHI
C
C     Notes:
C       1)
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1985/06/03  Code last modified; 1994/08/03
C-----------------------------------------------------------------------
      INTEGER   BUFF(256), CATIN(256), CATOUT(256), CNO1, CNO2, IERR
      REAL      CATR(256)
      DOUBLE PRECISION  CRDPRM(11)
      CHARACTER CRD1*40, CRD2*40, EPRFX2*8
      INCLUDE 'SKYVE.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (CATR, CATIN)
C-----------------------------------------------------------------------
C                                       Task initiation.
      CALL SKYINI (CNO1, CATIN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1010) IERR
         CALL MSGWRT (8)
         GO TO 999
            END IF
      IF (EQUINX.LT.0.0) EQUINX = CATR(KREPO)
C                                       Initialize the transformation.
      CALL SKYSET (CATIN, CRD1, CRD2, CATOUT, EPRFX2,
     *   CRDPRM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1020) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Do the transformation, writing
C                                       the map out in the process.
      CALL SKYDO (CNO1, EPRFX2, CRDPRM, CATOUT, CNO2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1030) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Do history.
      CALL SKYHI (CNO1, CNO2, CATOUT, CRD2, CRDPRM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1040) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Clean up.
 999  CALL DIE (IERR, BUFF)
      STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I3,' INITIATING SKYVE.')
 1020 FORMAT ('ERROR',I3,' INITIALIZING THE TRANSFORMATION.')
 1030 FORMAT ('ERROR',I3,' WRITING OUTPUT MAP.')
 1040 FORMAT ('ERROR',I3,' WRITING HISTORY.')
      END
      SUBROUTINE SKYINI (CNO1, CATIN, IERR)
C-----------------------------------------------------------------------
C     SKYINI reads adverbs and gets the input map header.
C
C     Given:
C          none
C
C     Returned:
C          CNO1        I     Catalogue slot number of the input map.
C          CATIN(256)  I     Catalogue header of the input map.
C          IERR        I     Error status, 0 means success.
C
C     Returned via commons INPARM, INCHAR:
C          IUSER       I     AIPS user number.
C          INNAME      C*12  Input map name.
C          INCLAS      C*6   Input map class.
C          INSEQ       I     Input map sequence number.
C          INDISK      I     Input disk number.
C          OUTNAM      C*12  Output map name.
C          OUTCLS      C*6   Output map class.
C          OUTSEQ      I     Output map sequence number.
C          OUTDSK      I     Output disk.
C          IMSIZE(2)   I     Image size in pixels.
C          BPARM(10)   R     Output geometry specification.
C          CPARM(10)   R     Output axis specification.
C          DOZERO      L     Zero output blanked pixels if set.
C
C     Returned via common DSSHDR:
C          Digitized Sky Survey plate solution parameters.
C
C     Called:
C          SKYVE:  {DSSIN}
C          APLSUB: {CATDIR, CATIO, GTPARM, H2CHR, IROUND, MSGWRT,
C                   RELPOP, VHDRIN}
C          APLGEN: {ZDCHIN}
C
C     Algorithm:
C
C     Notes:
C       1)
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1989/12/19  Code last modified; 1994/08/03
C-----------------------------------------------------------------------
      LOGICAL   RQUICK
      INTEGER   CATIN(256), CNO1, IERR, IRET, IROUND, J, NPARM
      REAL      BUFF(256), XBPARM(10), XCPARM(10), XIMSIZ(2), XINDSK,
     *   XINSEQ, XOUTDI, XOUTSQ
      HOLLERITH XINCLS(2), XINNAM(3), XOUTCL(2), XOUTNA(3)
      CHARACTER PRGNAM*6, PTYPE*2, STAT*4
      INCLUDE 'SKYVE.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /SKYINP/ XINNAM, XINCLS, XINSEQ, XINDSK, XOUTNA, XOUTCL,
     *   XOUTSQ, XOUTDI, XIMSIZ, XBPARM, XCPARM
      DATA PRGNAM /'SKYVE '/
C-----------------------------------------------------------------------
C                                       Initialize commons.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Get input parameters.
C                                       Fixed PPM 1996.09.30: was 47
      NPARM = 36
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XINNAM, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1010) IERR
         CALL MSGWRT (8)
         GO TO 999
            END IF
C                                       Release AIPS.
      IRET = 0
      IF (RQUICK) CALL RELPOP (IRET, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1020) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Decode the adverb values.
      IUSER  = NLUSER
C                                       Input map name.
      CALL H2CHR (12, 1, XINNAM, INNAME)
      CALL H2CHR (6,  1, XINCLS, INCLAS)
      INSEQ  = IROUND (XINSEQ)
      INDISK = IROUND (XINDSK)
C                                       Output map name.
      CALL H2CHR (12, 1, XOUTNA, OUTNAM)
      CALL H2CHR (6,  1, XOUTCL, OUTCLS)
      OUTSEQ = IROUND (XOUTSQ)
      OUTDSK = IROUND (XOUTDI)
C                                       Image size.
      IMSIZE(1) = IROUND (XIMSIZ(1))
      IMSIZE(2) = IROUND (XIMSIZ(2))
C                                       Transformation parameters.
      DO 30 J = 1, 10
         BPARM(J) = XBPARM(J)
         CPARM(J) = XCPARM(J)
 30   CONTINUE
C                                       Blanking control.
      DOZERO = BPARM(5).GT.0.5
C                                       Get the input map header.
      CNO1 = 1
      PTYPE = 'MA'
      CALL CATDIR ('SRCH', INDISK, CNO1, INNAME, INCLAS, INSEQ, PTYPE,
     *   IUSER, STAT, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1040) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
      CALL CATIO ('READ', INDISK, CNO1, CATIN, 'REST', BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1050) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Get the DSS plate parameters.
      CALL DSSIN (CNO1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1060) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SKYINI: ERROR',I3,' READING ADVERB VALUES.')
 1020 FORMAT ('SKYINI: ERROR',I3,' RESTARTING AIPS.')
 1040 FORMAT ('SKYINI: ERROR',I3,' SEARCHING FOR INPUT MAP HEADER.')
 1050 FORMAT ('SKYINI: ERROR',I3,' READING INPUT MAP HEADER.')
 1060 FORMAT ('SKYINI: ERROR',I3,' GETTING PLATE PARAMETERS.')
      END
      SUBROUTINE DSSIN (CNO, IERR)
C-----------------------------------------------------------------------
C     DSSIN reads Digital Sky Survey plate solution parameters from the
C     AIPS history file.  Although not recognized by AIPS, the FITS
C     header cards defining these parameters are expected to have been
C     recorded in the history file.
C
C     Given:
C          CNO         I     Catalogue slot number of the input map.
C
C     Returned via common DSSHDR:
C          Digitized Sky Survey plate solution parameters.
C
C     Returned:
C          IERR        I     Error status
C                               0: success
C                               1: plate parameter(s) not found in
C                                  history
C
C     Called:
C          APLSUB: {GETNUM, H2CHR, HICLOS, HIINIT, HILOCT, HIOPEN,
C                   MSGWRT}
C          APLGEN: {ZFIO}
C
C     Algorithm:
C          Scans the history file looking for the following header
C          cards and storing the corresponding values into the
C          DSSHDR common.
C
C          Corner pixel coordinates:
C             CNPIX1    CNPIX1
C             CNPIX2    CNPIY1
C
C          Approximate plate scale, arcsec/mm:
C             PLTSCALE  SCALE
C
C          Plate pixel size in X and Y, micron:
C             XPIXELSZ  XPIXSZ
C             YPIXELSZ  YPIXSZ
C
C          Julian equinox of the equatorial coordinates:
C             EQUINOX   EQUINX
C
C          Plate centre right ascension, degree:
C             PLTRAH    RA0
C             PLTRAM
C             PLTRAS
C
C          Plate centre declination, degree
C             PLTDECSN  DEC0
C             PLTDECD
C             PLTDECM
C             PLTDECS
C
C          Plate centre offsets, micron.
C             PPO3      XOFF0
C             PPO6      YOFF0
C
C          Xi plate solution coefficients:
C             AMDX1     DSSX(1)
C              ...       ...
C             AMDX13    DSSX(13)
C
C          Eta plate solution coefficients:
C             AMDY1     DSSY(1)
C              ...       ...
C             AMDY13    DSSY(13)
C
C     Notes:
C       1)
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1994/08/02  Code last modified; 1994/08/05
C-----------------------------------------------------------------------
      INTEGER   BLOCK, CARD, CNO, DECSGN, HIBUFF(256), HIND, HPTR, IERR,
     *          IHIS, J, LUNHI, NCARD, NERR, NHIS, TEN
      DOUBLE PRECISION DSSTMP
      CHARACTER HISTRY*132
C
      INCLUDE 'SKYVE.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNHI /26/
C-----------------------------------------------------------------------
C                                       Initialize.
      NCARD = 0
C
      CNPIX1 = 0.0
      CNPIX2 = 0.0
      SCALE  = 0.0
      XPIXSZ = 0.0
      YPIXSZ = 0.0
      EQUINX = -2000.0
      RA0    = 0.0
      DEC0   = 0.0
      XOFF0  = 0.0
      YOFF0  = 0.0
      DO 10 J = 1, 13
         DSSX(J) = 0.0
         DSSY(J) = 0.0
 10   CONTINUE
      DECSGN = 1
C                                       Open history file.
      CALL HIINIT (1)
      CALL HIOPEN (LUNHI, INDISK, CNO, HIBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1010) IERR
         CALL MSGWRT (8)
         GO TO 999
            END IF
C                                       Locate entry in history table.
      CALL HILOCT ('SRCH', LUNHI, HPTR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1020) IERR
         CALL MSGWRT (8)
         GO TO 999
            END IF
C                                       Determine no. of records.
      NHIS = HITAB(HPTR+2)
      HIND = HITAB(HPTR+1)
C                                       Read the first block.
      BLOCK = 1
      CARD  = 0
      CALL ZFIO ('READ', LUNHI, HIND, BLOCK, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Read through the history file.

      DO 20 IHIS = 1, NHIS
         CARD = CARD + 1
C                                       Read the next block.
         IF (CARD.GT.NHILPR) THEN
            BLOCK = BLOCK + 1
            CALL ZFIO ('READ', LUNHI, HIND, BLOCK, HIBUFF, IERR)
            IF (IERR.NE.0) GO TO 900
            CARD  = 1
               END IF
C
         J = (CARD-1)*NHIWPL + 5
         CALL H2CHR (72, 1, HIBUFF(J), HISTRY)
C                                       Search for recognized cards.
         IF (HISTRY(1:5).EQ.'CNPIX') THEN
            IF (HISTRY(1:9).EQ.'CNPIX1  =') THEN
               NCARD = NCARD + 1
               TEN = 10
               CALL GETNUM (HISTRY, 30, TEN, CNPIX1)
               IF (CNPIX1.EQ.DBLANK) GO TO 975
            ELSE IF (HISTRY(1:9).EQ.'CNPIX2  =') THEN
               NCARD = NCARD + 1
               TEN = 10
               CALL GETNUM (HISTRY, 30, TEN, CNPIX2)
               IF (CNPIX2.EQ.DBLANK) GO TO 975
                  END IF
         ELSE IF (HISTRY(2:9).EQ.'PIXELSZ=') THEN
            IF (HISTRY(1:1).EQ.'X') THEN
               NCARD = NCARD + 1
               TEN = 10
               CALL GETNUM (HISTRY, 30, TEN, XPIXSZ)
               IF (XPIXSZ.EQ.DBLANK) GO TO 975
            ELSE IF (HISTRY(1:1).EQ.'Y') THEN
               NCARD = NCARD + 1
               TEN = 10
               CALL GETNUM (HISTRY, 30, TEN, YPIXSZ)
               IF (YPIXSZ.EQ.DBLANK) GO TO 975
                  END IF
         ELSE IF (HISTRY(1:9).EQ.'EQUINOX =') THEN
            NCARD = NCARD + 1
            TEN = 10
            CALL GETNUM (HISTRY, 30, TEN, EQUINX)
C                                       special parse for EQUINOX
            IF (EQUINX.EQ.DBLANK) THEN
               IF (INDEX(HISTRY(10:),'1950').GT.0) THEN
                  EQUINX = 1950.0D0
               ELSE IF (INDEX(HISTRY(10:),'2000').GT.0) THEN
                  EQUINX = 2000.0D0
                     END IF
                  END IF
            IF (EQUINX.EQ.DBLANK) GO TO 975
         ELSE IF (HISTRY(1:1).EQ.'P') THEN
            IF (HISTRY(1:9).EQ.'PLTSCALE=') THEN
               NCARD = NCARD + 1
               TEN = 10
               CALL GETNUM (HISTRY, 30, TEN, SCALE)
               IF (SCALE.EQ.DBLANK) GO TO 975
            ELSE IF (HISTRY(1:3).EQ.'PLT') THEN
               IF (HISTRY(4:9).EQ.'RAH  =') THEN
                  NCARD = NCARD + 1
                  TEN = 10
                  CALL GETNUM (HISTRY, 30, TEN, DSSTMP)
                  IF (DSSTMP.EQ.DBLANK) GO TO 975
                  RA0 = RA0 + DSSTMP*15D0
               ELSE IF (HISTRY(4:9).EQ.'RAM  =') THEN
                  NCARD = NCARD + 1
                  TEN = 10
                  CALL GETNUM (HISTRY, 30, TEN, DSSTMP)
                  IF (DSSTMP.EQ.DBLANK) GO TO 975
                  RA0 = RA0 + DSSTMP/4D0
               ELSE IF (HISTRY(4:9).EQ.'RAS  =') THEN
                  NCARD = NCARD + 1
                  TEN = 10
                  CALL GETNUM (HISTRY, 30, TEN, DSSTMP)
                  IF (DSSTMP.EQ.DBLANK) GO TO 975
                  RA0 = RA0 + DSSTMP/240D0
               ELSE IF (HISTRY(4:9).EQ.'DECSN=') THEN
                  NCARD = NCARD + 1
                  IF (HISTRY(12:12).EQ.'-') DECSGN = -1
               ELSE IF (HISTRY(4:9).EQ.'DECD =') THEN
                  NCARD = NCARD + 1
                  TEN = 10
                  CALL GETNUM (HISTRY, 30, TEN, DSSTMP)
                  IF (DSSTMP.EQ.DBLANK) GO TO 975
                  DEC0 = DEC0 + DSSTMP
               ELSE IF (HISTRY(4:9).EQ.'DECM =') THEN
                  NCARD = NCARD + 1
                  TEN = 10
                  CALL GETNUM (HISTRY, 30, TEN, DSSTMP)
                  IF (DSSTMP.EQ.DBLANK) GO TO 975
                  DEC0 = DEC0 + DSSTMP/60D0
               ELSE IF (HISTRY(4:9).EQ.'DECS =') THEN
                  NCARD = NCARD + 1
                  TEN = 10
                  CALL GETNUM (HISTRY, 30, TEN, DSSTMP)
                  IF (DSSTMP.EQ.DBLANK) GO TO 975
                  DEC0 = DEC0 + DSSTMP/3600D0
                     END IF
            ELSE IF (HISTRY(1:3).EQ.'PPO') THEN
               IF (HISTRY(4:9).EQ.'3    =') THEN
                  NCARD = NCARD + 1
                  TEN = 10
                  CALL GETNUM (HISTRY, 30, TEN, XOFF0)
                  IF (XOFF0.EQ.DBLANK) GO TO 975
               ELSE IF (HISTRY(4:9).EQ.'6    =') THEN
                  NCARD = NCARD + 1
                  TEN = 10
                  CALL GETNUM (HISTRY, 30, TEN, YOFF0)
                  IF (YOFF0.EQ.DBLANK) GO TO 975
                     END IF
                  END IF
         ELSE IF (HISTRY(1:3).EQ.'AMD') THEN
            IF (HISTRY(5:9).EQ.'1   =') THEN
               J = 1
            ELSE IF (HISTRY(5:9).EQ.'2   =') THEN
               J = 2
            ELSE IF (HISTRY(5:9).EQ.'3   =') THEN
               J = 3
            ELSE IF (HISTRY(5:9).EQ.'4   =') THEN
               J = 4
            ELSE IF (HISTRY(5:9).EQ.'5   =') THEN
               J = 5
            ELSE IF (HISTRY(5:9).EQ.'6   =') THEN
               J = 6
            ELSE IF (HISTRY(5:9).EQ.'7   =') THEN
               J = 7
            ELSE IF (HISTRY(5:9).EQ.'8   =') THEN
               J = 8
            ELSE IF (HISTRY(5:9).EQ.'9   =') THEN
               J = 9
            ELSE IF (HISTRY(5:9).EQ.'10  =') THEN
               J = 10
            ELSE IF (HISTRY(5:9).EQ.'11  =') THEN
               J = 11
            ELSE IF (HISTRY(5:9).EQ.'12  =') THEN
               J = 12
            ELSE IF (HISTRY(5:9).EQ.'13  =') THEN
               J = 13
            ELSE
               GO TO 20
                  END IF
            NCARD = NCARD + 1
C
            IF (HISTRY(4:4).EQ.'X') THEN
               TEN = 10
               CALL GETNUM (HISTRY, 30, TEN, DSSX(J))
               IF (DSSX(J).EQ.DBLANK) GO TO 975
            ELSE IF (HISTRY(4:4).EQ.'Y') THEN
               TEN = 10
               CALL GETNUM (HISTRY, 30, TEN, DSSY(J))
               IF (DSSY(J).EQ.DBLANK) GO TO 975
                  END IF
               END IF
 20      CONTINUE
C                                       Close history file.
      CALL HICLOS (LUNHI, .FALSE., HIBUFF, NERR)
C                                       EQUINOX now picked up IMLOD
      IF ((NCARD.EQ.40) .AND. (EQUINX.LT.0.0)) THEN
         EQUINX = -1.
C                                       ELSE Must have exactly 41 cards
      ELSE IF (NCARD.LT.41) THEN
         IERR = 1
         MSGTXT = 'DSSIN: ERROR, TOO FEW PLATE PARAMETER CARDS.'
         GO TO 980
      ELSE IF (NCARD.GT.41) THEN
         IERR = 1
         MSGTXT = 'DSSIN: ERROR, DUPLICATED PLATE PARAMETER CARDS.'
         GO TO 980
            END IF
C                                       Apply declination sign.
      DEC0 = DECSGN*DEC0
      GO TO 999
C
 900  WRITE (MSGTXT,1900) IERR
      GO TO 980
 975  MSGTXT = 'DSSIN: BAD NUMBER ON CARD ' // HISTRY
      IERR = 1
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('DSSIN: ERROR',I3,' OPENING HISTORY FILE.')
 1020 FORMAT ('DSSIN: ERROR',I3,' SEARCHING FOR HISTORY TABLE ENTRY.')
 1900 FORMAT ('DSSIN: ERROR',I3,' DOING I/O')
      END
      SUBROUTINE SKYSET (CATIN, CRD1, CRD2, CATOUT, EPRFX2, CRDPRM,
     *   IERR)
C-----------------------------------------------------------------------
C     SKYSET sets parameters for the coordinate transformation and
C     constructs the catalogue header for the output map.
C
C     Given:
C          CATIN(256)  I     Catalogue header of the input map.
C
C     Given via common INPARM:
C          INDISK      I     Input disk number.
C          IMSIZE(2)   I     Image size in pixels.
C          BPARM(10)   R     Output geometry specification.
C          CPARM(10)   R     Output axis specification.
C          DOZERO      L     Zero output blanked pixels if set.
C
C     Given via common DSSHDR:
C          Digitized Sky Survey plate solution parameters.
C
C     Returned:
C          CRD1        C*40  Input coordinate system.
C          CRD2        C*40  Output coordinate system.
C          CATOUT(256) I     Catalogue header of the output map.
C          EPRFX2      C*8   Epoch prefix
C                               J = Julian
C                               B = Besselian
C                               b = Besselian without e-terms
C          CRDPRM(11)  D     Parameters to transform coordinates from
C                            from the output map to the input map.
C          IERR        I     Error status
C                               0: success
C                               1: inconsistent coordinate axis pair
C                               2: unrecognized input coordinate axes
C                               3: input coordinate epoch error
C                               4: unrecognized input map geometry
C                               5: inconsistent input map geometry
C                               6: invalid output coordinate system
C                               7: invalid output geometry
C                               8: error transforming header coordinates
C                               9: error defining transformation
C
C     Called:
C          SKYVE:  {DSSEQ}
C          APLSUB: {CHR2H, COPY, MSGWRT}
C          APLNOT: {CRDSET, CRDTRN}
C
C     Algorithm:
C
C     Notes:
C       1) Coordinate system codes
C             1: equatorial
C             2: galactic
C             3: ecliptic
C          Map projection codes
C             1: SIN, sine (orthographic)
C             2: TAN, tangent (gnomonic)
C             3: ARC, arc projection (zenithal equidistant)
C             4: NCP, north celestial pole tangent projection
C             5: STG, stereographic
C             6: AIT, Aitov
C             7: GLS, global sinusoid (Sanson-Flamsteed)
C             8: MER, Mercator
C
C       2) Space is reserved in the AIPS image header for the coordinate
C          epoch (viz equinox) but no allowance is made to distinguish
C          between the Bessel-Newcomb (FK4) or IAU1976 (FK5) systems of
C          precession and nutation.  A new keyword 'EPOCPRFX' has been
C          been created to solve this problem.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1994/07/27  Code last modified; 1994/08/05
C-----------------------------------------------------------------------
      INTEGER   CATIN(256), CATOUT(256), IERR, IPRFX, IROUND, OGEOM,
     *   OFRAME
      REAL      EPOCH2, PI, R2D
      DOUBLE PRECISION  CRDPRM(11), DEC, LAT, LNG, RA, ROTN, X2, Y2,
     *          XPX0, YPX0
      CHARACTER CRD1*40, CRD2*40, EPRFX2*8, FRAME(2,3)*4, GEOM(11)*4
C
      INCLUDE 'SKYVE.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C
C     Pi.
      PARAMETER (PI = 3.141592653589793238462643D0)
C
C     Factor to convert radians to degrees.
      PARAMETER (R2D = 180D0/PI)
C
      DATA FRAME /'RA--', 'DEC-', 'GLON', 'GLAT', 'ELON', 'ELAT'/
      DATA GEOM  /'-SIN', '-TAN', '-ARC', '-NCP', '-STG', '-GLS',
     *   '-MER', '-AIT', '-CAR', '-MOL', '-PAR' /
C-----------------------------------------------------------------------
C                                       Initialize.
      IERR = 0
      CALL COPY (256, CATIN, CATBLK)
C                                       DSS pixel coordinates at the
C                                       centre of the input map.
      XPX0 = CNPIX1 + CATBLK(KINAX)/2.0
      YPX0 = CNPIX2 + CATBLK(KINAX+1)/2.0
      CALL DSSEQ (XPIXSZ, YPIXSZ, RA0, DEC0, XOFF0, YOFF0, DSSX, DSSY,
     *   XPX0, YPX0, RA, DEC)
C                                       Input coordinate type.
      CRD1 = 'Equatorial J2000.0'
C                                       Determine the transformation to
C                                       be done.
      OFRAME = IROUND(BPARM(1))
      IF (OFRAME.EQ.0) OFRAME = 1
C                                       Coordinate system.
C                                       Equatorial coordinates.
      IF (OFRAME.EQ.1) THEN
         CRD2 = 'Equatorial'
C                                       Galactic coordinates.
      ELSE IF (OFRAME.EQ.2) THEN
         CRD2 = 'Galactic'
C                                       Ecliptic coordinates.
      ELSE IF (OFRAME.EQ.3) THEN
         CRD2 = 'Ecliptic'
      ELSE
         IERR = 6
         WRITE (MSGTXT,1010) OFRAME
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Epoch of equatorial or ecliptic
C                                       coordinates.
      EPRFX2 = ' '
      IF ((OFRAME.EQ.1) .OR. (OFRAME.EQ.3)) THEN
         EPOCH2 = BPARM(2)
         IF (EPOCH2.LE.0.0) EPOCH2 = 2000.0
C
         IPRFX  = IROUND (BPARM(3))
         IF (IPRFX.EQ.1) THEN
            EPRFX2 = 'J'
         ELSE IF (IPRFX.EQ.2) THEN
            EPRFX2 = 'B'
         ELSE IF (IPRFX.EQ.3) THEN
            EPRFX2 = 'b'
         ELSE
            EPRFX2 = 'J'
            END IF
C
         IF (OFRAME.EQ.1) WRITE (CRD2(12:),1000) EPRFX2, EPOCH2
         IF (OFRAME.EQ.3) WRITE (CRD2(10:),1000) EPRFX2, EPOCH2
         END IF
C                                       Output geometry defaults to
C                                       NCP.
      OGEOM = IROUND (BPARM(4))
      IF (OGEOM.LE.0) OGEOM = 1
C                                       Check validity.
      IF (OGEOM.LT.1 .OR. OGEOM.GT.11) THEN
         IERR = 7
         WRITE (MSGTXT,1020) OGEOM
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
C                                       Build the output map header.
      CALL CHR2H (4, FRAME(1,OFRAME), 1, CATH(KHCTP))
      CALL CHR2H (4, FRAME(2,OFRAME), 1, CATH(KHCTP+2))
      CALL CHR2H (4, GEOM(OGEOM), 1, CATH(KHCTP+1))
      CALL CHR2H (4, GEOM(OGEOM), 1, CATH(KHCTP+3))
C                                       Compute the coordinate reference
C                                       value.
      LNG = CPARM(3)
      LNG = LNG/60D0 + CPARM(2)
      LNG = LNG/60D0 + CPARM(1)
      IF (OFRAME.EQ.1) LNG = 15D0*LNG
C
      LAT = ABS(CPARM(8))
      LAT = LAT/60D0 + ABS(CPARM(7))
      LAT = LAT/60D0 + ABS(CPARM(6))
      IF (CPARM(6).LT.0.0 .OR. CPARM(7).LT.0.0 .OR. CPARM(8).LT.0.0)
     *   LAT = -LAT
C                                       Transform coordinates if
C                                       necessary.
      IF (LNG.LT.-360D0 .OR. LNG.GT.+360D0 .OR.
     *    LAT.LT.-90D0  .OR. LAT.GT.+90D0) THEN
C                                       Set the transformation
C                                       parameters...
         CALL CRDSET (CRD1, CRD2, CRDPRM, IERR)
         IF (IERR.NE.0) THEN
            IERR = 8
            MSGTXT = 'SKYSET: ERROR TRANSFORMING HEADER COORDINATES.'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       ...and do it.
         CALL CRDTRN (RA, DEC, CRDPRM, X2, Y2, ROTN)
         IF (LNG.LT.-360D0 .OR. LNG.GT.+360D0) LNG = X2
         IF (LAT.LT.-90D0  .OR. LAT.GT.90D0)   LAT = Y2
         END IF
C                                       Image size, defaults to input
C                                       size.
      IF (IMSIZE(1).GT.1) CATBLK(KINAX)   = IMSIZE(1)
      IF (IMSIZE(2).GT.1) CATBLK(KINAX+1) = IMSIZE(2)
C                                       Axis-1 parameters.
      CATD(KDCRV) = LNG
      IF (CPARM(4).NE.0.0) THEN
         CATR(KRCRP) = CPARM(4)
      ELSE
         CATR(KRCRP) = (CATBLK(KINAX)+1)/2.0
         END IF
C
      IF (CPARM(5).EQ.0.0) THEN
         CATR(KRCIC) = -(SCALE/3600D0)*(XPIXSZ/1000D0)
      ELSE
         CATR(KRCIC) = CPARM(5)/3600.0
         END IF
C
      CATR(KRCRT)   = 0.0
C                                       Axis-2 parameters.
      CATD(KDCRV+1) = LAT
      IF (CPARM(9).NE.0.0) THEN
         CATR(KRCRP+1) = CPARM(9)
      ELSE
         CATR(KRCRP+1) = (CATBLK(KINAX+1)+1)/2.0
         END IF
C
      IF (CPARM(10).EQ.0.0) THEN
         CATR(KRCIC+1) = (SCALE/3600D0)*(YPIXSZ/1000D0)
      ELSE
         CATR(KRCIC+1) = CPARM(10)/3600.0
         END IF
C
      CATR(KRCRT+1) = 0.0
C                                       Coordinate epoch (equinox).
      CATR(KREPO) = EPOCH2
C                                       Indeterminate pixel value.
      CATR(KRBLK) = FBLANK
      IF (DOZERO) CATR(KRBLK) = 0.0
C                                       Copy it to CATOUT.
      CALL COPY (256, CATBLK, CATOUT)
C                                       Parameters to transform from
C                                       the output map to the input map.
      CALL CRDSET (CRD2, CRD1, CRDPRM, IERR)
      IF (IERR.NE.0) THEN
         IERR = 9
         MSGTXT = 'SKYSET: ERROR DEFINING COORDINATE TRANSFORMATION.'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A1,F6.1)
 1010 FORMAT ('SKYSET: INVALID COORDINATE SYSTEM, BPARM(1) = ',I4)
 1020 FORMAT ('SKYSET: INVALID OUTPUT GEOMETRY, BPARM(4) = ',I4)
      END
      SUBROUTINE SKYDO (CNO1, EPRFX2, CRDPRM, CATOUT, CNO2, IERR)
C-----------------------------------------------------------------------
C     SKYDO transforms from the coordinate system and map projection,
C     of one map to that of another, storing the output map in the
C     process.
C
C     Given:
C          CNO1        I     Catalogue slot number of the input map.
C          EPRFX2      C*8   Epoch prefix
C                               J = Julian
C                               B = Besselian
C                               b = Besselian without e-terms
C          CRDPRM(11)  D     Parameters to transform coordinates from
C                            from the output map to the input map.
C
C     Given via common INPARM:
C          INNAME      C*12  Input map name.
C          INCLAS      C*6   Input map class.
C          INSEQ       I     Input map sequence number.
C          OUTNAM      C*12  Output map name.
C          OUTCLS      C*6   Output map class.
C          OUTSEQ      I     Output map sequence number.
C
C     Given and returned:
C          CATOUT(256) I     Catalogue header of the output map.
C
C     Returned:
C          CNO2        I     Catalogue slot number of the output map.
C          IERR        I     Error status
C                              0: success
C
C     Called:
C          SKYVE:  {RGINTP, SKTRAN}
C          APLSUB: {CATKEY, CHR2H, COPY, MAKOUT, MAPCLS, MAPOPN, MCREAT,
C                   MDISK, MINIT, MSGWRT}
C
C     Algorithm:
C
C     Notes:
C       1) Storage requirements...
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1994/07/27  Code last modified; 1994/08/05
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   MAX2X
      PARAMETER (MAX2X = 2*MAXIMG)
      INTEGER   BUFSZ, BNDX2, CATOUT(256), CNO1, CNO2, IERR, FIND2, IY2,
     *          J2, JY2, K2, KEYTYP, KY2, LOCS, LUN2, NR2, NUMKEY, NX2,
     *          NY2, WBUFF(256), WIN(4)
      REAL      AVX, AVY, BUFF2(MAX2X), COR, MAPMAX, MAPMIN, SDX, SDY,
     *          XPX1(MAX2X), YPX1(MAX2X)
      HOLLERITH HBUFF(2)
      DOUBLE PRECISION  CRDPRM(11)
      CHARACTER EPRFX2*8, PTYPE*2
      INCLUDE 'SKYVE.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN2 /17/
C-----------------------------------------------------------------------
C                                       Initialize statistics.
      SN  = 0
      SX  = 0.0
      SXX = 0.0
      SXY = 0.0
      SY  = 0.0
      SYY = 0.0
C                                       Create the output map.
      IF (IMSIZE(1).GT.MAX2X) THEN
         IMSIZE(1) = MAX2X
         CATOUT(KINAX) = IMSIZE(1)
         WRITE (MSGTXT,1010) IMSIZE
         CALL MSGWRT (6)
         END IF
C                                       Load the catalogue header for
C                                       the output map.
      CALL COPY (256, CATOUT, CATBLK)
C                                       Output map.
      CALL MAKOUT (INNAME, INCLAS, INSEQ, '      ', OUTNAM, OUTCLS,
     *   OUTSEQ)
      CALL CHR2H (12, OUTNAM, KHIMNO, CATR(KHIMN))
      CALL CHR2H (6,  OUTCLS, KHIMCO, CATR(KHIMC))
      CALL CHR2H (2,  'MA',   KHPTYO, CATR(KHPTY))
      CATBLK(KIIMS) = OUTSEQ
C                                       Create file.
      CALL MCREAT (OUTDSK, CNO2, WBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Open it.
      PTYPE = 'MA'
      OUTSEQ = CATBLK(KIIMS)
      CALL MAPOPN ('INIT', OUTDSK, OUTNAM, OUTCLS, OUTSEQ, PTYPE, IUSER,
     *   LUN2, FIND2, CNO2, CATBLK, WBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Record the epoch prefix.
      IF (EPRFX2.NE.' ') THEN
         CALL CHR2H (8, EPRFX2, 1, HBUFF)
         NUMKEY = 1
         LOCS   = 1
         KEYTYP = 3
         CALL CATKEY ('WRIT', OUTDSK, CNO2, 'EPOCPRFX', NUMKEY, LOCS,
     *      HBUFF, KEYTYP, WBUFF, IERR)
         IF (IERR.NE.0) THEN
            IERR = 3
            MSGTXT = 'SKYDO: ERROR RECORDING THE EPOCH PREFIX.'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
C                                       Set window.
      NX2 = CATBLK(KINAX)
      NY2 = CATBLK(KINAX+1)
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX2
      WIN(4) = NY2
C                                       Initialize output IO, force
C                                       single-buffering.
      BUFSZ = NBPS*(1 + (2*NX2-1)/NBPS)
      CALL MINIT ('WRIT', LUN2, FIND2, NX2, NY2, WIN, BUFF2, BUFSZ, 1,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Update the catalogue header for
C                                       the output map.
      CALL COPY (256, CATBLK, CATOUT)
C                                       Pre-write the output buffer.
      CALL MDISK ('WRIT', LUN2, FIND2, BUFF2, BNDX2, IERR)
      IF (BNDX2.NE.1) IERR = 9
      IF (IERR.NE.0 .OR. BNDX2.NE.1) THEN
         WRITE (MSGTXT,1050) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Set output buffer parameters.
      NR2 = MAX2X/NX2
      IF (2*(MAX2X-NX2*(NR2-1)).LT.BUFSZ) NR2 = (2*MAX2X-NBPS)/(2*NX2)
C                                       Loop doing the transformation
C                                       and writing the output map.
      MAPMAX = -1E30
      MAPMIN =  1E30
      DO 80 JY2 = 1, NY2, NR2
         KY2 = MIN(JY2+NR2-1, NY2)
C                                       Transform pixel coordinates of
C                                       each output row.
         CALL SKTRAN (CATOUT, CRDPRM, NX2, JY2, KY2, XPX1, YPX1, IERR)
C                                       Interpolate output values from
C                                       the input map.
         CALL RGINTP (CNO1, NX2, JY2, KY2, XPX1, YPX1, BUFF2, MAPMAX,
     *      MAPMIN, IERR)
C                                       Dump the output buffer.
         J2 = 1
         K2 = 1
         DO 70 IY2 = JY2, KY2
            IF (IY2.EQ.NY2) GO TO 70
            CALL MDISK ('WRIT', LUN2, FIND2, BUFF2(K2), BNDX2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1060) IERR
               CALL MSGWRT (8)
               GO TO 999
               END IF
C
            J2 = J2 + NX2
            IF (BNDX2.EQ.1) K2 = J2
 70      CONTINUE
 80   CONTINUE
C                                       Flush last buffer.
      CALL MDISK ('FINI', LUN2, FIND2, BUFF2(K2), BNDX2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1090) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Update the catalogue header for
C                                       the output map.
      CALL COPY (256, CATOUT, CATBLK)
      CATR(KRDMX) = MAPMAX
      CATR(KRDMN) = MAPMIN
      CALL COPY (256, CATBLK, CATOUT)
C                                       Close map file.
      CALL MAPCLS ('INIT', OUTDSK, CNO2, LUN2, FIND2, CATBLK, .TRUE.,
     *   WBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Report statistics.
      AVX = SX/SN
      SDX = SQRT((SXX - SN*AVX*AVX)/(SN - 1))
      WRITE (MSGTXT,1110) AVX, SDX
      CALL MSGWRT (3)
      AVY = SY/SN
      SDY = SQRT((SYY - SN*AVY*AVY)/(SN - 1))
      WRITE (MSGTXT,1120) AVY, SDY
      CALL MSGWRT (3)
      COR = ((SXY - SX*SY/SN)/(SN - 1))/(SDX*SDY)
      WRITE (MSGTXT,1130) COR
      CALL MSGWRT (3)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SKYDO: Image size truncated to',I5,' *',I5)
 1020 FORMAT ('SKYDO: ERROR',I3,' CREATING THE OUTPUT MAP.')
 1030 FORMAT ('SKYDO: ERROR',I3,' OPENING THE OUTPUT MAP.')
 1040 FORMAT ('SKYDO: ERROR',I3,' INITIALIZING THE OUTPUT MAP.')
 1050 FORMAT ('SKYDO: ERROR',I3,' PRE-WRITING THE OUTPUT MAP.')
 1060 FORMAT ('SKYDO: ERROR',I3,' WRITING THE OUTPUT MAP.')
 1090 FORMAT ('SKYDO: ERROR',I3,' FLUSHING THE OUTPUT MAP.')
 1100 FORMAT ('SKYDO: ERROR',I3,' CLOSING THE OUTPUT MAP.')
 1110 FORMAT ('Mean X-pixel shift:',F10.2,'  Std. Dev.:',F10.2)
 1120 FORMAT ('Mean y-pixel shift:',F10.2,'  Std. Dev.:',F10.2)
 1130 FORMAT ('Correlation coefficient:',F10.6)
      END
      SUBROUTINE SKTRAN (CATOUT, CRDPRM, NX2, JY2, KY2, XPX1, YPX1,
     *   IERR)
C-----------------------------------------------------------------------
C     SKTRAN transforms pixel coordinates for a number of consecutive
C     rows of the output map to the corresponding pixel coordinates in
C     the input map.
C
C     Given:
C          CATOUT(256) I     Catalogue header of the output map.
C          CRDPRM(11)  D     Parameters to transform coordinates from
C                            from the output map to the input map.
C          NX2         I     Output map size in pixels.
C          JY2,KY2     I     Range of Y-pixel values for the rows of
C                            the output map.
C     Given via common DSSHDR:
C          Digitized Sky Survey plate solution parameters.
C
C     Returned:
C          XPX1()      R     (X,Y) pixel coordinates in the input map
C      and YPX1()      R     corresponding to the rows of the output
C                            map.  These must be dimensioned to
C                            accomodate NX2*(KY2-JY2+1) coordinate
C                            pairs.
C          IERR        I     Error status
C                              0: success
C
C     Returned via common SKATS:
C          Transformation statistics.
C
C     Called:
C          APLSUB: {COPY, SETLOC, XYVAL, DSSPIX}
C          APLNOT: {CRDTRN}
C
C     Algorithm:
C          Computes the image coordinates at each pixel in the output
C          map using XYVAL, and transforms to the coordinate system of
C          the input map using CRDTRN.  It then converts to DSS pixel
C          coordinates using DSSPIX, and translates these to AIPS pixel
C          coordinates.
C
C     Notes:
C       1) The following is quoted directly from page 12 of the booklet
C          supplied with the Digitized Sky Survey CD set:
C
C            "The origin adopted in generating the astrometric solutions
C             is that the X,Y coordinates of the lower left hand corner
C             of the lower left hand pixel in a 14000 x 13999 pixel
C             image are (1.0,1.0).  Therefore, the center of the lower
C             left hand pixel is (1.5,1.5).  This is contrary to the
C             FITS standard (and the expectations of some popular image
C             analysis packages) which define the center coordinates of
C             the origin pixel to be (1.0,1.0).  If using software that
C             expects pixel centers to have integral coordinate values,
C             a (+0.5,+0.5) offset should be added to the measured X,Y
C             coordinates prior to computing celestial coordinates.
C             Failure to do so could result in a ~1".2 position error.
C
C            "One must also assure that any image display software
C             properly sets the absolute values of the origin
C             coordinates for the particular subimage being processed.
C             Some image display packages generate only relative
C             coordinates (i.e. the origin is always (0,0) or (1,1),
C             and, consequently, gross position errors could be
C             introduced.  The proper X,Y coordinates of the lower left
C             hand corner of the lower left hand pixel for any given
C             subimage are stored in the keywords CNPIX1 and CNPIX2
C             respectively."
C
C          The necessary correction is applied here.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1994/07/27  Code last modified; 1994/08/05
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   CATOUT(256), DEPTH(5), IB2, IE2, IERR, JY2, KY2, IX2,
     *          IY2, K2, NX2
      REAL      DX, DY, XPX1(*), XPX2, YPX1(*), YPX2
      DOUBLE PRECISION  CRDPRM(11), DEC, RA, ROTN, X2, XPIX, Y2, YPIX,
     *          Z2
C
      INCLUDE 'SKYVE.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
C
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
C                                 Set geometry for the output map.
      LOCNUM = 1
      CALL COPY (256, CATOUT, CATBLK)
      CALL SETLOC (DEPTH, .FALSE.)
C
      K2 = 1
      DO 40 IY2 = JY2, KY2
         YPX2 = FLOAT(IY2)
C
         DO 30 IB2 = 1, NX2, MAXIMG
            IE2 = MIN(IB2+MAXIMG-1, NX2)
C                                       Loop down this row of the output
C                                       map.
            DO 20 IX2 = IB2, IE2
               XPX2 = FLOAT(IX2)
C                                       Get the coordinates of this
C                                       pixel in the output map.
               CALL XYVAL (XPX2, YPX2, X2, Y2, Z2, IERR)
C                                       Transform to the coordinate
C                                       system of the input map.
               CALL CRDTRN (X2, Y2, CRDPRM, RA, DEC, ROTN)
C                                       Compute DSS pixel coordinates
C                                       in the input map.
               CALL DSSPIX (SCALE, XPIXSZ, YPIXSZ, RA0, DEC0, XOFF0,
     *            YOFF0, DSSX, DSSY, RA, DEC, XPIX, YPIX, IERR)
C                                       Compute AIPS pixel coords.
               XPX1(K2) = (XPIX - CNPIX1) + 0.5
               YPX1(K2) = (YPIX - CNPIX2) + 0.5
C                                       Accumulate statistics.
               SN  =  SN + 1
               DX  = IX2 - XPX1(K2)
               DY  = IY2 - YPX1(K2)
               SX  =  SX + DX
               SXX = SXX + DX*DX
               SXY = SXY + DX*DY
               SY  =  SY + DY
               SYY = SYY + DY*DY
C                                       Next.
               K2 = K2 + 1
 20         CONTINUE
 30      CONTINUE
 40   CONTINUE
C
      RETURN
      END
      SUBROUTINE DSSEQ (XPIXSZ, YPIXSZ, RA0, DEC0, XOFF0, YOFF0,
     *   DSSX, DSSY, XPIX, YPIX, RA, DEC)
C-----------------------------------------------------------------------
C     DSSEQ computes the J2000.0 equatorial coordinates of the specified
C     Digitized Sky Survey pixel coordinates.
C
C     Given:
C          XPIXSZ      D     Plate pixel size in X and Y, micron.
C      and YPIXSZ      D
C          RA0,DEC0    D     Plate centre J2000.0 right ascension and
C                            declination, in degrees.
C          XOFF0,YOFF0 D     Plate centre offsets, micron.
C          DSSX(13)    D     The coefficients for the plate solution for
C          DSSY(13)          computing standard plate coordinates
C                            (XI,ETA) in arcsec from plate offsets
C                            (X,Y), in mm.
C          XPIX,YPIX   D     DSS pixel coordinates.
C     Returned:
C          RA,DEC      D     Required J2000.0 right ascension and
C                            declination, in degrees.
C     Called:
C          none
C     Algorithm:
C          The equations for converting DSS pixel coordinates to
C          offsets from the plate centre are given on page 10 of the
C          booklet supplied with the Digitized Sky Survey CD set.
C
C          The equations for computing standard DSS plate coordinates
C          from plate offsets are given on page 11 of the booklet
C          supplied with the Digitized Sky Survey CD:
C
C             xi  = a1*x + a2*y + a3 + a4*x*x + a5*x*y + a6*y*y +
C                   a7*(x*x+y*y) + a8*x*x*x + a9*x*x*y + a10*x*y*y +
C                   a11*y*y*y + a12*x*(x*x+y*y) + a13*x*(x*x+y*y)**2
C
C             eta = b1*y + b2*x + b3 + b4*y*y + b5*x*y + b6*x*x +
C                   b7*(x*x+y*y) + b8*y*y*y + b9*x*y*y + b10*x*x*y +
C                   b11*x*x*x + b12*y*(x*x+y*y) + b13*y*(x*x+y*y)**2
C
C          The equations for computing J2000.0 right ascension and
C          declination from the standard coordinates are given on page
C          11 of the booklet supplied with the Digitized Sky Survey CD.
C          However, note that there is a misprint in the equation for
C          declination, the COS(RA0) term should be COS(RA-RA0).
C
C     Notes:
C       1)
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1994/08/03  Code last modified; 1994/08/04
C-----------------------------------------------------------------------
      DOUBLE PRECISION AS2R, CDEC, D2R, DEC, DEC0, DSSX(13), DSSY(13),
     *          ETA, F, PI, RA, RA0, TDEC, X, XI, XOFF0, XPIX, XPIXSZ,
     *          XX, XXYY, XY, Y, YOFF0, YPIX, YPIXSZ, YY
C
C     Pi.
      PARAMETER (PI = 3.141592653589793238462643D0)
C
C     Factor to convert degrees to radians.
      PARAMETER (D2R = PI/180D0)
C
C     Factor to convert arcsec to radians.
      PARAMETER (AS2R = D2R/3600D0)
C-----------------------------------------------------------------------
C                                       Compute offsets.
      X = (XOFF0 - XPIXSZ*XPIX)/1000D0
      Y = (YPIXSZ*YPIX - YOFF0)/1000D0
C                                       Compute temporaries.
      XX = X*X
      YY = Y*Y
      XY = X*Y
      XXYY = XX + YY
C                                       Compute standard coordinates.
      XI =     DSSX(3) +
     *      X*(DSSX(1) + X*(DSSX(4) + X*DSSX(8)))  +
     *      Y*(DSSX(2) + Y*(DSSX(6) + Y*DSSX(11))) +
     *     XY*(DSSX(5) + X*DSSX(9)  + Y*DSSX(10))  +
     *   XXYY*(DSSX(7) + X*DSSX(12) + DSSX(13)*X*XXYY)
      ETA =    DSSY(3) +
     *      Y*(DSSY(1) + Y*(DSSY(4) + Y*DSSY(8)))  +
     *      X*(DSSY(2) + X*(DSSY(6) + X*DSSY(11))) +
     *     XY*(DSSY(5) + Y*DSSY(9)  + X*DSSY(10))  +
     *   XXYY*(DSSY(7) + Y*DSSY(12) + DSSY(13)*Y*XXYY)
C                                       Convert to radians.
      XI  = XI*AS2R
      ETA = ETA*AS2R
C                                       Compute J2000.0 coordinates.
      CDEC = COS(DEC0*D2R)
      TDEC = TAN(DEC0*D2R)
      F = 1D0 - ETA*TDEC
      RA = ATAN((XI/CDEC)/F)/D2R + RA0
      DEC = ATAN(((ETA+TDEC)*COS((RA-RA0)*D2R))/F)/D2R
C
      RETURN
      END
      SUBROUTINE DSSPIX (SCALE, XPIXSZ, YPIXSZ, RA0, DEC0, XOFF0, YOFF0,
     *   DSSX, DSSY, RA, DEC, XPIX, YPIX, IERR)
C-----------------------------------------------------------------------
C     DSSPIX computes the pixel coordinates in a Digitized Sky Survey
C     plate corresponding to the specified J2000.0 equatorial
C     coordinate.  This requires inversion of the plate solution and
C     this it does by iteration.
C
C     Given:
C          SCALE       D     Approximate plate scale, arcsec/mm.
C          XPIXSZ      D     Plate pixel size in X and Y, micron.
C      and YPIXSZ      D
C          RA0,DEC0    D     Plate centre J2000.0 right ascension and
C                            declination, in degrees.
C          XOFF0,YOFF0 D     Plate centre offsets, micron.
C          DSSX(13)    D     The coefficients for the plate solution for
C          DSSY(13)          computing standard plate coordinates
C                            (XI,ETA) in arcsec from plate offsets
C                            (X,Y), in mm.
C          RA,DEC      D     Required J2000.0 right ascension and
C                            declination, in degrees.
C
C     Returned:
C          XPIX,YPIX   D     DSS pixel coordinates.
C          IERR        I     Error status, 0 means success.
C
C     Called:
C          SKYVE: {DSSCRD, EQSTD}
C
C     Algorithm:
C          The iteration formula is obtained by simultaneously solving
C          for DX and DY from the equations for the total differentials:
C
C              Dx = DX*dx/dX + Dy*dx/dY
C              Dy = DX*dy/dX + Dy*dy/dY
C
C          where
C
C                       x -> xi
C                       y -> eta
C                  DX, DY -> total differential of X and Y
C              d/dX, d/dY -> partial derivative with respect to X and Y
C
C          The equations for converting DSS pixel coordinates to
C          offsets from the plate centre are given on page 10 of the
C          booklet supplied with the Digitized Sky Survey CD set.
C
C     Notes:
C       1)
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1994/07/27  Code last modified; 1994/08/04
C-----------------------------------------------------------------------
      INTEGER   IERR, ITER, NITER
      DOUBLE PRECISION DEC, DEC0, DETA, DETADX, DETADY, DSSX(13),
     *          DSSY(13), DX, DXI, DXIDX, DXIDY, DY, ETA, ETA0, RA, RA0,
     *          SCALE, TOL, XOFF, XI, XI0, XOFF0, XPIX, XPIXSZ, YOFF,
     *          YOFF0, YPIX, YPIXSZ, Z
C-----------------------------------------------------------------------
C     IERR = 0
C                                       Initialize.
      NITER = 50
      TOL = (MIN(XPIXSZ,YPIXSZ)/100D0)/1000D0
C                                       Convert to standard coordinates.
      CALL EQSTD (RA0, DEC0, RA, DEC, XI0, ETA0, IERR)
C                                       Initial guess for plate offset.
      XOFF =  XI0/SCALE
      YOFF = ETA0/SCALE
C                                       Iterate.
      DO 10 ITER = 0, NITER
C                                       Compute standard coordinates
C                                       and their derivatives.
         CALL DSSCRD (DSSX, DSSY, XOFF, YOFF, XI, ETA, DXIDX, DXIDY,
     *      DETADX, DETADY, IERR)
C                                       Error terms.
         DXI   =  XI0 - XI
         DETA  = ETA0 - ETA
C                                       Compute correction.
         Z  = DXIDX*DETADY-DXIDY*DETADX
         DX = (DXI*DETADY - DETA*DXIDY)/Z
         DY = (DETA*DXIDX - DXI*DETADX)/Z
C                                       Apply correction.
         XOFF = XOFF + DX
         YOFF = YOFF + DY
C                                       Test for convergence.
         IF (ABS(DX).LT.TOL) THEN
            IF (ABS(DY).LT.TOL) GO TO 20
            END IF
 10   CONTINUE
C                                       Convert offsets to pixels.
 20   XPIX = (XOFF0 - 1000.0*XOFF)/XPIXSZ
      YPIX = (YOFF0 + 1000.0*YOFF)/YPIXSZ
C
      RETURN
      END
      SUBROUTINE DSSCRD (DSSX, DSSY, X, Y, XI, ETA, DXIDX, DXIDY,
     *   DETADX, DETADY, IERR)
C-----------------------------------------------------------------------
C     DSSCRD computes the standard DSS plate coordinates and their
C     partial derivatives for use in inverting the plate solution
C     equations.
C
C     Given:
C          DSSX(13)    D     The coefficients for the plate solution for
C          DSSY(13)          computing standard plate coordinates
C                            (XI,ETA) in arcsec from plate offsets
C                            (X,Y), in mm.
C          X,Y         D     (X,Y) plate offset, in mm.
C
C     Returned:
C          XI,ETA      D     Standard plate coordinates, in arcsec.
C          DXIDX       D     Derivative of XI  with respect to X.
C          DXIDY       D     Derivative of XI  with respect to Y.
C          DETADX      D     Derivative of ETA with respect to X.
C          DETADY      D     Derivative of ETA with respect to Y.
C          IERR        I     Error status, 0 means success.
C
C     Called:
C          none
C
C     Algorithm:
C          The equations for computing standard DSS plate coordinates
C          from plate offsets are given on page 11 of the booklet
C          supplied with the Digitized Sky Survey CD:
C
C             xi  = a1*x + a2*y + a3 + a4*x*x + a5*x*y + a6*y*y +
C                   a7*(x*x+y*y) + a8*x*x*x + a9*x*x*y + a10*x*y*y +
C                   a11*y*y*y + a12*x*(x*x+y*y) + a13*x*(x*x+y*y)**2
C
C             eta = b1*y + b2*x + b3 + b4*y*y + b5*x*y + b6*x*x +
C                   b7*(x*x+y*y) + b8*y*y*y + b9*x*y*y + b10*x*x*y +
C                   b11*x*x*x + b12*y*(x*x+y*y) + b13*y*(x*x+y*y)**2
C
C     Notes:
C       1) Adapted from the C function pltmodel() in the "getimage"
C          library supplied with the Digitized Sky Survey.  Note that
C          this routine has a bug in the computation of the derivative
C          of ETA with respect to Y.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1994/07/26  Code last modified; 1994/08/04
C-----------------------------------------------------------------------
      INTEGER   IERR
      DOUBLE PRECISION DETADX, DETADY, DSSX(13), DSSY(13), DXIDX, DXIDY,
     *          ETA, X, XI, XX, XXYY, XY, Y, YY
C-----------------------------------------------------------------------
      IERR = 0
C                                       Compute temporaries.
      XX = X*X
      YY = Y*Y
      XY = X*Y
      XXYY = XX + YY
C                                       Compute XI.
      XI =     DSSX(3) +
     *      X*(DSSX(1) + X*(DSSX(4) + X*DSSX(8)))  +
     *      Y*(DSSX(2) + Y*(DSSX(6) + Y*DSSX(11))) +
     *     XY*(DSSX(5) + X*DSSX(9)  + Y*DSSX(10))  +
     *   XXYY*(DSSX(7) + X*DSSX(12) + DSSX(13)*X*XXYY)
C                                       Derivative of XI wrt X.
      DXIDX =  DSSX(1) +
     *      X*(2.0*(DSSX(4) + DSSX(7)) + 3.0*X*(DSSX(8) + DSSX(12))) +
     *      Y*(DSSX(5) + Y*(DSSX(10) + DSSX(12))) +
     *      2.0*XY*DSSX(9) + XXYY*(4.0*XX + XXYY)*DSSX(13)
C                                       Derivative of XI wrt Y.
      DXIDY =  DSSX(2) +
     *      Y*(2.0*(DSSX(6) + DSSX(7)) + 3.0*Y*DSSX(11)) +
     *      X*(DSSX(5) + X*DSSX(9)) +
     *      2.0*XY*(DSSX(10) + DSSX(12)) + 4.0*XY*XXYY*DSSX(13)
C                                       Compute ETA.
      ETA =    DSSY(3) +
     *      Y*(DSSY(1) + Y*(DSSY(4) + Y*DSSY(8)))  +
     *      X*(DSSY(2) + X*(DSSY(6) + X*DSSY(11))) +
     *     XY*(DSSY(5) + Y*DSSY(9)  + X*DSSY(10))  +
     *   XXYY*(DSSY(7) + Y*DSSY(12) + DSSY(13)*Y*XXYY)
C                                       Derivative of ETA wrt X.
      DETADX = DSSY(2) +
     *      X*(2.0*(DSSY(6) + DSSY(7)) + 3.0*X*DSSY(11)) +
     *      Y*(DSSY(5) + Y*DSSY(9)) +
     *      2.0*XY*(DSSY(10) + DSSY(12)) + 4.0*XY*XXYY*DSSY(13)
C                                       Derivative of ETA wrt Y.
      DETADY = DSSY(1) +
     *      Y*(2.0*(DSSY(4) + DSSY(7)) + 3.0*Y*(DSSY(8) + DSSY(12))) +
     *      X*(DSSY(5) + X*(DSSY(10) + DSSY(12))) +
     *      2.0*XY*DSSY(9) + XXYY*(4.0*YY + XXYY)*DSSY(13)
C
      RETURN
      END
      SUBROUTINE EQSTD (RA0, DEC0, RA, DEC, XI, ETA, IERR)
C-----------------------------------------------------------------------
C     EQSTD converts J2000.0 equatorial coordinates to standard
C     coordinates on a Digitized Sky Survey plate.
C
C     Given:
C          RA0,DEC0    D     Plate centre J2000.0 right ascension and
C                            declination, in degrees.
C          RA,DEC      D     Required J2000.0 right ascension and
C                            declination, in degrees.
C
C     Returned:
C          XI,ETA      D     Standard plate coordinates, in arcsec.
C          IERR        I     Error status, 0 means success.
C
C     Called:
C          none
C
C     Algorithm:
C          The equations for computing J2000.0 right ascension and
C          declination from the standard coordinates are given on page
C          11 of the booklet supplied with the Digitized Sky Survey CD
C          and these are readily invertible.  However, note that there
C          is a misprint in the equation for declination, the COS(RA0)
C          term should be COS(RA-RA0).
C
C     Notes:
C       1) Adapted from the C function transeqstd() in the "getimage"
C          library supplied with the Digitized Sky Survey.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1994/07/26  Code last modified; 1994/08/05
C-----------------------------------------------------------------------
      INTEGER   IERR
      DOUBLE PRECISION CDEC, CDEC0, CDRA, D2R, DEC, DEC0, ETA, F, PI,
     *          RA, RA0, R2AS, SDEC, SDEC0, SDRA, XI, Z
C
C     Pi.
      PARAMETER (PI = 3.141592653589793238462643D0)
C
C     Factor to convert degrees to radians.
      PARAMETER (D2R = PI/180D0)
C
C     Factor to convert radians to arcsec.
      PARAMETER (R2AS = 180D0*3600D0/PI)
C-----------------------------------------------------------------------
      IERR = 0
C                                       Cache trigonometric evaluations.
      Z = DEC*D2R
      CDEC  = COS(Z)
      SDEC  = SIN(Z)
      Z = DEC0*D2R
      CDEC0 = COS(Z)
      SDEC0 = SIN(Z)
      Z = (RA-RA0)*D2R
      CDRA = COS(Z)
      SDRA = SIN(Z)
C                                       Compute common factor.
      F = R2AS/(SDEC*SDEC0 + CDEC*CDEC0*CDRA)
C                                       Compute standard coordinates.
      XI  = CDEC*SDRA*F
      ETA = (SDEC*CDEC0 - CDEC*SDEC0*CDRA)*F
C
      RETURN
      END
      SUBROUTINE RGINTP (CNO1, NX2, JY2, KY2, XPX1, YPX1, BUFF2, MAPMAX,
     *   MAPMIN, IERR)
C-----------------------------------------------------------------------
C     RGINTP reads through the input map and interpolates the pixel
C     values for a consecutive sequence of rows of the output map.
C
C     Given:
C          CNO1        I     Catalogue slot number of the input map.
C          NX2         I     Output map size in pixels.
C          JY2,KY2     I     Range of Y-pixel values for the rows of
C                            the output map.
C          XPX1()      R     (X,Y) pixel coordinates in the input map
C      and YPX1()      R     which correspond to this row of the output
C                            map.
C     Given via commons INPARM, INCHAR:
C          IUSER       I     AIPS user number.
C          INNAME      C*12  Input map name.
C          INCLAS      C*6   Input map class.
C          INSEQ       I     Input map sequence number.
C          INDISK      I     Input disk number.
C          DOZERO      L     Zero output blanked pixels if set.
C
C     Given and returned:
C          MAPMAX      R     Maximum output pixel value thus far.
C          MAPMIN      R     Minimum output pixel value thus far.
C
C     Returned:
C          BUFF2()     R     Output map IO buffer.
C          IERR        I     Error status
C                              0: success
C
C     Called:
C          SKYVE:  {INT3X3}
C          APLSUB: {IROUND, MAPCLS, MAPOPN, MDISK, MINIT, MSGWRT}
C
C     Algorithm:
C
C     Notes:
C       1)
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1990/07/11  Code last modified; 1990/07/30
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   MAX1X, MAX1X3
      PARAMETER (MAX1X  = 2*MAXIMG)
      PARAMETER (MAX1X3 = 3*MAX1X)
      INTEGER   BNDX1, BUFSZ, CATIN(256), CNO1, FIND1, IERR, IROUND,
     *   IX1, IX2, IY1, IY2, JY1, JY2, K1, K2, KY1, KY2, L1, LUN1, LY1,
     *   MY1, NDEFER, NR1, NX1, NX2, NY1, WBUFF(256), WIN(4)
      REAL      BUFF1(MAX1X3), BUFF2(1), DX, DY, MAPMAX, MAPMIN,
     *   V(-1:1,-1:1), XPX1(1), YPX1(1)
      CHARACTER PTYPE*2
      INCLUDE 'SKYVE.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN1 /16/
C-----------------------------------------------------------------------
C                                       Open the input map.
      PTYPE = 'MA'
      CALL MAPOPN ('READ', INDISK, INNAME, INCLAS, INSEQ, PTYPE, IUSER,
     *   LUN1, FIND1, CNO1, CATIN, WBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Set input buffer parameters.
      NX1 = CATIN(KINAX)
      NY1 = CATIN(KINAX+1)
      NR1 = ((3*MAX1X)/NX1) - 2
      IF (NR1.LT.1) THEN
         WRITE (MSGTXT,1020) NX1, MAX1X
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
         END IF
C
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX1
      WIN(4) = NY1
C                                       Force single-buffered IO.
      BUFSZ = NBPS*(1 + (2*NX1-1)/NBPS)
      CALL MINIT ('READ', LUN1, FIND1, NX1, NY1, WIN, BUFF1, BUFSZ,
     *   1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Read through the input map.
      MY1 = 1 + ((NY1 - MIN(NR1+2,NY1)) + (NR1-1))/NR1
      DO 100 LY1 = 1, MY1
         K1 = 1
         IF (LY1.EQ.1) THEN
C                                       First time through.
            JY1 = 1
            KY1 = MIN(NR1+2, NY1)
         ELSE
C                                       Copy the end of the previous
C                                       buffer to the start of this.
            L1 = NX1*((KY1-1)-JY1) + 1
            DO 50 IY1 = KY1-1, KY1
               DO 40 IX1 = 1, NX1
                  BUFF1(K1) = BUFF1(L1)
                  K1 = K1 + 1
                  L1 = L1 + 1
 40            CONTINUE
 50         CONTINUE
            JY1 = KY1 + 1
            KY1 = MIN(KY1+NR1, NY1)
            END IF
C                                       Fill the input buffers.
         DO 70 IY1 = JY1, KY1
            CALL MDISK ('READ', LUN1, FIND1, BUFF1(K1), BNDX1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1060) IERR
               CALL MSGWRT (8)
               GO TO 999
               END IF
            K1 = K1 + NX1
 70      CONTINUE
C
         IF (LY1.NE.1) JY1 = JY1 - 2
C                                       Interpolate in the input map.
         NDEFER = 0
         K2 = 1
         DO 90 IY2 = JY2, KY2
            DO 80 IX2 = 1, NX2
C                                       Find the nearest pixel.
               IX1 = IROUND(XPX1(K2))
               IY1 = IROUND(YPX1(K2))
C
               IF (IY1.LT.JY1) THEN
                  IF (JY1.EQ.1) THEN
C                                       Y-coordinate outside boundary of
C                                       input map.
                     BUFF2(K2) = FBLANK
                  ELSE
C                                       Done last time.
                     END IF
               ELSE IF (IY1.EQ.JY1 .AND. JY1.GT.1) THEN
C                                       Done last time.
               ELSE IF (IY1.GT.KY1) THEN
                  IF (KY1.EQ.NY1) THEN
C                                       Y-coordinate outside boundary of
C                                       input map.
                     BUFF2(K2) = FBLANK
                  ELSE
C                                       Defer till later.
                     NDEFER = NDEFER + 1
                     END IF
               ELSE IF (IY1.EQ.KY1 .AND. KY1.LT.NY1) THEN
C                                       Defer till next time.
                  NDEFER = NDEFER + 1
               ELSE IF (IX1.LT.1 .OR. IX1.GT.NX1) THEN
C                                       X-coordinate outside boundary of
C                                       input map.
                  BUFF2(K2) = FBLANK
               ELSE
                  K1 = NX1*(IY1-JY1) + IX1
                  IF (BUFF1(K1).EQ.FBLANK) THEN
C                                       Nearest input pixel is blanked.
                     BUFF2(K2) = FBLANK
                  ELSE
C                                       Get neighbouring pixel values.
                     K1 = K1 - NX1
                     IF (IY1-1.GE.JY1 .AND. IY1+1.LE.KY1 .AND.
     *                   IX1-1.GE.1   .AND. IX1+1.LE.NX1) THEN
C                                       Simple case, no edge nearby.
                        V(-1,-1) = BUFF1(K1-1)
                        V( 0,-1) = BUFF1(K1)
                        V(+1,-1) = BUFF1(K1+1)
C
                        K1 = K1 + NX1
                        V(-1,0) = BUFF1(K1-1)
                        V( 0,0) = BUFF1(K1)
                        V(+1,0) = BUFF1(K1+1)
C
                        K1 = K1 + NX1
                        V(-1,+1) = BUFF1(K1-1)
                        V( 0,+1) = BUFF1(K1)
                        V(+1,+1) = BUFF1(K1+1)
                     ELSE
C                                       Near the edge, must have some
C                                       blanking.
                        V(-1,-1) = FBLANK
                        V( 0,-1) = FBLANK
                        V(+1,-1) = FBLANK
                        IF (IY1-1.GE.JY1) THEN
                           IF (IX1-1.GE.1) V(-1,-1) = BUFF1(K1-1)
                           V(0,-1) = BUFF1(K1)
                           IF (IX1+1.LE.NX1) V(+1,-1) = BUFF1(K1+1)
                           END IF
C
                        K1 = K1 + NX1
                        V(-1,0) = FBLANK
                        V( 0,0) = FBLANK
                        V(+1,0) = FBLANK
                        IF (IX1-1.GE.1) V(-1,0) = BUFF1(K1-1)
                        V(0,0) = BUFF1(K1)
                        IF (IX1+1.LE.NX1) V(+1,0) = BUFF1(K1+1)
C
                        K1 = K1 + NX1
                        V(-1,+1) = FBLANK
                        V( 0,+1) = FBLANK
                        V(+1,+1) = FBLANK
                        IF (IY1+1.LE.KY1) THEN
                           IF (IX1-1.GE.1) V(-1,+1) = BUFF1(K1-1)
                           V(0,+1) = BUFF1(K1)
                           IF (IX1+1.LE.NX1) V(+1,+1) = BUFF1(K1+1)
                           END IF
                        END IF
C                                       Find the offset from the nearest
C                                       pixel.
                     DX  = XPX1(K2) - IX1
                     DY  = YPX1(K2) - IY1
C                                       Interpolate.
                     CALL INT3X3 (FBLANK, V, DX, DY, BUFF2(K2), IERR)
                     END IF
                  END IF
C                                       Output blanking control.
               IF (BUFF2(K2).EQ.FBLANK) THEN
                  IF (DOZERO) BUFF2(K2) = 0.0
               ELSE
C                                       Map extrema.
                  MAPMAX = MAX(BUFF2(K2), MAPMAX)
                  MAPMIN = MIN(BUFF2(K2), MAPMIN)
                  END IF
C
               K2 = K2 + 1
 80         CONTINUE
 90      CONTINUE
C                                       Any more to do?
         IF (NDEFER.EQ.0) GO TO 110
 100  CONTINUE
C                                       Close the input map file.
 110  CALL MAPCLS ('READ', INDISK, CNO1, LUN1, FIND1, CATIN, .TRUE.,
     *   WBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1120) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('RGINTP: ERROR',I3,' OPENING THE INPUT MAP.')
 1020 FORMAT ('RGINTP: INPUT IMAGE SIZE',I6,' EXCEEDS MAXIMUM',I6)
 1030 FORMAT ('RGINTP: ERROR',I3,' INITIALIZING THE INPUT MAP.')
 1060 FORMAT ('RGINTP: ERROR',I3,' READING THE INPUT MAP.')
 1120 FORMAT ('RGINTP: ERROR',I3,' CLOSING THE INPUT MAP.')
      END
      SUBROUTINE INT3X3 (FBLANK, V, DX, DY, VAL, IERR)
C-----------------------------------------------------------------------
C     INT3X3 does simple parabolic interpolation to find the pixel value
C     from its nearest neighbours.
C
C     Given:
C          FBLANK      R     Pixel blanking value.
C          V(3,3)      R     3x3 array if pixels surrounding the point
C                            to be interpolated.  Dimensioned as
C                            V(-1:1,-1:1) with the subscripts in (X,Y).
C          DX,DY       R     Offset in pixel units from V(0,0) of the
C                            point to be interpolated.
C
C     Returned:
C          VAL         R     The interpolated value.
C          IERR        I     Error status
C                              0: success
C                              1: input error
C
C     Called:
C          None
C
C     Algorithm:
C          Does a parabolic interpolation in X across the rows, and then
C          interpolates the the results in Y.  This is a somewhat ad hoc
C          procedure, and the result would be slightly different if the
C          interpolation was done in the reverse order.
C
C          In the presence of blanked pixels a first- or zeroth-order
C          interpolation is done if necessary.
C
C     Notes:
C       1) The simple case where no pixels are blanked is handled
C          separately for speed.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1990/07/25  Code last modified; 1990/07/25
C-----------------------------------------------------------------------
      INTEGER   I, IERR, J, NBLNK
      REAL      B, B1X, B2X, DX, DY, FBLANK, V(-1:1,-1:1), VAL, W(-1:1)
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 0
      IF (V(0,0).EQ.FBLANK .OR. ABS(DX).GT.0.5 .OR. ABS(DY).GT.0.5) THEN
         IERR = 1
         VAL = FBLANK
         GO TO 999
         END IF
C                                       Count blank pixels.
      NBLNK = 0
      DO 20 J = -1, 1
         DO 10 I = -1, 1
            IF (V(I,J).EQ.FBLANK) NBLNK = NBLNK + 1
 10      CONTINUE
 20   CONTINUE
C
      IF (NBLNK.EQ.0) THEN
C                                       Simple case, no blanking.
         DO 30 J = -1, 1
C                                       Interpolate in X.
            B = (V(1,J)-V(-1,J))/2.0
            W(J) = V(0,J) + (B + (V(1,J)-V(0,J)-B)*DX)*DX
 30      CONTINUE
C                                       Interpolate in Y.
         B = (W(1)-W(-1))/2.0
         VAL = W(0) + (B + (W(1)-W(0)-B)*DY)*DY
      ELSE
C                                       There must be some blanks.
         DO 40 J = -1, 1
C                                       Interpolate in X.
            W(J) = FBLANK
            IF (V(0,J).NE.FBLANK) THEN
C                                       Zeroth-order approximation.
               W(J) = V(0,J)
C                                       First-order approximation.
               IF (V(-1,J).NE.FBLANK) THEN
                  B1X = (V(0,J)-V(-1,J))*DX
                  W(J) = W(J) + B1X
                  END IF
C
               IF (V(+1,J).NE.FBLANK) THEN
                  B2X = (V(1,J)-V( 0,J))*DX
                  W(J) = W(J) + B2X
                  END IF
C                                       Second-order approximation.
               IF (V(-1,J).NE.FBLANK .AND. V(1,J).NE.FBLANK) THEN
                  W(J) = W(J) - ((B1X+B2X) + (B1X-B2X)*DX)/2.0
                  END IF
               END IF
 40      CONTINUE
C                                       Interpolate in Y.
         VAL = FBLANK
         IF (W(0).NE.FBLANK) THEN
C                                       Zeroth-order approximation.
            VAL = W(0)
C                                       First-order approximation.
            IF (W(-1).NE.FBLANK) THEN
               B1X = (W(0)-W(-1))*DX
               VAL = VAL + B1X
               END IF
C
            IF (W(+1).NE.FBLANK) THEN
               B2X = (W(1)-W( 0))*DX
               VAL = VAL + B2X
               END IF
C                                       Second-order approximation.
            IF (W(-1).NE.FBLANK .AND. W(1).NE.FBLANK) THEN
               VAL = VAL - ((B1X+B2X) + (B1X-B2X)*DX)/2.0
               END IF
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE SKYHI (CNO1, CNO2, CATOUT, CRD2, CRDPRM, IERR)
C-----------------------------------------------------------------------
C     SKYHI writes the history file for SKYVE.
C
C     Given:
C          CNO1        I     Catalogue slot number of the input map.
C          CNO2        I     Catalogue slot number of the output map.
C          CATOUT(256) I     Catalogue header of the output map.
C          CRD2        C*40  Output coordinate system.
C          CRDPRM(11)  D     Parameters to transform coordinates from
C                            from the output map to the input map.
C
C     Given via common INPARM:
C          INDISK      I     Input disk number.
C          OUTNAM      C*12  Output map name.
C          OUTCLS      C*6   Output map class.
C          OUTSEQ      I     Output map sequence number.
C          OUTDSK      I     Output disk.
C
C     Returned:
C          IERR        I     Error status
C                              0: success
C                              1: input error
C
C     Called:
C          APLSUB: {COORDD, COPY, H2CHR, HENCOO, HICLOS, HIADD, HIINIT,
C                   HISCOP, MSGWRT}
C
C     Algorithm:
C
C     Notes:
C       1)
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1990/08/16  Code last modified; 1990/08/16
C-----------------------------------------------------------------------
      INTEGER   CATOUT(256), CNO1, CNO2, HIBUFF(256), HM(2), IERR, J,
     *          LUNHI1, LUNHI2, WBUFF(256)
      REAL      CRDINC, SEC
      DOUBLE PRECISION  CRDPRM(11)
      CHARACTER CHSIGN*1, CRD2*40, CTYPE*8, FM*80, HTXT*72, STYP*4
      INCLUDE 'SKYVE.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUNHI1, LUNHI2 /26, 27/
C-----------------------------------------------------------------------
C                                       copy keywords
      CALL KEYCOP (INDISK, CNO1, OUTDSK, CNO2, IERR)
C                                       Initialize history.
      CALL HIINIT (2)
      CALL HISCOP (LUNHI1, LUNHI2, INDISK, OUTDSK, CNO1, CNO2, CATOUT,
     *   WBUFF, HIBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
      CALL HENCOO (TSKNAM, OUTNAM, OUTCLS, OUTSEQ, OUTDSK, LUNHI2,
     *   HIBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
      HTXT = TSKNAM
C                                       Output coordinate axes.
      HTXT(7:) = '-------------------------------' //
     *         '-------------------------------'
      CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C
      HTXT(7:) = 'Output coordinate system: ' // CRD2
      CALL COPY (256, CATOUT, CATBLK)
      CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C
      HTXT(7:) = 'Type    Pixels   Coord value  at Pixel' //
     *         '    Coord incr   Rotat'
      CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C
      DO 40 J = 1, 2
C                                       Get the axis type, RA and DEC
C                                       are treated specially.
         CALL H2CHR (8, 1, CATH(KHCTP+(J-1)*2), CTYPE)
         STYP = CTYPE(1:4)
C
         FM = ' '
         IF (STYP.EQ.'RA  ' .OR. STYP.EQ.'RA--') THEN
C                                       RA axis of some type.
            CALL COORDD (1, CATD(KDCRV-1+J), CHSIGN, HM, SEC)
            CRDINC = CATR(KRCIC-1+J)*3600.0
            IF (ABS(CRDINC).GE.1.0) THEN
               FM = '(A8,I6,2X,A1,1X,I2.2,I3.2,F7.3,F8.2,F14.3,F8.2)'
            ELSE
               FM = '(A8,I6,2X,A1,1X,I2.2,I3.2,F7.3,F8.2,F14.6,F8.2)'
               END IF
         ELSE IF (STYP.EQ.'DEC ' .OR. STYP.EQ.'DEC-') THEN
C                                       DEC axis of some type.
            CALL COORDD (2, CATD(KDCRV-1+J), CHSIGN, HM, SEC)
            CRDINC = CATR(KRCIC-1+J) * 3600.0
            IF (ABS(CRDINC).GE.1.0) THEN
               FM = '(A8,I6,3X,A1,I2.2,I3.2,F7.3,F8.2,F14.3,F8.2)'
            ELSE
               FM = '(A8,I6,3X,A1,I2.2,I3.2,F7.3,F8.2,F14.6,F8.2)'
               END IF
            END IF
C
         IF (FM.NE.' ') THEN
            WRITE (HTXT(7:),FM) CTYPE, CATBLK(KINAX-1+J), CHSIGN,
     *         HM, SEC, CATR(KRCRP-1+J), CRDINC, CATR(KRCRT-1+J)
            IF (HTXT(31:31).EQ.' ') HTXT(31:31) = '0'
         ELSE
C                                       Not an RA or DEC axis.
            WRITE (HTXT(7:),1030) CTYPE, CATBLK(KINAX-1+J),
     *         CATD(KDCRV-1+J), CATR(KRCRP-1+J), CATR(KRCIC-1+J),
     *         CATR(KRCRT-1+J)
            END IF
C
         CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
         IF (IERR.NE.0) GO TO 990
 40   CONTINUE
C
      HTXT(7:) = '-------------------------------' //
     *         '-------------------------------'
      CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Record the transformation
C                                       parameters.
      HTXT(7:) = 'Coordinate transformation parameters ' //
     *           '(see EXPLAIN SKYVE).'
      CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C
      IF (CRDPRM(6).NE.0D0) THEN
         HTXT(7:) = 'E-terms removed from the output coordinates (deg):'
         CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
         IF (IERR.NE.0) GO TO 990
C
         WRITE (HTXT(7:),1050) (CRDPRM(J), J=6,8)
         CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C
      HTXT(7:) = 'Euler angles for the coordinate rotation (deg):'
      CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C
      WRITE (HTXT(7:),1050) (CRDPRM(J), J=1,3)
      CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C
      IF (CRDPRM(9).NE.0D0) THEN
         HTXT(7:) = 'E-terms added to the input coordinates (deg):'
         CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
         IF (IERR.NE.0) GO TO 990
C
         WRITE (HTXT(7:),1050) (CRDPRM(J), J=9,11)
         CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C
      HTXT(7:) = '-------------------------------' //
     *         '-------------------------------'
      CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C
C                                       Clean up
      CALL HICLOS (LUNHI2, .TRUE., HIBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Error exit.
 990  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1991) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SKYHI: ERROR',I3,' CREATING HISTORY FILE.')
 1020 FORMAT ('SKYHI: ERROR',I3,' UPDATING THE HISTORY FILE.')
 1030 FORMAT (A8,I6,2X,1PE14.7,0PF8.2,1PE14.7,0PF8.2)
 1050 FORMAT (3F20.15)
 1060 FORMAT ('SKYHI: ERROR',I3,' CLOSING THE HISTORY FILE.')
 1991 FORMAT ('SKYHI: ERROR',I3,' WRITING THE HISTORY FILE.')
      END
