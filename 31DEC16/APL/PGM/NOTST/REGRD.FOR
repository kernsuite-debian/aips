LOCAL INCLUDE 'REGRD.INC'
C                                       Local common for REGRD.
      LOGICAL   DOZERO
      INTEGER   IMSIZE(2), INSEQ, INDISK, IUSER, OUTDSK, OUTSEQ
      REAL      APARM(10), BPARM(10), CPARM(10)
      CHARACTER INNAME*12, INCLAS*6, OUTNAM*12, OUTCLS*6
      COMMON /INPARM/ IUSER, INSEQ, INDISK, OUTSEQ, OUTDSK, IMSIZE,
     *   APARM, BPARM, CPARM, DOZERO
      COMMON /INCHAR/ INNAME, INCLAS, OUTNAM, OUTCLS
LOCAL END
      PROGRAM REGRD
C-----------------------------------------------------------------------
C! Regrid an image from one coordinate frame and geometry to another.
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2002, 2005, 2008-2009, 2013, 2015
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
C   REGRD regrids an image from one coordinate frame and geometry to
C   a second frame and geometry specified by adverb values.
C   Adverbs:
C      INNAME     Input map name.
C      INCLASS    Input map class.
C      INSEQ      Input map sequence number.
C      INDISK     Input disk number.
C      OUTNAME    Output map name.
C      OUTCLASS   Output map class.
C      OUTSEQ     Output map sequence number.
C      OUTDISK    Output disk number.
C      IMSIZE     Image size in pixels.
C      APARM      Input map parameters, (if not specified in the map
C                 header)
C                    1) epoch of mean coordinates
C                    2) epoch prefix
C                       1: Julian    (eg J2000.0)
C                       2: Besselian (eg B1950.0)
C                       3: Besselian without E-terms (eg b1950)
C      BPARM      Output map parameters
C                    1) coordinate system
C                       1: equatorial (mean of epoch)
C                       2: galactic
C                       3: ecliptic (mean of epoch)
C                    2) epoch of mean coordinates
C                    3) epoch prefix
C                       1: Julian    (eg J2000.0)
C                       2: Besselian (eg B1950.0)
C                    4) projection,
C                       1: SIN, sine (orthographic)
C                       2: TAN, tangent (gnomonic)
C                       3: ARC, arc projection (zenithal equidistant)
C                       4: NCP, north celestial pole tangent projection
C                       5: STG, stereographic
C                       6: AIT, Aitov
C                       7: GLS, global sinusoid (Sanson-Flamsteed)
C                       8: MER, Mercator
C                    5) Code for blanking on output,
C                       0: indefinite
C                       1: zero
C      CPARM      Output axis specification.
C                 1-5: First axis,
C                       1: reference value, hour (or degree)
C                       2: reference value, min  (or arcmin)
C                       3: reference value, sec  (or arcsec)
C                       4: reference pixel
C                       5: coordinate increment (arcsec)
C                    6-10: Second axis, similarly.
C   Called:
C      REGRD:  {REGRHI, RGRDO, RGRINI, RGRSET}
C      APLSUB: {DIE, MSGWRT}
C   Author: Mark Calabretta, Australia Telescope.
C      Origin; 1985/Jun/03  Code last modified; 1990/Aug/16
C-----------------------------------------------------------------------
      INTEGER   BUFF(256), CATIN(256), CATOUT(256), CNO1, CNO2, IERR
      DOUBLE PRECISION  CRDPRM(11)
      CHARACTER CRD1*40, CRD2*40, EPRFX2*8
      INCLUDE 'REGRD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Task initiation.
      CALL RGRINI (CNO1, CATIN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1010) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Initialize the transformation.
      CALL RGRSET (CNO1, CATIN, CRD1, CRD2, CATOUT, EPRFX2, CRDPRM,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1020) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Do the transformation, writing
C                                       the map out in the process.
      CALL RGRDO (CNO1, CATIN, EPRFX2, CRDPRM, CATOUT, CNO2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1030) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Do history.
      CALL REGRHI (CNO1, CATIN, CRD1, CNO2, CATOUT, CRD2, CRDPRM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1040) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Clean up.
 999  CALL DIE (IERR, BUFF)
      STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I3,' INITIATING REGRD.')
 1020 FORMAT ('ERROR',I3,' INITIALIZING THE TRANSFORMATION.')
 1030 FORMAT ('ERROR',I3,' WRITING OUTPUT MAP.')
 1040 FORMAT ('ERROR',I3,' WRITING HISTORY.')
      END
      SUBROUTINE RGRINI (CNO1, CATIN, IERR)
C-----------------------------------------------------------------------
C   RGRINI reads adverbs and gets the input map header.
C   Given:
C      none
C   Returned:
C      CNO1        I     Catalogue slot number of the input map.
C      CATIN(256)  I     Catalogue header of the input map.
C      IERR        I     Error status, 0 means success.
C
C   Returned via commons INPARM, INCHAR:
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
C          APARM(10)   R     Input epoch defaults.
C          BPARM(10)   R     Output geometry specification.
C          CPARM(10)   R     Output axis specification.
C          DOZERO      L     Zero output blanked pixels if set.
C
C     Called:
C          APLSUB: {GTPARM, H2CHR, IROUND, MSGWRT, RELPOP, VHDRIN}
C          APLGEN: {ZDCHIN}
C-----------------------------------------------------------------------
      LOGICAL   RQUICK
      INTEGER   CATIN(256), CNO1, IERR, IRET, IROUND, J, NPARM
      REAL      BUFF(256), XAPARM(10), XBPARM(10), XCPARM(10),
     *   XIMSIZ(2), XINDSK, XINSEQ, XOUTDI, XOUTSQ
      HOLLERITH XINCLS(2), XINNAM(3), XOUTCL(2), XOUTNA(3)
      CHARACTER PRGNAM*6, PTYPE*2, STAT*4
      INCLUDE 'REGRD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /RGRINP/ XINNAM, XINCLS, XINSEQ, XINDSK, XOUTNA, XOUTCL,
     *   XOUTSQ, XOUTDI, XIMSIZ, XAPARM, XBPARM, XCPARM
      DATA PRGNAM /'REGRD '/
C-----------------------------------------------------------------------
C                                       Initialize commons.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Get input parameters.
      NPARM = 46
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
         APARM(J) = XAPARM(J)
         BPARM(J) = XBPARM(J)
         CPARM(J) = XCPARM(J)
 30      CONTINUE
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
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I3,' READING ADVERB VALUES.')
 1020 FORMAT ('ERROR',I3,' RESTARTING AIPS.')
 1040 FORMAT ('RGRINI: ERROR',I3,' SEARCHING FOR INPUT MAP HEADER.')
 1050 FORMAT ('RGRINI: ERROR',I3,' READING INPUT MAP HEADER.')
      END
      SUBROUTINE RGRSET (CNO1, CATIN, CRD1, CRD2, CATOUT, EPRFX2,
     *   CRDPRM, IERR)
C-----------------------------------------------------------------------
C     RGRSET sets parameters for the coordinate transformation and
C     constructs the catalogue header for the output map.
C
C     Given:
C          CNO1        I     Catalogue slot number of the input map.
C          CATIN(256)  I     Catalogue header of the input map.
C
C     Given via common INPARM:
C          INDISK      I     Input disk number.
C          IMSIZE(2)   I     Image size in pixels.
C          APARM(10)   R     Input epoch defaults.
C          BPARM(10)   R     Output geometry specification.
C          CPARM(10)   R     Output axis specification.
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
C          APLSUB: {CATKEY, CHR2H, COPY, H2CHR, MSGWRT, SETLOC, XYVAL}
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
C-----------------------------------------------------------------------
      INTEGER   CATIN(256), CATOUT(256), CNO1, DEPTH(5), IBUFF(256),
     *   IERR, IFRAME, IGEOM, IPRFX, IROUND, KEYTYP, LOCS,
     *   NUMKEY, OGEOM, OFRAME
      REAL      EPOCH1, EPOCH2, SCL, XPX1, YPX1
      HOLLERITH HBUFF(2)
      DOUBLE PRECISION  CRDPRM(11), LAT, LNG, ROTN, X1, X2, Y1, Y2, Z1
      CHARACTER CRD1*40, CRD2*40, EPRFX1*1, EPRFX2*8, FRAME(2,3)*4,
     *   GEOM(11)*4
      INCLUDE 'REGRD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA DEPTH /5*1/
      DATA FRAME /'RA--', 'DEC-', 'GLON', 'GLAT', 'ELON', 'ELAT'/
      DATA GEOM  / '-SIN', '-TAN', '-ARC', '-NCP', '-STG',
     *   '-GLS', '-MER', '-AIT', '-CAR', '-MOL', '-PAR'/
C-----------------------------------------------------------------------
C                                       Initialize.
      IERR = 0
C                                       Determine the input coordinate
C                                       system and map projection.
C                                       Copy CATIN to CATBLK for SETLOC.
      CALL COPY (256, CATIN, CATBLK)
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
C                                       X-Y not a position pair.
      IF (AXTYP(LOCNUM).NE.1) THEN
         IERR = 1
         MSGTXT = 'RGRSET: INCONSISTENT COORDINATE AXIS PAIR.'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Coordinates at the centre of
C                                       the input map.
      XPX1 = (CATBLK(KINAX)+1)/2.0
      YPX1 = (CATBLK(KINAX+1)+1)/2.0
      CALL XYVAL (XPX1, YPX1, X1, Y1, Z1, IERR)
C                                       Check input coordinate types.
C                                       Ecliptic coordinates.
      IF (LABTYP(LOCNUM).EQ.21) THEN
         CRD1 = 'Ecliptic'
         IFRAME = 3
C                                       Galactic coordinates.
      ELSE IF (LABTYP(LOCNUM).EQ.43) THEN
         CRD1 = 'Galactic'
         IFRAME = 2
C                                       Equatorial coordinates.
      ELSE IF (LABTYP(LOCNUM).EQ.65) THEN
         CRD1 = 'Equatorial'
         IFRAME = 1
      ELSE
         IERR = 2
         MSGTXT = 'RGRSET: UNRECOGNIZED INPUT COORDINATE AXIS PAIR.'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Coordinate epoch.
      IF ((IFRAME.EQ.1) .OR. (IFRAME.EQ.3)) THEN
         EPOCH1 = CATR(KREPO)
         IF (EPOCH1.LE.0.0) EPOCH1 = APARM(1)
         IF (EPOCH1.LE.0.0) EPOCH1 = 2000.0
C
         NUMKEY = 1
         KEYTYP = 3
         CALL CATKEY ('REED', INDISK, CNO1, 'EPOCPRFX', NUMKEY, LOCS,
     *      HBUFF, KEYTYP, IBUFF, IERR)
         IF (IERR.NE.0) THEN
            IERR = 3
            MSGTXT = 'RGRSET: ERROR OBTAINING THE EPOCH PREFIX'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C
         EPRFX1 = ' '
         IF (LOCS.EQ.-1) THEN
C                                       Keyword not found.
            IPRFX  = IROUND(APARM(2))
            IF (IPRFX.EQ.1) THEN
               EPRFX1 = 'J'
            ELSE IF (IPRFX.EQ.2) THEN
               EPRFX1 = 'B'
            ELSE IF (IPRFX.EQ.3) THEN
               EPRFX1 = 'b'
            ELSE
               IF (EPOCH1.EQ.1950.0) THEN
                  EPRFX1 = 'B'
               ELSE
                  EPRFX1 = 'J'
                  END IF
               END IF
         ELSE
C                                       Decode the keyword.
            CALL H2CHR (1, 1, HBUFF, EPRFX1)
            IF (EPRFX1.NE.'J' .AND. EPRFX1.NE.'B' .AND.
     *          EPRFX1.NE.'b') THEN
               IERR = 3
               MSGTXT = 'RGRSET: BAD EPOCH PREFIX, ' // EPRFX1
               CALL MSGWRT (8)
               GO TO 999
               END IF
            END IF
C
         IF (IFRAME.EQ.1) WRITE (CRD1(12:),1000) EPRFX1, EPOCH1
         IF (IFRAME.EQ.3) WRITE (CRD1(10:),1000) EPRFX1, EPOCH1
         END IF
C                                       Check the input map projection.
      IGEOM = AXFUNC(1,LOCNUM) - 1
      IF ((IGEOM.LT.1) .OR. (IGEOM.GT.11)) THEN
         IERR = 4
         MSGTXT = 'RGRSET: UNRECOGNIZED INPUT MAP GEOMETRY.'
         CALL MSGWRT (8)
         GO TO 999
      ELSE IF (AXFUNC(1,LOCNUM).NE.AXFUNC(2,LOCNUM)) THEN
         IERR = 5
         MSGTXT = 'RGRSET: INCONSISTENT INPUT MAP GEOMETRY.'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Determine the transformation to
C                                       be done.
      OFRAME = IROUND(BPARM(1))
C                                       Output frame defaults to input.
      IF (OFRAME.EQ.0) THEN
         OFRAME = IFRAME
         END IF
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
         IF (EPOCH2.LE.0.0) EPOCH2 = EPOCH1
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
            IF (EPRFX1.NE.' ') THEN
               EPRFX2 = EPRFX1
            ELSE IF (EPOCH2.EQ.1950.0) THEN
               EPRFX2 = 'B'
            ELSE
               EPRFX2 = 'J'
               END IF
            END IF
C
         IF (OFRAME.EQ.1) WRITE (CRD2(12:),1000) EPRFX2, EPOCH2
         IF (OFRAME.EQ.3) WRITE (CRD2(10:),1000) EPRFX2, EPOCH2
         END IF
C                                       Check that the output geometry
C                                       is valid.
      OGEOM = IROUND (BPARM(4))
C                                       Output geometry defaults to
C                                       input.
      IF (OGEOM.LE.0) OGEOM = IGEOM
C
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
      IF ((LNG.EQ.0.0D0) .AND. (LAT.EQ.0.0D0) .AND. (CPARM(4).EQ.0.)
     *   .AND. (CPARM(5).EQ.0.0) .AND. (CPARM(9).EQ.0.0) .AND.
     *   (CPARM(10).EQ.0.0D0)) THEN
         LNG = -400.0D0
         LAT = -100.0D0
         END IF
C                                       Set the transformation
C                                       parameters..
      CALL CRDSET (CRD1, CRD2, CRDPRM, IERR)
      IF (IERR.NE.0) THEN
         IERR = 8
         MSGTXT = 'RGRSET: ERROR TRANSFORMING HEADER COORDINATES.'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       ...and do it.
      CALL CRDTRN (X1, Y1, CRDPRM, X2, Y2, ROTN)
      IF (LNG.LT.-360D0 .OR. LNG.GT.+360D0) LNG = X2
      IF (LAT.LT.-90D0  .OR. LAT.GT.90D0)   LAT = Y2
C                                       pointing position
      IF ((IFRAME.EQ.1) .AND. (OFRAME.EQ.1)) THEN
         X1 = CATD(KDORA)
         Y1 = CATD(KDODE)
         CALL CRDTRN (X1, Y1, CRDPRM, X2, Y2, ROTN)
         CATD(KDORA) = X2
         CATD(KDODE) = Y2
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
         SCL = REAL(CATIN(KINAX)-1)/REAL(CATBLK(KINAX)-1)
         CATR(KRCIC) = CATR(KRCIC)*SCL
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
         SCL = REAL(CATIN(KINAX+1)-1)/REAL(CATBLK(KINAX+1)-1)
         CATR(KRCIC+1) = CATR(KRCIC+1)*SCL
      ELSE
         CATR(KRCIC+1) = CPARM(10)/3600.0
         END IF
C
      CATR(KRCRT+1) = 0.0
C                                       Coordinate epoch (equinox).
      CATR(KREPO) = EPOCH2
C                                       Indeterminate pixel value.
      CATR(KRBLK) = FBLANK
C                                       Copy it to CATOUT.
      CALL COPY (256, CATBLK, CATOUT)
C                                       Parameters to transform from
C                                       the output map to the input map.
      CALL CRDSET (CRD2, CRD1, CRDPRM, IERR)
      IF (IERR.NE.0) THEN
         IERR = 9
         MSGTXT = 'RGRSET: ERROR DEFINING COORDINATE TRANSFORMATION.'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A1,F6.1)
 1010 FORMAT ('RGRSET: INVALID COORDINATE SYSTEM, BPARM(1) = ',I4)
 1020 FORMAT ('RGRSET: INVALID OUTPUT GEOMETRY, BPARM(4) = ',I4)
      END
      SUBROUTINE RGRDO (CNO1, CATIN, EPRFX2, CRDPRM, CATOUT, CNO2, IERR)
C-----------------------------------------------------------------------
C   RGRDO transforms from the coordinate system and map projection, of
C   one map to that of another, storing the output map in the process.
C   Given:
C          CNO1        I     Catalogue slot number of the input map.
C          CATIN(256)  I     Catalogue header of the input map.
C          EPRFX2      C*8   Epoch prefix
C                               J = Julian
C                               B = Besselian
C                               b = Besselian without e-terms
C          CRDPRM(11)  D     Parameters to transform coordinates from
C                            from the output map to the input map.
C   Given via common INPARM:
C          INNAME      C*12  Input map name.
C          INCLAS      C*6   Input map class.
C          INSEQ       I     Input map sequence number.
C          OUTNAM      C*12  Output map name.
C          OUTCLS      C*6   Output map class.
C          OUTSEQ      I     Output map sequence number.
C   Given and returned:
C          CATOUT(256) I     Catalogue header of the output map.
C   Returned:
C          CNO2        I     Catalogue slot number of the output map.
C          IERR        I     Error status
C                              0: success
C   Called:
C          REGRD:  {RGINTP}
C          APLSUB: {CATIO, CATKEY, CHR2H, COPY, HIINIT, HISCOP, HENCOO,
C                   HICLOS, MAKOUT, MAPCLS, MAPOPN, MCREAT, MDISK,
C                   MINIT, MSGWRT}
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   MAX2X
      PARAMETER (MAX2X = 2*MAXIMG)
      INTEGER   BUFSZ, BNDX2, CATIN(256), CATOUT(256), CNO1, CNO2, IERR,
     *   FIND2, IY2, J2, JY2, K2, KEYTYP, KY2, LOCS, LUN2, NR2, NUMKEY,
     *   NX2, NY2, WBUFF(256), WIN(4), INDX7, INDX6, INDX5, INDX4,
     *   INDX3, IBLKOF, DEPTH(5)
      REAL      BUFF2(MAX2X), MAPMAX, MAPMIN, XPX1(MAX2X), YPX1(MAX2X)
      HOLLERITH HBUFF(2)
      DOUBLE PRECISION  CRDPRM(11)
      CHARACTER EPRFX2*8, PTYPE*2
      INCLUDE 'REGRD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (DEPTH(1), INDX3), (DEPTH(2), INDX4), (DEPTH(3),
     *   INDX5), (DEPTH(4), INDX6), (DEPTH(5), INDX7)
      DATA LUN2 /17/
C-----------------------------------------------------------------------
C                                       Create the output map.
      IF (IMSIZE(1).GT.MAXIMG) THEN
         IMSIZE(1) = MAXIMG
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
C                                      Create file.
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
            MSGTXT = 'RGRDO: ERROR RECORDING THE EPOCH PREFIX.'
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
C                                       Set output buffer parameters.
      NR2 = MAX2X/NX2
      IF (2*(MAX2X-NX2*(NR2-1)).LT.BUFSZ) NR2 = (2*MAX2X-NBPS)/(2*NX2)
      MAPMAX = -1E30
      MAPMIN =  1E30
C                                       Update the catalogue header for
C                                       the output map.
      CALL COPY (256, CATBLK, CATOUT)
      CATOUT(KINAX+6) = MAX (1, CATOUT(KINAX+6))
      CATOUT(KINAX+5) = MAX (1, CATOUT(KINAX+5))
      CATOUT(KINAX+4) = MAX (1, CATOUT(KINAX+4))
      CATOUT(KINAX+3) = MAX (1, CATOUT(KINAX+3))
      CATOUT(KINAX+2) = MAX (1, CATOUT(KINAX+2))
C                                       loop over planes
      DO 100 INDX7 = 1,CATOUT(KINAX+6)
      DO 100 INDX6 = 1,CATOUT(KINAX+5)
      DO 100 INDX5 = 1,CATOUT(KINAX+4)
      DO 100 INDX4 = 1,CATOUT(KINAX+3)
      DO 100 INDX3 = 1,CATOUT(KINAX+2)
         CALL COPY (256, CATOUT, CATBLK)
         LOCNUM = 1
         CALL SETLOC (DEPTH, .FALSE.)
         CALL COPY (256, CATIN, CATBLK)
         LOCNUM = 2
         CALL SETLOC (DEPTH, .FALSE.)
C                                       Initialize output IO, force
C                                       single-buffering.
         BUFSZ = NBPS*(1 + (2*NX2-1)/NBPS)
C                                       Block offset for source file.
         CALL COMOFF (CATOUT(KIDIM), CATOUT(KINAX), DEPTH, IBLKOF, IERR)
         IBLKOF = IBLKOF + 1
         CALL MINIT ('WRIT', LUN2, FIND2, NX2, NY2, WIN, BUFF2, BUFSZ,
     *      IBLKOF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Pre-write the output buffer.
         CALL MDISK ('WRIT', LUN2, FIND2, BUFF2, BNDX2, IERR)
         IF (BNDX2.NE.1) IERR = 9
         IF (IERR.NE.0 .OR. BNDX2.NE.1) THEN
            WRITE (MSGTXT,1050) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Loop doing the transformation
C                                       and writing the output map.
         DO 80 JY2 = 1,NY2,NR2
            KY2 = MIN(JY2+NR2-1, NY2)
C                                       Transform pixel coordinates of
C                                       each output row.
            CALL RGTRAN (CRDPRM, NX2, JY2, KY2, XPX1, YPX1, IERR)
C                                       Interpolate output values from
C                                       the input map.
            CALL RGINTP (DEPTH, CNO1, NX2, JY2, KY2, XPX1, YPX1,
     *         BUFF2, MAPMAX, MAPMIN, IERR)
C                                       Dump the output buffer.
            J2 = 1
            K2 = 1
            DO 70 IY2 = JY2,KY2
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
 70            CONTINUE
 80         CONTINUE
C                                       Flush last buffer.
         CALL MDISK ('FINI', LUN2, FIND2, BUFF2(K2), BNDX2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 100     CONTINUE
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
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('RGRDO: Image size truncated to',I5,' *',I5)
 1020 FORMAT ('RGRDO: ERROR',I3,' CREATING THE OUTPUT MAP.')
 1030 FORMAT ('RGRDO: ERROR',I3,' OPENING THE OUTPUT MAP.')
 1040 FORMAT ('RGRDO: ERROR',I3,' INITIALIZING THE OUTPUT MAP.')
 1050 FORMAT ('RGRDO: ERROR',I3,' PRE-WRITING THE OUTPUT MAP.')
 1060 FORMAT ('RGRDO: ERROR',I3,' WRITING THE OUTPUT MAP.')
 1090 FORMAT ('RGRDO: ERROR',I3,' FLUSHING THE OUTPUT MAP.')
 1100 FORMAT ('RGRDO: ERROR',I3,' CLOSING THE OUTPUT MAP.')
      END
      SUBROUTINE RGTRAN (CRDPRM, NX2, JY2, KY2, XPX1, YPX1, IERR)
C-----------------------------------------------------------------------
C   RGTRAN transforms pixel coordinates for a number of consecutive rows
C   of the output map to the corresponding pixel coordinates in the
C   input map.
C   Given:
C      CRDPRM(11)  D     Parameters to transform coordinates from
C                        from the output map to the input map.
C      NX2         I     Output map size in pixels.
C      JY2,KY2     I     Range of Y-pixel values for the rows of
C                        the output map.
C   Returned:
C      XPX1()      R     (X,Y) pixel coordinates in the input map
C      YPX1()      R     corresponding to the rows of the output
C                        map.  These must be dimensioned to
C                        accomodate NX2*(KY2-JY2+1) coordinate
C                        pairs.
C      IERR        I     Error status
C                        0: success
C   Called:
C      APLSUB: {COPY, SETLOC, XYVAL, XYPIX}
C      APLNOT: {CRDTRN}
C   Algorithm:
C      Computes the image coordinates at each pixel in the output map
C      using XYVAL, and transforms to the coordinate system of the input
C      map using CRDTRN.  It then converts to pixel coordinates using
C      XYPIX.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   IB2, IE2, IERR, JY2, KY2, IX2, IY2, J, K2, NX2
      REAL      XPX1(1), XPX2, YPX1(1), YPX2
      DOUBLE PRECISION  CRDPRM(11), ROTN, X1(MAXIMG), X2, Y1(MAXIMG),
     *          Y2, Z2
C      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
      K2 = 1
      DO 40 IY2 = JY2, KY2
         YPX2 = FLOAT(IY2)
C
         DO 30 IB2 = 1, NX2, MAXIMG
            IE2 = MIN(IB2+MAXIMG-1, NX2)
C                                       Set geometry for the output
C                                       map.
            LOCNUM = 1
C                                       Loop down this row of the output
C                                       map.
            J = 1
            DO 10 IX2 = IB2, IE2
               XPX2 = FLOAT(IX2)
C                                       Get the coordinates of this
C                                       pixel in the output map.
               CALL XYVAL (XPX2, YPX2, X2, Y2, Z2, IERR)
C                                       Transform to the coordinate
C                                       system of the input map.
               CALL CRDTRN (X2, Y2, CRDPRM, X1(J), Y1(J), ROTN)
               J = J + 1
 10            CONTINUE
C                                       Set geometry for the input map.
            LOCNUM = 2
C                                       Compute pixel coordinates in the
C                                       input map.
            J = 1
            DO 20 IX2 = IB2, IE2
               CALL XYPIX (X1(J), Y1(J), XPX1(K2), YPX1(K2), IERR)
               J = J + 1
               K2 = K2 + 1
 20            CONTINUE
 30         CONTINUE
 40      CONTINUE
C
      RETURN
      END
      SUBROUTINE RGINTP (DEPTH, CNO1, NX2, JY2, KY2, XPX1, YPX1,
     *   BUFF2, MAPMAX, MAPMIN, IERR)
C-----------------------------------------------------------------------
C   RGINTP reads through the input map and interpolates the pixel values
C   for a consecutive sequence of rows of the output map.
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
C          REGRD:  {INT3X3}
C          APLSUB: {IROUND, MAPCLS, MAPOPN, MDISK, MINIT, MSGWRT}
C
C     Algorithm:
C
C     Notes:
C       1)
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1990/Jul/11. Code last modified; 1990/Jul/30
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   MAX1X, MAX1X3
      PARAMETER (MAX1X  = 2*MAXIMG)
      PARAMETER (MAX1X3 = 3*MAX1X)
      INTEGER   BNDX1, BUFSZ, CATIN(256), CNO1, FIND1, IERR, IROUND,
     *   IX1, IX2, IY1, IY2, JY1, JY2, K1, K2, KY1, KY2, L1, LUN1, LY1,
     *   MY1, NDEFER, NR1, NX1, NX2, NY1, WBUFF(256), WIN(4),
     *   DEPTH(5), IBLKOF
      REAL      BUFF1(MAX1X3), BUFF2(1), DX, DY, MAPMAX, MAPMIN,
     *   V(-1:1,-1:1), XPX1(*), YPX1(*)
      CHARACTER PTYPE*2
      INCLUDE 'REGRD.INC'
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
C                                       Block offset for source file.
      CALL COMOFF (CATIN(KIDIM), CATIN(KINAX), DEPTH, IBLKOF, IERR)
      IBLKOF = IBLKOF + 1
      CALL MINIT ('READ', LUN1, FIND1, NX1, NY1, WIN, BUFF1, BUFSZ,
     *   IBLKOF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Read through the input map.
      MY1 = 1 + ((NY1 - MIN(NR1+2,NY1)) + (NR1-1))/NR1
      DO 100 LY1 = 1, MY1
         K1 = 1
C                                       First time through.
         IF (LY1.EQ.1) THEN
            JY1 = 1
            KY1 = MIN(NR1+2, NY1)
C                                       Copy the end of the previous
C                                       buffer to the start of this.
         ELSE
            L1 = NX1*((KY1-1)-JY1) + 1
            DO 50 IY1 = KY1-1, KY1
               DO 40 IX1 = 1, NX1
                  BUFF1(K1) = BUFF1(L1)
                  K1 = K1 + 1
                  L1 = L1 + 1
 40               CONTINUE
 50            CONTINUE
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
 70         CONTINUE
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
                     IF (IERR.NE.0) THEN
                        WRITE (MSGTXT, 1910) IX2, IY2, IERR
                        CALL MSGWRT(8)
                        IERR=991
                        GO TO 999
                        END IF
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
 80            CONTINUE
 90         CONTINUE
C                                       Any more to do?
         IF (NDEFER.EQ.0) GO TO 110
 100     CONTINUE
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
 1010 FORMAT ('RGRDO: ERROR',I3,' OPENING THE INPUT MAP.')
 1020 FORMAT ('RGRDO: INPUT IMAGE SIZE',I6,' EXCEEDS MAXIMUM',I6)
 1030 FORMAT ('RGRDO: ERROR',I3,' INITIALIZING THE INPUT MAP.')
 1060 FORMAT ('RGRDO: ERROR',I3,' READING THE INPUT MAP.')
 1120 FORMAT ('RGRDO: ERROR',I3,' CLOSING THE INPUT MAP.')
 1910 FORMAT ('RGRDO: DEBUG: INT3X3 error on ',I12,', ',I12,': ',I4)
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
C          Origin; 1990/Jul/25. Code last modified; 1990/Jul/25
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
 10         CONTINUE
 20      CONTINUE
C
      IF (NBLNK.EQ.0) THEN
C                                       Simple case, no blanking.
         DO 30 J = -1, 1
C                                       Interpolate in X.
            B = (V(1,J)-V(-1,J))/2.0
            W(J) = V(0,J) + (B + (V(1,J)-V(0,J)-B)*DX)*DX
 30         CONTINUE
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
 40         CONTINUE
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
      SUBROUTINE REGRHI (CNO1, CATIN, CRD1, CNO2, CATOUT, CRD2, CRDPRM,
     *   IERR)
C-----------------------------------------------------------------------
C     REGRHI writes the history file for REGRD.
C
C     Given:
C          CNO1        I     Catalogue slot number of the input map.
C          CATIN(256)  I     Catalogue header of the input map.
C          CRD1        C*40  Input coordinate system.
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
C          None
C
C     Algorithm:
C
C     Notes:
C       1)
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1990/Aug/16. Code last modified; 1990/Aug/16
C-----------------------------------------------------------------------
      INTEGER   CATIN(256), CATOUT(256), CNO1, CNO2, HIBUFF(256), HM(2),
     *          IERR, J, K, LUNHI1, LUNHI2, WBUFF(256)
      REAL      CRDINC, SEC
      DOUBLE PRECISION  CRDPRM(11)
      CHARACTER CHSIGN*1, CRD1*40, CRD2*40, CTYPE*8, FM*80, HTXT*72,
     *          STYP*4
      INCLUDE 'REGRD.INC'
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
C                                       Input and output coordinate
C                                       axes.
      DO 50 K = 1, 2
         HTXT(7:) = '-------------------------------' //
     *            '-------------------------------'
         CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
         IF (IERR.NE.0) GO TO 990
C
         IF (K.EQ.1) THEN
            HTXT(7:) = 'Input  coordinate system: ' // CRD1
            CALL COPY (256, CATIN, CATBLK)
         ELSE
            HTXT(7:) = 'Output coordinate system: ' // CRD2
            CALL COPY (256, CATOUT, CATBLK)
            END IF
         CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
         IF (IERR.NE.0) GO TO 990
C
         HTXT(7:) = 'Type    Pixels   Coord value  at Pixel' //
     *            '    Coord incr   Rotat'
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
               CRDINC = CATR(KRCIC-1+J) * 3600.0
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
     *            HM, SEC, CATR(KRCRP-1+J), CRDINC, CATR(KRCRT-1+J)
               IF (HTXT(31:31).EQ.' ') HTXT(31:31) = '0'
            ELSE
C                                       Not an RA or DEC axis.
               WRITE (HTXT(7:),1030) CTYPE, CATBLK(KINAX-1+J),
     *            CATD(KDCRV-1+J), CATR(KRCRP-1+J), CATR(KRCIC-1+J),
     *            CATR(KRCRT-1+J)
               END IF
C
            CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
            IF (IERR.NE.0) GO TO 990
 40         CONTINUE
 50      CONTINUE
C
      HTXT(7:) = '-------------------------------' //
     *         '-------------------------------'
      CALL HIADD (LUNHI2, HTXT, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Record the transformation
C                                       parameters.
      HTXT(7:) = 'Coordinate transformation parameters ' //
     *           '(see EXPLAIN REGRD).'
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
 1010 FORMAT ('REGRHI: ERROR',I3,' CREATING HISTORY FILE.')
 1020 FORMAT ('REGRHI: ERROR',I3,' UPDATING THE HISTORY FILE.')
 1030 FORMAT (A8,I6,2X,1PE14.7,0PF8.2,1PE14.7,0PF8.2)
 1050 FORMAT (3F20.15)
 1060 FORMAT ('REGRHI: ERROR',I3,' CLOSING THE HISTORY FILE.')
 1991 FORMAT ('REGRHI: ERROR',I3,' WRITING THE HISTORY FILE.')
      END
