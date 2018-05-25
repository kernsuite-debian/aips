      SUBROUTINE GETFRQ (DISKIN, CNOIN, APARM, OLDSOU, CVLSOU, FRQSEL,
     *   CATIN, IRET)
C-----------------------------------------------------------------------
C! Determine all frequency information associated with uv data.
C# UV Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2012
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
C   Routine to get all freq, vel type parameters needed for the
C   shifting routine
C   Inputs:
C      DISKIN          I        Volume number
C      CNOIN           I        File catalogue number
C      APARM(10)       R        User supplied input parameters, if
C                               all zero will use catalogue info.
C      OLDSOU          I
C      CVLSOU          I        Source number for which info desired.
C      FRQSEL          I        Freq ID for which need freq info
C      CATIN(256)      I        Catalog header
C   Outputs in common:
C      REFFRQ(NUMIF)   D        Freq. (Hz) of ref. pixel - same
C                               one to which VEL refers.
C      RSTFRQ(NUMIF)   D        Rest freq. (Hz) of the transition
C                               in each IF
C      VEL(NUMIF)      D        Desired velocity of the reference
C                               pixel of each IF (m/s)
C      HELIO           L        .TRUE. if velocity reference frame
C                               is heliocentric/barycentric.
C      RADIO           L        .TRUE. if velocity follows the
C                               radio definition.
C      SOUDUN(*)       I        > 0 if source already dealt with
C      IRET            I        Error: = 0 => all OK
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISKIN, CNOIN, OLDSOU, CVLSOU, IRET, CATIN(256),
     *   FRQSEL
      REAL      APARM(10)
C
      INTEGER   ALTAX, IFNO, TMPLUN, I, CVER, ICHLUN, CATBLK(256),
     *   MLOCF, MLOCIF, NUMIF
      REAL      REFPIX, ALTRFP, RTEMP, NPIX, BUFF1(1024), CATR(256)
      DOUBLE PRECISION REFVEL, FINT, CATD(128), REFF
      LOGICAL   EQUAL, USEAPA, FZ, TELLUS, MULTI
      HOLLERITH CATH(256)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCVL.INC'
      EQUIVALENCE (CATBLK, CATD, CATR, CATH)
C-----------------------------------------------------------------------
      IRET = 0
      CALL COPY (256, CATIN, CATBLK)
      CALL AXEFND (8, 'FREQ    ', CATBLK(KIDIM), CATH(KHCTP), MLOCF,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT, 2010) IRET
         GO TO 990
         END IF
      CALL AXEFND (8, 'IF      ', CATBLK(KIDIM), CATH(KHCTP), MLOCIF,
     *   IRET)
      IF (IRET.NE.0) MLOCIF = -1
      IRET = 0
C                                       Set base ref. freq.
      REFF = CATD(KDCRV+MLOCF)
      NUMIF = 1
      IF (MLOCIF.GT.0) NUMIF = CATBLK(KINAX+MLOCIF)
C                                       Single or multi-source file
      CALL MULSDB (CATBLK, MULTI)
      IF (MULTI) GO TO 100
C
      IF (OLDSOU.GT.0) GO TO 999
C                                       Get frequency info.
      CVER = 1
      ICHLUN = 44
      CALL CHNDAT ('READ', BUFF1, DISKIN, CNOIN, CVER, CATBLK, ICHLUN,
     *   NUMIF, CFOFF, CSBAND, CFINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) THEN
        WRITE (MSGTXT,2005) IRET
        GO TO 990
        END IF
      USEAPA = .FALSE.
C                                       Use APARMs ?
      DO 20 I = 1, 6
         IF (APARM(I).NE.0.0) USEAPA = .TRUE.
 20      CONTINUE
      IF (USEAPA) THEN
         WRITE (MSGTXT,1000)
      ELSE
         WRITE (MSGTXT,1010)
         END IF
      CALL MSGWRT (4)
C                                       Extract information from
C                                       alternate reference pixels
      ALTAX = CATBLK (KIALT)
C                                       Check if vel info available
      HELIO = .FALSE.
      RADIO = .FALSE.
      IF (USEAPA) THEN
         IF (APARM(3).EQ.1) HELIO = .TRUE.
         IF (APARM(4).EQ.1) RADIO = .TRUE.
      ELSE IF (.NOT.USEAPA) THEN
         IF (ALTAX .GT. 256) THEN
            RADIO = .TRUE.
            ALTAX = ALTAX - 256
            END IF
         IF (ALTAX .EQ. 2) HELIO = .TRUE.
         END IF
C                                       Inform user
      IF (HELIO) THEN
         WRITE (MSGTXT,1020)
      ELSE
         WRITE (MSGTXT,1030)
         END IF
      CALL MSGWRT (4)
      IF (RADIO) THEN
         WRITE (MSGTXT,1040)
      ELSE
         WRITE (MSGTXT,1050)
         END IF
      CALL MSGWRT (4)
C                                       Determine freq. of ref pixel,
C                                       for each IF
      DO 25 I = 1, NUMIF
         REFFRQ(I) = REFF + CFOFF(I)
 25      CONTINUE
C                                       For single source files, there
C                                       can be only one rest frequency
C                                       for all IFs (nowhere to store
C                                       more than one).
C
C                                       Determine rest freq. of line
C                                       from user or header and
C                                       replicate to all IFs
      IF (USEAPA) THEN
         DO 30 I = 1, NUMIF
            RSTFRQ(I) = APARM(5)
            RSTFRQ(I) = RSTFRQ(I) + APARM(6)
 30         CONTINUE
      ELSE
         DO 35 I = 1, NUMIF
            RSTFRQ(I) = CATD(KDRST)
 35         CONTINUE
         END IF
C                                       Inform user
      RTEMP = RSTFRQ(1) / 1.0E6
      WRITE (MSGTXT,1060) RTEMP
      CALL MSGWRT (4)
      IF (RSTFRQ(1).LE.0.D0) THEN
         WRITE (MSGTXT,1070)
         IRET = 1
         GO TO 990
         END IF
C                                       Find from user or header, a
C                                       velocity (REFVEL) and the pixel
C                                       to which it refers (ALTRFP)
      IF (USEAPA) THEN
         ALTRFP = APARM(2)
      ELSE
         ALTRFP = CATR(KRARP)
C                                       Fill in for later use
         APARM(2) = ALTRFP
         END IF
C                                       Inform user
      WRITE (MSGTXT,1080) ALTRFP
      CALL MSGWRT (4)
C
      IF (USEAPA) THEN
         REFVEL = APARM(1)
      ELSE
         REFVEL = CATD(KDARV)
C                                       Fill in for later use
         APARM(1) = REFVEL
         END IF
C                                       Inform user
      RTEMP = REFVEL / 1.0E3
      WRITE (MSGTXT,1090) RTEMP
      CALL MSGWRT (4)
C                                       Work out velocity that
C                                       corresponds to REFFRQ for
C                                       each IF
      REFPIX = CATR(KRCRP+MLOCF)
C
      IF (ALTRFP .EQ. REFPIX) THEN
         DO 40 I = 1, NUMIF
            VEL(I) = REFVEL
 40         CONTINUE
      ELSE
         DO 50 I = 1, NUMIF
C                                       Signed frequency increment
            FINT = CSBAND(I) * CFINC(I)
C                                       Pixel of desired velocity
            NPIX = REFPIX
C                                       Get velocity of reference pixel
            CALL VELPIX (ALTRFP, REFVEL, REFFRQ(I), REFPIX, FINT,
     *         RADIO, NPIX, VEL(I))
 50         CONTINUE
         END IF
      GO TO 999
C
C                                       Information from multi-source
C                                       files
C                                       First see if need to update
C                                       information
 100  IF (CVLSOU .EQ. OLDSOU) GO TO 999
C                                       Get frequency info.
      CVER = 1
      ICHLUN = 44
      CALL CHNDAT ('READ', BUFF1, DISKIN, CNOIN, CVER, CATBLK, ICHLUN,
     *   NUMIF, CFOFF, CSBAND, CFINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) THEN
        WRITE (MSGTXT,2005) IRET
        GO TO 990
        END IF
      HELIO = .FALSE.
      RADIO = .FALSE.
      TELLUS = SOUDUN(CVLSOU).EQ.0
      SOUDUN(CVLSOU) = 1
C                                       Get source information
C                                       Fill in values in DSOU.INC
      TMPLUN = 50
      CALL GETSOU (CVLSOU, DISKIN, CNOIN, CATBLK, TMPLUN, IRET)
      IF ( IRET .NE. 0 ) THEN
         WRITE (MSGTXT,2000) IRET
         GO TO 990
         END IF
C                                       Determine velocity type
C                                       Assume LSR is default
      WRITE (MSGTXT,1110) SNAME
      IF (TELLUS) CALL MSGWRT (4)
      EQUAL = (VELTYP(1:5).EQ.'HELIO') .OR. (VELTYP(1:5).EQ.'BARYC')
      IF (EQUAL) THEN
         HELIO = .TRUE.
         END IF
      IF (HELIO) THEN
         WRITE (MSGTXT,1020)
      ELSE
         WRITE (MSGTXT,1030)
         END IF
      IF (TELLUS) CALL MSGWRT (4)
C                                       Determine velocity definition
C                                       Assume optical is default
      EQUAL = VELDEF .EQ. 'RADIO   '
      IF ( EQUAL ) THEN
         RADIO = .TRUE.
         END IF
      IF (RADIO) THEN
         WRITE (MSGTXT,1040)
      ELSE
         WRITE (MSGTXT,1050)
         END IF
      IF (TELLUS) CALL MSGWRT (4)
C                                       Fill in REFFRQ & VEL arrays
C                                       This is almost the correct
C                                       freq. - there is one other
C                                       offset, the DOPOFF stored in
C                                       the CL tables which is
C                                       designed to catch the possibil.
C                                       of the same source being
C                                       observed at different freqs.
C                                       during an observing run. So to
C                                       ensure the correct freq is
C                                       obtained call FRQUPD for each
C                                       UT
      FZ = .TRUE.
      DO 150 IFNO = 1, NUMIF
C                                       Freq. of ref. pixel.  FREQO
C                                       from source table
         REFFRQ(IFNO) = REFF + CFOFF(IFNO) + FREQO(IFNO)
C                                       Velocity of ref. pixel from
C                                       source table
         VEL(IFNO)    = LSRVEL(IFNO)
C                                       Rest frequency of line from
C                                       source table
         RSTFRQ(IFNO) = RESTFQ(IFNO)
         IF (RSTFRQ(IFNO).GT.0.D0) FZ = .FALSE.
 150     CONTINUE
      IF (FZ) THEN
         WRITE (MSGTXT,1100)
         IRET = 1
         GO TO 990
         END IF
C                                       Tell user
      WRITE (MSGTXT,1120)
      IF (TELLUS) CALL MSGWRT (4)
      DO 160 IFNO = 1, NUMIF
         RTEMP = RSTFRQ(IFNO) / 1.0E6
         WRITE (MSGTXT,1150) IFNO, RTEMP
         IF (TELLUS) CALL MSGWRT (4)
 160     CONTINUE
      WRITE (MSGTXT,1130) CATR(KRCRP+MLOCF)
      IF (TELLUS) CALL MSGWRT (4)
      WRITE (MSGTXT,1140)
      IF (TELLUS) CALL MSGWRT (4)
      DO 170 IFNO = 1, NUMIF
         RTEMP = VEL(IFNO) / 1.0E3
         WRITE (MSGTXT,1160) IFNO, RTEMP
         IF (TELLUS) CALL MSGWRT (4)
 170     CONTINUE
C
      GO TO 999
C                                       Write error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Will use velocity and frequency information from APARMS')
 1010 FORMAT ('Will use velocity and frequency information from ',
     *   'Cat. hdr')
 1020 FORMAT ('Velocity type is heliocentric')
 1030 FORMAT ('Velocity type is LSR')
 1040 FORMAT ('Velocity definition is RADIO')
 1050 FORMAT ('Velocity definition is OPTICAL')
 1060 FORMAT ('Rest frequency = ',F16.6,' MHz')
 1070 FORMAT ('GETFRQ: ILLEGAL REST FREQUENCY, MUST BE SET WITH APARM',
     *   ' OR SETDEF')
 1080 FORMAT ('Ref. pixel for new velocity is',F9.2)
 1090 FORMAT ('New velocity at given pixel is',F11.4,' km/s')
 1100 FORMAT ('GETFRQ: REST FREQS FOR ALL IFS ARE ZERO - USE SETJY')
 1110 FORMAT ('Velocity and frequency information for ',A16)
 1120 FORMAT ('Rest frequencies for each IF:')
 1130 FORMAT ('Reference channel for velocity =',F9.2)
 1140 FORMAT ('New velocity at reference channel for each IF (km/s):')
 1150 FORMAT ('       IF: ',I3,' Rest freq. = ',F16.6,' MHz')
 1160 FORMAT ('       IF: ',I3,' New velocity = ',F10.4,' km/s')
 2000 FORMAT ('GETFRQ: ERROR',I3,' OBTAINING SOURCE INFORMATION')
 2005 FORMAT ('GETFRQ: ERROR',I3,' GETTING FREQ. INFO. WITH CHNDAT')
 2010 FORMAT ('GETFRQ: ERROR',I3,' DETERMINING FREQ. POINTER')
      END
