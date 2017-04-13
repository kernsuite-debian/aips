C
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
C
C               *****  CATREC.DCL  *****    JULY 15, 1980
C                                           FEBRUARY 12, 1987
C       THE FOLLOWING IS THE DEFINITION OF THE CATALOG RECORD BLOCK:
C
        INTEGER   CATREC(256)                   ! THE CATALOG RECORD
        INTEGER   CATSIZ                        ! SIZE OF A CATALOG REC
C                                               !   (BYTES)
C       **  MAP IDENTIFICATION SECTION  **
        INTEGER   MAPNAM, MAPCAT, SOURCE
        INTEGER   QUAL, BAND, DATTYP, MAPTYP, MODTYP, MAPNX, MAPNY
        REAL MAPDAT, MAPTIM
        EQUIVALENCE (MAPNAM, CATREC(1))         ! MAP NAME (12 CHARS)
        EQUIVALENCE (MAPCAT, CATREC(8))         ! MAP CATEGORY (12 CHAR)
        EQUIVALENCE (SOURCE, CATREC(15))        ! SOURCE NAME (8 CHARS)
        EQUIVALENCE (QUAL,   CATREC(20))        ! SOURCE QUALIFIER
        EQUIVALENCE (BAND,   CATREC(21))        ! BAND (1=1.3CM, 2=2CM,
C                                               !   3=6CM, 4=20CM)
        EQUIVALENCE (DATTYP, CATREC(22))        ! TYPE OF DATA IN FILE
        EQUIVALENCE (MAPTYP, CATREC(23))        ! TYPE OF MAP DATA
        EQUIVALENCE (MODTYP, CATREC(24))        ! MODEL TYPE
        EQUIVALENCE (MAPDAT, CATREC(25))        ! DATE MAP WAS CREATED
        EQUIVALENCE (MAPTIM, CATREC(27))        ! TIME MAP WAS CREATED
        EQUIVALENCE (MAPNX,  CATREC(29))        ! NUM. CELLS IN X DIR
        EQUIVALENCE (MAPNY,  CATREC(30))        ! NUM. CELLS IN Y DIR
C
C       **  AUXILIARY MAP IDENTIFICATION SECTION  **
        INTEGER   MLABEL,XLABEL,YLABEL
        INTEGER  MOBSDT,DIFBEM
        REAL OBSDAT,EPOCH,CELLDX,CELLDY,MAPROT
        REAL BW,DFREQ,DVEL,SUBFLX,BEAMPA,BHPMAJ,BHPMIN,CLNFLX
      DOUBLE PRECISION
     *   CNRAE,CNDECE,CNRAO,CNDECO,MPHRAE,MPHDCE
      DOUBLE PRECISION
     *   MPHRAO,MPHDCO,OPHRAE,OPHDCE,OPHRAO,OPHDCO
        EQUIVALENCE (MLABEL, CATREC(41))        ! MAP LABEL (80 CHARS)
        EQUIVALENCE (OBSDAT, CATREC(82))        ! FIRST DATE OBSERVATION
        EQUIVALENCE (MOBSDT, CATREC(84))        ! MULT. OBS DATE
C                                               !             INDICATOR
C                                               !   1 = JUST ONE DATE
C                                               !   2 = MULTIPLE DATES
        EQUIVALENCE (EPOCH,  CATREC(85))        ! DATE FOR CNRAE,
C                                               !           CNDECE, ETC.
        EQUIVALENCE (CNRAE,  CATREC(87))        ! CENTER CELL RA AT
C                                               !      EPOCH
C                                               !   (STORED IN RADIANS)
C                                               !   ("CENTER" IS UPPER
C                                               ! RIGHT CELL OF THE FOUR
C                                               ! IN THE CENTER OF A
C                                               ! MAP.)
        EQUIVALENCE (CNDECE, CATREC(91))        ! CENTER CELL DEC AT
C                                               !       EPOCH
        EQUIVALENCE (CNRAO,  CATREC(95))        ! CENTER RA AT
C                                               !      OBSERVATION DATE
        EQUIVALENCE (CNDECO, CATREC(99))        ! CENTER DEC AT
C                                               !      OBSERVATION DATE
C                       ! THE FOLLOWING FOUR PARAMETERS ARE THE COORDS
C                       !   OF THE PHASE TRACKING CENTER IN THE DATA
C                       !   THAT WAS USED IN MAKING THE MAP:
        EQUIVALENCE (MPHRAE, CATREC(103))       ! MAP PHASE TRACKING
C                                               !  CENTER AT EPOCH
        EQUIVALENCE (MPHDCE, CATREC(107))       ! PHASE TRACKING CENTER
C                                               !  AT EPOCH
        EQUIVALENCE (MPHRAO, CATREC(111))       ! PH. TR. CENTER AT
C                                               !  OBS. DATE
        EQUIVALENCE (MPHDCO, CATREC(115))
C               ! THE FOLLOWING FOUR PARAMETERS ARE THE COORDS
C               !   OF THE PHASE TRACKING CENTER IN THE ORIGINAL
C               !   DATA PRODUCED BY THE SYNCHRONOUS SYSTEM.
C               !   THIS IS THE SAME AS THE COORDINATES OF WHERE
C               !   THE ANTENNAS WERE POINTING:
        EQUIVALENCE (OPHRAE, CATREC(119))       ! ORIGINAL PH. TR.
C                                               !  CENTER EPOCH
        EQUIVALENCE (OPHDCE, CATREC(123))
        EQUIVALENCE (OPHRAO, CATREC(127))       ! ORIG. PH. TR.
C                                               !  CENTER OBS. DATE
        EQUIVALENCE (OPHDCO, CATREC(131))
        EQUIVALENCE (CELLDX, CATREC(135))       ! CELL SIZE
C                                               !  (RADIANS) - X
        EQUIVALENCE (CELLDY, CATREC(137))       ! CELL SIZE
C                                               !  (RADIANS) - Y
        EQUIVALENCE (MAPROT, CATREC(139))       ! MAP ROTATION ANGLE
C                                               !  (RADIANS OF
C                                               ! CLOCKWISE ROTATION)
        EQUIVALENCE (XLABEL, CATREC(141))       ! X COORDINATE LABEL
        EQUIVALENCE (YLABEL, CATREC(148))       ! Y COORDINATE LABEL
        EQUIVALENCE (BW,     CATREC(155))       ! BAND WIDTH
        EQUIVALENCE (DFREQ,  CATREC(157))       ! FREQUENCY DIFFERENCE
C                                               !  BETWEEN ADJACENT
C                                               !  CHANNELS (MIN DIF.
C                                               !  IF NOT ALL THE SAME)
        EQUIVALENCE (DVEL,   CATREC(159))       ! VELOCITY DIFFERENCE
C                                               !   BETWEEN
C                                               !   ADJACENT CHANNELS
        EQUIVALENCE (SUBFLX, CATREC(161))       ! TOTAL SUBTRACTED
C                                               !   FLUX (JY)
        EQUIVALENCE (BEAMPA, CATREC(163))       ! POSITION ANGLE OF
C                                               !   FITTED BEAM
        EQUIVALENCE (BHPMAJ, CATREC(165))       ! HALF POWER WIDTH
C                                               !   ALONG MAJOR
C                                               !   AXIS OF FITTED BEAM
        EQUIVALENCE (BHPMIN, CATREC(167))       ! WIDTH ALONG MINOR AXIS
        EQUIVALENCE (DIFBEM, CATREC(169))       ! DIFFERENT BEAM FLAG
C                                               !   1 = DIFFERENT BEAMS
C                                               !       NOT USED
C                                               !     I.E., BEAMPA,
C                                               !     BHPMAJ, AND
C                                               !     BHPMIN USED FOR
C                                               !     ALL CLEANING
C                                               !   2 = DIFFERENT BEAMS
C                                               !     USED FOR CLEANING,
C                                               !     SEE CLNPA, CHPMAJ,
C                                               !     CHPMIN IN CLEANED
C                                               !     SOURCES FILE
        EQUIVALENCE (CLNFLX, CATREC(170))       ! TOTAL CLEANED FLUX -
C                                               !  SUM OF FLUXES OF
C                                               !  CLEANED COMPONENTS
C                                               !   (JY)
C
C       **  MAPPING PARAMETERS SECTION  **
        INTEGER  WTTYPE, TAPER, CONVLV, UNGRID, PBCORR
        REAL TAPWDX, TAPWDY, CNVWDX, CNVWDY
        EQUIVALENCE (WTTYPE, CATREC(182))       ! TYPE OF WEIGHTING OF
C                                               !  DATA:
C                                               !   1="NATURAL"
C                                               !     (CELL SUM)
C                                               !   2="UNIFORM"
C                                               !     (CELL AVERAGE)
        EQUIVALENCE (TAPER,  CATREC(183))       ! TYPE OF TAPER FUNCTION
C                                               !  USED:
C                                               !   1=GAUSSIAN
C                                               !   2=LINEAR
        EQUIVALENCE (TAPWDX, CATREC(184))       ! TAPER PARAMETER IN
C                                               !  X DIRECTION (SIGMA
C                                               !  FOR GAUSSIAN, DIST.
C                                               !  TO ZERO FOR LINEAR)
        EQUIVALENCE (TAPWDY, CATREC(186))       ! TAPER PARAMETER IN Y
C                                               !  DIRECTION
        EQUIVALENCE (CONVLV, CATREC(188))       ! CONVOLVING FUNCTION:
C                                               !   1=BOXCAR
C                                               !     (II FUNCTION)
C                                               !   2=GAUSSIAN
        EQUIVALENCE (CNVWDX, CATREC(189))       ! CONVOLUTION
C                                               !  PARAMETER IN
C                                               !  X DIRECTION
C                                               !  (WIDTH FOR BOXCAR,
C                                               !  SIGMA FOR GAUSSIAN)
        EQUIVALENCE (CNVWDY, CATREC(191))       ! CONVOLUTION PARAM.
C                                               !  IN Y DIR.
        EQUIVALENCE (UNGRID, CATREC(193))       ! INDICATION OF WHETHER
C                                               !  OR NOT EFFECTS OF
C                                               !  GRIDDING WERE UNDONE
C                                               !  (1=NO, 2=YES)
        EQUIVALENCE (PBCORR, CATREC(194))       ! INDICATION OF WHETHER
C                                               !  OR NOT THE MAP HAS
C                                               !  BEEN CORRECTED FOR
C                                               !  THE PRIMARY BEAM
C                                               !  ATTENUATION
C                                               !  (1=NO, 2=YES)
C
C       **  SPECTRAL LINE CHANNEL INFORMATION SECTION  **
        INTEGER  VELREF, CHANFL, CHANWT
        INTEGER  IMAXX, IMAXY, IMINX, IMINY, MAPSCL
        REAL CHFREQ, CHVEL, IMAX, IMIN, PSCALE, PTRANS
        EQUIVALENCE (VELREF, CATREC(203))       ! VELOCITY REFERENCE
C                                               !  FRAME
        EQUIVALENCE (CHFREQ, CATREC(204))       ! FREQUENCY OF CHANNEL
        EQUIVALENCE (CHVEL,  CATREC(206))       ! VELOCITY OF CHANNEL
        EQUIVALENCE (CHANFL, CATREC(208))       ! FLAG FOR THE CHANNEL
        EQUIVALENCE (CHANWT, CATREC(209))       ! WEIGHTING FACTOR FOR
C                                               !  CHANNEL
        EQUIVALENCE (IMAX,   CATREC(210))       ! MAX STRENGTH (JY)
        EQUIVALENCE (IMAXX,  CATREC(212))       ! X PIXEL COORD OF IMAX
        EQUIVALENCE (IMAXY,  CATREC(213))       ! Y PIXEL COORD OF IMAX
        EQUIVALENCE (IMIN,   CATREC(214))       ! MIN STRENGTH (JY)
        EQUIVALENCE (IMINX,  CATREC(216))       ! X PIXEL COORD OF IMIN
        EQUIVALENCE (IMINY,  CATREC(217))       ! Y PIXEL COORD OF IMIN
        EQUIVALENCE (MAPSCL, CATREC(218))       ! MAP SCALE FACTOR
        EQUIVALENCE (PSCALE, CATREC(219))       ! PIXEL SCALE FACTOR
        EQUIVALENCE (PTRANS, CATREC(221))       ! PIXEL TRANSLATION
C                                               !  AMOUNT
C
C       **  MAP ACCESS INFORMATION SECTION  **
        INTEGER   MDBVER, BEAPTR, NCHANS, NFITS, NSUBS, NCLNS
        INTEGER   CELFMT, MAPORG
        INTEGER   MAPFIL, USRNUM
        EQUIVALENCE (MDBVER, CATREC(233))       ! MAP DATA BASE VERSION
        EQUIVALENCE (MAPFIL, CATREC(234))       ! MAP FILE NAME
        EQUIVALENCE (BEAPTR, CATREC(239))       ! BEAM POINTER
        EQUIVALENCE (NCHANS, CATREC(240))       ! NUMBER OF FREQUENCY
C                                               !  CHANNELS
        EQUIVALENCE (NFITS,  CATREC(241))       ! NUMBER OF FITTED
C                                               !  SOURCES
        EQUIVALENCE (NSUBS,  CATREC(242))       ! NUMBER OF SUBTRACTED
C                                               !  SOURCES
        EQUIVALENCE (NCLNS,  CATREC(243))       ! NUMBER OF CLEANED
C                                               !  SOURCES
        EQUIVALENCE (CELFMT, CATREC(244))       ! CELL DATA FORMAT
        EQUIVALENCE (MAPORG, CATREC(245))       ! MAP DATA ORGANIZATION
        EQUIVALENCE (USRNUM, CATREC(246))       ! USER NUMBER OF MAP'S
C                                               !  OWNER
C
        COMMON /CATCOM/CATSIZ,CATREC            ! DEFINE THE CATALOG
C                                               !  COMMON
