LOCAL INCLUDE 'PRBM.INC'
C                                       local include for primary beam
C                                       NSTEP number of points in the
C                                       primary beam calculation
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NSTEP
      PARAMETER (NSTEP = 1000)
      INTEGER   NMAX, BTYPE
      PARAMETER (NMAX = 5000)
      REAL   ASTEP, PRBEAM(NSTEP), POINTE(NMAX), DANTT, LAMBDA, TIMTOL,
     *   COEF, PRBMA, PRBMI, PRBP
      REAL   HPHASR, HAMPFR
      LOGICAL DOONE
      DOUBLE PRECISION DELT, RASH, DECSH, RASHS, DECSHS
      LOGICAL   DOPOIN, DOPHAS, DOPRBM, DOSHFT, FIRST
      COMMON /PRMBM/ DELT, RASH, DECSH, RASHS, DECSHS, ASTEP, PRBEAM,
     *   POINTE, DANTT, LAMBDA, TIMTOL, COEF, PRBMA, PRBMI, PRBP,
     *   HPHASR, HAMPFR,
     *   BTYPE, DOPOIN, DOPHAS, DOPRBM, DOSHFT, FIRST, DOONE
LOCAL END
C
LOCAL INCLUDE 'UVCON.INC'
C                                       Local include for UVCON
      INCLUDE 'PRBM.INC'
C      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
C                                       Max number of elevations at the
C                                       second input file
      INTEGER   NELMAX
      PARAMETER (NELMAX = 100000)
C
      INTEGER   NCOMP(MAXFLD), BCOMP(MAXFLD)
      INTEGER   CATBLK(256), UVBLK(256), SEQOUT, DISINT, NUMHIS,
     *   JBUFSZ, N, MODEL, METHOD, DISOUT, CNOINT, SEQ2, DISK2, VER,
     *   ISTOKE, NFRCHA, NFRGR, NFRTOT,  NNLEV(NMAX), NLINE2,
     *   IAN(NELMAX)
      LOGICAL    DOUTFI, DOIN2F, ONECHA
      HOLLERITH XINFIL(12), XI2FIL(12), XOFILE(12), XNAME2(3),
     *   XCLAS2(2), XNAMOU(3), XCLAOU(2), XCMETH, XCMOD
      CHARACTER INFILE*48, I2FILE*48, OFILE*48, NAMOUT*12, CLAOUT*6,
     *   NAME2*12, CLAS2*6, HISCRD(2*NMAX+11)*64, CMETH*4, CMOD*4,
     *   ARNAME*8
      REAL      XS2, XDISK2, XVER, XSOUT, XDISO, XNMAPS, XBCOMP(MAXAFL),
     *   XNCOMP(MAXAFL), XFLUX, FACTOR, SMODEL(7), RASHIF(MAXAFL),
     *   DECSHI(MAXAFL), APARM(10), BPARM(10), CPARM(10),
     *   CNOISE, BUFFER(UVBFSS),BUFF1(UVBFSS), BUFF2(UVBFSS),
     *   BUFF3(UVBFSS), SEFD(NMAX), BANDW, SEFDMI, EFF(NMAX),
     *   NOISTE(NMAX), TSKYZE, RANGPB, PRBMAJ, PRBMIN, PRBPA,
     *   ELEVA(NELMAX), TSYS(NELMAX), EFFEL(NELMAX)
      REAL    DOW
      DOUBLE PRECISION SITELA, SITELO, DELTA, HMIN, HMAX, ELMIN,
     *   BLKMAX, TAU, BLKREJ, FREQNM, LX(NMAX), LY(NMAX), LZ(NMAX),
     *   LXORI(NMAX), LYORI(NMAX), LZORI(NMAX),
     *   DIAM(NMAX), TAUSEC, XCENT, YCENT, ZCENT, GSTIAT
      COMMON /BUFRS/ BUFFER, BUFF1, BUFF2, BUFF3, JBUFSZ
      COMMON /INPARM/ XINFIL, XI2FIL, XOFILE, XNAME2, XCLAS2, XS2,
     *   XDISK2, XVER, XNAMOU,
     *   XCLAOU, XSOUT, XDISO, XNMAPS,  XBCOMP, XNCOMP, XFLUX, XCMETH,
     *   XCMOD, FACTOR, SMODEL, RASHIF, DECSHI, APARM, BPARM, CPARM,
     *   PRBMAJ, PRBMIN, PRBPA, DOW
      COMMON /CHRCOM/ INFILE, I2FILE, NAMOUT, NAME2, CLAS2, CLAOUT,
     *   HISCRD, CMETH, CMOD, ARNAME
      COMMON /CHR2/ OFILE
      COMMON /LOG/ DOUTFI, DOIN2F, ONECHA
      COMMON /MAPHDR/ CATBLK, UVBLK
      COMMON /FRED/ XCENT, YCENT, ZCENT, SITELA, SITELO, DELTA, HMIN,
     *   HMAX, ELMIN, GSTIAT,
     *   LXORI, LYORI, LZORI,
     *   BLKMAX, TAU, LX, LY, LZ, DIAM, BLKREJ, FREQNM, N,
     *   MODEL, METHOD, DISOUT, CNOINT, NUMHIS, SEQOUT, DISINT,
     *   SEQ2, DISK2, VER, BCOMP, NCOMP, ISTOKE, SEFD, BANDW, SEFDMI,
     *   TAUSEC, CNOISE, NFRCHA, NFRGR, NFRTOT, EFF, NOISTE, NNLEV,
     *   TSKYZE, RANGPB, IAN, ELEVA, TSYS, EFFEL, NLINE2
LOCAL END
      PROGRAM UVCON
C-----------------------------------------------------------------------
C! Simulates uv coverage of an arbitrary interferometer array.
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1999-2003, 2005-2010, 2012, 2014-2015
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
C   UVCON creates an AIPS uv data base and fills with simulated data.
C   See the UVCON EXPLAIN file or Going AIPS for better descriptions
C   of the UV data file and catalog header structure and the contents
C   and structure of the antenna (AN) table.
C   There are sufficient comments in NEWHED to describe simple cases.
C   See Going AIPS for assistance in compiling and linking this task
C   onn your system.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         INFILE        Input file name. (antenna info.)
C      I2FILE         I2FILE        Second input file name.
C                                   (antenna elevation dependence.)
C      OUTFILE        OFILE         Output file name. (antenna info.)
C      IN2NAME        NAME2         Name of image.
C      IN2CLASS       CLAS2         Class of image.
C      IN2SEQ         SEQ2          Seq. of image.
C      IN2DISK        DISK2         Vol. of image.
C      INVER          VER           Version no. of CC file.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is source name
C      OUTCLASS       CLAOUT        Class of the output uv file.
C                                   Default 'UVDATA'
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISOUT         Disk number of the output file.
C      NMAPS          NMAPS         Number of input images.
C      BCOMP(64)      BCOMP         Start clean component to use
C                                   1 per field.
C      NCOMP(64)      NCOMP         Last Clean component no to use
C                                      1 per field, 0 => all
C      CMETHOD        METHOD        Modeling method:
C                                   'DFT' = FDT method
C                                   'GRID' = gridded FFT method.
C                                   '    ' = DFT.
C      CMODEL         MODEL         Model type, 'COMP'=>CC
C                                   'IMAG'=> image.
C      FACTOR         FACGRD        Multiplicative factor for MODEL
C                                      (default=1.0)
C      SMODEL         SMODEL        MODEL
C                                   1 = Flux density (Jy)
C                                   2 = RA offset (arcsec E. pos)
C                                   3 = Dec offset (arcsec N. pos)
C                                   4-7 model parameters
C      RASHIFT(64)    RASHIF        Shift of the tangent point
C                                   at RA direction
C      DECSHIFT(64)   DECSHI        Shift of the tangent point
C                                   at DEC direction
C      APARM(10)      APARM         User specified array.
C                                   1: nominal frequency, GHz
C                                   2: nominal wavelength, cm
C                                   3: Source declination, deg
C                                   4: Min hour angle, hours
C                                   5: Max hour angle, hours
C                                   6: Min antenna elevation, deg
C                                   7: Integration time, sec
C                                   8: Bandwidth, MHz
C                                   9: Number of freq. channels
C                                   10:Max blockage
C      BPARM(10)      BPARM         User specified array.
C                                   1:  Factor to the calculated
C                                       noise. 0 => 1
C                                      -1 = > 0 (no noise)
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'UVCON.INC'
C      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSCD.INC'
      DATA PRGM /'UVCON '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVFILN (PRGM, IRET)
C                                       exit if calculate the OUTFILE
C                                       and BPARM.EQ.0
      IF (IRET .EQ. 10) THEN
         IRET = 0
         GO TO 990
         END IF
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL GETUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL FILHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE UVFILN (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVFILN gets input parameters for UVCON and creates an output file.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                4 => error creating output file.
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in UVCON for more details.
C
C-----------------------------------------------------------------------
      CHARACTER PRGN*6, OLDNAM*12, DEFNAM*12
      CHARACTER UNITS*8
      REAL   RAINC, DECINC, BEMJ, BEMN, B2PIX
     *   , EPS
      INTEGER  NPX, NPY
      HOLLERITH CATH(256)
      INTEGER  JERR, IERR, IROUND, NPARM, MXFLD, I
      LOGICAL   T, F, WASOME
      REAL      CATR(256)
C      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVCON.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATR, CATH, CATBLK)
      DATA DEFNAM /'UV DATA FILE'/
      DATA T, F /.TRUE.,.FALSE./
      DATA EPS /0.00001/
C-----------------------------------------------------------------------
C                                       the coefficient for calculating
C                                       the gaussian primary beam
      COEF = 4 * ALOG(2.0)
      CALL RANDIN (JERR)
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      MXFLD = MAXAFL
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
C      NPARM = 59 + 2 * MXFLD
C      NPARM = 71 + 2 * MXFLD
C      NPARM = 71 + 4 * MXFLD
C      NPARM = 74 + 4 * MXFLD
C                                       I2FILE added
C      NPARM = 86 + 4 * MXFLD
C                                       DOW added
C      NPARM = 87 + 4 * MXFLD
C                                       CPARM added
      NPARM = 97 + 4 * MXFLD
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
C                                       VLA or VLBA
      IF (INFILE(10:13) .EQ. 'VLA-') ARNAME = 'VLA     '
      IF (INFILE(10:13) .EQ. 'VLBA') ARNAME = 'VLBA    '

      CALL H2CHR (48, 1, XI2FIL, I2FILE)
      CALL H2CHR (48, 1, XOFILE, OFILE)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XCMETH, CMETH)
      CALL H2CHR (4, 1, XCMOD, CMOD)
C                                       calculate the output file?
      DOUTFI = OFILE(1:1) .NE. ' '
C                                       use the second input file?
      DOIN2F = I2FILE(1:1) .NE. ' '
C                                       Get other input

      CALL GETINP (IERR)
      IF (IERR.NE.0) JERR = IERR
C                                       exit if calculate output file
C                                       and BPARM(10).EQ.0
      IF (DOUTFI .AND. BPARM(10).EQ.0) THEN
         JERR = 10
         GO TO 999
         END IF
C                                       calculate primary beam
C                                       of the array antenna in the
C                                       normalized coordinates
C                                       U = pi * (d/lambda) *sin(theta)
C                                       Consider all antennas are
C                                       identical circular dishes
      BTYPE = BPARM(6) + 0.01
C                                       range of the primary beam
C                                       at one side
      RANGPB = BPARM(9)
C
      IF (RANGPB .LT. 0.01) RANGPB = 2.5
      CALL PRB
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
C                                       Crunch input parameters.
      SEQOUT = IROUND (XSOUT)
      SEQ2 = IROUND (XS2)
      DISOUT = IROUND (XDISO)
      DISK2 = IROUND (XDISK2)
C                                       default is 28arcmin- the VLA
C                                       FWHM at L band (21cm)
      IF (PRBMAJ .EQ. 0) PRBMAJ = 0.4666667
      IF (PRBMIN .EQ. 0) PRBMIN = 0.4666667
C                                       convert the gausian primary
C                                       beam paramers to radians
      PRBPA = PRBPA * DG2RAD
      PRBMAJ = PRBMAJ * DG2RAD
      PRBMIN = PRBMIN * DG2RAD
      PRBP = PRBPA
      PRBMA = PRBMAJ
      PRBMI = PRBMIN
C                                       CC file version number
      VER = IROUND (XVER)
C                                       Number of fields
      MFIELD = 1
      IF (XNMAPS.GT.0.0) MFIELD = IROUND (XNMAPS)
C                                       Start component number
      LIMFLX = XFLUX
      NONEG = F
      WASOME = F
      DO 20 I = 1,MFIELD
         IF (I.LE.MAXAFL) THEN
            BCOMP(I) = XBCOMP(I) + 0.1
            BCOMP(I) = MAX (BCOMP(I), 1)
            NCOMP(I) = ABS (XNCOMP(I)) + 0.1
            IF (XNCOMP(I).LE.-0.5) NONEG = T
            IF (NCOMP(I).GT.0) WASOME = T
         ELSE
            BCOMP(I) = 1
            NCOMP(I) = 0
            IF (WASOME) NCOMP(I) = 1000000000
            END IF
   20    CONTINUE
      FACGRD(1) = -FACTOR
      IF (ABS(FACGRD(1)) .LT. 1.0E-20) FACGRD(1) = -1.0
      FACGRD(2) = 1
C                                       Check model
      DOPTMD = ABS (SMODEL(1)) .GT. 1.0E-20
      PTFLX = SMODEL(1)
      PTRAOF = SMODEL(2)
      PTDCOF = SMODEL(3)
      PARMOD(1) = SMODEL(4)
      PARMOD(2) = SMODEL(5)
      PARMOD(3) = SMODEL(6)
      PARMOD(4) = SMODEL(7)
C                                       Consider pointing error?
      DOPOIN = BPARM(3) .GT. EPS
C      DOPOIN = (BPARM(3)+BPARM(4)+BPARM(5)) .GT. EPS
C                                       Consider phase errors?
      DOPHAS = BPARM(3) .LT. 0
      IF(DOPHAS) THEN
         HPHASR = ABS(BPARM(3))
         HAMPFR = BPARM(4)
         DOONE = BPARM(5) .EQ. 0
         END IF
C                                       multiply the model by primary
C                                       beam?
      DOPRBM = BPARM(6) .GT. 0.001
C                                       Shift the UV data by
C                                       RASHIFT, DECSHIFT? or stay the
C                                       tangent point at RA=0, DEC
C                                       =APARM(3)
      DOSHFT = BPARM(8) .LT. 0.01
C                                       IF the difference  of the
C                                       current and  previos time is
C                                       less than TIMTOL, the pointing
C                                       error or the simulated phase
C                                       of the given antenna is
C                                       not changed
      TIMTOL = (BPARM(7)-0.1/60) /60.0/24.0
C                                       Let TIMTOL 0.1sec less
      IF (TIMTOL .LT. 0.01/60.0/24.0) TIMTOL = 1.0/60.0/24.0
C                                       estimate pointing errors for
C                                       the antennas.
      IF (DOPOIN) CALL POERR
C                                       If pointing error or phase
C                                       simulation then
C                                       CMETH=DFT and CMOD = COMP
      IF ((DOPOIN.OR.DOPHAS) .AND.
     *   (CMETH .EQ. 'GRID' .OR. CMOD .EQ. 'IMAG') ) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1015)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1020)
         CALL MSGWRT (8)
         CMETH = 'DFT '
         CMOD = 'COMP'
         END IF

C                                       If variable primary beam
C                                       is simulated then
C                                       CMETH=DFT and CMOD = COMP
      IF ((BTYPE .EQ. 5) .AND.
     *   (CMETH .EQ. 'GRID' .OR. CMOD .EQ. 'IMAG') ) THEN
         WRITE (MSGTXT,1070)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1075)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1080)
         CALL MSGWRT (8)
         CMETH = 'DFT '
         CMOD = 'COMP'
         END IF
C                                       Get  modeling method
      METHOD = -1
      IF (CMETH.EQ.'DFT ') METHOD = -1
      IF (CMETH.EQ.'GRID') METHOD = 1
C                                       Get  model type
      MODEL = 0
      IF (CMOD.EQ.'COMP') MODEL = 1
      IF (CMOD.EQ.'IMAG') MODEL = 2
C
C
C                                       Create new header.
      CALL NEWHED (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Get uv header info and
C                                       verify header structure.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Put new values in CATBLK.
C                                       Get naming defaults
      OLDNAM = DEFNAM
      CALL MAKOUT (OLDNAM, '      ', 0, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
C                                       Image type ='UV'
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C                                       shift UVCREA here from bottom
C                                       Create output file.
      CCNO = 1
      CALL UVCREA (DISOUT, CCNO, BUFFER, IERR)
      IF (IERR .NE. 0) THEN
         WRITE (MSGTXT,1060) IERR
         JERR = 4
         GO TO 990
         END IF
C                                       Get info on model file(s)
      ISTOKE = 1
      IF (DOPTMD) THEN
         DO3DIM = .FALSE.
      ELSE
         IF (NAME2 .EQ. ' ') THEN
            WRITE (MSGTXT,1025)
            JERR = 1
            GO TO 990
            END IF
         CALL SETGDS (DISOUT, CCNO, NAME2, CLAS2, SEQ2, DISK2, MFIELD,
C         CALL SETGDS (0, 0, NAME2, CLAS2, SEQ2, DISK2, MFIELD,
     *      VER, NCOMP, BCOMP, MODEL, METHOD, BUFF1, BUFF2, ISTOKE,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         IF (MODEL.EQ.2) THEN
            MSGTXT = 'Using images for the source model'
         ELSE
            MSGTXT = 'Using Clean Component source model'
            END IF
         CALL MSGWRT (3)
C                                       Find header parameters of the
C                                       model
         CALL FHEAD (NAME2, CLAS2, SEQ2, DISK2, UNITS, RAINC, DECINC,
     *      BEMJ, BEMN, NPX, NPY, BUFF1, IERR)
         IF (IERR .NE. 0) THEN
            WRITE (MSGTXT,1030)
            JERR = 1
            GO TO 990
            END IF
C                                       If NPX = 2**(integer)?
         CALL TWON (NPX, IERR)
         IF (IERR .NE. 0) THEN
            WRITE (MSGTXT,1040)
            JERR = 1
            GO TO 990
            END IF

C                                       If NPY = 2**(integer)?
         CALL TWON (NPY, IERR)
         IF (IERR .NE. 0) THEN
            WRITE (MSGTXT,1050)
            JERR = 1
            GO TO 990
            END IF

C
         IF (UNITS(4:7).EQ.'BEAM' .AND. CMOD.EQ.'IMAG') THEN
            WRITE (MSGTXT,1065)
            CALL MSGWRT (8)
C                                       Number of pixels in the beam
C                                       1.1331 = twopi/(8*ln(2))
            B2PIX = ABS(1.1331*(BEMJ*BEMN) / (RAINC*DECINC))
            IF (B2PIX .EQ. 0) THEN
               B2PIX = 1
               WRITE (MSGTXT,1067)
               CALL MSGWRT (8)
               END IF
            FACGRD(1) = FACGRD(1) / B2PIX
            END IF
         END IF
C                                       UVCREA was here
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISOUT
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = 2
C                                       Get SEQ. no. used.
      SEQOUT = CATBLK(KIIMS)
      CALL COPY  (256, CATBLK, UVBLK)
C                                       Write Antenna file.
      CALL ANTFIL
      JERR = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVFILN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('CMETHOD and CMODEL are forced to DFT and COMP')
 1015 FORMAT ('because pointing error you request works only ')
 1020 FORMAT ('if CMETHOD = DFT and CMODEL = COMP.')
 1025 FORMAT ('SMODEL=0 =>the model name (IN2NAME) can not be blank')
 1030 FORMAT ('Error reading header parameters of the model')
 1040 FORMAT ('Number of pixels along the model RA should be 2**n')
 1050 FORMAT ('Number of pixels along the model DEC should be 2**n')
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1065 FORMAT ('The amplitudes are rescaled by BEAM/PIXEL')
 1067 FORMAT ('There is no data of beam. So the rescaling canceled.')
 1070 FORMAT ('CMETHOD and CMODEL are forced to DFT and COMP')
 1075 FORMAT ('because variable primary beam you request works only')
 1080 FORMAT ('if CMETHOD = DFT and CMODEL = COMP.')
      END
      SUBROUTINE GETUV (IRET)
C-----------------------------------------------------------------------
C   GETUV creates the scratch file with the visibilities corresponding
C   to the calculated noise, adds model and writes the output file
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48
      INTEGER   IRET, IPTRO, LENBU, IA1, IA2, NIOUT, LRECO, KBIND, VO,
     *   BO, NUMVIS, XCOUNT, ISIZE, FRQSEL,
     *   NCHAN, CHAN, LUNINT, INDINT, IH, NUMT, IFR, IFRCHA, IFRGR
      LOGICAL   T, F, DOMSG, DOSUM, ELIOK, ELJOK, BLKIOK, BLKJOK
      LOGICAL   DOINT
      INTEGER   IERR
      INCLUDE 'UVCON.INC'
C                                       Local variables
      DOUBLE PRECISION H, ANBMAX(NMAX), AZ, EL(NMAX), PARANG, FI, FJ,
     *   UANT(NMAX), VANT(NMAX), WANT(NMAX), TIMDAY, HANT,
     *   ANTLON, ANTLAT, XANT, HGRWCH, YANT, ZANT, XYANT, FREQQ,
     *   HTANG, DETANG, HPOIN, DEPOIN, TIMST
      REAL      U, V, W, TEMP, PRODSE, NOISEC, TSKYI, TSKYJ,
     *   TINTI, EFINTI, TINTJ, EFINTJ, ELIDEG, ELJDEG
      INTEGER   I, J, INDEX
C      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSCD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA LUNINT /17/
      DATA VO, BO, LENBU /0, 1, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Convert GSTIA0 to rad
      GSTIAT=GSTIA0*DG2RAD
      LRECO = LREC
      CALL UVSIZE (LRECO, NVIS, ISIZE)
C                                       Create scratch file.
      CALL SCREAT (ISIZE, BUFFER, IRET)
      IF (IRET .NE. 0) THEN
         WRITE (MSGTXT,1030) IRET
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Open scrtch file.
      CALL ZPHFIL ('SC', SCRVOL(1), SCRCNO(1), 1, PHNAME, IRET)
      CALL ZOPEN (LUNINT, INDINT, SCRVOL(1), PHNAME, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Init scrtch file for write
C                                       LRECO = length of output rec.
      LRECO = LREC
      CALL UVINIT ('WRIT', LUNINT, INDINT, NVIS, VO, LRECO, LENBU,
     *   JBUFSZ, BUFFER, BO, KBIND, IRET)
      IF (IRET .NE. 0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NUMVIS = 0
      XCOUNT = 0
C
C                                       record visibilities
      NUMT = (HMAX - HMIN) / TAU + 1.0D0
      DO 100 IH = 1, NUMT
         H = HMIN + (IH - 1) * TAU
C                                       H is hour angle at the array
C                                       center
         HGRWCH = H + SITELO
C                                       HGRWCH is hour angle at
C                                       GREENWICH
C         TIMDAY = 0.5D0 + H /TWOPI / 1.00274D0
C         TIMDAY = H / TWOPI / 1.00274D0
C         TIMDAY = DMOD(TIMDAY, 1.0D0)
C                                       Time corresponding to the hour
C                                       angle
         TIMDAY = (HGRWCH-GSTIAT) /TWOPI / 1.00274D0
         IF (IH .EQ. 1) TIMST = TIMDAY
C                                       Make times positive
         IF (TIMST .LE. 0) TIMDAY = TIMDAY + 1.D0/1.00274D0
         DO 20 I = 1, N
C                                       ANBMAX is for blockage
            ANBMAX(I) = 0.0D0
C                                       return back to the absolute
C                                       coordinate relatively the Earth
C                                       center
            XANT = XCENT + LX(I)
            YANT = YCENT + LY(I)
            ZANT = ZCENT + LZ(I)
            XYANT = SQRT(XANT*XANT + YANT*YANT)
C                                       Longitute of the antenna
C                                       Minus before YANT to get
C                                       positive to west.
C                                       All 4 coordinate systems
C                                       are converted to RH!
            ANTLON = ATAN2(-YANT, XANT)
            ANTLAT = ATAN2(ZANT, XYANT)
C                                       HH is hour angle at the given
C                                       antenna
C                                       wrong sign of west ANTLON
C                                       LK 04/26/07
C            HANT = H + ANTLON - SITELO
            HANT = H - ANTLON + SITELO
            HPOIN = HANT - RASH / COS(DELTA)
            DEPOIN = DELTA + DECSH
            CALL GETANG (HPOIN, DEPOIN, ANTLAT, AZ, EL(I), PARANG)
 20         CONTINUE
C                                       RA, DEC depend on DOSHIFT
         HTANG = HGRWCH - RA*DG2RAD
         DETANG = DEC * DG2RAD
         CALL UVANT (HTANG, DETANG, N, LX, LY, LZ, UANT, VANT, WANT)
         DO 40 I = 1,N-1
            DO 30 J = I+1,N
               CALL BLOCK (I, J, UANT, VANT, WANT, DIAM, FI, FJ)
               ANBMAX(I) = MAX (ANBMAX(I), FI)
               ANBMAX(J) = MAX (ANBMAX(J), FJ)
 30            CONTINUE
 40         CONTINUE
         DO 90 I = 1,N-1
            ELIOK = EL(I) .GE. ELMIN
            BLKIOK = ANBMAX(I) .LE. BLKREJ
C                                       TSKY of the antenna I
            IF (EL(I) .GT. 0.1) THEN
               TSKYI = TSKYZE / SIN(EL(I))
            ELSE
               TSKYI = 0
               END IF
            DO 80 J = I+1,N
               ELJOK = EL(J) .GE. ELMIN
               BLKJOK = ANBMAX(J) .LE. BLKREJ
C                                       TSKY of the antenna J
               IF (EL(J) .GT. 0.1) THEN
                  TSKYJ = TSKYZE / SIN(EL(J))
               ELSE
                  TSKYJ = 0
                  END IF
               IF ((BLKIOK .AND. BLKJOK) .AND.
     *            (ELIOK .AND. ELJOK)) THEN
                  IA1 = I
                  IA2 = J
                  IFR = 0
                  DO 60 IFR = 1, NFRTOT
                     IF (NFRGR .EQ. 0) THEN
                        IF (ONECHA) THEN
C                                       one channel UV data
C                                       for each frequency
                           FREQQ = FREQNM + (IFR - 1) * BANDW
                        ELSE
C                                       multi channel data
C                                       UV for the first frequency
                           FREQQ = FREQNM
                           END IF
                     ELSE
C
                        IFRGR = (IFR-1) / NFRCHA + 1
                        IFRCHA = IFR - (IFRGR-1)*NFRCHA
                        FREQQ = CPARM(IFRGR+1)*1.E6 +
     *                     (IFRCHA - 1) * BANDW
                        END IF
C                                       use the VLA/VLBA convention:
C                                       the bigger antenna number is
C                                       subtructed of the smaller one
                     U = (UANT(I)-UANT(J)) * FREQQ / VELITE
                     V = (VANT(I)-VANT(J)) * FREQQ / VELITE
                     W = (WANT(I)-WANT(J)) * FREQQ / VELITE
                     NUMVIS = NUMVIS+1
                     BUFFER(IPTRO+ILOCU) = U
                     BUFFER(IPTRO+ILOCV) = V
                     BUFFER(IPTRO+ILOCW) = W
                     BUFFER(IPTRO+ILOCT) = TIMDAY
C                                       baseline
                     IF (ILOCB.GE.0) THEN
                        BUFFER(IPTRO+ILOCB) = IA1 * 256 + IA2
                     ELSE
                        BUFFER(IPTRO+ILOCA1) = IA1
                        BUFFER(IPTRO+ILOCA2) = IA2
                        BUFFER(IPTRO+ILOCSA) = 1.0
                        END IF
C                                       freqid
C                     BUFFER(IPTRO+ILOCFQ) = IFR


C                                       restore the product of SEFDs
C                                       taking into account the sky
C                                       noise depending on elevation
                     PRODSE = SEFD(I)*(NOISTE(I)+TSKYI) *
     *                  SEFD(J)*(NOISTE(J)+TSKYJ)
C-----------------------------------------
C                                       Interpolate the IN2FILE's data
                     DOINT = .TRUE.
                     IF (DOIN2F) THEN
                        ELIDEG = EL(I)*RAD2DG
                        ELJDEG = EL(J)*RAD2DG
                        CALL UVCINT (NLINE2, IAN, ELEVA, TSYS, EFFEL,
     *                     I, ELIDEG, TINTI, EFINTI, IERR)
                        DOINT = DOINT .AND. IERR.EQ.0
                        CALL UVCINT (NLINE2, IAN, ELEVA, TSYS, EFFEL,
     *                     J, ELJDEG, TINTJ, EFINTJ, IERR)
                        DOINT = DOINT .AND. IERR.EQ.0
C                                       remove the efficiency taken
C                                       from INFILE EFF() and substitute
C                                       it by the interpolated value
C                                       EFINT
                        IF (DOINT) PRODSE =
     *                     (SEFD(I)*EFF(I)/EFINTI*TINTI) *
     *                     (SEFD(J)*EFF(J)/EFINTJ*TINTJ)
                        END IF
C------------------------------------------
C                                       NOISEC is the RMS of the
C                                       calculated noise
                     NOISEC = SQRT (PRODSE / 2 / TAUSEC / BANDW)
C
                     IF (ONECHA) THEN
                        INDEX = IPTRO + NRPARM
                     ELSE
                        INDEX = IPTRO + NRPARM + (IFR-1)*3
                        END IF
C                                       TEMP is a random 'gaussian'
C                                       number with mean = 0 and
C                                       RMS = 1
                     CALL NOISE (TEMP)
C                                       Real part of the visibility
                     BUFFER(INDEX) = TEMP * NOISEC * CNOISE
C                                       Image part of the visibility
                     CALL NOISE (TEMP)
                     BUFFER(INDEX + 1) = TEMP * NOISEC * CNOISE
C                                       Weight of the visibility
                     BUFFER(INDEX + 2) = SEFDMI / PRODSE
C                                       Record
C                                       IM, RE, W for each frequency
                     IF (ONECHA) THEN
C
                        XCOUNT = XCOUNT + 1
                        IPTRO = IPTRO + LRECO
                        NIOUT = NIOUT + 1
                        CALL UVDISK('WRIT',LUNINT,INDINT,BUFFER,NIOUT,
     *                     KBIND, IRET)
                        IPTRO = KBIND
                        NIOUT = 0
                        END IF
 60                  CONTINUE
C                                       record all channels
                  IF (.NOT. ONECHA) THEN
                     XCOUNT = XCOUNT + 1
                     IPTRO = IPTRO + LRECO
                     NIOUT = NIOUT + 1
                     CALL UVDISK('WRIT',LUNINT,INDINT,BUFFER,NIOUT,
     *                  KBIND, IRET)
                     IPTRO = KBIND
                     NIOUT = 0
                     END IF
C
                  END IF
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C                                       Do history file entries:
      NUMHIS=2*N+11
      WRITE (HISCRD(1),2001) N
      WRITE (HISCRD(2),2002) DELTA * RAD2DG
      WRITE (HISCRD(3),2003) HMIN/TWOPI*24.0D0, HMAX/TWOPI*24.0D0
      WRITE (HISCRD(4),2004) TAU / TWOPI * 86400.0D0
      WRITE (HISCRD(5),2005) ELMIN * RAD2DG
      WRITE (HISCRD(6),2006) BLKREJ
      WRITE (HISCRD(7),2007) SITELA * RAD2DG, SITELO * RAD2DG
      WRITE (HISCRD(8),2008) FREQNM
      WRITE (HISCRD(9),2009) VELITE / FREQNM
      WRITE (HISCRD(10),2010)
      DO  200 I = 1,N
         WRITE (HISCRD(2*I+10),2011) I, LX(I), LY(I), LZ(I), DIAM(I)
         WRITE (HISCRD(2*I+11),2012) I, EFF(I), NOISTE(I), NNLEV(I)
 200     CONTINUE
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNINT, INDINT, BUFFER, NIOUT, KBIND, IRET)
      IF (IRET .NE. 0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, SCRVOL(1), SCRCNO(1), LUNINT, CATBLK, IRET)
C                                       close the scratch file
      CALL ZCLOSE (LUNINT, INDINT, IRET)
      IRET = 0
      CHAN = 1
C      NCHAN = 1
      NCHAN = CATBLK(KINAX+2)
      DOMSG = T
      DOSUM = F
      FRQSEL = -1
C                                       fit UVMSUB inputs
      CNOINT = 1
      DISINT = 0
      CALL UVMSUB (DISINT, CNOINT, DISOUT, CCNO, 0, MODEL, METHOD,
     *   CHAN, NCHAN, DOSUM, DOMSG, UVBLK, JBUFSZ, FRQSEL, BUFF1,
     *   BUFF2, BUFF3, IRET)
      IF (.NOT.DOPTMD) CALL UNSETG (BUFF2)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('GETUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('ERROR',I3,' CREATING SCRATCH FILE')
 1150 FORMAT ('GETUV: ERROR',I3,' WRITING VIS FILE')
 2001 FORMAT (' Number of antennas:  ',I3)
 2002 FORMAT (' Declination:  ',F10.5,' degrees')
 2003 FORMAT (' Hour-angle coverage:  ',F10.5,' to ',F10.5,' hours')
 2004 FORMAT (' Integration time:  ',F10.3,' seconds')
 2005 FORMAT (' Minimum allowed elevation angle:  ',F10.5,' degrees')
 2006 FORMAT (' Maximum allowed fractional blockage:  ',F10.5)
 2007 FORMAT (' Site latitude, longitude: ',2F8.3,' deg')
 2008 FORMAT (' Nominal frequency:  ',1PE20.12,' Hz')
 2009 FORMAT (' Nominal wavelength:  ',1PE20.12,' meters')
 2010 FORMAT (' Antenna x-y-z coordinates and diameters (all in ',
     *   'meters):')
 2011 FORMAT (I3,3G16.7, G12.5)
 2012 FORMAT (I3, ' ef=', F5.2, ' T=', F6.1, ' nlev=', I2)
      END
      SUBROUTINE FILHIS
C-----------------------------------------------------------------------
C   FILHIS creates and fills a history file.
C-----------------------------------------------------------------------
      CHARACTER   HILINE*72, ATIME*8, ADATE*12, LABEL*8, TELE*8, OBSR*8
      HOLLERITH CATH(256)
      INTEGER   LUN, I, IERR, TIME(3), DATE(3)
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
      LOGICAL   T
      INCLUDE 'UVCON.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      DATA LUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HICREA (LUN, DISOUT, CCNO, CATBLK, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
C                                       Get current date/time.
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (HILINE,1010) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       New history
      WRITE (HILINE,1011) TSKNAM, INFILE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       UV output
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISOUT, LUN,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       MODEL
      CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       SOURCE
      WRITE (HILINE,2000) TSKNAM, SOURCE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Number of visibilities
      WRITE (HILINE,2001) TSKNAM, CATBLK(KIGCN)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Telescope, observer name.
      CALL H2CHR (8, 1, CATH(KHTEL), TELE)
      CALL H2CHR (8, 1, CATH(KHOBS), OBSR)
      WRITE (HILINE,2002) TSKNAM, TELE, OBSR
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       primary beam
C
C                                       pointing error
      IF (DOPOIN) THEN
         WRITE (HILINE,2003)
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       multiplication by the primary
C                                       beam
      IF (DOPRBM) THEN
         WRITE (HILINE,2004)
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       AIPS release
      WRITE (HILINE,2010) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any other history.
      IF (NUMHIS.LE.0) GO TO 20
         WRITE (LABEL,1015) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
C                                       Close HI file
 20   CALL HICLOS (LUN, T, BUFFER, IERR)
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISOUT, CCNO, CATBLK, 'REST', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILHIS: ERROR',I3,' CREATE/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,'INFILE =''',A,'''')
 1015 FORMAT (A6,' /')
 2000 FORMAT (A6,' SOURCE = ''',A8,'''')
 2001 FORMAT (A6,' / Number of visibilities copied=',I9)
 2002 FORMAT (A6,' / Telescope = ',A8,' Observer = ',A8)
 2003 FORMAT ('Pointing errors have been applied')
 2004 FORMAT ('The model nas been multiplied by the primary beam.')
 2010 FORMAT (A6,' RELEASE = ''',A7,' ''')
      END
      SUBROUTINE ANTFIL
C-----------------------------------------------------------------------
C   ANTFIL creates and fills the antenna file.
C-----------------------------------------------------------------------
      HOLLERITH CATH(256)
      INTEGER   IERR, ANVER, LUN, I
      REAL      CATR(256)
      DOUBLE PRECISION JD, GMSTM, GASTM, RATE
C      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVCON.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR)
      DATA LUN /27/
C-----------------------------------------------------------------------
C                                       Make sure there is antenna info
      IF (N.LE.0) GO TO 999
C                                       Setup for AN table initization
         NUMORB = 0
         NOPCAL = 2
         ANTNIF = 1
C                                       Position of the earth's pole
         POLRXY(1) = 0.0
         POLRXY(2) = 0.0
         UT1UTC = 0.0
         DATUTC = 0.0
         TIMSYS = 'UTC'
C                                       Array name
C         ANAME = 'VLBA'
         ANAME = ARNAME
C                                       Array center (rel to center of
C                                       earth)
         IF (ANAME.EQ.'VLBA') THEN
            ARRAYC(1) = 0
            ARRAYC(2) = 0
            ARRAYC(3) = 0
         ELSE
            ARRAYC(1) = XCENT
            ARRAYC(2) = YCENT
            ARRAYC(3) = ZCENT
            END IF
C                                       Get GST0 and Earth rotation rate
         CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
         CALL JULDAY (RDATE, JD)
         CALL GSTROT (JD, GMSTM, GASTM, RATE)
         GSTIA0 = GMSTM
         DEGPDY = RATE
         SAFREQ = FREQ
         ANFQID = -1
         ANVER = 1
         ANTNIF = 1
         XYZHAN = 'RIGHT'
         TFRAME = ' '
C                                       Create/init file
         CALL ANTINI ('WRIT', BUFFER, DISOUT, CCNO, ANVER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       init basic AN record
         ANNAME = ' '
         STAXOF = 0.0
         STAXYZ(1) = 0.0D0
         STAXYZ(2) = 0.0D0
         STAXYZ(3) = 0.0D0
         ORBPRM(1) = 0.0D0
         NOSTA = 0
         MNTSTA = 0
         POLAA = 0.0
         POLAB = 0.0
         CALL RFILL (3, 0.0, POLCA)
         CALL RFILL (3, 0.0, POLCB)
         POLTYA = 'R'
         POLTYB = 'L'
         DIAMAN = 0.0
         FWHMAN(1) = 0.0
C                                       AN records
         DO 20 I = 1,N
            IF (ANAME.EQ.'VLA') THEN
               STAXYZ(1) = LXORI(I)
               STAXYZ(2) = LYORI(I)
               STAXYZ(3) = LZORI(I)
            ELSE IF (ANAME.EQ.'VLBA') THEN
               STAXYZ(1) = LXORI(I)
               STAXYZ(2) = LYORI(I)
               STAXYZ(3) = LZORI(I)
            ELSE
               STAXYZ(1) = LX(I)
               STAXYZ(2) = LY(I)
               STAXYZ(3) = LZ(I)
               END IF
            NOSTA = I
            IANRNO = I
            CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) GO TO 990
 20         CONTINUE
C                                       Fill in header and close
         CALL TABIO ('CLOS', 1, IANRNO, ANNAME, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 990
         GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1020) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ERROR ',I3,' OCCURED WRITING ANTENNA FILE')
      END
      SUBROUTINE NEWHED (IRET)
C-----------------------------------------------------------------------
C   NEWHED is a routine in which the catalog header is constructed.
C   Necessary values can be read in the areas markes "USER CODE
C   GOES HERE".
C
C   NOTE: the AIPS convention for the coordinate reference value
C   for the STOKES axis is that 1,2,3,4 represent I, Q, U, V
C   stokes' parameters and -1,-2,-3,-4 represent RR, LL, RL and
C   LR correlator values.  Currently set for R and L polarization
C   ie Ref. value = -1 and increment = -1.
C
C   The MINIMUM information required here is that
C   required to define the size of the output file; ie.
C      CATBLK(KIGCN)   = I   number of visibility records
C      CATBLK(KIPCN) = Number of random parameters.
C      CATBLK(KIDIM)= the number of axes,
C      CATBLK(KINAX+i) = the dimension of each axis.
C    Input:
C     CATBLK(256)    I     Output catalog header, also CATR, CATD
C                          The OUTNAME, OUTCLASS, OUTSEQ are entered
C                          elsewhere.
C    Output:
C     CATBLK(256)    I     Modified output catalog header.
C     IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER RTYPES(9)*8, TYPES(7)*8, UNITS*8, OBSDAT*8,
     *   TELE*8, OBSR*8, INSTR*8
      HOLLERITH CATH(256)
      INTEGER   I, NAXIS, NRAN, NCHAN, NPOLN, NDIM(7), INDEX, IRET,
     *   XCOUNT, NUMT
      REAL      CATR(256), CRPIX(7), CRINC(7), EPOCH
      DOUBLE PRECISION    CATD(128), CRVAL(7)
      INCLUDE 'UVCON.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE  (CATBLK, CATH, CATR, CATD)
C                                       User definable values
C                                       Random parameters.
C                                         No. random parameters.
      DATA NRAN /7/
C                                         Rand. parm. names.
      DATA RTYPES /'UU-L', 'VV-L', 'WW-L', 'TIME1', 'SUBARRAY',
     *   'ANTENNA1', 'ANTENNA2', 2*' '/
C                                       Uniform axes.
C                                         No. axes.
      DATA NAXIS /5/
C                                         Axes names.
      DATA TYPES /'COMPLEX ','STOKES  ','FREQ    ',
     *   'RA      ','DEC     ',2*'        '/
C                                         Axis dimensions
      DATA NDIM /3,1,1,1,1,0,0/
C                                         Reference values
      DATA CRVAL /1.0D0, -1.0D0, 5*0.0D0/
C                                         Reference pixel.
      DATA CRPIX /7*1.0/
C                                         Coordinate increment.
      DATA CRINC /1.0, -1.0, 0.0, 0.0, 0.0, 2*0.0/
C                                       Epoch of position.
      DATA EPOCH /2000.0/
C                                       Units
      DATA UNITS /'JY      '/
C                                       "Observing" date
      DATA OBSDAT /'20000101'/
C-----------------------------------------------------------------------
C                                       Zero fill CATBLK
      CALL FILL (256, 0, CATBLK)
C                                       Fill axis arrays.
C                                       Random axis names
      DO 10 I = 1,KIPTPN
         INDEX = KHPTP + (I-1) * 2
         IF (I.LE.7) THEN
            CALL CHR2H (8, RTYPES(I), 1, CATH(INDEX))
         ELSE
            CALL CHR2H (8, RTYPES(8), 1, CATH(INDEX))
            END IF
 10      CONTINUE
C                                       Uniform axes
      DO 30 I = 1,KICTPN
C                                       Init dimension
         CATBLK(KINAX+I-1) = NDIM(I)
C                                       Init. increment.
         CATR(KRCIC+I-1) = CRINC(I)
C                                       Init. rotation.
         CATR(KRCRT+I-1) = 0.0
C                                       Init. ref pixel.
         CATR(KRCRP+I-1) = CRPIX(I)
C                                       Init. ref value.
         CATD(KDCRV+I-1) = CRVAL(I)
C                                       Fill axis type from
C                                       TYPES
         INDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, TYPES(I), 1, CATH(INDEX))
 30      CONTINUE
C                                       Fill in values.
C                                       Fill other character strings.
C                                       Source name
      SOURCE = ' '
C                                       Telescope.
      TELE = ' '
C                                       Receiver
      INSTR = ' '
C                                       Observer's name.
      OBSR = ' '
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = NRAN
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = EPOCH
C                                       Convolving beam
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
      CATBLK(KINIT) = 0
C                                       Max. min.
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
C                                       Shift
      CATR(KRXSH) = 0.0
      CATR(KRYSH) = 0.0
C                                       "Old" (observed) position.
      CATD(KDORA) = 0.0D0
      CATD(KDODE) = 0.0D0
C                                       Rest Frequency
      CATD(KDRST) = 0.0D0
C                                       Alternate ref. value & pixel
      CATD(KDARV) = 0.0D0
      CATR(KRARP) = 0.0
      CATBLK(KIALT) = 0
C                                       Sort order ('**'=>unsorted)
      CALL CHR2H (2, 'TB', 1, CATH(KITYP))
C                                       No magic value blanking.
      CATR(KRBLK) = 0.0
C                                       Units
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C-----------------------------------------------------------------------
C                                       Enter values for data:
C                                       XCOUNT = number of vis. record.
C                                       RA = Right ascension (2000)
C                                          in degrees.
C                                       DEC = Declination in degrees.
C                                       FREQ = frequency of obs in Hz.
C                                       BANDW = bandwidth or channel
C                                           separation.
C                                       NCHAN = Number of freq chan.
C                                       NPOLN = number of polarization
C                                            correlators.
C                                       OBSDAT = Reference date of time
C                                          tags for data as "yyyymmdd"
C                                       SOURCE = source name (8 char)
C                                       TELE = telescope name (8 char)
C                                       INSTR = Receiver name (8 char)
C                                       OBSR = Observers name (8 char)
C
C                                       Figure out file size:
      NUMT = (HMAX - HMIN) / TAU + 1.0D0
      XCOUNT = NUMT * (N * (N-1) / 2)
      IF (ONECHA) THEN
         NCHAN = 1
         XCOUNT = XCOUNT * NFRTOT
      ELSE
         NCHAN = NFRTOT
         END IF
      NPOLN = 1
C                                       Tangent point of the UV data
      RA = 0.0D0
      DEC = DELTA / TWOPI * 360.0D0
C
      FREQ = FREQNM
C                                       Insert values in header.
C                                       Number of vis.
      CATBLK(KIGCN) = XCOUNT
C                                       Position at the reference pixel
      IF (DOSHFT) THEN
         CATD(KDCRV+3) = RA + (RASH/COS(DELTA))*RAD2DG
         CATD(KDCRV+4) = DEC + DECSH*RAD2DG
      ELSE
         CATD(KDCRV+3) = RA
         CATD(KDCRV+4) = DEC
         END IF
C                                       Position of the pointing
C                                       direcrion
      CATD(KDORA) = RA + (RASH/COS(DELTA))*RAD2DG
      CATD(KDODE) = DEC + DECSH*RAD2DG
C                                       Frequency
      CATD(KDCRV+2) = FREQ
C                                       Bandwidth.
      CATR(KRCIC+2) = BANDW
C                                       Number of frequencies.
      CATBLK(KINAX+2) = NCHAN
C                                       Number of polarizations.
      CATBLK(KINAX+1) = NPOLN
C                                       Observing date.
      CALL CHR2H (8, OBSDAT, 1, CATH(KHDOB))
C                                       Object.
      CALL CHR2H (8, SOURCE, 1, CATH(KHOBJ))
C                                       Telescope.
      CALL CHR2H (8, TELE, 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, INSTR, 1, CATH(KHINS))
C                                       Observer's name.
      CALL CHR2H (8, OBSR, 1, CATH(KHOBS))
C
C                                       Finished.
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE GETINP (IERR)
C-----------------------------------------------------------------------
C   Routine to ask user for information
C   Input from common:  INFILE  C*48 File name.
C   Output: IERR                I    Return code 0=>OK
C-----------------------------------------------------------------------
      INTEGER   IERR
      INCLUDE 'UVCON.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      N = 0
C                                       Atmosphere noise at zenith
      TSKYZE = BPARM(2)
C                                       Read antenna info from file
      CALL GETANT (INFILE, I2FILE, OFILE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Use a nominal wavelength of 1mm
C                                       if both APARM(1) and APARM(2)
C                                       are less than or equal to zero.
C                                       Otherwise, if APARM(1) is
C                                       greater than zero then use a
C                                       nominal frequency of APARM(1)
C                                       Hz; otherwise, if APARM(2) is
C                                       greater than zero then use a
C                                       nominal wavelength of APARM(2)
C                                       meters:
      IF ((APARM(1).LE.0.0) .AND. (APARM(2).LE.0.0)) THEN
         FREQNM = 299.7924562D9
      ELSE
         IF (APARM(1).GT.0.) THEN
            FREQNM = APARM(1) * 1.0D9
         ELSE
            FREQNM = 2.997924562D10 / APARM(2)
            END IF
         END IF
C                                       wavelength in meters
      LAMBDA = 299.7924562D6 / FREQNM
C                                       declination of the model center
C                                       in degrees
      DELT = APARM(3)
C                                       declination of the model center
C                                       in radians
      DELTA = DELT * DG2RAD
C                                       shift of the tangent point
C                                       for the first field, in arcsec
C                                       RASH is given at the picture
      RASHS = RASHIF(1)
      DECSHS = DECSHI(1)
C                                       shift of the tangent point,
C                                       in radians
      RASH = RASHS / 3600 * DG2RAD
      DECSH = DECSHS / 3600 * DG2RAD
C
      HMIN =  APARM(4) * TWOPI / 24.0D0
      HMAX =  APARM(5) * TWOPI / 24.0D0
      ELMIN = APARM(6) * DG2RAD
C                                       default for APARM(7)=1.E6
C                                       to simulate snapshot
      IF (APARM(7) .EQ. 0) APARM(7) = 1.E6
      TAUSEC = APARM(7)
      TAU = TAUSEC * TWOPI * 1.00274D0 / 86400.0D0
      BANDW = APARM(8) * 1.E6
      IF (BANDW .LT. 0) THEN
C                                       one channel data created
         ONECHA = .TRUE.
         BANDW = -BANDW
      ELSE
C                                       multi channel data created
         ONECHA = .FALSE.
         IF (BANDW .EQ. 0) BANDW = 1.E6
         END IF
      NFRCHA = APARM(9) + 0.01
      IF (NFRCHA .LE. 0) NFRCHA = 1
C                                       Frequency groups (IFs)
      NFRGR = CPARM(1) + 0.01
      NFRTOT = NFRCHA*NFRGR
      IF (NFRGR .LE. 0) NFRTOT = NFRCHA
C                                       Fource ONECHA=.TRUE. IF
C                                       NFRGR .GT. 0
      IF (NFRGR .GT. 0) ONECHA = .TRUE.
C                                       default for APARM(10)=1
C                                       to exclude the blockage
C                                       consideration
      IF (APARM(10) .EQ. 0) APARM(10) = 1
      BLKREJ = APARM(10)
C                                       CNOISE is the factor to
C                                       multiply noise
      IF (BPARM(1) .EQ. 0) THEN
         CNOISE = 1
      ELSE
         IF (BPARM(1) .LT. 0) THEN
            CNOISE = 0
         ELSE
            CNOISE = BPARM(1)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE GETANT (FILE, FILE2, OUFILE, IERR)
C-----------------------------------------------------------------------
C  This subroutine reads, from an input file specified by name "file",
C  the antenna information.
C  The first (ascii) data record should specify n, the number of
C  elements.
C  The second data record should specify the site latitude and
C  longitude, in degrees.
C  The third record must specify two conversion factors: first,
C  a multiplicative factor for converting antenna coordinates to meters,
C  and, second, a factor for converting antenna diameters to meters.
C  (E.g., if the station coordinates are in nanoseconds and the antenna
C  diameters are in feet, then the two factors that need to be supplied
C  are 0.2997924562 (approximately) and 0.3048 (exactly).)
C  Then, n records must follow, each with eight numbers:  The coordinate
C  system identificator; the three coordinates of the antenna location;
C  antenna diameter; efficiency of the antenna; noise temperature;
C  number of level in the digitizer.
C
C  add reading the second input file: FILE2 with four columns:
C  1. Antenna number
C  2. Elevation
C  3. TSYS
C  4. Efficiency

C   Inputs:
C    FILE     C*48  File name
C    FILE2    C*48  Second file name
C    OUFILE   C*48  output file name
C   Outputs in common:
C    N        I     Number of antennas
C    LX(*)    D     "X" coordinate of antennas (m)
C    LY(*)    D     "Y" coordinate of antennas (m)
C    LZ(*)    D     "Z" coordinate of antennas (m)
C    DIAM(*)  D     Antenna diameters (m)
C    SITELA   D     Site latidude (rad)
C    SITELO   D     Site longitude (rad)
C    ELEVA(*) R     Elevations of antennas
C    TSYS(*)  R     System temperature of antennas
C    EFFEL(*) R     Efficiency of antennas
C   Outputs:
C    IERR     I     Return code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER FILE*48, FILE2*48, OUFILE*48
      INTEGER   IERR
      INTEGER   NCH
      INTEGER   LUN, LUNPR, FIND, PFIND, NBYTES, KBP, CENTER, NL
      LOGICAL   F
C
      DOUBLE PRECISION XTEMP, YTEMP, ZTEMP, LAT, LONG, ALT
      INTEGER   I, J, COOR, MODE, JT, JTRIM
      REAL  AEFF, NLEV, ALFA, EFFIC, TEMPER, SEFDCU
      CHARACTER LINE*80
      DOUBLE PRECISION CONST1, CONST2, X, SINLA, SINLO, COSLA,
     *   COSLO
C
      INCLUDE 'UVCON.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       open file for record the output
C                                       file at the equatorial
C                                       coordinate system
      IF (DOUTFI) THEN
         LUNPR = 3
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OUFILE, F, IERR)
         END IF
C                                       Open input text file for read
      LUN = 10
      CALL ZTXOPN ('READ', LUN, FIND, FILE, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1001)
         GO TO 990
         END IF



C                                       Get number of antennas
   10 CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C-----------------------------------
C                                       skip comments
      IF (LINE(1:1) .EQ. ';') THEN
         IF (DOUTFI) THEN
            NCH = JTRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
            END IF
         GO TO 10
         END IF
C---------------------------------
C                                       Get value
      KBP = 1
      NBYTES = 80
      CALL GETNUM (LINE, NBYTES, KBP, X)
      N = X + 0.1
C                                       record number of antennas
      IF (DOUTFI) THEN
         WRITE (LINE, 3010) N
         NCH = JTRIM(LINE)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
         END IF
C                                       Tell user
      WRITE (MSGTXT,2000) N
      CALL MSGWRT (6)
C                                       get site latitude, longtitude
      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       Get values of LAT, LONG and ALT
C                                       of the array center
      KBP = 1
      CALL GETNUM (LINE, NBYTES, KBP, X)
      SITELA = X
      CALL GETNUM (LINE, NBYTES, KBP, X)
      SITELO = X
      CALL GETNUM (LINE, NBYTES, KBP, X)
      ALT = X
C                                       record site latitude, longtitude
      IF (DOUTFI) THEN
         WRITE (LINE, 3020) SITELA, SITELO
         NCH = JTRIM(LINE)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
         END IF
C                                       Tell user
      WRITE (MSGTXT,2001) SITELA
      CALL MSGWRT (6)
      WRITE (MSGTXT,2100) SITELO
      CALL MSGWRT (6)
      SITELA = SITELA * DG2RAD
      SITELO = SITELO * DG2RAD
C                                       Get conversion constants
      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       Get values
      KBP = 1
      CALL GETNUM (LINE, NBYTES, KBP, X)
      CONST1 = X
      CALL GETNUM (LINE, NBYTES, KBP, X)
      CONST2 = X
C                                       record conversion constants
      IF (DOUTFI) THEN
         WRITE (LINE, 3030)
         NCH = JTRIM(LINE)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
         END IF
C                                       Tell user
      WRITE (MSGTXT,2002) CONST1
      CALL MSGWRT (6)
      WRITE (MSGTXT,2003) CONST2
      CALL MSGWRT (6)
      SINLA = SIN(SITELA)
      SINLO = SIN(SITELO)
      COSLA = COS(SITELA)
      COSLO = COS(SITELO)
C                                       convert geodetic LAT, LONG, ALT
C                                       of the array center to geocenter
C                                       RH equatorial coordinate system
C                                       XCENT, YCENT, ZCENT
      MODE = 0
      CALL GEOXYZ (MODE, SITELO, SITELA, ALT, XCENT, YCENT, ZCENT, IERR)

C                                       Read antenna info
      DO 100 I = 1,N
 20      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         IF (IERR.NE.0) GO TO 980
         JT = JTRIM (LINE)
C                                       skip comments
         IF (LINE(1:1) .EQ. ';') THEN
            IF (DOUTFI) THEN
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
               END IF
            GO TO 20
            END IF
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
C                                       efficiency of the antenna
         IF (KBP .LE. NBYTES) THEN
            CALL GETNUM (LINE, NBYTES, KBP, X)
            EFFIC = X
            IF (EFFIC .EQ. 0) EFFIC = 0.5
         ELSE
            EFFIC = 0.5
            END IF
         EFF(I) = EFFIC
C                                       Noise temperature
         IF (KBP .LE. NBYTES) THEN
            CALL GETNUM (LINE, NBYTES, KBP, X)
            TEMPER = X
            IF (TEMPER .EQ. 0) TEMPER = 50
         ELSE
            TEMPER = 50
            END IF
         NOISTE(I) = TEMPER
C                                       Levels of digitizer
         IF (KBP .LE. NBYTES) THEN
            CALL GETNUM (LINE, NBYTES, KBP, X)
            NLEV = X
            IF (NLEV .EQ. 0) NLEV = 2.0
         ELSE
            NLEV = 2.0
            END IF
         IF (NLEV .GT. 4 .OR. NLEV .LT. 2) NLEV = 4.0
         NL = NLEV + 0.1
         NNLEV(I) = NL
C                                       Coordinates are relatively the
C                                       array center or relatively the
C                                       Earth center
         IF (KBP .LE. NBYTES) THEN
            CALL GETNUM (LINE, NBYTES, KBP, X)
            CENTER = X
            IF (CENTER .NE. 0) CENTER = 1
         ELSE
            CENTER = 0
            END IF
C                                       Calculate SEFD of the antennas
            AEFF = EFFIC * PI * DIAM(I) * DIAM(I) / 4
C                                       Alfa estimates the noise
C                                       increasing because of digitizing
C                                       The following formula gives a
C                                       good approximation when the
C                                       noise is considered to be
C                                       proportional to the SEFD's
C                                       product
            ALFA = 2.24 / SQRT(NLEV)
C                                       calculate SEFD/TEMPER to
C                                       multiply it later by sum of
C                                       TEMPER and atmosphere noise
C                                       depending on elevation
            SEFD(I) = ALFA * 2760 / AEFF
         IF (COOR .EQ. 2) THEN
            LAT = LX(I) * DG2RAD
            LONG = LY(I) * DG2RAD
            ALT = LZ(I)
            WRITE (MSGTXT,2200) I, LX(I), LY(I), LZ(I), DIAM(I),
     *         EFFIC, TEMPER, NL
         ELSE
            WRITE (MSGTXT,2004) I, LX(I), LY(I), LZ(I), DIAM(I),
     *         EFFIC, TEMPER, NL
            END IF
         CALL MSGWRT (6)
C                                       store the original LX, LY, LZ
         LXORI(I) = LX(I)
         LYORI(I) = LY(I)
         LZORI(I) = LZ(I)
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
C
            LX(I) = XTEMP
            LY(I) = YTEMP
            LZ(I) = ZTEMP
            END IF
C
         IF (COOR .EQ. 2) THEN
C                                       convert geodetic LAT, LONG, ALT
C                                       of the antenna to geocenter
C                                       RH equatorial coordinate system
            CALL GEOXYZ (MODE, LONG, LAT, ALT, LX(I), LY(I), LZ(I),
     *         IERR)
            CENTER = 1
            END IF
         IF (COOR .EQ. 3) THEN
C                                       Recalculate antenna coordinates
C                                       from the RH equatorial
C                                       coordinate system
C                                       (X to the array center meridian)
C                                       VLA for example to the RH
C                                       equatorial coordinate system
C                                       (X to the Greenwich meridian)
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
C                                       record the antenna lines
         IF (DOUTFI) THEN
            WRITE (LINE, 3040) LX(I), LY(I), LZ(I), DIAM(I),
     *         EFFIC, TEMPER, NL
            NCH = JTRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
            END IF
         WRITE (MSGTXT,2004) I, LX(I), LY(I), LZ(I), DIAM(I),
     *      EFFIC, TEMPER, NL
         CALL MSGWRT (6)
 100     CONTINUE
C                                       considering all antennas
C                                       identical take the diameter of
C                                       the first antenna for
C                                       calculation of the primary beam
      DANTT = DIAM(1)
C                                       close input file
      CALL ZTXCLS (LUN, FIND, IERR)
C                                       close output file
      IF (DOUTFI)  CALL ZTXCLS (LUNPR, PFIND, IERR)
C                                       calculate minimum of product
C                                       SEFDs at zenith
      SEFDMI = 1.E10
      DO 140 I = 1,N-1
         DO 120 J = I+1,N
            SEFDCU = SEFD(I)*(NOISTE(I) + TSKYZE) *
     *         SEFD(J)*(NOISTE(J) + TSKYZE)
            SEFDMI = MIN(SEFDMI, SEFDCU)
  120       CONTINUE
  140    CONTINUE
C
C                                       Open second input text file
C                                       for read
      IF (DOIN2F) THEN
         LUN = 10
         CALL ZTXOPN ('READ', LUN, FIND, FILE2, F, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1002)
            GO TO 990
            END IF
C
         WRITE (MSGTXT,2005)
         CALL MSGWRT (6)
C                                       Read elevation info
         I = 0
  200    CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
C                                       get the file end
            IF (IERR .EQ. 2) THEN
               IERR = 0
               GO TO 970
               END IF
            IF (IERR.NE.0) GO TO 985
            JT = JTRIM (LINE)
C                                       skip comments
            IF (LINE(1:1) .EQ. ';') GO TO 200
            I = I + 1
C                                       Get values
            KBP = 1
C                                       Antenna number
            CALL GETNUM (LINE, NBYTES, KBP, X)
            IAN(I) = X
C                                       Elevation
            CALL GETNUM (LINE, NBYTES, KBP, X)
            ELEVA(I) = X
C                                       TSYS in degrees
            CALL GETNUM (LINE, NBYTES, KBP, X)
            TSYS(I) = X
C                                       Efficiency
            CALL GETNUM (LINE, NBYTES, KBP, X)
            EFFEL(I) = X
C
            WRITE (MSGTXT,2006) IAN(I), ELEVA(I), TSYS(I), EFFEL(I)
            CALL MSGWRT (6)
C
            GO TO 200

C                                       Number of lines in the
C                                       second input file
  970    NLINE2 = I
C                                       End of the second file reading
C
C                                       close input file
         CALL ZTXCLS (LUN, FIND, IERR)
         END IF
C
      GO TO 999
C                                       Infile read error
 980  WRITE (MSGTXT,1980) IERR
      GO TO 990
C                                       Second file read error
  985 WRITE (MSGTXT,1985) IERR
      GO TO 990
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ERROR ',I3,' OPENING ANTENNA INFO TEXT FILE')
 1002 FORMAT ('ERROR ',I3,' OPENING SECOND INPUT TEXT FILE')
 1980 FORMAT ('ERROR ',I3,' READING ANTENNA INFO TEXT FILE')
 1985 FORMAT ('ERROR ',I3,' READING SECOND INPUT TEXT FILE')
 2000 FORMAT ('Number of antennas = ',I4)
 2001 FORMAT ('Site latitude = ',F8.3,' degrees')
 2100 FORMAT ('Site longtitude = ',F8.3,' degrees')
 2002 FORMAT ('Coordinate conversion factor to meters =',1PE12.5)
 2003 FORMAT ('Diameter conversion factor to meters =',1PE12.5)
 2004 FORMAT ('A(',I4,'):',3F9.0,'; d=',F4.0,
     *   ' ef=', F5.2, ' T=', F6.1, 1X, I2)
 2005 FORMAT (1X, ' ANT ', 3X,'  EL ', 6X, '  TSYS ', 5X, ' EFF ')
 2006 FORMAT (I4, 5X, F5.2, 6X, F7.2, 5X, F5.2)
 2200 FORMAT ('A(',I4,'): ',F4.1,'deg; ',
     *   F6.1,'deg; ', F6.1,'; d=',F4.0,
     *   ' ef=', F5.2, ' T=', F6.1, 1X, I2)
 3010 FORMAT (I4)
 3020 FORMAT (2F15.6)
 3030 FORMAT ('1 1')
 3040 FORMAT ('0', 3(F14.4), F5.0, F5.2, F6.1, I2)
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
      SUBROUTINE UVANT (H, DELTA, N, LX, LY, LZ, UANT, VANT, WANT)
C-----------------------------------------------------------------------
C  Given the hour-angle h, the source declination delta, the number
C  of elements n, and the element locations (lx(i),ly(i),lz(i)),
C  i = 1,...,n, this subroutine computes the antenna-based spatial
C  frequency coordinates (uant(i),vant(i),want(i)), i = 1,...,n.
C   Inputs:
C    H        D    Hour angle (rad)
C    DELTA    D    Source declination (rad)
C    N        I    Number of antennas
C    LX(*)    D    "X" component of antenna position (m)
C    LY(*)    D    "Y" component of antenna position (m)
C    LZ(*)    D    "Z" component of antenna position (m)
C   Outputs:
C    UANT(*)  D    "U" component of project coordinate (m)
C    VANT(*)  D    "V" component of project coordinate (m)
C    WANT(*)  D    "W" component of project coordinate (m)
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION H, DELTA, LX(*), LY(*), LZ(*), UANT(*),
     *   VANT(*), WANT(*)
      INTEGER   I
      DOUBLE PRECISION SH, CH, SD, CD
C-----------------------------------------------------------------------
      SH = SIN (H)
      CH = COS (H)
      SD = SIN (DELTA)
      CD = COS (DELTA)
      DO 100 I = 1,N
         UANT(I) = SH * LX(I) + CH * LY(I)
         VANT(I) = -SD * (CH * LX(I) - SH * LY(I)) + CD * LZ(I)
         WANT(I) = CD * (CH * LX(I) - SH * LY(I)) + SD * LZ(I)
 100     CONTINUE
C
 999  RETURN
      END
C
      SUBROUTINE NOISE (ANOISE)
C-----------------------------------------------------------------------
C   Random noise generator
C    Output: ANOISE  R    Result
C-----------------------------------------------------------------------
      REAL      ANOISE, TEMP
      INTEGER   J
C-----------------------------------------------------------------------
      ANOISE = -6.0
      DO 10 J = 1,12
         CALL RANDUM (TEMP)
         ANOISE = ANOISE + TEMP
 10      CONTINUE
C
 999  RETURN
      END
C
      SUBROUTINE QXXPTS (C, U, VS, INCVS, INCF, INCS, MCOMP, NVIS,
     *   NF, NS, FLAG)
C-----------------------------------------------------------------------
C   This is the private version to add pointing error simulation
C-----------------------------------------------------------------------
C   Vector compiler version
C   Subtracts the model visibility derived from CLEAN components from
C   visibility records.  Arbitrary numbers of frequencies and
C   polarizations can be processed.  The weights of the visibilities
C   are multiplied times the amplitude of the model visibility.
C   Inputs:
C      C     I  Base address of CLEAN components, increment = 4
C               0 = Amplitude
C               1 = -2 * PI * X
C               2 = -2 * PI * Y
C               3 = -2 * PI * Z
C      U     I  Base address of U's, assumed followed by V, W
C      VS    I  Base address of vis rec. (real, imag, wt)
C      INCVS I  Increment of VS for next visibility
C      INCF  I  Increment of VS for next frequency
C      INCS  I  Increment of VS for next IF (RR of LL)
C      MCOMP I  Number of CLEAN components.
C      NVIS  I  Number of visibilities.
C      NF    I  Number of frequencies.
C      NS    I  Number of IF (usually 1 or 2)
C      FLAG  I  If FLAG < 0 multiply model vis by i (SQRT(-1))
C   Also uses AP locations 0 and 1 and expects an array of length NS
C   beginning in location 2+NF composed of the correlator factors.
C   Beginning in location 2 should be an array of length NF :
C              Freq(0) / Freq(ref) - 1.0
C              Freq(1) / Freq(ref) - 1.0
C                      .
C                      .
C                      .
C              Freq(NF-1) / Freq(ref) - 1.0
C   Also 1 or 2 factors to multiply model by and a factor (0 or 1) to
C   multiply the input data.  These 3 words follow the freq table
C   Note: all addresses are 0 relative and needed to be incremented by
C   1 to work in Fortran.
C-----------------------------------------------------------------------
      INTEGER   C, U, VS, INCVS, INCS, INCF, MCOMP, NVIS, NF, NS, FLAG
C
      INTEGER   X, JVS, IVS, IF, IV, IS, ICOMP
      LONGINT   F, S, JX, IU, JAMP, JA
      DOUBLE PRECISION FREQF, SUMRE, SUMIM, REMOD, IMMOD, PHS
C
      DOUBLE PRECISION DTHETA, FACT, UU, PFACT1, PFACT2, ANGL, POINT1,
     *   POINT2, PHAS1, PHAS2, TIMNEW, TIMOLD, DTNOLD, AMPF1, AMPF2,
     *   AMPF12
      INTEGER   BASE, IA1, IA2, IA, ISTEP, IROUND
C
      INCLUDE 'INCS:DAPC.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'UVCON.INC'
      REAL   POIOLD(NMAX), PHAOLD(NMAX), TEMP
      REAL   AMPOLD(NMAX)
      REAL   XX, YY, XXT, YYT, SKYBEA
      DOUBLE PRECISION   HALOC, COSZA, DARG, DARG2, DAZ

      SAVE TIMOLD
C-----------------------------------------------------------------------
C                                        Make sure that there is data.
      IF ((MCOMP.LE.0) .OR. (NVIS.LE.0) .OR. (NF.LE.0) .OR.
     *   (NS.LE.0))  GO TO 999
C                                       Setup array addresses
      X = C + 1
      IU = U - 1 + PSAPOF
      IVS = VS
C                                        Begin visibility loop
C$OMP PARALLEL DO PRIVATE (IV, JVS, F, S, IF, SUMRE, SUMIM, JX, JAMP,
C$OMP+                     FREQF, ICOMP, PHS, REMOD, IMMOD, IS,
C$OMP+                     JA, IVS, IU), SHARE(APCORE)
      DO 300 IV = 1,NVIS
C                                       variable primary beam
         IF (DOPRBM .AND. BTYPE .EQ. 5) THEN
            TIMNEW = APCORE(IU + 4)
            DTNOLD = ABS (TIMOLD - TIMNEW)
C                                       Consider that PRBP, PRBMA
C                                       do not change if time differs
C                                       less than TIMTOL
            IF (DTNOLD .GT. TIMTOL) THEN
               TIMOLD = TIMNEW
C                                       calculate elevation and azimuth
C                                       of the array center
C
C                                       Hour angle at the array center
               HALOC = TIMNEW*TWOPI*1.00274D0 - SITELO + GSTIAT
C                                       Limit to between 0 and 2pi
               HALOC = DMOD (HALOC, TWOPI)
C                                       translate to between -pi and pi
               IF (HALOC.GT.PI) HALOC = HALOC - TWOPI
               IF (HALOC.LT.-PI) HALOC = HALOC + TWOPI
C                                       cos of zenith angle
               COSZA = SIN (SITELA) * SIN (DELTA) + COS
     *                  (SITELA)* COS (DELTA) * COS (HALOC)
C                                       AZ = ATAN2(SD*CL - CD*SL*CH,
C                                       CD*SH)
               DARG = SIN (DELTA) * COS (SITELA) -
     *                  COS (DELTA) * SIN (SITELA) * COS(HALOC)
               DARG2 = COS (DELTA) * SIN (HALOC)
               DAZ = ATAN2 (DARG, DARG2)
               DAZ = MOD (DAZ - PI/2.0D0, TWOPI)
               IF (DAZ.LT.0.0) DAZ = DAZ + TWOPI
C
               PRBP = DAZ
               PRBMA = PRBMI / COSZA
               END IF
            END IF
C                                       random pointing error
C                                       adding to each antenna
         IF (DOPOIN) THEN
            BASE = APCORE(IU + 5)
            IA1 = BASE / 256
            IA2 = BASE - IA1*256
            TIMNEW = APCORE(IU + 4)
C                                       pointing error random among
C                                       antennas and in time
C
C                                       TEMP is a random 'gaussian'
C                                       number with mean =0 and
C                                       RMS = 1
            DTNOLD = ABS (TIMOLD - TIMNEW)
C                                       Consider that pointing can
C                                       change if time difference is
C                                       more than given tolerance
            IF (DTNOLD .GT. TIMTOL) THEN
               TIMOLD = TIMNEW
C                                       initialize the antennas
C                                       pointing error being very big
               DO 40 IA = 1,N
                  POIOLD(IA) = 100
   40             CONTINUE
C
               CALL NOISE (TEMP)
               POINT1 = POINTE(IA1) + BPARM(5) / 3600. * TEMP * DG2RAD
               POIOLD(IA1) = POINT1
               CALL NOISE (TEMP)
               POINT2 = POINTE(IA2) + BPARM(5) / 3600. * TEMP * DG2RAD
               POIOLD(IA2) = POINT2
            ELSE
               IF (POIOLD(IA1) .LT. 50) THEN
                  POINT1 = POIOLD(IA1)
               ELSE
                  CALL NOISE (TEMP)
                  POINT1 = POINTE(IA1) + BPARM(5) / 3600. * TEMP
     *               * DG2RAD
                  POIOLD(IA1) = POINT1
                  END IF
C
               IF (POIOLD(IA2) .LT. 50) THEN
                  POINT2 = POIOLD(IA2)
               ELSE
                  CALL NOISE (TEMP)
                  POINT2 = POINTE(IA2) + BPARM(5) / 3600. * TEMP
     *               * DG2RAD
                  POIOLD(IA2) = POINT2
                  END IF
               END IF
            END IF
C---------------------------
C                                       random phase adding to the
C                                       antennas
         IF (DOPHAS) THEN
            BASE = APCORE(IU + 5)
            IA1 = BASE / 256
            IA2 = BASE - IA1*256
            TIMNEW = APCORE(IU + 4)
            DTNOLD = ABS (TIMNEW - TIMOLD)
C                                       Consider that phase can
C                                       change if time difference is
C                                       more than given tolerance
            IF (DTNOLD.GE.TIMTOL .OR. (FIRST .AND. IV.EQ.1)) THEN
               TIMOLD = TIMNEW
C                                       initialize the antennas
C                                       phase error being very big
               DO 60 IA = 1,N
                  PHAOLD(IA) = 100
                  AMPOLD(IA) = 100
   60             CONTINUE
C
C                                       TEMP is a random even disri-
C                                       buted number at the range (0,1)
C
C                                       Random phase
               CALL RANDUM (TEMP)
               PHAS1 = 2*HPHASR*TEMP - HPHASR
               PHAOLD(IA1) = PHAS1
               CALL RANDUM (TEMP)
               PHAS2 = 2*HPHASR*TEMP - HPHASR
               PHAOLD(IA2) = PHAS2
C                                       Random amplitude factor
               CALL RANDUM (TEMP)
               AMPF1 = 2*HAMPFR*TEMP - HAMPFR
               AMPOLD(IA1) = AMPF1
               CALL RANDUM (TEMP)
               AMPF2 = 2*HAMPFR*TEMP - HAMPFR
               AMPOLD(IA2) = AMPF2
            ELSE
C                                       Random phase
C
C                                       first antenna
               IF (PHAOLD(IA1) .LT. 50) THEN
                  PHAS1 = PHAOLD(IA1)
               ELSE
                  CALL RANDUM (TEMP)
                  PHAS1 = 2*HPHASR*TEMP - HPHASR
                  PHAOLD(IA1) = PHAS1
                  END IF
C                                       second antenna
               IF (PHAOLD(IA2) .LT. 50) THEN
                  PHAS2 = PHAOLD(IA2)
               ELSE
                  CALL RANDUM (TEMP)
                  PHAS2 = 2*HPHASR*TEMP - HPHASR
                  PHAOLD(IA2) = PHAS2
                  END IF
C                                       Random amplitude factor
C
C                                       first antenna
               IF (AMPOLD(IA1) .LT. 50) THEN
                  AMPF1 = AMPOLD(IA1)
               ELSE
                  CALL RANDUM (TEMP)
                  AMPF1 = 2*HAMPFR*TEMP - HAMPFR
                  AMPOLD(IA1) = AMPF1
                  END IF
C                                       second antenna
               IF (AMPOLD(IA2) .LT. 50) THEN
                  AMPF2 = AMPOLD(IA2)
               ELSE
                  CALL RANDUM (TEMP)
                  AMPF2 = 2*HAMPFR*TEMP - HAMPFR
                  AMPOLD(IA2) = AMPF2
                  END IF
               END IF
            END IF
C---------------------------
C                                        Get ready for freq. loop.
         JVS = IVS
         F = 1 + PSAPOF
         S = NF + 1 + PSAPOF
C                                        Begin frequency loop.
         DO 200 IF = 1,NF
C                                       Loop over component
            SUMRE = 0.0D0
            SUMIM = 0.0D0
            JX = X + PSAPOF
            JAMP = C + PSAPOF
            IF (ABS (APCORE(F+1)).GT.1.0D-20) THEN
               FREQF = 1.0D0 + APCORE(F+1)
            ELSE
               FREQF = 1.0D0
               END IF
            DO 100 ICOMP = 1, MCOMP
C                                       W term 0.5(l**2 + m**2)
               IF (DOW.GT.0) APCORE(JX+2) =
     *            (APCORE(JX)**2 + APCORE(JX+1)**2)/2.0D0/TWOPI
               AMPF12 = 1
               PHS = FREQF * (APCORE(JX) * APCORE(IU+1)
     *                      + APCORE(JX+1) * APCORE(IU+2)
C                                       change sign to minus to match
C                                       with UVMOD. LK March 19 2009
     *                      - APCORE(JX+2) * APCORE(IU+3))
C                                       add the random phase =
C                                       random phase of AN2
C                                       - random phase of AN1
C                                       and the random factor
C                                       as Rick requested
               IF (DOPHAS) THEN
                  IF (DOONE .AND. ICOMP.GT.1) GO TO 110
                  PHS = PHS + PHAS2 - PHAS1
                  AMPF12 = EXP(AMPF1+AMPF2)
  110             CONTINUE
                  END IF
C
               IF (DOPOIN) THEN
C                                       calculate the distance of the
C                                       component (in radians) from the
C                                       reference direction
C                                       Do it only for the first vis.
C                                       to save the time
                  IF (IV.EQ.1)
     *               DTHETA = SQRT(APCORE(JX)*APCORE(JX) +
     *                  APCORE(JX+1)*APCORE(JX+1)) / TWOPI
                  ANGL = ABS(DTHETA + POINT1)
                  UU = DANTT / LAMBDA * SIN(ANGL)
                  ISTEP = IROUND(UU /ASTEP) + 1
                  IF (ISTEP .LE. NSTEP) THEN
                     PFACT1 = PRBEAM(ISTEP)
                  ELSE
                     PFACT1 = 0
                     END IF
C                                       pointing error of ant2
                  ANGL = ABS(DTHETA + POINT2)
                  UU = DANTT / LAMBDA * SIN(ANGL)
                  ISTEP = IROUND(UU /ASTEP) + 1
                  IF (ISTEP .LE. NSTEP) THEN
                     PFACT2 = PRBEAM(ISTEP)
                  ELSE
                     PFACT2 = 0
                     END IF
                  FACT = PFACT1 * PFACT2
C                                       Do sum with the pointing error
C                                       correction
                  SUMRE = SUMRE + APCORE(JAMP) * COS(PHS) * FACT
                  SUMIM = SUMIM + APCORE(JAMP) * SIN(PHS) * FACT
               ELSE
                  IF (DOPRBM .AND. BTYPE.EQ.5) THEN

                     XX = APCORE(JX) / TWOPI
                     YY = APCORE(JX+1) / TWOPI
C
C                                       the primary beam is gaussian
C                                       rotate by BPA
                     XXT = XX*COS(PRBP) - YY*SIN(PRBP)
                     YYT = XX*SIN(PRBP) + YY*COS(PRBP)
C
                     UU = (XXT/PRBMA)**2 + (YYT/PRBMI)**2
                     ISTEP = IROUND(UU /ASTEP) + 1
C                                       SKYBEA is CC comp multiplied by
C                                       the primary beam
                     IF (ISTEP .LE. NSTEP) THEN
                        SKYBEA = PRBEAM(ISTEP)*APCORE(JAMP)
                     ELSE
                        SKYBEA = 0
                        END IF
C
                     SUMRE = SUMRE + SKYBEA * COS(PHS) * AMPF12
                     SUMIM = SUMIM + SKYBEA * SIN(PHS) * AMPF12
                  ELSE
C
                     SUMRE = SUMRE + APCORE(JAMP) * COS(PHS) * AMPF12
                     SUMIM = SUMIM + APCORE(JAMP) * SIN(PHS) * AMPF12
                     END IF
                  END IF
               JX = JX + 4
               JAMP = JAMP + 4
 100           CONTINUE
C                                       Correct visibility
            JA = JVS + PSAPOF
C                                        Setup
            IF (FLAG.LT.0) THEN
               REMOD = -SUMIM
               IMMOD = SUMRE
            ELSE
               REMOD = SUMRE
               IMMOD = SUMIM
               END IF
C                                       Loop over Stokes' (1 or 2)
C                                       drop data
            IF (APCORE(S+3).EQ.0.0D0) THEN
               DO 150 IS = 1,NS
                  APCORE(JA) = REMOD * APCORE(S+IS)
                  APCORE(JA+1) = IMMOD * APCORE(S+IS)
                  JA = JA + INCS
 150              CONTINUE
C                                       Subtract model from data
            ELSE
               DO 160 IS = 1,NS
                  APCORE(JA) = APCORE(JA) - REMOD * APCORE(S+IS)
                  APCORE(JA+1) = APCORE(JA+1) - IMMOD * APCORE(S+IS)
                  JA = JA + INCS
 160              CONTINUE
               END IF
C                                        Update vis pointer.
            JVS = JVS + INCF
            F = F + 1
 200        CONTINUE
C                                        Update pointers.
         IVS = IVS + INCVS
         IU = IU + INCVS
 300     CONTINUE
C$OMP END PARALLEL DO
C
 999  RETURN
      END
C
      SUBROUTINE POERR
C-----------------------------------------------------------------------
C   POERR estimates the pointing errors of the antennas
C-----------------------------------------------------------------------
C   Inputs in common:
C       BPARM(3)   R  RMS (constant in time) of the pointing error
C                     in arcsec
C       BPARM(4)   R  global pointing error (const for all antennas
C                     and time in arcsec
C   Outputs in common:
C       POINTE(*)  R  Pointing errors of the antennas in radians
C   Output:
C       IRET       I  Error
C
      INTEGER   I
      REAL      TEMP
      INCLUDE 'UVCON.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C
      DO 100 I = 1, N
C                                       TEMP is a random 'gaussian'
C                                       number with mean =0 and
C                                       RMS = 1
         CALL NOISE (TEMP)
         POINTE(I) = (BPARM(3) * TEMP  + BPARM(4)) / 3600. * DG2RAD
  100    CONTINUE
C
      RETURN
      END
      SUBROUTINE PRB
C-----------------------------------------------------------------------
C   PRB estimates array of the primary beam's values
C-----------------------------------------------------------------------
C   Inputs in common:
C       BTYPE      I  Type of the beam
C                  1 => Circular dish with a table type of ilumination
C                       PRBEAM(U) = 2*I_1(U)/U
C                  2 => Circular dish with the ilumination of 10db
C                       down at the dish's edge (ALMA)
C                       PRBEAM(U) = [1/(b+0.5)] *
C                                   [b*2*I_1(U)/U + 0.5*8 *I_2(U)/U^2]
C                       b = 0.46
C                  3 => Circular dish with the ilumination of 15db
C                       down at the dish's edge (ALMA)
C                       PRBEAM(U) = [1/(b+0.5)] *
C                                   [b*2*I_1(U)/U + 0.5*8 *I_2(U)/U^2]
C                       b = 0.216
C                  >3 => Gaussian beam
C                       PRBMAJ- FWHM major axis;
C                       PRBMIN- FWHM minor axis;
C                       PRBPA - Positian angle
C       RANGPB     Range of the primary beam calculation
C   Outputs in common:
C       PRBEAM(*)  R  Array of the primary beam's values
C
      INTEGER   I, MSTEP
      REAL BESSJ1, J0BES, J1BES, J2BES, LAM1, LAM2, UU, B
      DOUBLE PRECISION   BESSJ0, UUU
      INCLUDE 'UVCON.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C
C                                       calculate primary beam
C                                       of the array antenna in the
C                                       normalized coordinates
C                                       U = pi * (d/lambda) *sin(theta)
C                                       or in gaussian shape
      MSTEP = NSTEP
      ASTEP = RANGPB / MSTEP
      DO 10 I = 1, MSTEP
         IF (BTYPE .EQ. 0) THEN
C                                       primary beam is indefinetely
C                                       wide
            PRBEAM(I) = 1
         ELSE
            UU = ASTEP * (I - 1) * PI
            IF (I .EQ. 1) THEN
               PRBEAM(I) = 1
            ELSE
               IF ((BTYPE.EQ.1 .OR. BTYPE.EQ.2) .OR. BTYPE.EQ.3) THEN
                  J1BES = BESSJ1(UU)
                  LAM1 = 2 * J1BES / UU
                  END IF
               IF (BTYPE .EQ. 1) THEN
C                                       flat ilumination
                  PRBEAM(I) = LAM1 ** 2
               ELSE
                  IF (BTYPE.EQ.2 .OR. BTYPE.EQ.3) THEN
C                                       ilumination with 10db down at
C                                       the edge
                     IF (BTYPE .EQ. 2)   B = 0.46
C                                       ilumination with 15db down at
C                                       the edge
                     IF (BTYPE .EQ. 3) B = 0.216
C                                       Bessel funcion of zero
C                                       order
                     UUU = UU
                     J0BES = BESSJ0(UUU)
C                                       Bessel funcion of the second
C                                       order
                     J2BES = LAM1 - J0BES
C                                       LAMBDA funcion of the second
C                                       order
                     LAM2 = 2 * (4/(UU*UU)) * J2BES
                     PRBEAM(I) = ((B * LAM1 + 0.5 * LAM2) / (B + 0.5))
     *                  **2
                  ELSE
C                                       gaussian beam
                     UU = UU/PI
                     PRBEAM(I) = EXP(-COEF*UU)
                     END IF
                  END IF
               END IF
            END IF
   10    CONTINUE
C
      RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE FHEAD (MODNAM, MODCLS, MODSEQ, MODVOL,
     *   UNITS, RAINC, DECINC, BEMJ, BEMN, NPX, NPY,BUFFER, IERR)
C-----------------------------------------------------------------------
C! Find some parameters of the IMH
C# UV Modeling
C-----------------------------------------------------------------------
C   FHEAD some parameters of the IMH.
C   In particular:
C      1.Units
C      2.Increament in RA--SIN
C      3.Increament in DEC--SIN
C      4.Beam major axis
C      5.Beam minor axis
C      6.Number of pixels along RA
C      7.Number of pixels along DEC
C   Inputs:
C      MODNAM   C*12      Name of model file(s).
C      MODCLS   C*6       Class of first model file.
C      MODSEQ   I         Model file(s) sequence number.
C      MODVOL   I         Model file(s) disk number
C      BUFFER   R(1024)   Work buffer
C   Output:
C      UNITS    C*8       Units
C      RAINC    R         Increament in RA--SIN, arcsec
C      DECINC   R         Increament in DEC--SIN, arcsec
C      BEMJ     R         Beam major axis, arcsec
C      BEMN     R         Beam minor axis, arcsec
C      NPX      I         Number of pixels at RA direction
C      NPY      I         Number of pixels at DEC direction
C      IERR     I         Error
C-----------------------------------------------------------------------
      CHARACTER UNITS*8
      REAL   RAINC, DECINC, BEMJ, BEMN
      INTEGER   NPX, NPY
      CHARACTER MODNAM*12, MODCLS*6
      INTEGER   MODSEQ, MODVOL, NMOD, IERR
      REAL      BUFFER(*)
C
      CHARACTER ITYPE*2, STAT*4
      HOLLERITH CATCH(256)
      INTEGER   CATCLN(256), IRET
      REAL      CATCR(256)
      DOUBLE PRECISION CATCD(128)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATCLN, CATCR, CATCH, CATCD)
C-----------------------------------------------------------------------
C                                       Other info
      IERR = 0
      NMOD = 1
C
C                                       Set class.
      ITYPE = '  '
      CALL CATDIR ('SRCH', MODVOL, NMOD, MODNAM, MODCLS, MODSEQ,
     *   ITYPE, NLUSER, STAT, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, MODNAM, MODCLS, MODSEQ, ITYPE,
     *      MODVOL, NLUSER
         IERR = 2
         IF (IRET.EQ.5) IERR = 3
         IF (IRET.EQ.3) IERR = 4
         GO TO 990
         END IF
C                                       Read catalog block
      CALL CATIO ('READ', MODVOL, NMOD, CATCLN, 'READ',
     *   BUFFER, IRET)
      IF (IRET.NE.0) THEN
         IERR = 4
         IF (IRET.GT.4) IERR = 2
         WRITE (MSGTXT,1011) IRET
         GO TO 990
         END IF
C                                       Units
      CALL H2CHR (8, 1, CATCH(KHBUN), UNITS)
      RAINC = CATCR(KRCIC)
      DECINC = CATCR(KRCIC+1)
      BEMJ = CATCR(KRBMJ)
      BEMN = CATCR(KRBMN)
      NPX = CATCLN(KINAX)
      NPY = CATCLN(KINAX+1)
C
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,2X,A2,' DISK=',
     *   I3,' USID=',I4)
 1011 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE VISDFT (OPCODE, CHANEL, NCHAN, DISKI, CNOSCI, DISKO,
     *   CNOSCO, IFIELD, DOSUM, DOMSG, CATR, JBUFSZ, BUFF1, BUFF2,
     *   IBUFF, IRET)
C-----------------------------------------------------------------------
C   This private version allows to multiply the model by the
C   primary beam
C-----------------------------------------------------------------------
C   VISDFT subtracts/divides CLEAN components from/into ungridded
C   visibility data by a DFT model computation.  Only model components
C   of a single type are processed.  The type is determined from the
C   first component to be processed in the first field.
C   All un subtracted data processed in one call.
C   Inputs:
C      OPCODE   C*4      Opcode 'SUB ', or 'DIV '
C      CHANEL   I        Frequency channel
C      NCHAN    I        Number of frequency channels.
C      DISKI    I        Input file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCI   I        Input file catalog slot number or /CFILES/
C                        scratch file number.
C      DISKO    I        Output file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      IFIELD   I        Field to do (0 -> all)
C      DOSUM    L        If true sum the flux in each field
C      DOMSG    L        If true give percent done messages.
C      CATR     R(256)   UV data catalog header record.
C      JBUFSZ   I        Size of BUFF1,2, IBUFF in AIPS bytes, each
C                        must be at least 4096 words.
C   Inputs: from commons
C      MFIELD   I        Number of fields
C      NCLNG    I(16)    Number of components per field. -
C                        changed if flux limit hit
C      NSUBG    I(16)    The next component to subtract.
C      CCDISK   I(16)    Disk numbers of the clean images.
C      CCCNO    I(16)    Catalog slot numbers of clean images.
C      CCVER    I(*)     CC file version number for each field.
C      NGRDAT   L        If FALSE get map size, scaling etc. parms
C                        from the model map cat. header. If TRUE
C                        then the values filled in by GRDAT must
C                        already be filled into the common.
C      LREC     I        Length in words of vis record.
C      NVIS     I        Number of vis. records
C      NONEG    L        Stop reading comps. from a file past the first
C                        negative component.
C      LIMFLX   R        Stop if abs(flux) < LIMFLX
C      DOPTMD   L        Use the point model specified by PTFLX, PTRAOF,
C                        PTDCOF
C      PTFLX    R        Point model flux density (Jy) (I pol. only)
C      PTRAOF   R        Point model RA offset from uv phase center
C                        (asec)
C      PTDCOF   R        Point model Dec. offset from uv phase center
C      PARMOD   R(6)     Model parameters for non point models; used
C                        only if DOPTMOD is true.
C                        1=> model type, 0=point, 1=gaussian, 3=sphere
C                        Gaussian: (2)=major axis(asec), (3)=minor axis
C                                  (4)=PA (degrees)
C                        Sphere: (2)=radius (asec).
C      KSTOK    I        (DGDS.INC) If a point model is specified a
C                        value of 2 indicates a Q pol model, 3 U and
C                        4 V pol.AC
C   In/out:
C      CNOSCO   I        IN: output file catalog slot number or /CFILES/
C                        scratch file number. Will create a scratch file
C                        if CNOSCO and DISKO .le. 0.
C                        Out: file /CFILES/ number if created.
C   Output:
C      BUFF1    R(*)     I/O buffers.
C      BUFF2    R(*)     I/O buffers.
C      IBUFF    I(*)     I/O Buffer.
C      IRET     I        Return code, 0 => ok, otherwise not.
C                            9 => Buffers too small to load AP.
C                           10 => Too many components for division.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   CHANEL, NCHAN, DISKI, CNOSCI, DISKO, CNOSCO, IFIELD,
     *   JBUFSZ, IBUFF(*), IRET
      LOGICAL   DOSUM, DOMSG
      REAL      BUFF1(*), BUFF2(*), CATR(256)
C
      CHARACTER KEYS(7)*8, NAME*48, MDTYP(4)*8, ERRTXT*40
      INTEGER   JCOMP, JNCOMP, CCOUNT, XNCOMP, MXCMP, CURCMP, JT,
     *   NCOMP, J, MCOMP, VO, BO, ISIZE, INIO, MMCOMP, NNCOR, IDATA, UV,
     *   LLREC, IAPBUF, IAPCC, IAPCT, LMCOMP, IAPTMP, VIS, WRK, LLNMOD,
     *   MCHAN, JNCS, JNCF, KAP, SFLAG, APSIZ, MXCC, INIO2, LUNC, VOL,
     *   INDEX, ITYPE, NIOUT, KBIND, LENBU, LENMOD, JLREC, JNREC, FINDI,
     *   FINDO, I, LUNI, LUNO, ITIME(3), IBIND, LFIELD, LMOD(4), NKEY,
     *   KOLS(7), RAKOL, DECKOL, FLXKOL, TYPKOL, MAJKOL, MINKOL, PAKOL,
     *   IPCLST, IPCDNE, NCALL, NTIMES, MODTYP, LRPARM, LF1, LF2, ISTEP,
     *   IROUND, NEED, MSGSAV
      REAL      XXOFF, YYOFF, ZZOFF, CONST, RECORD(20), FACT2(4), CPA,
     *   SPA, XMAJ, XMIN, CONST2, ABFACG, XYZ(3), XP(3), UMAT(3,3),
     *   PMAT(3,3), UUU
     *   , UU, DTHETA, PFACT, XX, YY, XXT, YYT
      DOUBLE PRECISION XTLST, PCTOT, PCLST, XRA, XDEC
      LOGICAL   T, F, DIVIDE
      INCLUDE 'PRBM.INC'
      INCLUDE 'INCS:PSTD.INC'
C      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   DONE(MAXFLD), ONZE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      PARAMETER (CONST = DG2RAD * TWOPI)
      SAVE ONZE
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL),
     *   (KOLS(5) ,MAJKOL),               (KOLS(6), MINKOL),
     *   (KOLS(7), PAKOL)
      DATA KEYS /'DELTAX  ', 'DELTAY  ', 'FLUX    ',
     *   'TYPE OBJ', 'MAJOR AX', 'MINOR AX', 'POSANGLE'/
      DATA LMOD /4, 7, 7, 6/
      DATA MDTYP /'Point   ', 'Gaussian', 'Unknown ', 'Sphere  '/
      DATA VO, BO, MXCC /0, 1, 1024/
      DATA LUNI, LUNO, LUNC /22,23,29/
      DATA T, F /.TRUE.,.FALSE./, ONZE/.FALSE./
C-----------------------------------------------------------------------
C                                       CONST2 converts FWHM(deg) to
C                                       coefficients for u*u, v*v, u*v
      CONST2 = DG2RAD * (PI / 1.17741022) * SQRT (0.5)
C                                       Check if divide.
      DIVIDE = OPCODE.EQ.'DIV '
C                                       Tell kind of operation.
      IF (DIVIDE) THEN
         MSGTXT = 'VISDFT: Begin DFT component division'
      ELSE
         MSGTXT = 'VISDFT: Begin DFT component subtraction'
         END IF
      CALL MSGWRT (2)
      MCHAN = NCHAN
      NNCOR = 1
C                                       Get un-compressed UV increments
      CALL UVINCS (INCS, INCF, INCIF, NRPARM, LREC, JNCS, JNCF, LRPARM,
     *   LLREC)
      LFIELD = 0
      CCOUNT = 0
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = MFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         END IF
C                                       Decide component type.
C                                       From model passed
      IF (DOPTMD) THEN
         MODTYP = PARMOD(1) + 0.5
C                                       From CC table, field 1
      ELSE
         LFIELD = LF1
C                                       Get field info. if nec.
         IF (.NOT.NGRDAT) THEN
            CALL GRDAT (F, LFIELD, CATR, IBUFF(2049), IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       If NGRDAT read CLEAN CATBLK.
         IF (NGRDAT) THEN
            ERRTXT = 'READING CLEAN CATBLK'
            CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD), KLNBLK,
     *         'REST', IBUFF(2049), IRET)
            IF ((IRET.GT.0) .AND. (IRET.LT.5)) GO TO 990
            END IF
         JNREC = 1
         JLREC = 0
         NKEY = 0
         ERRTXT = 'OPENING CLEAN COMPS FILE'
         CALL TABINI ('READ', 'CC', CCDISK(LFIELD), CCCNO(LFIELD),
     *      CCVER(LFIELD), KLNBLK, LUNC, NKEY, JNREC, JLREC, BUFF1,
     *      IBUFF, IRET)
         IF (IRET.GT.1) GO TO 990
C                                       For point model
         MODTYP = 0
C                                       More complex models
C                                       Find columns (physical)
         IF (JLREC.GT.3) THEN
            NKEY = JLREC
            IF (JLREC.GT.7) NKEY = 7
            ERRTXT = 'FINDING REQUIRED CC COLUMNS'
            CALL FNDCOL (NKEY, KEYS, 8, F, IBUFF, KOLS, IRET)
            IF (IRET.NE.0) GO TO 990
C                                       Read 1st record
            JCOMP = NSUBG(LFIELD)
            CALL TABIO ('READ', 0, JCOMP, RECORD, IBUFF, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1090) IRET, JCOMP
               GO TO 995
               END IF
C                                       Get model type.
            MODTYP = RECORD(TYPKOL) + 0.5
            END IF
C                                       Close CLEAN components file.
         CALL TABIO ('CLOS', 0, JCOMP, RECORD, IBUFF, IRET)
         END IF
C                                       Bad model type.
      IF ((MODTYP.NE.0) .AND. (MODTYP.NE.1) .AND. (MODTYP.NE.3)) THEN
         WRITE (MSGTXT,1002) MODTYP
         GO TO 995
         END IF
C                                       Determine no. CC to sub.
      XNCOMP = 0
      DO 10 LFIELD = LF1,LF2
         DONE(LFIELD) = F
         XNCOMP = XNCOMP + NCLNG(LFIELD) - NSUBG(LFIELD) + 1
 10      CONTINUE
C                                       Check for point model.
      IF (DOPTMD) XNCOMP = 1
      LFIELD = LF1 - 1
C                                       Tell model type once
      IF (.NOT.ONZE) THEN
C                                       Tell model type
         MSGTXT = 'VISDFT: Model components of type '//MDTYP(MODTYP+1)
         CALL MSGWRT (2)
         ONZE = .NOT. ONZE
C                                       Check Buffer size
         IF ((XNCOMP.GT.10) .AND. (JBUFSZ/2.LT.4096)) THEN
            MSGTXT = 'VISDFT: SCRATCH BUFFER TOO SMALL FOR CCs!'
            CALL MSGWRT (8)
            IRET = 9
            GO TO 999
            END IF
         END IF
C                                       Set model length
      LENMOD = LMOD(MODTYP+1)
C                                       Determine size of uv I/O and
C                                       the number of CC that will fit.
      LENBU = ((JBUFSZ-2*NBPS) / 2) / (LLREC*2)
C                                       How much data fits in AP?
      JT = 15 + LENBU * LLREC
      IF (MXCC.GT.JT) JT = MXCC
      NEED = JT + NCHAN + (XNCOMP+2)*LENMOD
      NEED = NEED / 1024
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL QINIT (NEED, 0, KAP)
      MSGSUP = MSGSAV
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = JT + NCHAN + (XNCOMP/10+2)*LENMOD
         NEED = NEED / 1024
         NEED = MIN (32*1024, NEED) + 2
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
             IRET = 8
             MSGTXT = 'VISDFT (LOCAL VERSION) CANNOT GET NEEDED MEMORY'
             GO TO 995
             END IF
          END IF
      APSIZ = PSAPNW * 1024
      CALL QRLSE
      MXCMP = (APSIZ - JT - NCHAN)  / LENMOD
      MXCMP = MXCMP - 2
      CALL RFILL (20, 0.0, RECORD)
C                                       Set AP pointers.
C                                       UV=UV pointer, VIS=vis pointer
C                                       IAPCC=CLEAN components pointer.
      IDATA = 12 + NCHAN
      UV = IDATA + ILOCU
      IAPCC = 15 + LENBU * LLREC + NCHAN
      IF  (MXCC.GT.IAPCC) IAPCC = MXCC

      LLNMOD = LENMOD
C                                       Compute number of passes.
      NTIMES = (1.0 * XNCOMP) / MXCMP + 0.99999
      NTIMES = MAX (NTIMES, 1)
C                                       Only one pass allowed for
C                                       division.  No can do.
      IF (DIVIDE .AND. (NTIMES.GT.1)) THEN
         ERRTXT = 'TOO MANY COMPONENTS FOR DIVISION'
         IRET = 10
         GO TO 990
         END IF
C                                       Fix for Division scaling
      ABFACG = 1.0
      IF (DIVIDE) ABFACG = ABS(FACGRD(1))
C                                       Open uv files.
C                                       Set input file name.
      VOL = DISKI
      IF (DISKI.LE.0) VOL = SCRVOL(CNOSCI)
      IF (DISKI.GT.0) CALL ZPHFIL ('UV', VOL, CNOSCI, 1, NAME, IRET)
      IF (DISKI.LE.0) CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCI), 1, NAME,
     *   IRET)
C                                       Open input file.
      ERRTXT = 'OPEN-FOR-READ VIS FILE'
      CALL ZOPEN (LUNI, FINDI, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Create scratch file if necessary
      IF ((DISKO.LE.0) .AND. (CNOSCO.EQ.0)) THEN
         CALL UVSIZE (LREC, NVIS, ISIZE)
         ERRTXT = 'CREATING SCRATCH FILE'
         CALL SCREAT (ISIZE, BUFF2, IRET)
         CNOSCO = NSCR
         IF (IRET.GT.0) THEN
            IF (IRET.EQ.1) ERRTXT = 'NO SPACE FOR SCRATCH FILE'
            GO TO 990
            END IF
C                                       End if creating scratch file
         END IF
C                                       Open vis file for write.
      IF (DISKO.LE.0) THEN
         VOL = SCRVOL(CNOSCO)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCO), 1, NAME, IRET)
      ELSE
         VOL = DISKO
         CALL ZPHFIL ('UV', VOL, CNOSCO, 1, NAME, IRET)
         END IF
      ERRTXT = 'OPEN-FOR-WRITE VIS FILE'
      CALL ZOPEN (LUNO, FINDO, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Loop, subtracting Max Component
C                                       In AP each pass
      DO 500 NCALL = 1,NTIMES
C                                       Setup for % done messages.
         PCTOT = NVIS
         PCLST = PCTOT * (NCALL - 1)
         IPCLST = (100. / NTIMES ) * (NCALL - 1)
C                                       Set AP loc for next CC load
         IAPCT = IAPCC
C                                       Determine. no. this pass.
         MMCOMP = 0
         NCOMP = MIN( MXCMP, XNCOMP)
C                                       Grab AP.
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
             IRET = 8
             MSGTXT = 'VISDFT (LOCAL VERS2) CANNOT GET NEEDED MEMORY'
             GO TO 995
             END IF
C                                       Initialize REAL time clock for
C                                       AP roller.
         CALL ZTIME (ITIME)
         XTLST = (ITIME(1) * 60.00) + ITIME(2) + (ITIME(3) / 60.0)
C                                       If Not single component model.
         IF (.NOT.DOPTMD) THEN
C                                       Find next FIELD.
C                                       Loop back here for next field.
 70         LFIELD = LFIELD + 1
C           '                            See if done.
            IF (LFIELD.GT.LF2) GO TO 150
            IF (DONE(LFIELD)) GO TO 70
C                                       See if there are CC's.
            IF ((NSUBG(LFIELD).GT.NCLNG(LFIELD))) GO TO 70
C                                       Get field info. if nec.
            IF (.NOT.NGRDAT) THEN
               CALL GRDAT (F, LFIELD, CATR, IBUFF(2049), IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       If NGRDAT read CLEAN CATBLK.
            IF (NGRDAT) THEN
               ERRTXT = 'READING CLEAN CATBLK'
               CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD),
     *            KLNBLK, 'REST', IBUFF(2049), IRET)
               IF ((IRET.GT.0) .AND. (IRET.LT.5)) GO TO 990
               END IF
C                                       Set field center offsets.
            XXOFF = DXCG(LFIELD) * CCROT + DYCG(LFIELD) * SSROT
            YYOFF = DYCG(LFIELD) * CCROT - DXCG(LFIELD) * SSROT
            ZZOFF = DZCG(LFIELD)
            CALL XYSHFT (RA, DEC, XSHIFT(LFIELD), YSHIFT(LFIELD),
     *         MAPROT, XRA, XDEC)
            IF (DO3DIM) THEN
               CALL PRJMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT,
     *            PMAT)
            ELSE
               CALL P2DMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT,
     *            PMAT)
               END IF
C                                       Load CLEAN components into AP.
C                                       Open components file.
            JNREC = 1
            JLREC = 0
            NKEY = 0
            ERRTXT = 'OPENING CLEAN COMPS FILE'
            CALL TABINI ('READ', 'CC', CCDISK(LFIELD), CCCNO(LFIELD),
     *         CCVER(LFIELD), KLNBLK, LUNC, NKEY, JNREC, JLREC, BUFF1,
     *         IBUFF, IRET)
            IF (IRET.GT.1) GO TO 990
            JCOMP = NSUBG(LFIELD)
C                                       Find columns (physical)
            NKEY = JLREC
            TYPKOL = 7
            IF (JLREC.GT.7) NKEY = 7
            ERRTXT = 'FINDING REQUIRED CC COLUMNS'
            CALL FNDCOL (NKEY, KEYS, 8, F, IBUFF, KOLS, IRET)
            IF (IRET.GT.0) GO TO 990
C                                       Make sure that there are some
            IF (IBUFF(5).LE.0) GO TO 140
            IF (NCLNG(LFIELD).LE.0) NCLNG(LFIELD) = IBUFF(5)
C                                       Loop loading components.
            IAPBUF = 10
            CURCMP = MMCOMP + 1
C                                       Check next component
            IF (JCOMP.GT.NCLNG(LFIELD)) GO TO 140
            DO 130 J = CURCMP,NCOMP,MXCC
               JT = J - 1
               JNCOMP = 0
               MCOMP = NCOMP - J + 1
               IF (MCOMP.GT.MXCC) MCOMP = MXCC
               IF (MCOMP.GT.(NCLNG(LFIELD)-NSUBG(LFIELD)+1))
     *            MCOMP = NCLNG(LFIELD) - NSUBG(LFIELD) + 1
               DO 110 I = 1,MCOMP
C                                       Check if finished field
                  IF (JCOMP.GT.NCLNG(LFIELD)) GO TO 120
                  CALL TABIO ('READ', 0, JCOMP, RECORD, IBUFF, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1090) IRET, JCOMP
                     GO TO 995
                     END IF
C                                        Check that point comp.
                  JCOMP = JCOMP + 1
                  JT = JT + 1
                  ITYPE = RECORD(TYPKOL) + 0.5
                  IF ((ITYPE.EQ.MODTYP) .AND. (IRET.EQ.0)) THEN
C                                       Check negative component limit
                     DONE(LFIELD) = (NONEG.AND.(RECORD(FLXKOL).LE.0.0))
     *                  .OR. (ABS(RECORD(FLXKOL)).LT.LIMFLX)
                     IF (DONE(LFIELD)) THEN
                        NCLNG(LFIELD) = JCOMP - 1
                        GO TO 120
                        END IF
C                                       If req. sum flux
                     IF (DOSUM) THEN
                        FLUXG(LFIELD) = FLUXG(LFIELD) + RECORD(FLXKOL)
                        TFLUXG = TFLUXG + RECORD(FLXKOL)
                        END IF
                     JNCOMP = JNCOMP + 1
                     CCOUNT = CCOUNT + 1
                     XP(1) = (RECORD(RAKOL) + XPOFF(LFIELD)) * CONST
                     XP(2) = (RECORD(DECKOL) + YPOFF(LFIELD)) * CONST
                     XP(3) = 0.0
                     CALL PRJMUL (2, XP, UMAT, XYZ)
C                                       BUFF1 is measured in radians
C                                       *TWOPI
                     BUFF1(JNCOMP) = XYZ(1) + XXOFF
                     BUFF1(1024+JNCOMP) = XYZ(2) + YYOFF
                     BUFF1(2048+JNCOMP) = XYZ(3) + ZZOFF
C                                       Handle scaling for division
                     BUFF1(3072+JNCOMP) = ABFACG * RECORD(FLXKOL)
C
                     IF (DOPRBM) THEN
                        XX = BUFF1(JNCOMP) / TWOPI
                        YY = BUFF1(1024+JNCOMP) / TWOPI
                        IF (BTYPE .LE. 3) THEN
C                                       the primary beam is related with
C                                       the dish ilumination
C
C                                       the distance of the given
C                                       component from the shifted
C                                       center in radians
                           DTHETA = SQRT(XX*XX + YY*YY)
                           UU = DANTT / LAMBDA * SIN(DTHETA)
                           ISTEP = IROUND(UU /ASTEP) + 1
                           IF (ISTEP .LE. NSTEP) THEN
                              PFACT = PRBEAM(ISTEP)
                           ELSE
                              PFACT = 0
                              END IF
                           END IF
C
                        IF (BTYPE .EQ. 4) THEN
C                                       the primary beam is gaussian
C
C                                       rotate by BPA
                           XXT = XX*COS(PRBP) - YY*SIN(PRBP)
                           YYT = XX*SIN(PRBP) + YY*COS(PRBP)
                           UU = (XXT/PRBMA)**2 + (YYT/PRBMI)**2
                           ISTEP = IROUND(UU /ASTEP) + 1
                           IF (ISTEP .LE. NSTEP) THEN
                              PFACT = PRBEAM(ISTEP)
                           ELSE
                              PFACT = 0
                              END IF
                           END IF
C
                        IF (BTYPE .EQ. 5) THEN
C                                       the primary beam is variable
C                                       gaussian. No correction here.
C                                       Correct later at QXXPTS
                           PFACT = 1.0
                           END IF
C                                       muliply by beam
                        BUFF1(3072+JNCOMP) = BUFF1(3072+JNCOMP)*PFACT
                        END IF
C                                       Gaussian
                     IF (MODTYP.EQ.1) THEN
C                                       Convert to convenient
C                                       coefficients.
                         CPA = COS (DG2RAD*RECORD(PAKOL))
                         SPA = SIN (DG2RAD*RECORD(PAKOL))
                         XMAJ = RECORD(MAJKOL) * CONST2
                         XMIN = RECORD(MINKOL) * CONST2
                         BUFF2(JNCOMP) = - (((CPA * XMAJ)**2) +
     *                      (SPA * XMIN)**2)
                         BUFF2(1024+JNCOMP) = - (((SPA * XMAJ)**2)
     *                      + (CPA * XMIN)**2)
                         BUFF2(2048+JNCOMP) = - 2.0 * CPA * SPA *
     *                      (XMAJ*XMAJ - XMIN*XMIN)
                        END IF
C                                       Sphere
                     IF (MODTYP.EQ.3) THEN
                        BUFF1(3072+JNCOMP) = 3.0 * BUFF1(3072+JNCOMP)
                        BUFF2(JNCOMP) = RECORD(MAJKOL) * 0.109662271
                        BUFF2(1024+JNCOMP) = 0.1
                        END IF
                     END IF
 110              CONTINUE
C                                       Load components
 120           IF (JNCOMP.GT.0) THEN
                  LMCOMP = JNCOMP
                  MMCOMP = MMCOMP + LMCOMP
C                                       Load into AP
                  IAPBUF = 10
C                                       x component
                  CALL QPUT (BUFF1, IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 1
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
C                                       y component
                  CALL QPUT (BUFF1(1025), IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 2
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
C                                       z component
                  CALL QPUT (BUFF1(2049), IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 3
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
C                                       Flux density
                  CALL QPUT (BUFF1(3073), IAPBUF, LMCOMP, 2)
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPCT, LLNMOD, LMCOMP)
                  CALL QWR
C                                       Gaussian
                  IF (MODTYP.EQ.1) THEN
C                                       Coef 1.
                     CALL QPUT (BUFF2, IAPBUF, LMCOMP, 2)
                     IAPTMP = IAPCT + 4
                     CALL QWD
                     CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                     CALL QWR
C                                       Coef 2.
                     CALL QPUT (BUFF2(1025), IAPBUF, LMCOMP, 2)
                     IAPTMP = IAPCT + 5
                     CALL QWD
                     CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                     CALL QWR
C                                       Coef 3.
                     CALL QPUT (BUFF2(2049), IAPBUF, LMCOMP, 2)
                     IAPTMP = IAPCT + 6
                     CALL QWD
                     CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                     CALL QWR
                     END IF
C                                       Sphere
                  IF (MODTYP.EQ.3) THEN
C                                       Radius
                     CALL QPUT (BUFF2, IAPBUF, LMCOMP, 2)
                     IAPTMP = IAPCT + 4
                     CALL QWD
                     CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                     CALL QWR
C                                       Minimum argument
                     CALL QPUT (BUFF2(1025), IAPBUF, LMCOMP, 2)
                     IAPTMP = IAPCT + 5
                     CALL QWD
                     CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                     CALL QWR
                     END IF
                  IAPCT = IAPCT + (LLNMOD * LMCOMP)
                  END IF
C                                       Check if finished field.
               IF ((JCOMP.GT.NCLNG(LFIELD)) .OR. DONE(LFIELD))
     *            GO TO 140
 130           CONTINUE
C                                       Close CLEAN components file.
 140        CALL TABIO ('CLOS', 0, JCOMP, RECORD, IBUFF, IRET)
C                                       Update field sub. count.
            NSUBG(LFIELD) = JCOMP
C                                       Check if need another field.
            IF (JT.LT.NCOMP) GO TO 70
C                                      Check no. comps.
 150        IF ((MMCOMP.EQ.0) .AND. (NCALL.EQ.1)) THEN
               ERRTXT = 'NO POINT COMPONENTS FOUND'
               IRET = 1
               GO TO 990
               END IF
C                                       No comps on later pass is OK
            IF (MMCOMP.LE.0) GO TO 510
C                                       Load correlator factors
            CALL GETCTL (CATR, FACT2, IRET)
            IF (IRET.NE.0) GO TO 999
         ELSE
C                                       Else, single component model.
            CALL RFILL (LENMOD, 0.0, BUFF1)
            BUFF1(2) = PTRAOF * CONST / 3600.
            BUFF1(3) = PTDCOF * CONST / 3600.
C                                       Point
            IF (MODTYP.EQ.0) BUFF1(1) = PTFLX
C                                       Gaussian
            IF (MODTYP.EQ.1) THEN
C                                       Convert to convenient
C                                       coefficients.
               CPA = COS (DG2RAD * PARMOD(4))
               SPA = SIN (DG2RAD * PARMOD(4))
               XMAJ = PARMOD(2) * CONST2 * 2.77777778E-4
               XMIN = PARMOD(3) * CONST2 * 2.77777778E-4
               BUFF1(5) = -(((CPA * XMAJ)**2) + (SPA * XMIN)**2)
               BUFF1(6) = -(((SPA * XMAJ)**2) + (CPA * XMIN)**2)
               BUFF1(7) = -2.0 *  CPA * SPA * (XMAJ*XMAJ - XMIN*XMIN)
               BUFF1(1) = PTFLX
               END IF
C                                       Uniform sphere
            IF (MODTYP.EQ.3) THEN
               BUFF1(5) = PARMOD(2) * 0.109662271 * 2.7777778E-4
               BUFF1(1) = PTFLX * 3.0
               BUFF1(6) = 0.1
               END IF
            MMCOMP = 1
            CALL QPUT (BUFF1, IAPCC, LLNMOD, 2)
            IAPCT = IAPCC + LLNMOD
C                                       Set Stokes' for point model
            FACT2(1) = 1.0
            FACT2(2) = 1.0
            IF (ICOR0.LT.0) THEN
C                                       RR,LL etc.
C                                       Q?
               IF (KSTOK.EQ.2) THEN
                  FACT2(1) = 1.0
                  FACT2(2) = 1.0
                  VOFF = (3 - ABS (ICOR0)) * INCS
               ELSE IF (KSTOK.EQ.3) THEN
                  FACT2(1) = 1.0
                  FACT2(2) = -1.0
                  VOFF = (3 - ABS (ICOR0)) * INCS
               ELSE IF (KSTOK.EQ.4) THEN
                  FACT2(1) = 1.0
                  FACT2(2) = -1.0
                  VOFF = 0
                  END IF
            ELSE
C                                       True Stokes'
               IF ((KSTOK.GE.2) .AND. (KSTOK.LE.4)) THEN
                  FACT2(1) = 1.0
                  FACT2(2) = 0.0
                  VOFF = (KSTOK - ICOR0) * INCS
                  END IF
               END IF
            NSTOK = 1
            IF ((CATR(KRCIC+JLOCS).LT.0.0) .AND. (NCOR.GE.2)) NSTOK = 2
            IF ((NSTOK.EQ.2) .AND. (ABS (ICOR0).EQ.2)) NSTOK = 1
C                                       End if not single comp. model
            END IF
C                                       Correct for FACGRD
         NNCOR = NSTOK
         FACT2(1) = FACT2(1) * FACGRD(1)
         FACT2(2) = FACT2(2) * FACGRD(1)
         WRK = MCHAN + 2
         UUU = 1.0
         IF (FACGRD(2).EQ.0.0) UUU = 0.0
         CALL QPUT (UUU, WRK+2, 1, 2)
         CALL QPUT (FACT2, WRK, NNCOR, 2)
         SFLAG = 1
C                                       Set flag for UPOL and RL,LR data
         IF ((KSTOK.EQ.3) .AND. (ICOR0.LT.0)) SFLAG = -1
C                                       Set vis pointer
         VIS = IDATA + LRPARM + (CHANEL-1) * JNCF + (VOFF/INCS)*JNCS
C                                       Fill frequency table
         BUFF2(1) = (FREQG(CHANEL) / FREQ) - 1.0D0
         IF (NCHAN.GT.1) THEN
            DO 175 I = 2,NCHAN
               INDEX = CHANEL + I - 1
               BUFF2(I) = (FREQG(INDEX) / FREQ) - 1.0D0
 175           CONTINUE
            END IF
         CALL QWD
         CALL QPUT (BUFF2, 2, MCHAN, 2)
         CALL QWAIT
C                                       Init for read & write
C                                       visibility file
C                                       Init vis file for write
         ERRTXT = 'INIT-FOR-READ VIS FILE'
         CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LREC, LENBU,
     *      JBUFSZ, BUFF1, BO, IBIND, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Init vis file for read.
         ERRTXT = 'INIT-FOR-WRITE VIS FILE'
         CALL UVINIT ('WRIT', LUNO, FINDO, NVIS, VO, LREC, LENBU,
     *      JBUFSZ, BUFF2, BO, KBIND, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Subtract model from vis data.
C                                       Loop:  Read vis. record.
         FIRST = .TRUE.
C                                       identify the first block
 200        CONTINUE
            ERRTXT = 'READING VIS FILE'
            CALL UVDISK ('READ', LUNI, FINDI, BUFF1, INIO2, IBIND, IRET)
            INIO = INIO2
            IF (IRET.NE.0) GO TO 990
C                                       Exit if no more data
            IF (INIO.LE.0) GO TO 300
C                                       Uncompress Vis and put in AP
            CALL BUFPUT (INIO, NRPARM, LREC, LLREC, IDATA, BUFF1(IBIND),
     *           IBUFF(1))
C                                       Do the arithmetic: divide
            IF (DIVIDE) THEN
C                                       Point
               IF (MODTYP.EQ.0) CALL QPTDIV (IAPCC, UV, VIS, LLREC,
     *            JNCF, JNCS, MMCOMP, INIO, MCHAN, NNCOR)
C                                       Gaussian
               IF (MODTYP.EQ.1) CALL QGADIV (IAPCC, UV, VIS, LLREC,
     *            JNCF, JNCS, MMCOMP, INIO, MCHAN, NNCOR)
C                                       Sphere
               IF (MODTYP.EQ.3) CALL QSPDIV (IAPCC, UV, VIS, LLREC,
     *            JNCF, JNCS, MMCOMP, INIO, MCHAN, NNCOR)
C                                       Subtract
            ELSE
C                                       Point
               IF (MODTYP.EQ.0) CALL QXXPTS (IAPCC, UV, VIS, LLREC,
     *            JNCF, JNCS, MMCOMP, INIO, MCHAN, NNCOR, SFLAG)
C                                       The next block is not first
               FIRST = .FALSE.
C                                       Gaussian
               IF (MODTYP.EQ.1) CALL QGASUB (IAPCC, UV, VIS, LLREC,
     *            JNCF, JNCS, MMCOMP, INIO, MCHAN, NNCOR, SFLAG)
C                                       Sphere
               IF (MODTYP.EQ.3) CALL QSPSUB (IAPCC, UV, VIS, LLREC,
     *            JNCF, JNCS, MMCOMP, INIO, MCHAN, NNCOR, SFLAG)
               END IF
            CALL QWR
C                                       Get UVs from AP (maybe pack UV)
            CALL BUFGET (INIO, NRPARM, LREC, LLREC, IDATA, BUFF2(KBIND),
     *         IBUFF(1))
C                                       Write vis record.
            NIOUT = INIO
            ERRTXT = 'WRITING VIS FILE'
            CALL UVDISK ('WRIT', LUNO, FINDO, BUFF2, NIOUT, KBIND, IRET)
            IF (IRET.NE.0) GO TO 990
C                                       Check if time for % done
C                                       message.
            PCLST = PCLST + NIOUT
            IPCDNE = (100.0 / NTIMES) * (PCLST / PCTOT) + 0.5
            IPCDNE = IPCDNE - MOD (IPCDNE, 10)
C                                       Write % done message.
            IF ((IPCDNE.GT.IPCLST) .AND. (DOMSG)) THEN
               WRITE (MSGTXT,1240) IPCDNE
               IF (IPCDNE.LE.100) CALL MSGWRT (2)
               IPCLST = IPCDNE
               END IF
C                                       Check if time for AP roller
            CALL QROLL (IAPCT, IBUFF, JBUFSZ, IRET)
            IF (IRET.NE.0) GO TO 999
            GO TO 200
C                                       Finish write
 300     CALL QRLSE
         NIOUT = 0
         ERRTXT = 'FLUSHING VIS FILE'
         CALL UVDISK ('FLSH', LUNO, FINDO, BUFF2, NIOUT, KBIND, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Update no. comp. left.
         XNCOMP = XNCOMP - MXCMP
C                                       Check if done.
         IF (XNCOMP.LE.0.01) GO TO 510
C                                       Input now output.
C                                       Close old input
         CALL ZCLOSE (LUNI, FINDI, IRET)
C                                       Set new input file name.
         IF (DISKO.LE.0) THEN
            VOL = SCRVOL(CNOSCO)
            CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCO), 1, NAME, IRET)
         ELSE
            VOL = DISKO
            CALL ZPHFIL ('UV', VOL, CNOSCO, 1, NAME, IRET)
            END IF
C                                       Open new input file.
         ERRTXT = 'OPEN-FOR-READ VIS FILE'
         CALL ZOPEN (LUNI, FINDI, VOL, NAME, T, F, T, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Open vis file for write.
C                                       Set output file name.
C                                       End big loop, N Comps per pass
 500     CONTINUE
C                                       Close files
 510  CALL ZCLOSE (LUNI, FINDI, IRET)
      CALL ZCLOSE (LUNO, FINDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  WRITE(MSGTXT,2000,ERR=999) IRET, ERRTXT
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1002 FORMAT ('VISDFT: ILLEGAL MODEL TYPE =',I3)
 1090 FORMAT ('VISDFT: ERROR',I5,' READING CLEAN COMPS REC',I5)
 1240 FORMAT ('Model computation is ',I5,' percent complete')
 2000 FORMAT ('VISDFT: ERROR',I5,' ',A)
      END
      SUBROUTINE PLNGET (IDISK, ICNO, CORN, JWIN, XOFF, YOFF, NOSCR,
     *   NX, NY, BUFF1, BUFF2, BUFSZ1, BUFSZ2, LUN1, LUN2, IRET)
C-----------------------------------------------------------------------
C! reads subimage of a plane and writes it to scratch file with shifts
C# Map-util
C-----------------------------------------------------------------------
C   This private version allows to multiply the model by the
C   primary beam
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
      REAL   XINCR, YINCR, XPIX, YPIX, DTHETA, PFACT, UU,
     *   XTETA, YTETA, XX, YY
      INTEGER   ICOLS, IROW, ISTEP, IROUND
C
      CHARACTER PHNAME*48, IFILE*48
      INTEGER   IERR, WIN(4), FIND1, FIND2, BIND1, BIND2, BO, RBO, I4,
     *   IFIRST, ILAST, IOUT, KORN(7), IADD, INDEX, LIM, IOFF, LIM1, MX,
     *   MY, JOFF1, JOFF2, LIMIT, NUM, OFFX, OFFY, IWIN(4), MMX, MMY
      LOGICAL   T, F
      INCLUDE 'PRBM.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
C
      INTEGER   CATBLK(256)
      REAL      CATR(256)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
C
      INCLUDE 'INCS:PSTD.INC'
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
C                                       increments and ref. point from
C                                       header
      XINCR = CATR(KRCIC) * DG2RAD
      YINCR = CATR(KRCIC+1) * DG2RAD
      XPIX = CATR(KRCRP)
      YPIX = CATR(KRCRP+1)
C                                       Make sure there are NO blanks
      IF (CATR(KRBLK).NE.0.0) THEN
         IRET = 3
         WRITE (MSGTXT,1020)
         GO TO 990
         END IF
C                                       Determine mapsize
      MX = CATBLK(KINAX)
      MY = CATBLK(KINAX+1)
C                                       Check defaults on IWIN
      CALL COPY (4, JWIN, IWIN)
      IF ((MX.GT.NX) .AND. ((IWIN(1).EQ.0) .OR. (IWIN(3).EQ.0))) THEN
         IWIN(1) = (MX/2+1) - (NX/2) - OFFX
         IWIN(3) = (MX/2+1) + (NX/2-1) - OFFX
         OFFX = 0
         END IF
      IF ((MY.GT.NY) .AND. ((IWIN(2).EQ.0) .OR. (IWIN(3).EQ.0))) THEN
         IWIN(2) = (MY/2+1) - (NY/2) - OFFY
         IWIN(4) = (MY/2+1) + (NY/2-1) - OFFY
         OFFY = 0
         END IF
      IF (IWIN(1).LE.0) IWIN(1) = 1
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
      IF (IERR.NE.0) THEN
         IRET = 9
         WRITE (MSGTXT,1045) IERR
         GO TO 990
         END IF
C                                       Set window for output.
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
C                                        Make sure input window .le.
C                                        output file size.
      IF ((NX.LT.MMX) .OR. (NY.LT.MMY)) THEN
         IRET = 10
         WRITE (MSGTXT,1050) NX, NY, MMX, MMY
         GO TO 990
         END IF
      INDEX = - OFFY
      IF (INDEX.LT.0) INDEX = MMY - OFFY
      WIN(2) = INDEX + 1
C                                       If OFFX positive
      LIM = OFFX
      JOFF1 = MMX - OFFX - 1
      JOFF2 = - OFFX - 1
      IADD = (NX - MMX) / 2.0 + 0.6
C                                       If OFFX .LE. 0
      IF (OFFX.LE.0) THEN
         LIM = MMX + OFFX
         JOFF1 = - OFFX - 1
         JOFF2 = - MMX - OFFX - 1
         END IF
      JOFF1 = JOFF1 + IADD
      JOFF2 = JOFF2 + IADD
      LIMIT = MMX
      LIM = MIN (LIM, LIMIT)
      LIM1 = LIM + 1
C                                       Open output map file.
      CALL ZPHFIL ('SC', SCRVOL(NOSCR), SCRCNO(NOSCR), 1, PHNAME, IERR)
      CALL ZOPEN (LUN2, FIND2, SCRVOL(NOSCR), PHNAME, T, T, F, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
C                                       Open input file.
      CALL ZPHFIL ('MA', IDISK, ICNO, 1, IFILE, IERR)
      CALL ZOPEN (LUN1, FIND1, IDISK, IFILE, T, T, F, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1065) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       Init files.
      CALL MINIT ('READ', LUN1, FIND1, MX, MY, IWIN, BUFF1, BUFSZ1, BO,
     *   IERR)
      IF (IERR.NE.0) THEN
         IRET = 5
         WRITE (MSGTXT,1070) IERR
         GO TO 970
         END IF
      CALL MINIT ('WRIT', LUN2, FIND2, NX, NY, WIN, BUFF2, BUFSZ2, RBO,
     *   IERR)
      IOUT = WIN(2) - 1
      IF (IERR.NE.0) THEN
         IRET = 6
         WRITE (MSGTXT,1080) IERR
         GO TO 970
         END IF
C                                       Finally do what you're here for
      DO 200 I4 = 1,NY
         IOUT = IOUT + 1
C                                       Restart at first of output file
C                                       Finish write.
         IF (IOUT.GT.NY) THEN
            CALL MDISK ('FINI', LUN2, FIND2, BUFF2, BIND2, IERR)
            IF (IERR.NE.0) THEN
               IRET = 8
               WRITE (MSGTXT,1090) IERR, NY
               GO TO 970
               END IF
            INDEX = 1
            WIN(4) = WIN(2) - 1
            WIN(2) = 1
            CALL MINIT ('WRIT', LUN2, FIND2, NX, NY, WIN, BUFF2, BUFSZ2,
     *         RBO, IERR)
            IOUT = WIN(2)
            IF (IERR.NE.0) THEN
               IRET = 6
               WRITE (MSGTXT,1080) IERR
               GO TO 970
               END IF
            END IF
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
         IF ((IOUT.GE.IFIRST) .AND. (IOUT.LE.ILAST)) THEN
C                                       Read map row.
            CALL MDISK ('READ', LUN1, FIND1, BUFF1, BIND1, IERR)
            IF (IERR.NE.0) THEN
               IRET = 7
               WRITE (MSGTXT,1105) IERR, I4
               GO TO 970
               END IF
C                                       Move to output buffer.
            IROW = IOUT - IFIRST + 1
            IF (DOPRBM) THEN
               YTETA = (IROW - YPIX) * YINCR - DECSH
               DO 120 ICOLS = 1, MX
                  XTETA = (ICOLS - XPIX) * XINCR - RASH
                  IF (BTYPE.LE.3) THEN
C                                       the primary beam is related with
C                                       the dish ilumination

C                                       the distance of the given
C                                       component from the shifted
C                                       center in radians

                     DTHETA = SQRT(XTETA*XTETA+YTETA*YTETA)
                     UU = DANTT / LAMBDA * SIN(DTHETA)
                     ISTEP = IROUND(UU /ASTEP) + 1
                     IF (ISTEP .LE. NSTEP) THEN
                        PFACT = PRBEAM(ISTEP)
                     ELSE
                        PFACT = 0
                        END IF
                     END IF

                  IF (BTYPE .EQ. 4) THEN
C                                       the primary beam is gaussian
C
C                                       rotate by BPA
                     XX = XTETA*COS(PRBP) - YTETA*SIN(PRBP)
                     YY = XTETA*SIN(PRBP) + YTETA*COS(PRBP)
                     UU = (XX/PRBMA)**2 + (YY/PRBMI)**2
                     ISTEP = IROUND(UU /ASTEP) + 1
                     IF (ISTEP .LE. NSTEP) THEN
                        PFACT = PRBEAM(ISTEP)
                     ELSE
                        PFACT = 0
                        END IF
                     END IF
C                                       muliply by beam
                  BUFF1(BIND1+ICOLS-1) = BUFF1(BIND1+ICOLS-1)*PFACT
  120             CONTINUE
               END IF
            IOFF = BIND2 + JOFF1
            CALL RCOPY (LIM, BUFF1(BIND1), BUFF2(IOFF+1))
            IF (LIM.LT.LIMIT) THEN
               IOFF = BIND2 + JOFF2
               NUM = LIMIT - LIM1 + 1
               CALL RCOPY (NUM, BUFF1(BIND1+LIM1-1), BUFF2(IOFF+LIM1))
               END IF
            END IF
 200     CONTINUE
C                                       Finish write.
      CALL MDISK ('FINI', LUN2, FIND2, BUFF2, BIND2, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         WRITE (MSGTXT,1090) IERR, NY
         END IF
C                                       Close real map file.
 970  IF (IRET.NE.0) CALL MSGWRT (8)
      IF (FIND1.GT.0) CALL ZCLOSE (LUN1, FIND1, IERR)
C                                       Close integer map file.
 980  IF (FIND2.GT.0) CALL ZCLOSE (LUN2, FIND2, IERR)
      GO TO 999
C                                       message only
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PLNGET: ERROR',I3,' READING INPUT CATBLK')
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
      SUBROUTINE GRDAT (DOXY, IFIELD, CATR, BUFF1, IRET)
C-----------------------------------------------------------------------
C   GRDAT gets information about a CLEAN components file for GRDSUB.
C   Checks to see if data in XY order.
C   The system version prepares the shifts between the model center
C   and the center given at input parameters (RA=0, DEC= APARM(3)).
C   The first version of the private GRDAT excludes the shift at all.
C   The second version prepares the shift of the map with the center
C   given at input parameters (RA=0, DEC= APARM(3)). The shift is
C   given at the input parameters RASHIFT, DECSHIFT
C   Input:
C      DOXY     L        If true then check sort order ('X*')
C      IFIELD   I        Current field number.  <0 => beam -IFIELD
C      CATR     R(256)   The uv data file catalog header record.
C   Input from commons:
C      RA       D        RA of uv data. (deg.)
C      DEC      D        Declination of uv data.
C      DO3DIM   L        T => images produced by reprojecting u,v,w
C   Output:
C      BUFF1    R(256)   Working buffer.
C      IRET     I        Return error code, 0=>OK, otherwise failed.
C   Output in common /MAPDES/
C      FLDSZ    I(2,*)   Size of the CLEAN map field.
C      NXUNF    I        Dimension (cells) of the map in RA to be used
C                        to determine uniform wt. counting box if 0
C                        when called.
C      NYUNF    I        Dimensions (cells) of the map in Dec to be
C                        used to determine uniform wt. counting box if
C                        0 when called.
C      CELLSG   R(2)     Cellsize of the CLEAN maps (asec.)
C      SCLUG    R(*)     Scaling to cells in u coordinate.
C      SCLVG    R(*)     Scaling to cells in v coordinate.
C      SCLWG    R(*)     Scaling to cells in w coordinate.
C      SCLUM    R        Scaling to cells in u for NXUNF
C      SCLVM    R        Scaling to cells in v for NYUNF
C      DXCG     R(*)     Position offset parameter in x (RA) corrected
C                        to uv data set rotation
C      DYCG     R(*)     Position offset parameter in y (dec) corrected
C                        to uv data set rotation
C      DZCG     R(*)     Position offset parameter in z.
C                        DXCG, DYCG, DZCG is equal zero in the local
C                        version of the GRDAT
C      XSHIFT   R(*)     X shift arc sec
C      YSHIFT   R(*)     Y shift in arc sec
C      XPOFF    R(*)     Pixel offset in x for clean components to
C                        field phase center from tang. point. (deg.)
C      YPOFF    R(*)     Pixel offset in Y for clean components to
C                        field phase center from tang. point. (deg.)
C      XFLD     R(*)     Field of view in X (RA) in seconds.
C      YFLD     R(*)     Field of view in Y (dec) in seconds.
C      CCROT    R        Cosine of position angle difference between
C                        orientation of uv data and CLEAN image times
C                        constant for coordinate conversion.
C      SSROT    R        Sine of position angle difference between
C                        orientation of uv data and CLEAN image.
C                        CCROT = 1, SSROT = 0 in the local
C                        version of the GRDAT
C      OSFX     R        Over sampling factors in X
C      OSFY     R        Over sampling factors in Y
C      ICNTRX   I(*)     Center cell number in X
C      ICNTRY   I(*)     Center cell number in Y
C      KLNBLK   I(256)   Catalog header record of current CLEAN field.
C-----------------------------------------------------------------------
      LOGICAL   DOXY
      INTEGER   IFIELD, BUFF1(*), IRET
      REAL      CATR(256)
C
      CHARACTER CHTEMP*8
      HOLLERITH CLNH(256)
      INTEGER   NX, NY, JFIELD, KVOL, KCNO, IRAOFF, IDECOF, IERR, NCHK,
     *   INDEX, LFIELD
      LOGICAL   DOUBX, DOUBY
      REAL      CLNR(256)
      DOUBLE PRECISION CLND(128), CLNRA, CLNDEC, RAX, DECX
      INCLUDE 'PRBM.INC'
      INCLUDE 'INCS:PSTD.INC'
C      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KLNBLK, CLNR, CLND, CLNH)
C-----------------------------------------------------------------------
C                                       If beam do field 1
      JFIELD = IFIELD
      IF (IFIELD.LE.0) THEN
         JFIELD = 1
         LFIELD = MAX (1, -IFIELD)
         MSGTXT = 'GRDAT: beam defines image - probably should not'
         CALL MSGWRT (6)
      ELSE
         JFIELD = IFIELD
         LFIELD = 1
         END IF
C                                       Check sort order in /UVHDR/.
C                                       Wrong sort order.
      IF ((ISORT(1:1).NE.'X') .AND. (DOXY)) THEN
         IRET = 9
         WRITE (MSGTXT,1000) ISORT, 'X*'
         GO TO 990
         END IF
      IF (IFIELD.LE.0) THEN
         KVOL = BEMVOL(LFIELD)
         KCNO = CNOBEM(LFIELD)
      ELSE
         KVOL = CCDISK(JFIELD)
         KCNO = CCCNO(JFIELD)
         END IF
      CALL CATIO ('READ', KVOL, KCNO, KLNBLK, 'REST', BUFF1, IRET)
      IF ((IRET.NE.0) .AND. (IRET.LE.4)) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Get field information
C                                       Drop hopefully obsolete fix
C     CALL FXSHFT (KLNBLK, IERR)
C                                       Field size
      FLDSZ(1,JFIELD) = KLNBLK(KINAX)
      FLDSZ(2,JFIELD) = KLNBLK(KINAX+1)
C                                       Uniform weighting size.
      IF (NXUNF.LE.0) NXUNF = KLNBLK(KINAX)
      IF (NYUNF.LE.0) NYUNF = KLNBLK(KINAX+1)
C                                       Cellsize
      CELLSG(1) = CLNR(KRCIC) * 3.6E3
      CELLSG(2) = CLNR(KRCIC+1) * 3.6E3
C                                       Over sampling factor.
      OSFX = 1.0
      OSFY = 1.0
C                                       Double if possible
      DOUBX = FLDSZ(1,JFIELD).LE.2048
      DOUBY = FLDSZ(2,JFIELD).LE.2048
      IF (DOUBX) OSFX = 2.0
      IF (DOUBY) OSFY = 2.0
      NX = FLDSZ(1,JFIELD)
      NY = FLDSZ(2,JFIELD)
C                                       Field of view
      XFLD(JFIELD) = ABS (CELLSG(1)) * NX
      YFLD(JFIELD) = ABS (CELLSG(2)) * NY
C                                       Set map center.
      ICNTRX(JFIELD) = NX / 2
      ICNTRY(JFIELD) = NY / 2 + 1
C                                       u,v,w scaling
      SCLUG(JFIELD) = NX * ABS (CELLSG(1)) * DG2RAD / 3600.0D0
      SCLVG(JFIELD) = -NY * CELLSG(2) * DG2RAD / 3600.0D0
      SCLWG(JFIELD) = 1.0
      SCLUM = NXUNF * ABS (CELLSG(1)) * DG2RAD / 3600.0D0
      SCLVM = -NYUNF * CELLSG(2) * DG2RAD / 3600.0D0
C                                       Find "CLEAN" RA and Dec axes.
      INDEX = JLOCR * 2
      CALL H2CHR (8, 1, CATR(KHCTP+INDEX), CHTEMP)
      IRAOFF = 0
      NCHK = 4
      IF (CHTEMP(1:4).EQ.'RA  ') NCHK = 2
      CALL AXEFND (NCHK, CHTEMP, KICTPN, CLNH(KHCTP), IRAOFF, IERR)
      INDEX = JLOCD * 2
      CALL H2CHR (8, 1, CATR(KHCTP+INDEX), CHTEMP)
      IDECOF = 1
      NCHK = 4
      IF (CHTEMP(1:4).EQ.'DEC ') NCHK = 3
      CALL AXEFND (NCHK, CHTEMP, KICTPN, CLNH(KHCTP), IDECOF, IERR)
C                                       Get position offsets of phase
C                                       centers.
      IF (DO3DIM) THEN
C                                       reference declination of the
C                                       model should be equal 0
         CLNRA = 0
C                                       reference declination of the
C                                       model should be equal APARM(3)
         CLNDEC = DELT
         RAX = RA
         DECX = DEC
      ELSE
C                                       reference declination of the
C                                       model should be equal 0
         CLNRA = 0 + CLNR(KRXSH)
C                                       reference declination of the
C                                       model should be equal APARM(3)
         CLNDEC = DELT + CLNR(KRYSH)
         RAX = RA + CATR(KRXSH)
         DECX = DEC + CATR(KRYSH)
         END IF
C                                       Prepare for shift and rotate
C                                       of model.
      CALL ROTFND (CLNR, MAPROT, IRET)
      CALL ROTFND (CATR, UVROT, IRET)
C                                       Field offset of the UV data
C                                       relatively desired
C                                       center of the model in arcsec
C   The following 7 cards are instead of SHFTXY.FOR
      IF (DOSHFT) THEN
         XSHIFT(JFIELD) = -RASHS
         YSHIFT(JFIELD) = -DECSHS
      ELSE
         XSHIFT(JFIELD) = 0.0
         YSHIFT(JFIELD) = 0.0
         END IF
C                                       set shift terms for field cent.
C                                       -NCP projection
      IF (TYPUVD.EQ.-1) THEN
         CALL SHINCP (RAX, DECX, MAPROT, CLNRA, CLNDEC, DXCG(JFIELD),
     *      DYCG(JFIELD), DZCG(JFIELD))
C                                       -SIN projection
      ELSE
         CALL SHISIN (RAX, DECX, MAPROT, CLNRA, CLNDEC, DXCG(JFIELD),
     *      DYCG(JFIELD), DZCG(JFIELD))
         END IF
C                                       Get reference pixel offsets from
C                                       tangent point
      XPOFF(JFIELD) = (CLNR(KRCRP+IRAOFF) - KLNBLK(KINAX+IRAOFF)/2)
     *   * CLNR(KRCIC+IRAOFF)
      YPOFF(JFIELD) = (CLNR(KRCRP+IDECOF) - KLNBLK(KINAX+IDECOF)/2 - 1)
     *   * CLNR(KRCIC+IDECOF)
C                                       No rotation in the local
C                                       version of the GRDAT
      CCROT = 1
      SSROT = 0
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UV DATA IN WRONG SORT ORDER =''',A2,''' NOT ''',A2,'''')
 1010 FORMAT ('GRDAT: ERROR',I5,' READING CATBLK ')
      END
      SUBROUTINE TWON (NPIX, IERR)
C-----------------------------------------------------------------------
C   This subroutine put IERR=0 if NPIX.EQ.2**N and
C   put IERR=1 if NPIX.NE.2**N
C
C   Inputs:
C    NPIX     I    Given integer number
C   Outputs:
C    IERR     I    Error: IF (NPIX.EQ.2**N) IERR = 0
C                         ELSE IERR = 1
C-----------------------------------------------------------------------
      INTEGER NPIX, IERR, IDIV, LDIV, IREST
C-----------------------------------------------------------------------
      IERR = 0
      LDIV = NPIX
   10 IDIV = LDIV / 2.0
      IREST = LDIV - 2 * IDIV
      IF (IREST .NE. 0) THEN
         IERR = 1
         GO TO 999
      ELSE
         LDIV = IDIV
         IF (IDIV .NE. 1) GO TO 10
         END IF
C
 999  RETURN
      END
C
      SUBROUTINE UVCINT (NLINE2, IAN, ELEVA, TSYS, EFFEL,
     *   IANTN, ELEVAT, TINTER, EINTER, IERR)
C-----------------------------------------------------------------------
C   Routine interpolates the given elevation of the given antenna
C   based on the givenarray of elevation, TSYS, and Efficiency
C   INPUTS
C      NLINE2    I    number of elements at the following arrays
C      IAN(*)    I    array of antenna numbers
C      ELEVA(*)  R    array of elevations, in degrees
C      TSYS(*)   R    array of system temperatures, in degrees
C      EFFEL(*)  R    array of antenna efficiencies
C      IANTN     I    Antenna number
C      ELEVAT    R    Elevation of the antenna
C   Output:
C      TINTER    R    Interpolated TSYS for the given IANTN, ELEVAT
C      EINTER    R    Interpolated Efficiensy for the given IANTN, ELEVAT
C      IERR      I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   NLINE2, IAN(*), IANTN, IERR, I
      REAL      ELEVA(*), TSYS(*), EFFEL(*), ELEVAT, TINTER, EINTER,
     *          ELEFT, ERIGHT, ELEVI, TSYSL, TSYSR, EFFR, EFFL
C-----------------------------------------------------------------------
      IERR = 10
C                                       Interpolate TSYS and Efficiency
C                                       using the data obtained from
C                                       the input arrays
      ELEFT = 0.0
      ERIGHT = 100.0
      DO 100 I = 1, NLINE2
         ELEVI = ELEVA(I)
C                                       select antenna
         IF (IANTN.EQ.IAN(I)) THEN
C                                       find the nearest (to ELEVAT)
C                                       right elevation and the relevant
C                                       TSYS and Eficiency
            IF ((ELEVI.GT.ELEVAT) .AND. (ELEVI.LT.ERIGHT)) THEN
               ERIGHT = ELEVI
               TSYSR = TSYS(I)
               EFFR = EFFEL(I)
               END IF
C                                       find the nearest (to ELEVAT)
C                                       left elevation and the relevant
C                                       TSYS and Eficiency
            IF ((ELEVI.LT.ELEVAT) .AND. (ELEVI.GT.ELEFT)) THEN
               ELEFT = ELEVI
               TSYSL = TSYS(I)
               EFFL = EFFEL(I)
               END IF
C                                       At least one line with IANTN
C                                       has been found
            IERR = 0
            END IF
 100     CONTINUE

C                                       No line with antenna IANTN
      IF (IERR .EQ. 10) GO TO 999
C
      IF (ERIGHT.EQ.100) THEN
         IF (ELEFT.EQ.0) THEN
            TINTER = 0
            EINTER = 0
         ELSE
            TINTER = TSYSL
            EINTER = EFFL
            END IF
      ELSE
         IF (ELEFT.EQ.0) THEN
            TINTER = TSYSR
            EINTER = EFFR
         ELSE
C                                       make the interpolation itself
            TINTER = TSYSL +
     *         (TSYSR-TSYSL) * (ELEVAT-ELEFT) / (ERIGHT-ELEFT)
            EINTER = EFFL +
     *         (EFFR - EFFL) * (ELEVAT-ELEFT) / (ERIGHT-ELEFT)
            END IF
         END IF
 999  RETURN
      END
